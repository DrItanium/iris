/**
 * @copyright 
 * iris
 * Copyright (c) 2013-2019, Joshua Scoggins and Contributors
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "encoding.h"
#include <variant>
#include <functional>

namespace iris::instructions {
#define X(title, fmt) \
    UnsignedDoubleWord \
    title ( const title ## Instruction & s) noexcept {\
        return ((Instruction)s).getRawBits(); \
    }
#include "InstructionFormats.def"
#undef X
Bits
zeroRegister(RegisterIndex t) noexcept {
    return MemoryAssignRegisterImmediate({t, 0x0000_imm16});
}
Bits
swap(RegisterIndex a, RegisterIndex b) noexcept {
    return MemorySwapRegisters({a, b});
}
Bits
nop(RegisterIndex idx) noexcept {
    return MemorySwapRegisters({idx, idx});
}
Bits
twoTimes(RegisterIndex dest, RegisterIndex src) noexcept {
    return ArithmeticShiftLeftUnsignedImmediate({dest, src, 1});
}
Bits
twoDivide(RegisterIndex dest, RegisterIndex src) noexcept {
    return ArithmeticShiftRightUnsignedImmediate({dest, src, 1});
}
Bits 
ret(RegisterIndex link) noexcept {
    return iris::instructions::BranchRegister(link);
}

Bits 
twoTimes(RegisterIndex targetRegister) noexcept {
    return twoTimes(targetRegister, targetRegister);
}
Bits 
twoDivide(RegisterIndex value) noexcept {
    return twoDivide(value, value);
}
Bits 
invert(RegisterIndex dest, RegisterIndex src) noexcept {
    return ArithmeticBitwiseNot({dest, src});
}
Bits 
invert(RegisterIndex dest) noexcept { 
    return invert(dest, dest); 
}
Bits 
square(RegisterIndex dest, RegisterIndex src) noexcept {
    return ArithmeticMultiplySigned({dest, src, src });
}
Bits 
square(RegisterIndex dest) noexcept {
    return square(dest, dest);
}
Bits 
greaterThanZero(RegisterIndex dest, RegisterIndex src) noexcept {
    return CompareGreaterThanSignedImmediate8({dest, src, 0});
}
Bits 
lessThanZero(RegisterIndex dest, RegisterIndex src) noexcept {
    return CompareLessThanSignedImmediate8({dest, src, 0});
}
Bits 
equalsZero(RegisterIndex dest, RegisterIndex src) noexcept {
    return CompareEqualsImmediate8({dest, src, 0});
}
Bits 
notEqualsZero(RegisterIndex dest, RegisterIndex src) noexcept {
    return CompareNotEqualsImmediate8({dest, src, 0});
}
Bits 
increment(RegisterIndex target) noexcept {
    return ArithmeticAddUnsignedImmediate({target, target, 1});
}
Bits 
decrement(RegisterIndex target) noexcept {
    return ArithmeticAddSignedImmediate({target, target, -1});
}
Bits
greaterThanOrEqualToZero(RegisterIndex dest, RegisterIndex src) noexcept {
    return CompareGreaterThanOrEqualToSignedImmediate8({dest, src, 0});
}
Bits
lessThanOrEqualToZero(RegisterIndex dest, RegisterIndex src) noexcept {
    return CompareLessThanOrEqualToSignedImmediate8({dest, src, 0});
}
auto 
branchIfNotZero(RegisterIndex src, AddressTypes loc) noexcept {
    return std::visit([src](auto&& addr) { return branchConditional(src, addr); }, loc);
}
using CompareOperation = std::function<Bits(RegisterIndex, RegisterIndex)>;
auto
branchIfCompareZero(RegisterIndex temp, RegisterIndex src,
        AddressTypes dest,
        CompareOperation fn) noexcept {
    return std::make_tuple(fn(temp, src),
                           std::visit([temp](auto&& value) { return branchConditional(temp, value); }, dest));
}
auto
branchIfZero(RegisterIndex cond, RegisterIndex src0, AddressTypes addr) noexcept {
    return branchIfCompareZero(cond, src0, addr, equalsZero);
}
auto
branchIfGreaterThanZero(RegisterIndex temp, RegisterIndex src, AddressTypes dest) noexcept {
    return branchIfCompareZero(temp, src, dest, greaterThanZero);
}

auto
branchIfLessThanZero(RegisterIndex temp, RegisterIndex src, AddressTypes dest) noexcept {
    return branchIfCompareZero(temp, src, dest, lessThanZero);
}

auto
branchIfGreaterThanOrEqualToZero(RegisterIndex temp, RegisterIndex src, AddressTypes loc) noexcept {
    return branchIfCompareZero(temp, src, loc, greaterThanOrEqualToZero);
}
auto
branchIfLessThanOrEqualToZero(RegisterIndex temp, RegisterIndex src, AddressTypes loc) noexcept {
    return branchIfCompareZero(temp, src, loc, lessThanOrEqualToZero);
}

Bits
select(RegisterIndex cond, RegisterIndex then, RegisterIndex _else) noexcept {
    return BranchSelect({cond, then, _else});
}

auto
selectCompareWithZero(RegisterIndex cond, RegisterIndex src, RegisterIndex then, RegisterIndex _else, 
        std::function<Bits(RegisterIndex, RegisterIndex)> compareWithZero) noexcept {
    return std::make_tuple(compareWithZero(cond, src), 
                           select(cond, then, _else));
}

auto
selectIfZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
    return selectCompareWithZero(cond, src0, then, _else, equalsZero);
}
auto
selectIfNotZero(RegisterIndex, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
    return select(src0, then, _else);
}
auto
selectIfGreaterThanZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
    return selectCompareWithZero(cond, src0, then, _else, greaterThanZero);
}
auto
selectIfLessThanZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
    return selectCompareWithZero(cond, src0, then, _else, lessThanZero);
}

auto
selectIfGreaterThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
    return selectCompareWithZero(cond, src0, then, _else, greaterThanOrEqualToZero);
}
auto
selectIfLessThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
    return selectCompareWithZero(cond, src0, then, _else, lessThanOrEqualToZero);
}

Bits
pop(RegisterIndex sp, RegisterIndex dest) noexcept {
    return MemoryStackPop({sp, dest});
}

Bits
storeData(RegisterIndex addr, RegisterIndex value, UnsignedByte offset) noexcept {
    return MemoryDataStoreWithOffset({addr, value, offset});
}
Bits
storeData(RegisterIndex addr, Address value) noexcept {
    return MemoryDataStoreImmediateValue({addr, value});
}

Bits
loadData(RegisterIndex addr, RegisterIndex dest, UnsignedByte offset) noexcept {
    return MemoryDataLoadWithOffset({addr, dest, offset});
}
Bits
storeIO(RegisterIndex addr, RegisterIndex value, UnsignedByte offset) noexcept {
    return MemoryIOStoreWithOffset({addr, value, offset});
}
Bits
storeIO(RegisterIndex addr, Address value) noexcept {
    return MemoryIOStoreImmediateValue({addr, value});
}

Bits
loadIO(RegisterIndex addr, RegisterIndex dest, UnsignedByte offset) noexcept {
    return MemoryIOLoadWithOffset({addr, dest, offset});
}

Bits
loadCode(RegisterIndex addr, RegisterIndex lower, UnsignedByte offset) noexcept {
    return MemoryCodeLoadWithOffset({addr, lower, offset});
}
Bits
storeCode(RegisterIndex addr, RegisterIndex lower, UnsignedByte offset) noexcept {
    return MemoryCodeStoreWithOffset({addr, lower, offset});
}
Bits
loadCodeThenIncrement(RegisterIndex addr, RegisterIndex lower, UnsignedByte amount) noexcept {
    // always have to decrement the amount by one 
    return MemoryCodeLoadAndIncrement({addr, lower, --amount});
}
Bits
storeCodeThenIncrement(RegisterIndex addr, RegisterIndex lower, UnsignedByte amount) noexcept {
    return MemoryCodeStoreAndIncrement({addr, lower, --amount});
}
Bits
loadCodeThenDecrement(RegisterIndex addr, RegisterIndex lower, UnsignedByte amount) noexcept {
    // always have to decrement the amount by one 
    return MemoryCodeLoadAndDecrement({addr, lower, --amount});
}
Bits
storeCodeThenDecrement(RegisterIndex addr, RegisterIndex lower, UnsignedByte amount) noexcept {
    return MemoryCodeStoreAndDecrement({addr, lower, --amount});
}

Bits
getIP(RegisterIndex r) noexcept {
    return MemoryMoveFromIP({r});
}
Bits
setIP(RegisterIndex r) noexcept {
    return MemoryMoveToIP({r});
}

Bits
bitwiseNot(RegisterIndex dest, RegisterIndex src) noexcept {
    return ArithmeticBitwiseNot({dest, src});
}

#define X(kind) \
    Bits \
    bitwise ## kind ( RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { \
        return ArithmeticBitwise ## kind ( { dest, src0, src1 } ); \
    }
X(And);
X(Or);
X(Nor);
X(Nand);
X(Xor);
#undef X




} // end namespace iris::instructions
