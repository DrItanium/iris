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
nop() noexcept {
    return nop(0_r);
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
call(RegisterIndex link, RegisterIndex reg) noexcept {
    return BranchRegisterAndLink({ reg, link });
}
Bits 
call(RegisterIndex link, Address imm16) noexcept {
    return BranchImmediateAndLink({link, imm16});
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
auto 
branchIfNotZero(RegisterIndex, RegisterIndex src, RegisterIndex loc) noexcept {
    return BranchConditionalRegister({loc, src});
}
auto 
branchIfNotZero(RegisterIndex, RegisterIndex src, Address loc) noexcept {
    return BranchConditionalImmediate({src, loc});
}
using CompareOperation = std::function<Bits(RegisterIndex, RegisterIndex)>;
auto
branchIfCompareZero(RegisterIndex temp, RegisterIndex src, RegisterIndex dest, CompareOperation fn) noexcept {
    return std::make_tuple(fn(temp, src), BranchConditionalRegister({dest, temp}));
}
auto
branchIfCompareZero(RegisterIndex temp, RegisterIndex src, Address addr, CompareOperation fn) noexcept {
    return std::make_tuple(fn(temp, src), BranchConditionalImmediate({temp, addr}));
}
auto 
branchIfZero(RegisterIndex temp, RegisterIndex src, RegisterIndex loc) noexcept {
    return branchIfCompareZero(temp, src, loc, equalsZero);
}
auto 
branchIfZero(RegisterIndex temp, RegisterIndex src, Address loc) noexcept {
    return branchIfCompareZero(temp, src, loc, equalsZero);
}
auto
branchIfGreaterThanZero(RegisterIndex temp, RegisterIndex src, RegisterIndex dest) noexcept {
    return branchIfCompareZero(temp, src, dest, greaterThanZero);
}
auto
branchIfGreaterThanZero(RegisterIndex temp, RegisterIndex src, Address dest) noexcept {
    return branchIfCompareZero(temp, src, dest, greaterThanZero);
}

auto
branchIfLessThanZero(RegisterIndex temp, RegisterIndex src, RegisterIndex dest) noexcept {
    return branchIfCompareZero(temp, src, dest, lessThanZero);
}
auto
branchIfLessThanZero(RegisterIndex temp, RegisterIndex src, Address dest) noexcept {
    return branchIfCompareZero(temp, src, dest, lessThanZero);
}

} // end namespace iris::instructions
