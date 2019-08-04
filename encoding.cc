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
MultiInstructionExpression
branchIfNotZero(RegisterIndex src, AddressTypes loc) noexcept {
    return std::visit([src](auto&& addr) { return branchConditional(src, addr); }, loc);
}
using CompareOperation = std::function<Bits(RegisterIndex, RegisterIndex)>;
MultiInstructionExpression
branchIfCompareZero(RegisterIndex temp, RegisterIndex src,
        AddressTypes dest,
        CompareOperation fn) noexcept {
    return std::make_tuple(fn(temp, src),
                           std::visit([temp](auto&& value) { return branchConditional(temp, value); }, dest));
}
MultiInstructionExpression
branchIfZero(RegisterIndex cond, RegisterIndex src0, AddressTypes addr) noexcept {
    return branchIfCompareZero(cond, src0, addr, equalsZero);
}
MultiInstructionExpression
branchIfGreaterThanZero(RegisterIndex temp, RegisterIndex src, AddressTypes dest) noexcept {
    return branchIfCompareZero(temp, src, dest, greaterThanZero);
}

MultiInstructionExpression
branchIfLessThanZero(RegisterIndex temp, RegisterIndex src, AddressTypes dest) noexcept {
    return branchIfCompareZero(temp, src, dest, lessThanZero);
}

MultiInstructionExpression
branchIfGreaterThanOrEqualToZero(RegisterIndex temp, RegisterIndex src, AddressTypes loc) noexcept {
    return branchIfCompareZero(temp, src, loc, greaterThanOrEqualToZero);
}

MultiInstructionExpression
branchIfLessThanOrEqualToZero(RegisterIndex temp, RegisterIndex src, AddressTypes loc) noexcept {
    return branchIfCompareZero(temp, src, loc, lessThanOrEqualToZero);
}

Bits
select(RegisterIndex cond, RegisterIndex then, RegisterIndex _else) noexcept {
    return BranchSelect({cond, then, _else});
}

MultiInstructionExpression
selectCompareWithZero(RegisterIndex cond, RegisterIndex src, RegisterIndex then, RegisterIndex _else, 
        std::function<Bits(RegisterIndex, RegisterIndex)> compareWithZero) noexcept {
    return std::make_tuple(compareWithZero(cond, src), 
                           select(cond, then, _else));
}

MultiInstructionExpression
selectIfZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
    return selectCompareWithZero(cond, src0, then, _else, equalsZero);
}
MultiInstructionExpression
selectIfNotZero(RegisterIndex, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
    return select(src0, then, _else);
}
MultiInstructionExpression
selectIfGreaterThanZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
    return selectCompareWithZero(cond, src0, then, _else, greaterThanZero);
}
MultiInstructionExpression
selectIfLessThanZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
    return selectCompareWithZero(cond, src0, then, _else, lessThanZero);
}

MultiInstructionExpression
selectIfGreaterThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
    return selectCompareWithZero(cond, src0, then, _else, greaterThanOrEqualToZero);
}
MultiInstructionExpression
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


MultiInstructionExpression
halt(RegisterIndex temp, Address code) noexcept {
    return std::make_tuple(zeroRegister(temp), storeIO(temp, code));
}
MultiInstructionExpression
cube(RegisterIndex dest, RegisterIndex src, RegisterIndex temporary) noexcept {
    return std::make_tuple(square(temporary, src), 
            opMultiplyUnsigned(dest, src, temporary));
}

MultiInstructionExpression
getDivRemainder(RegisterIndex quotient, RegisterIndex remainder, 
                RegisterIndex numerator, RegisterIndex denominator) noexcept {
    return std::make_tuple(opDivideUnsigned(quotient, numerator, denominator),
                           opRemainderUnsigned(remainder, numerator, denominator));
}

MultiInstructionExpression
indirectLoadData(RegisterIndex dest, RegisterIndex addr, UnsignedByte offset) noexcept {
    return std::make_tuple(loadData(dest, addr, offset), 
            loadData(dest, dest));
}
MultiInstructionExpression
indirectStoreData(RegisterIndex dest, RegisterIndex addr, RegisterIndex temporary, UnsignedByte offset) noexcept {
    return std::make_tuple(loadData(temporary, dest, offset),
            storeData(temporary, addr));
}

void
MultiInstructionExpression::addInstruction(Bits b) {
    _instructions.emplace_back(b);
}
void
MultiInstructionExpression::addInstruction(DelayedBits b) {
    _instructions.emplace_back(b);
}
void
MultiInstructionExpression::addInstruction(ExternalDelayedBits b) {
    _instructions.emplace_back(b);
}
void
MultiInstructionExpression::defer(DelayedBits b) {
    addInstruction(b);
    mark();
}
void
MultiInstructionExpression::addInstruction(ComplexBinaryInstruction tup) {
    addInstruction(std::get<0>(tup));
    addInstruction(std::get<1>(tup));
}

void
MultiInstructionExpression::addInstruction(MultiInstructionExpression&& other) {
    _instructions.insert(_instructions.cend(), other.begin(), other.end());
}

void
MultiInstructionExpression::resolve() {
    auto location = dataPop();
    if (auto& scopeMarker = _instructions.at(location); std::holds_alternative<DelayedBits>(scopeMarker)) {
        // shove the bits in place of the Instruction itself as we are done at this point
        _instructions.at(location).emplace<Bits>(std::get<DelayedBits>(scopeMarker)(location));
    } else if (std::holds_alternative<ExternalDelayedBits>(scopeMarker)) {
        _instructions.at(location).emplace<Bits>(std::get<ExternalDelayedBits>(scopeMarker)(*this, location));
    }
}

MultiInstructionExpression::MultiInstructionExpression(Bits b) { addInstruction(b); }
MultiInstructionExpression::MultiInstructionExpression(ComplexBinaryInstruction b) { addInstruction(b); }
MultiInstructionExpression::MultiInstructionExpression(DelayedBits b) { addInstruction(b); }
MultiInstructionExpression::MultiInstructionExpression(ExternalDelayedBits b) { addInstruction(b); }

void
MultiInstructionExpression::forwardJump() {
    defer([this](auto jumpPos) {
                // jumpPos denotes where this defered instruction lives
                // this must be done before we install the instruction to jump
                // to as it can cause issues with the defer chain
                return branch(size() - jumpPos);
            });
}
void
MultiInstructionExpression::conditionalForwardJump(RegisterIndex cond) {
    defer([this, cond](auto jumpPos) {
                return branchConditional(cond, size() - jumpPos);
            });
}
void
MultiInstructionExpression::forwardJumpTarget() {
   resolve(); // just perform the resolve as we see it now as we had to embed 
}

void
MultiInstructionExpression::backwardJumpTarget() {
    mark();
}
void
MultiInstructionExpression::backwardJump() {
    auto location = dataPop();
    addInstruction(branch(-(size() - location)));
}
void
MultiInstructionExpression::conditionalBackwardJump(RegisterIndex cond) {
    auto location = dataPop();
    addInstruction(branchConditional(cond, -(size() - location)));
}

void
MultiInstructionExpression::dataPush(size_t value) {
    _dataStack.emplace_front(value);
}
size_t
MultiInstructionExpression::dataPop() {
    if (_dataStack.empty()) {
        throw Exception("Data stack empty!");
    }
    size_t top = _dataStack.front();
    _dataStack.pop_front();
    return top;
}

void
MultiInstructionExpression::dataStackSwap() {
    if (_dataStack.size() < 2) {
        throw Exception("Too few elements to perform dataStackSwap");
    }
    auto top = dataPop();
    auto lower = dataPop();
    dataPush(top);
    dataPush(lower);
}

void
MultiInstructionExpression::ifComponent(RegisterIndex idx) {
    addInstruction(equalsZero(idx, idx));
    conditionalForwardJump(idx);
}

void
MultiInstructionExpression::elseComponent() {
    forwardJump(); // embed the new forward jump
    dataStackSwap(); // now swap so that we resolve it after the fact
    forwardJumpTarget();
}
void
MultiInstructionExpression::thenComponent() {
    // now do the forwardJumpTarget
    forwardJumpTarget();
}

void
MultiInstructionExpression::ifStatement(RegisterIndex cond, Body onTrue) {
    ifComponent(cond);
    onTrue(*this);
    thenComponent();
}
void
MultiInstructionExpression::ifStatement(RegisterIndex cond, Body onTrue, Body onFalse) {
    ifComponent(cond);
    onTrue(*this);
    elseComponent();
    onFalse(*this);
    thenComponent();
}


} // end namespace iris::instructions
