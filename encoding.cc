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
using CompareOperation = std::function<Bits(RegisterIndex, RegisterIndex)>;

MultiInstructionExpression
getDivRemainder(RegisterIndex quotient, RegisterIndex remainder, 
                RegisterIndex numerator, RegisterIndex denominator) noexcept {
    return std::make_tuple(opDivide(quotient, numerator, denominator, OrdinalOperation()),
                           opRemainder(remainder, numerator, denominator, OrdinalOperation()));
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
    addInstruction(onTrue);
    thenComponent();
}
void
MultiInstructionExpression::ifStatement(RegisterIndex cond, Body onTrue, Body onFalse) {
    ifComponent(cond);
    addInstruction(onTrue);
    elseComponent();
    addInstruction(onFalse);
    thenComponent();
}
void
MultiInstructionExpression::addInstruction(Body b) {
    b(*this);
}


} // end namespace iris::instructions
