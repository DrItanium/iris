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
#include "iris.h"
#include "opcodes.h"

namespace iris {

void
Core::invoke(const iris::ErrorInstruction&) {
    throw ErrorInstructionException();
}

void
Core::invoke(const iris::BranchSelectInstruction& s) {
    // BranchSelect ConditionalRegister TrueAddress FalseAddress
    auto [ cond, onTrue, onFalse] = s.arguments();
    branchTo(getRegisterValue(getRegisterValue<bool>(cond) ? onTrue : onFalse));
}

void
Core::invoke(const iris::BranchConditionalImmediateInstruction& s) {
    if (auto [ cond, to ] = s.arguments(); getRegisterValue<bool>(cond)) {
        branchTo(to);
    }
}

void
Core::invoke(const iris::BranchConditionalRegisterAndLinkInstruction& s) {
    auto [ dest, cond, link ] = s.arguments();
    if (getRegisterValue<bool>(cond)) {
        updateLinkRegister(link);
        branchTo(getRegisterValue(dest));
    }
}

void
Core::invoke(const iris::BranchRegisterInstruction& s) {
    auto [ dest ] = s.arguments();
    branchTo(getRegisterValue(dest));
}

void
Core::invoke(const iris::BranchRegisterAndLinkInstruction& s) {
    auto [ address, link ] = s.arguments();
    updateLinkRegister(link);
    branchTo(getRegisterValue(address));
}

void
Core::invoke(const iris::BranchConditionalRegisterInstruction& s) {
    if (auto [ dest, cond ] = s.arguments(); getRegisterValue<bool>(cond)) {
        branchTo(getRegisterValue<Word>(dest));
    }
}
void
Core::invoke(const iris::BranchConditionalRelativeImmediateInstruction& s) {
    if (auto [ cond, offset ] = s.arguments(); getRegisterValue<bool>(cond)) {
        relativeBranchTo<SignedWord>(offset);
    }
}

} // end namespace iris
