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
Core::invoke(const std::monostate&) {
    throw UnimplementedOperationException();
}

void
Core::invoke(const iris::ErrorInstruction&) {
    throw ErrorInstructionException();
}

void
Core::invoke(const iris::BranchSelectInstruction& s) {
    // BranchSelect ConditionalRegister TrueAddress FalseAddress
    auto [ cond, onTrue, onFalse] = s.arguments();
    _ip.put(getRegisterValue(getRegisterValue<bool>(cond) ? onTrue : onFalse));
    _advanceIP = false;
}

void
Core::invoke(const iris::BranchConditionalImmediateInstruction& s) {
    auto [ cond, to ] = s.arguments();
    if (getRegisterValue<bool>(cond)) {
        _ip.put(to);
        _advanceIP = false;
    }
}

void
Core::invoke(const iris::BranchConditionalRegisterAndLinkInstruction& s) {
    auto [ dest, cond, link ] = s.arguments();
    if (getRegisterValue<bool>(cond)) {
        setRegisterValue(link, _ip.get() + 1);
        _ip.put(getRegisterValue(dest));
        _advanceIP = false;
    }
}

void
Core::invoke(const iris::BranchRegisterInstruction& s) {
    auto [ dest ] = s.arguments();
    _ip.put(getRegisterValue(dest));
    _advanceIP = false;
}

void
Core::invoke(const iris::BranchRegisterAndLinkInstruction& s) {
    auto [ address, link ] = s.arguments();
    setRegisterValue(link, _ip.get() + 1);
    _ip.put(getRegisterValue(address));
    _advanceIP = false;
}

void
Core::invoke(const iris::BranchConditionalRegisterInstruction& s) {
    if (auto [ dest, cond ] = s.arguments(); getRegisterValue<bool>(cond)) {
        _ip.put(getRegisterValue<Word>(dest));
        _advanceIP = false;
    }
}
void
Core::invoke(const iris::BranchConditionalRelativeImmediateInstruction& s) {
    if (auto [ cond, offset ] = s.arguments(); getRegisterValue<bool>(cond)) {
        _ip.put(_ip.get<SignedWord>() + offset);
        _advanceIP = false;
    }
}

} // end namespace iris
