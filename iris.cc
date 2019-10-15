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
#include "types.h"
#include "iris.h"
#include "opcodes.h"

namespace iris {

Core::Core() noexcept : _io(*this) {
    // 0-16 are hardwired in r0-r16 (first 17 registers)
    for (int i = 0; i < 17; ++i) {
        _regs[i].hardwireTo(i);
    }
}

Register&
Core::getDestinationRegister(RegisterIndex idx) noexcept {
    return _regs[static_cast<Byte>(idx)];
}
const Register&
Core::getSourceRegister(RegisterIndex idx) const noexcept {
    return _regs[static_cast<Byte>(idx)];
}

DoubleRegister
Core::getDoubleRegister(RegisterIndex lower, RegisterIndex upper) const noexcept {
    return DoubleRegister::make(_regs, lower, upper);
}


void
Core::cycle() {
    // load an instruction from the current instruction pointer
    invoke(loadCode(_ip.get()));
    if (_advanceIP) {
        ++_ip;
    }
    _advanceIP = true;
}

void
Core::run() {
    _executing = true;
    do {
        try {
            cycle();
        } catch (DivideByZeroException& ex) {
            /// @todo try to dispatch to an interrupt vector...
        } catch (Exception& ex) {
            /// @todo implement logic to handle edge cases such as divide by zero and other such handling
        }
    } while (_executing);
}


void
Core::terminateCycle() {
    _executing = false;
}

void
Core::installIOMemoryMap(const IOMemoryMap& map) {
    _io.installMemoryMap(map);
}

void
Core::terminateCore(Core& c, Word code) {
    c.terminateCycle();
    c._terminateCell = code;
}
Word
Core::readTerminateCell(Core& c) {
    return c._terminateCell;
}

void 
Core::invoke(DoubleWord ibits) {
    switch (Instruction inst(ibits); inst.getOpcodeIndex()) {
#define X(t, f) case t ## Instruction :: RawValue : \
        invoke(t ## Instruction(inst)); \
        break;
#include "InstructionFormats.def"
#undef X
        default:
            throw BadOperationException();
    }
}


} // end namespace iris
