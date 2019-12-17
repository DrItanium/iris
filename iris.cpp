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

Core::Core() noexcept {
}




void
Core::cycle() {
    // load an instruction from the current instruction pointer
    invoke(loadCode(this->getIP()));
    if (shouldAdvanceIP()) {
        advanceIP();
    }
    allowAdvanceIP();
}

void
Core::run() {
    resetExecutionStatus();
    do {
        cycleHandler();
    } while (getExecutingStatus());
}


void
Core::terminateCycle() {
    stopExecution();
}

void
Core::terminateCore(Core& c, Word code) {
    c.terminateCycle();
    c.setTerminateCell(code);
}
Word
Core::readTerminateCell(Core& c) {
    return c.getTerminateCell();
}

void 
Core::invoke(LongOrdinal ibits) {
    switch (Instruction inst(ibits); inst.getOpcode()) {
#define X(t, opcode) case opcode : \
        invoke<opcode>(inst); \
        break;
#include "InstructionFormats.def"
#undef X
        default:
            raiseBadOperation();
    }
}


} // end namespace iris
