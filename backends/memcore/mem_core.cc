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
#include "mem_core.h"
#include "opcodes.h"

namespace iris {
InMemoryCore::InMemoryCore() noexcept : Core(), _io(*this) {
    // 0-16 are hardwired in r0-r16 (first 17 registers)
    for (int i = 0; i < 17; ++i) {
        _regs[i].hardwireTo(i);
    }
    // basic io reservations the processor will reserve
    _io.mapIntoMemory(0, Core::readTerminateCell, Core::terminateCore);
}
void 
InMemoryCore::mapIntoIOSpace(Address addr, std::tuple<MMIOReadFunction, MMIOWriteFunction> tup) {
    _io.mapIntoMemory(addr, tup);
}
void 
InMemoryCore::mapIntoIOSpace(Address addr, MMIOReadFunction read) {
    _io.mapIntoMemory(addr, read);

}
void 
InMemoryCore::mapIntoIOSpace(Address addr, MMIOWriteFunction write) {
    _io.mapIntoMemory(addr, write);

}
void 
InMemoryCore::mapIntoIOSpace(Address addr, MMIOReadFunction read, MMIOWriteFunction write) {
    _io.mapIntoMemory(addr, read, write);

}

LongOrdinal 
InMemoryCore::loadFromCodeMemory(Address addr) {
    return _code[addr];
}
Ordinal 
InMemoryCore::loadFromDataMemory(Address addr) {
    return _data[addr];
}


Ordinal 
InMemoryCore::loadFromStackMemory(Address addr) {
    return _stack[addr];
}
Ordinal 
InMemoryCore::loadFromIOMemory(Address addr) {
    return _io.load(addr);
}
void 
InMemoryCore::storeToCodeMemory(Address addr, LongOrdinal value) {
    _code[addr] = value;    
}
void 
InMemoryCore::storeToDataMemory(Address addr, Ordinal value) {
    _data[addr] = value;
}

void
InMemoryCore::storeToStackMemory(Address addr, Ordinal value) {
    _stack[addr] = value;
}

void 
InMemoryCore::storeToIOMemory(Address addr, Ordinal value) {
    _io.store(addr, value);
}

void
InMemoryCore::putDoubleRegister(RegisterIndex lower, RegisterIndex upper, LongOrdinal value) noexcept {
    DoubleRegister::make(_regs, lower, upper).put(value);
}

void
InMemoryCore::putDoubleRegister(RegisterIndex lower, LongOrdinal value) noexcept {
    DoubleRegister::make(_regs, lower).put(value);
}

LongOrdinal 
InMemoryCore::retrieveDoubleRegister(RegisterIndex lower, RegisterIndex upper) const noexcept {
    return DoubleRegister::make(_regs, lower, upper).get<LongOrdinal>();
}

LongOrdinal
InMemoryCore::retrieveDoubleRegister(RegisterIndex lower) const noexcept {
    return DoubleRegister::make(_regs, lower).get<LongOrdinal>();
}

void
InMemoryCore::raiseDivideByZero() {
    throw DivideByZeroException();
}

void
InMemoryCore::raiseErrorInstruction() {
    throw ErrorInstructionException();
}

void
InMemoryCore::raiseBadOperation() {
    throw BadOperationException();
}

void
InMemoryCore::cycleHandler() {
    try {
        cycle();
    } catch(DivideByZeroException& ex) {
        /// @todo try to dispatch to an interrupt vector
    } catch(Exception& ex) {
        /// @todo implement logic to handle edge cases
    }
}

} // end namespace iris
