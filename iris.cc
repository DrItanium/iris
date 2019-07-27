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

namespace iris {



DestinationRegister
Core::getDestinationRegister(RegisterIndex idx) noexcept {
    return _regs[static_cast<Byte>(idx)];
}
SourceRegister
Core::getSourceRegister(RegisterIndex idx) const noexcept {
    return _regs[static_cast<Byte>(idx)];
}
DoubleRegister
Core::getDoubleRegister(RegisterIndex lower, RegisterIndex upper) noexcept {
    return DoubleRegister::make(_regs, lower, upper);
}

const DoubleRegister
Core::getDoubleRegister(RegisterIndex lower, RegisterIndex upper) const noexcept {
    return DoubleRegister::make(_regs, lower, upper);
}


DoubleWord
Core::loadCode(Address addr) {
    return _code[addr];
}
void
Core::storeCode(Address addr, DoubleWord w) {
    _code[addr] = w;
}

void 
Core::storeData(Address addr, Word value) {
    _data[addr] = value;
}
Word
Core::loadData(Address addr) {
    return _data[addr];
}


Word
IOMemoryBank::load(Address address) {
    return _storage[address].read(_core);
}
void
IOMemoryBank::store(Address address, Word value) {
    _storage[address].write(_core, value);
}
void
IOMemoryBank::mapIntoMemory(Address address, MMIOReadFunction read, MMIOWriteFunction write) {
    _storage[address] = LambdaMMIOEntry(read, write);
}
void
IOMemoryBank::mapIntoMemory(Address address, MMIOEntry& entry) {
    _storage[address] = CaptiveMMIOEntry(entry);
}
void
IOMemoryBank::mapIntoMemory(Address addr, MMIOReadFunction r) {
    mapIntoMemory(addr, r, LambdaMMIOEntry::illegalWriteError);
}
void
IOMemoryBank::mapIntoMemory(Address addr, MMIOWriteFunction w) {
    mapIntoMemory(addr, LambdaMMIOEntry::illegalReadError, w);
}
void
IOMemoryBank::mapIntoMemory(Address addr, std::tuple<MMIOReadFunction, MMIOWriteFunction> t) {
    auto [ r, w ] = t;
    mapIntoMemory(addr, r, w);
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
MMIOEntry::write(Core&, Word) {
    throw MemoryStoreException("illegal io write!");
}

Word
MMIOEntry::read(Core&) {
    throw MemoryLoadException("illegal io read!");
    return 0;
}

void
LambdaMMIOEntry::write(Core& c, Word value) {
    _write(c, value);
}

Word
LambdaMMIOEntry::read(Core& c) {
    return _read(c);
}

void
CaptiveMMIOEntry::write(Core& c, Word value) {
    _other.write(c, value);
}

Word
CaptiveMMIOEntry::read(Core& c) {
    return _other.read(c);
}

LambdaMMIOEntry::LambdaMMIOEntry(MMIOReadFunction read, MMIOWriteFunction write) : _read(read), _write(write) { }
CaptiveMMIOEntry::CaptiveMMIOEntry(MMIOEntry& capture) : _other(capture) { }

void
Core::terminateCycle() {
    _executing = false;
}

void
Core::installIOMemoryMap(const IOMemoryMap& map) {
    _io.installMemoryMap(map);
}
void
IOMemoryBank::installMemoryMap(const IOMemoryMap& map) {
    for (const auto& entry : map) {
        auto [ addr, value ] = entry;
        std::visit([this, addr](auto&& value) { mapIntoMemory(addr, value); }, value);
    }
}
void
IOMemoryBank::mapIntoMemory(Address baseAddress, ComplexMemoryMapping fn) {
    fn(*this, baseAddress);
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


Word
Core::loadIO(Address addr) {
    return _io.load(addr);
}

void
Core::storeIO(Address addr, Word value) {
    _io.store(addr, value);
}


QuadRegister
Core::getQuadRegister(RegisterIndex start) noexcept {
    return QuadRegister::make(_regs, start);
}

const QuadRegister
Core::getQuadRegister(RegisterIndex start) const noexcept {
    return QuadRegister::make(_regs, start);
}



} // end namespace iris
