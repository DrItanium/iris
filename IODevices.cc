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
#include "IODevices.h"

namespace iris {

Word
IOMemoryBank::load(Address address) {
    if (!addressClaimed(address)) {
        throw MMIOException("Address ", address, " is unmapped, cannot read from it");
    }
    return _storage[address]->read(_core);
}
void
IOMemoryBank::store(Address address, Word value) {
    if (!addressClaimed(address)) {
        throw MMIOException("Address ", address, " is unmapped, cannot write to it");
    }
    _storage[address]->write(_core, value);
}
void
IOMemoryBank::mapIntoMemory(Address address, MMIOReadFunction read, MMIOWriteFunction write) {
    return emplaceIntoMemory<LambdaMMIOEntry>(address, read, write);
}
void
IOMemoryBank::mapIntoMemory(Address address, MMIOEntry& entry) {
    return emplaceIntoMemory<CaptiveMMIOEntry>(address, entry);
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
MMIOEntry::write(Core&, Word) {
    throw MemoryStoreException("illegal io write!");
}

Word
MMIOEntry::read(Core&) {
    throw MemoryLoadException("illegal io read!");
    return 0;
}

} // end namespace iris
