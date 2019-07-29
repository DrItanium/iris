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
RandomNumberGenerator::RandomNumberGenerator() : _seed(std::mt19937_64::default_seed), _current(0), _rng() { }

ComplexMemoryMapping
RandomNumberGenerator::mapping() {
    return [this](IOMemoryBank& bank, Address baseOffset) {
        // mapping is as follows
        bank.installMemoryMap({ 
                { baseOffset, seedLowest() },
                { baseOffset + 1, seedLower() },
                { baseOffset + 2, seedHigher() }, 
                { baseOffset + 3, seedHighest() },
                { baseOffset + 4, seedCommit() },
                { baseOffset + 5, rngLowest() },
                { baseOffset + 6, rngLower() },
                { baseOffset + 7, rngHigher() },
                { baseOffset + 8, rngHighest() },
                { baseOffset + 9, rngNext() },
                });

    };
}
MemoryCellAction 
RandomNumberGenerator::seedLowest() {
    return std::make_tuple([this](Core&) { return Word(_seed); },
                           [this](Core&, Word value) { 
                           _seed = encodeBits<UnsignedQuadWord, Word, 
                                              0x0000'0000'0000'FFFF,
                                              0>(_seed, value); 
                                              });
}
MemoryCellAction 
RandomNumberGenerator::seedLower() {
    return std::make_tuple([this](Core&) { return Word(_seed >> 16); },
                           [this](Core&, Word value) { 
                           _seed = encodeBits<UnsignedQuadWord, Word, 
                                              0x0000'0000'FFFF'0000,
                                              16>(_seed, value); 
                                              });
}
MemoryCellAction 
RandomNumberGenerator::seedHigher() {
    return std::make_tuple([this](Core&) { return Word(_seed >> 32); },
                           [this](Core&, Word value) { 
                           _seed = encodeBits<UnsignedQuadWord, Word, 
                                              0x0000'FFFF'0000'0000,
                                              32>(_seed, value); 
                                              });
}
MemoryCellAction 
RandomNumberGenerator::seedHighest() {
    return std::make_tuple([this](Core&) { return Word(_seed >> 48); },
                           [this](Core&, Word value) { 
                           _seed = encodeBits<UnsignedQuadWord, Word, 
                                              0xFFFF'0000'0000'0000,
                                              48>(_seed, value); 
                                              });
}
MemoryCellAction
RandomNumberGenerator::seedCommit() {
    return std::make_tuple([this](Core&) { return 0; },
                           [this](Core&, Word) {
                                _rng.seed(_seed);
                           });
}
#if 0
MemoryCellAction 
RandomNumberGenerator::rngLowest() {
}
MemoryCellAction 
RandomNumberGenerator::rngLower() {
}
MemoryCellAction 
RandomNumberGenerator::rngHigher() {
}
MemoryCellAction 
RandomNumberGenerator::rngHighest() {
}
MemoryCellAction
RandomNumberGenerator::rngCommit() {
    return std::make_tuple([this](Core&) { return 0; },
                           [this](Core& c, Word cmd) {
                                switch (cmd) {
                                    case 0: // next
                                        _current = _rng();
                                        break;
                                    case 1:
                                        _rng.discard(c.c

                                        
                                }
                           }
}
#endif
} // end namespace iris
