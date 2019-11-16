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
#ifndef IRIS_IODEVICES_H__
#define IRIS_IODEVICES_H__
#include <memory>
#include <functional>
#include <variant>
#include <map>
#include "types.h"
#include "exceptions.h"
#include "mem_bank.h"
namespace iris {

class Core;
using MMIOWriteFunction = std::function<void(Core&, Word)>;
using MMIOReadFunction = std::function<Word(Core&)>;
struct MMIOEntry {
    public:
        MMIOEntry() = default;
        virtual ~MMIOEntry() = default;
        virtual void write(Core&, Word) = 0;
        virtual Word read(Core&) = 0;

};
struct LambdaMMIOEntry : public MMIOEntry {
    public:
        static void illegalWriteError(Core&, Word) {
            throw MemoryStoreException("illegal io write!");
        }
        static Word illegalReadError(Core&) {
            throw MemoryLoadException("illegal io read!");
            return 0;
        }
    public:
        LambdaMMIOEntry(MMIOReadFunction read, MMIOWriteFunction write) : _read(read), _write(write) { }
        ~LambdaMMIOEntry() override = default;
        void write(Core& c, Word addr) override { _write(c, addr); }
        Word read(Core& c) override { return _read(c); }
    private:
        MMIOReadFunction _read;
        MMIOWriteFunction _write;

};
struct CaptiveMMIOEntry : public MMIOEntry {
    public:
        CaptiveMMIOEntry(MMIOEntry& other) : _other(other) { }
        ~CaptiveMMIOEntry() override = default;
        void write(Core& c, Word w) override { _other.write(c, w); }
        Word read(Core& c) override { return _other.read(c); }
    private:
        MMIOEntry& _other;
};
/**
 * the MMIO space that is exposed to the program, one registers functions at
 * addresses into the space. Writing to an address which is not registers
 * results in nothing happening. Reading from an address will return all ones.
 */
class IOMemoryBank {
    public:
        using MMIOTable = NumericalStorageBank<std::unique_ptr<MMIOEntry>, MemoryBankElementCount>;
    public:
        IOMemoryBank(Core& c) : _core(c) { }
        ~IOMemoryBank() = default;
        Word load(Address);
        void store(Address, Word);
        bool addressClaimed(Address addr) const noexcept { return static_cast<bool>(_storage[addr]); }
        template<typename T, typename ... Args>
        void emplaceIntoMemory(Address addr, Args&& ... args) {
            if (addressClaimed(addr)) {
                throw MMIOException("Address ", std::hex, addr, " is already claimed by another device!");
            }
            _storage[addr] = std::make_unique<T>(args...);
        }
        void mapIntoMemory(Address addr, std::tuple<MMIOReadFunction, MMIOWriteFunction> tup) {
            auto [ r, w ] = tup;
            emplaceIntoMemory<LambdaMMIOEntry>(addr, r, w);
        }
        void mapIntoMemory(Address addr, MMIOReadFunction read) { mapIntoMemory(addr, read, LambdaMMIOEntry::illegalWriteError); }
        void mapIntoMemory(Address addr, MMIOWriteFunction write) { mapIntoMemory(addr, LambdaMMIOEntry::illegalReadError, write); }
        void mapIntoMemory(Address addr, MMIOReadFunction read, MMIOWriteFunction write) {
            emplaceIntoMemory<LambdaMMIOEntry>(addr, read, write);
        }
        void mapIntoMemory(Address addr, MMIOEntry& entry) { emplaceIntoMemory<CaptiveMMIOEntry>(addr, entry); }
    private:
        Core& _core;
        IOMemoryBank::MMIOTable _storage;

};
} // end namespace iris
#endif // end IRIS_IODEVICES_H__

