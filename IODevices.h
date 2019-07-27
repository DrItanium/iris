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
#include "types.h"
#include "exceptions.h"
namespace iris {

class Core;
using MMIOWriteFunction = std::function<void(Core&, Word)>;
using MMIOReadFunction = std::function<Word(Core&)>;
struct MMIOEntry {
    public:
        MMIOEntry() = default;
        virtual ~MMIOEntry() = default;
        virtual void write(Core&, Word);
        virtual Word read(Core&);

};
struct LambdaMMIOEntry : MMIOEntry {
    public:
        static void illegalWriteError(Core&, Word) {
            throw MemoryStoreException("illegal io write!");
        }
        static Word illegalReadError(Core&) {
            throw MemoryLoadException("illegal io read!");
            return 0;
        }
    public:
        LambdaMMIOEntry(MMIOReadFunction read = illegalReadError, MMIOWriteFunction write = illegalWriteError);
        virtual ~LambdaMMIOEntry() = default;
        void write(Core&, Word value) override;
        Word read(Core&) override;
    private:
        MMIOReadFunction _read;
        MMIOWriteFunction _write;

};
struct CaptiveMMIOEntry : MMIOEntry {
    public:
        CaptiveMMIOEntry(MMIOEntry& other);
        virtual ~CaptiveMMIOEntry() = default;
        void write(Core&, Word) override;
        Word read(Core&) override;
    private:
        MMIOEntry& _other;
};
class IOMemoryBank;
using ComplexMemoryMapping = std::function<void(IOMemoryBank&, Address)>;
using MemoryMapEntryKind = std::variant<MMIOEntry,
      std::tuple<MMIOReadFunction, MMIOWriteFunction>,
      MMIOReadFunction,
      MMIOWriteFunction,
      ComplexMemoryMapping>;
using MemoryMapEntry = std::tuple<Address, MemoryMapEntryKind>;
/**
 * Description of the io memory map to be installed into IO memory
 */
using IOMemoryMap = std::map<Address, MemoryMapEntryKind>;
/**
 * the MMIO space that is exposed to the program, one registers functions at
 * addresses into the space. Writing to an address which is not registers
 * results in nothing happening. Reading from an address will return all ones.
 */
class IOMemoryBank {
    public:
        using MMIOTable = NumericalStorageBank<MMIOEntry, MemoryBankElementCount>;
    public:
        IOMemoryBank(Core& c) : _core(c) { }
        ~IOMemoryBank() = default;
        Word load(Address);
        void store(Address, Word);
        void mapIntoMemory(Address, std::tuple<MMIOReadFunction, MMIOWriteFunction>);
        void mapIntoMemory(Address, MMIOReadFunction);
        void mapIntoMemory(Address, MMIOWriteFunction);
        void mapIntoMemory(Address, MMIOReadFunction, MMIOWriteFunction);
        void mapIntoMemory(Address, MMIOEntry&);
        void mapIntoMemory(Address, ComplexMemoryMapping);
        void installMemoryMap(const IOMemoryMap&);
    private:
        Core& _core;
        IOMemoryBank::MMIOTable _storage;

};
} // end namespace iris
#endif // end IRIS_IODEVICES_H__

