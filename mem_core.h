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
#ifndef IRIS_MEMCORE_H__
#define IRIS_MEMCORE_H__
#include <array>
#include <functional>
#include <map>
#include "iris.h"
#include "IODevices.h"

namespace iris {
/**
 * Generic template for defining a memory bank of 2^16 elements.
 * @tparam T the type of each memory bank cell
 */
template<typename T>
using MemoryBank = NumericalStorageBank<std::enable_if_t<std::is_integral_v<T>, T>, MemoryBankElementCount>;
/**
 * Separate memory space that holds the instructions the iris core executes;
 * DoubleWords are stored in this location.
 */
using CodeMemoryBank = MemoryBank<LongOrdinal>;

/**
 * Location to store data values in an iris core; 2^16 words worth of storage
 * is provided by default.
 */
using DataMemoryBank = MemoryBank<Ordinal>;
/**
 * Iris comes equipped with a full 2^16 words worth of stack space that is
 * accessed by separate instructions. It is only possible to do pushes and pops
 * to stack memory. However, the number of stack pointers is only limited by
 * the number of registers.
 */
using StackMemoryBank = MemoryBank<Ordinal>;
/** 
 * Use ram to denote the different memory spaces
 */
class InMemoryCore : public Core {
    public:
        InMemoryCore() noexcept;
        ~InMemoryCore() override = default;
        void mapIntoIOSpace(Address, std::tuple<MMIOReadFunction, MMIOWriteFunction>);
        void mapIntoIOSpace(Address, MMIOReadFunction);
        void mapIntoIOSpace(Address, MMIOWriteFunction);
        void mapIntoIOSpace(Address, MMIOReadFunction, MMIOWriteFunction);
    protected:
        LongOrdinal loadFromCodeMemory(Address addr) override;
        Ordinal loadFromDataMemory(Address addr) override;
        Ordinal loadFromStackMemory(Address addr) override;
        Ordinal loadFromIOMemory(Address addr) override;
        void storeToCodeMemory(Address addr, LongOrdinal value) override;
        void storeToDataMemory(Address addr, Ordinal value) override;
        void storeToStackMemory(Address addr, Ordinal value) override;
        void storeToIOMemory(Address addr, Ordinal value) override;
    private:
        CodeMemoryBank _code;
        DataMemoryBank _data;
        StackMemoryBank _stack;
        IOMemoryBank _io;

};
} // end namespace iris

#endif // end IRIS_MEMCORE_H__
