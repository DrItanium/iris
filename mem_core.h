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
#include "mem_bank.h"

namespace iris {
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
        Ordinal getTerminateCell() const noexcept override { return _terminateCell; }
        Ordinal getIP() const noexcept override { return _ip.get<Word>(); }
        void setIP(Ordinal value) noexcept override { _ip.put(value); }
        void resetExecutionStatus() noexcept override { _executing = true; }
        bool getExecutingStatus() const noexcept override { return _executing; }
        void setTerminateCell(Ordinal value) noexcept override { _terminateCell = value; }
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
        Register _ip;
        bool _executing = false;
        bool _advanceIP = true;
        Word _terminateCell = 0;

};
} // end namespace iris

#endif // end IRIS_MEMCORE_H__
