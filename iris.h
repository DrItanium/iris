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
#ifndef IRIS_IRIS_H__
#define IRIS_IRIS_H__
#include <cstdint>
#include <variant>
#include <optional>
#include <array>
#include <iostream>
#include <type_traits>
#include <tuple>
#include <cstddef>
#include <string>
#include <functional>
#include <map>
#include <exception>
#include <sstream>
#include "types.h"
#include "exceptions.h"
#include "IODevices.h"
#include "register.h"
namespace iris {
#define X(title , fmt) \
    class title ## Format ;
#include "InstructionFormats.def"
#undef X
class Core {
    public:
        static void terminateCore(Core&, Word);
        static Word readTerminateCell(Core&);
    public:
        Core() : _io(*this) { }
        ~Core() = default;
        void run();
        void installIOMemoryMap(const IOMemoryMap& map);
        void terminateCycle();
    private:
        // use tag dispatch to call the right routines
#define X(title, fmt) \
        void invoke(const title ## Format &);
#include "InstructionFormats.def"
#undef X
    private:
        void cycle();
    private:
        DestinationRegister getDestinationRegister(RegisterIndex idx) noexcept;
        SourceRegister getSourceRegister(RegisterIndex idx) const noexcept;
        DoubleRegister getDoubleRegister(RegisterIndex start, RegisterIndex next) noexcept;
        const DoubleRegister getDoubleRegister(RegisterIndex start, RegisterIndex next) const noexcept;
        void invoke(DoubleWord bits);
        QuadRegister getQuadRegister(RegisterIndex start) noexcept;
        const QuadRegister getQuadRegister(RegisterIndex start) const noexcept;
    private:
        inline const DoubleRegister getDoubleRegister(RegisterIndex start) const noexcept {
            return getDoubleRegister(start, static_cast<RegisterIndex>(static_cast<Byte>(start) + 1));
        }
        inline DoubleRegister getDoubleRegister(RegisterIndex start) noexcept {
            return getDoubleRegister(start, static_cast<RegisterIndex>( static_cast<Byte>(start) + 1));
        }
        inline void incrementRegister(RegisterIndex idx, std::underlying_type_t<RegisterIndex> times = 0) noexcept {
            // it is impossible to actually do zero here!
            if (UnsignedWord actualCount = static_cast<UnsignedWord>(times) + 1; actualCount == 1) {
                ++getDestinationRegister(idx);
            } else {
                setRegisterValue(idx, getRegisterValue(idx) + actualCount);
            }
        }
        inline void decrementRegister(RegisterIndex idx, std::underlying_type_t<RegisterIndex> times = 0) noexcept {
            // it is impossible to actually do zero here!
            if (UnsignedWord actualCount = static_cast<UnsignedWord>(times) + 1; actualCount == 1) {
                --getDestinationRegister(idx);
            } else {
                setRegisterValue(idx, getRegisterValue(idx) - actualCount);
            }
        }
        template<typename T>
        inline void setDoubleRegisterValue(RegisterIndex lower, RegisterIndex upper, T value) noexcept {
            getDoubleRegister(lower, upper).put<T>(value);
        }
        template<typename T>
        inline void setDoubleRegisterValue(RegisterIndex lower, T value) noexcept {
            getDoubleRegister(lower).put<T>(value);
        }
        template<typename T = DoubleWord>
        inline T getDoubleRegisterValue(RegisterIndex lower, RegisterIndex upper) const noexcept {
            return getDoubleRegister(lower, upper).get<T>();
        }
        template<typename T = DoubleWord>
        inline T getDoubleRegisterValue(RegisterIndex lower) const noexcept {
            return getDoubleRegister(lower).get<T>();
        }
        template<typename T>
        inline void setRegisterValue(RegisterIndex idx, T value) noexcept {
            getDestinationRegister(idx).put(value);
        }
        template<typename T = Word>
        inline T getRegisterValue(RegisterIndex idx) const noexcept {
            return getSourceRegister(idx).get<T>();
        }
        constexpr auto getTerminateCell() const noexcept { return _terminateCell; }
    private:
        Word loadIO(Address);
        void storeIO(Address, Word);
        Word loadData(Address);
        void storeData(Address, Word);
        DoubleWord loadCode(Address);
        void storeCode(Address, DoubleWord);
    private:
        RegisterBank _regs;
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


#endif // end IRIS_H__
