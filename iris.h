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
    public: 

        template<typename T>
        DoubleWord loadCode(T addr) {
            using K = std::decay_t<T>;
            if constexpr (std::is_same_v<K, RegisterIndex>) {
                return _code[getRegisterValue(addr)];
            } else if constexpr (std::is_same_v<K, Address>) {
                return _code[addr];
            } else {
                static_assert(false_v<K>, "Illegal type!");
            }
        }
        inline DoubleWord loadCode(RegisterIndex addr, Address offset) {
            return loadCode(getRegisterValue<Address>(addr, offset));
        }
        inline DoubleWord loadCode(Address addr, Address offset) {
            return loadCode(addr + offset);
        }

        template<typename T>
        Word loadIO(T addr) {
            using K = std::decay_t<T>;
            if constexpr (std::is_same_v<K, RegisterIndex>) {
                return _io.load(getRegisterValue(addr));
            } else if constexpr (std::is_same_v<K, Address>) {
                return _io.load(addr);
            } else {
                static_assert(false_v<K>, "Illegal type!");
            }
        }
        inline Word loadIO(RegisterIndex addr, Address offset) {
            return loadIO(getRegisterValue<Address>(addr, offset));
        }
        inline Word loadIO(Address addr, Address offset) {
            return loadIO(addr + offset);
        }
        template<typename T>
        Word loadStack(T addr) {
            using K = std::decay_t<T>;
            if constexpr (std::is_same_v<K, RegisterIndex>) {
                return _stack[getRegisterValue(addr)];
            } else if constexpr (std::is_same_v<K, Address>) {
                return _stack[addr];
            } else {
                static_assert(false_v<K>, "Illegal type!");
            }
        }
        template<typename T>
        Word loadData(T addr) {
            using K = std::decay_t<T>;
            if constexpr (std::is_same_v<K, RegisterIndex>) {
                return _data[getRegisterValue(addr)];
            } else if constexpr (std::is_same_v<K, Address>) {
                return _data[addr];
            } else {
                static_assert(false_v<K>, "Illegal type!");
            }
        }
        template<typename A, typename V>
        void storeData(A address, V value) {
            if constexpr (std::is_same_v<A, Address>) {
                if constexpr (std::is_same_v<V, Word>) {
                    _data[address] = value;
                } else if constexpr (std::is_same_v<V, RegisterIndex>) {
                    _data[address] = getRegisterValue(value);
                } else {
                    static_assert(false_v<V>, "Bad value kind!");
                }
            } else if constexpr (std::is_same_v<A, RegisterIndex>) {
                auto addr = getRegisterValue(address);
                if constexpr (std::is_same_v<V, Word>) {
                    _data[addr] = value;
                } else if constexpr (std::is_same_v<V, RegisterIndex>) {
                    if (address == value) {
                        _data[addr] = addr;
                    } else {
                        _data[addr] = getRegisterValue(value);
                    }
                } else {
                    static_assert(false_v<V>, "Bad value kind!");
                }
            } else {
                static_assert(false_v<A>, "Bad address kind!");
            }
        }
        template<typename V>
        void storeData(RegisterIndex addr, V value, Address offset) {
            storeData(getRegisterValue<Address>(addr, offset), value);
        }
        template<typename A, typename V>
        void storeStack(A address, V value) {
            if constexpr (std::is_same_v<A, Address>) {
                if constexpr (std::is_same_v<V, Word>) {
                    _stack[address] = value;
                } else if constexpr (std::is_same_v<V, RegisterIndex>) {
                    _stack[address] = getRegisterValue(value);
                } else {
                    static_assert(false_v<V>, "Bad value kind!");
                }
            } else if constexpr (std::is_same_v<A, RegisterIndex>) {
                auto addr = getRegisterValue(address);
                if constexpr (std::is_same_v<V, Word>) {
                    _stack[addr] = value;
                } else if constexpr (std::is_same_v<V, RegisterIndex>) {
                    if (address == value) {
                        _stack[addr] = addr;
                    } else {
                        _stack[addr] = getRegisterValue(value);
                    }
                } else {
                    static_assert(false_v<V>, "Bad value kind!");
                }
            } else {
                static_assert(false_v<A>, "Bad address kind!");
            }
        }
        template<typename A, typename V>
        void storeCode(A address, V value) {
            if constexpr (std::is_same_v<A, Address>) {
                if constexpr (std::is_same_v<V, DoubleWord>) {
                    _data[address] = value;
                } else if constexpr (std::is_same_v<V, RegisterIndex>) {
                    _data[address] = getDoubleRegisterValue(value);
                } else {
                    static_assert(false_v<V>, "Bad value kind!");
                }
            } else if constexpr (std::is_same_v<A, RegisterIndex>) {
                auto addr = getRegisterValue(address);
                if constexpr (std::is_same_v<V, DoubleWord>) {
                    _data[addr] = value;
                } else if constexpr (std::is_same_v<V, RegisterIndex>) {
                    _data[addr] = getDoubleRegisterValue(value);
                } else {
                    static_assert(false_v<V>, "Bad value kind!");
                }
            } else {
                static_assert(false_v<A>, "Bad address kind!");
            }
        }
        template<typename V>
        inline void storeCode(RegisterIndex addr, V value, Address offset) {
            return storeCode(getRegisterValue<Address>(addr, offset), value);
        }
        template<typename A, typename V>
        void storeIO(A address, V value) {
            if constexpr (std::is_same_v<A, Address>) {
                if constexpr (std::is_same_v<V, Word>) {
                    _io.store(address, value);
                } else if constexpr (std::is_same_v<V, RegisterIndex>) {
                    _io.store(address, getRegisterValue(value));
                } else {
                    static_assert(false_v<V>, "Bad value kind!");
                }
            } else if constexpr (std::is_same_v<A, RegisterIndex>) {
                auto addr = getRegisterValue(address);
                if constexpr (std::is_same_v<V, Word>) {
                    _io.store(addr, value);
                } else if constexpr (std::is_same_v<V, RegisterIndex>) {
                    if (address == value) {
                        _io.store(addr, addr);
                    } else {
                        _io.store(addr, getRegisterValue(value));
                    }
                } else {
                    static_assert(false_v<V>, "Bad value kind!");
                }
            } else {
                static_assert(false_v<A>, "Bad address kind!");
            }
        }
        template<typename V>
        inline void storeIO(RegisterIndex addr, V value, Address offset) {
            return storeIO(getRegisterValue<Address>(addr, offset), value);
        }
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
        inline void setDoubleRegisterValue(RegisterIndex lower, T value) noexcept {
            getDoubleRegister(lower).put<T>(value);
        }
        template<typename T = DoubleWord>
        inline T getDoubleRegisterValue(RegisterIndex lower) const noexcept {
            return getDoubleRegister(lower).get<T>();
        }
    public:
        template<typename T>
        inline void setRegisterValue(RegisterIndex idx, T value) noexcept {
            getDestinationRegister(idx).put(value);
        }
        template<typename T = Word>
        inline T getRegisterValue(RegisterIndex idx) const noexcept {
            return getSourceRegister(idx).get<T>();
        }
        template<typename T = Word>
        inline T getRegisterValue(RegisterIndex idx, T offset) const noexcept {
            return getSourceRegister(idx).get<T>() + offset;
        }
    public:
        constexpr auto getTerminateCell() const noexcept { return _terminateCell; }
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
