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
    class title ## Instruction ;
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
            return loadCode<Address>(getRegisterValue<Address>(addr, offset));
        }
        inline DoubleWord loadCode(Address addr, Address offset) {
            return loadCode<Address>(addr + offset);
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
            return loadIO<Address>(getRegisterValue<Address>(addr, offset));
        }
        inline Word loadIO(Address addr, Address offset) {
            return loadIO<Address>(addr + offset);
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
        template<typename T, typename R = Word>
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
        inline Word loadData(RegisterIndex addr, Address offset) {
            return loadData<Address>(getRegisterValue<Address>(addr, offset));
        }
        inline Word loadData(Address addr, Address offset) {
            return loadData<Address>(addr + offset);
        }
        template<typename A, typename V>
        void storeData(A address, V value) {
            if constexpr (std::is_same_v<A, Address>) {
                if constexpr (std::is_same_v<V, Word>) {
                    _data[address] = value;
                } else if constexpr (std::is_same_v<V, DoubleWord>) {
                    _data[address] = value;
                    _data[address+1] = value >> 16;
                } else if constexpr (std::is_same_v<V, QuadWord>) {
                    _data[address] = value;
                    _data[address+1] = value >> 16;
                    _data[address+2] = value >> 32;
                    _data[address+3] = value >> 48;
                } else if constexpr (std::is_same_v<V, RegisterIndex>) {
                    _data[address] = getRegisterValue(value);
                } else {
                    static_assert(false_v<V>, "Bad value kind!");
                }
            } else if constexpr (std::is_same_v<A, RegisterIndex>) {
                auto addr = getRegisterValue(address);
                if constexpr (std::is_same_v<V, Word>) {
                    _data[addr] = value;
                } else if constexpr (std::is_same_v<V, DoubleWord>) {
                    _data[addr] = value;
                    _data[addr+1] = value >> 16;
                } else if constexpr (std::is_same_v<V, QuadWord>) {
                    _data[addr] = value;
                    _data[addr+1] = value >> 16;
                    _data[addr+2] = value >> 32;
                    _data[addr+3] = value >> 48;
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
                } else if constexpr (std::is_same_v<V, QuadWord>) {
                    _data[address] = value;
                    _data[address + 1] = value >> 32;
                } else if constexpr (std::is_same_v<V, RegisterIndex>) {
                    _data[address] = getDoubleRegisterValue(value);
                } else {
                    static_assert(false_v<V>, "Bad value kind!");
                }
            } else if constexpr (std::is_same_v<A, RegisterIndex>) {
                auto addr = getRegisterValue(address);
                if constexpr (std::is_same_v<V, DoubleWord>) {
                    _data[addr] = value;
                } else if constexpr (std::is_same_v<V, QuadWord>) {
                    _data[addr] = value;
                    _data[addr + 1] = value >> 32;
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
                } else if constexpr (std::is_same_v<V, DoubleWord>) {
                    _io.store(address, value);
                    _io.store(address + 1, value >> 16);
                } else if constexpr (std::is_same_v<V, QuadWord>) {
                    _io.store(address, value);
                    _io.store(address + 1, value >> 16);
                    _io.store(address + 2, value >> 32);
                    _io.store(address + 3, value >> 48);
                } else if constexpr (std::is_same_v<V, RegisterIndex>) {
                    _io.store(address, getRegisterValue(value));
                } else {
                    static_assert(false_v<V>, "Bad value kind!");
                }
            } else if constexpr (std::is_same_v<A, RegisterIndex>) {
                auto addr = getRegisterValue(address);
                if constexpr (std::is_same_v<V, Word>) {
                    _io.store(addr, value);
                } else if constexpr (std::is_same_v<V, DoubleWord>) {
                    _io.store(address, value);
                    _io.store(address + 1, value >> 16);
                } else if constexpr (std::is_same_v<V, QuadWord>) {
                    _io.store(address, value);
                    _io.store(address + 1, value >> 16);
                    _io.store(address + 2, value >> 32);
                    _io.store(address + 3, value >> 48);
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
            storeIO(getRegisterValue<Address>(addr, offset), value);
        }
        inline void storeIO(RegisterIndex addr, const DoubleRegister& reg, Address offset) { storeIO(addr, reg.get(), offset); }
        inline void storeIO(RegisterIndex addr, const QuadRegister& reg, Address offset) { storeIO(addr, reg.get(), offset); }
        inline void storeIO(RegisterIndex addr, const Register& reg, Address offset) { storeIO(addr, reg.get(), offset); }
        inline void storeData(RegisterIndex addr, const DoubleRegister& reg, Address offset) { storeData(addr, reg.get(), offset); }
        inline void storeData(RegisterIndex addr, const QuadRegister& reg, Address offset) { storeData(addr, reg.get(), offset); }
        inline void storeData(RegisterIndex addr, const Register& reg, Address offset) { storeData(addr, reg.get(), offset); }
        inline void storeStack(RegisterIndex addr, const Register& reg) { storeStack(addr, reg.get()); }
        inline void storeCode(RegisterIndex addr, const DoubleRegister& reg, Address offset) { storeCode(addr, reg.get(), offset); }
        inline void storeCode(RegisterIndex addr, const QuadRegister& reg, Address offset) { storeCode(addr, reg.get(), offset); }
        template<typename R = Word>
        inline R loadData(RegisterIndex addr, Address offset) {
            auto loc = getRegisterValue<Address>(addr, offset); 
            if constexpr (std::is_same_v<R, QuadWord>) {
                auto lowest = static_cast<QuadWord>(loadData<Address>(loc));
                auto lower = static_cast<QuadWord>(loadData<Address>(loc+1)) << 16;
                auto higher = static_cast<QuadWord>(loadData<Address>(loc+2)) << 32;
                auto highest = static_cast<QuadWord>(loadData<Address>(loc+3)) << 48;
                return lowest | lower | higher | highest;
            } else if constexpr (std::is_same_v<R, DoubleWord>) {
                auto low = static_cast<DoubleWord>(loadData<Address>(loc));
                auto high = static_cast<DoubleWord>(loadData<Address>(loc+1)) << 16;
                return low | high;
            } else if constexpr (std::is_same_v<R, Word>) {
                return loadData(loc);
            } else {
                static_assert(false_v<R>, "Bad return kind!");
            }
        }
        template<typename R = DoubleWord>
        inline R loadCode(RegisterIndex addr, Address offset) {
            auto loc = getRegisterValue<Address>(addr, offset); 
            if constexpr (std::is_same_v<R, QuadWord>) {
                auto lower = static_cast<QuadWord>(loadCode<Address>(loc));
                auto upper = static_cast<QuadWord>(loadCode<Address>(loc+1)) << 32; 
                return lower | upper;
            } else if constexpr (std::is_same_v<R, DoubleWord>) {
                return loadCode(loc);
            } else {
                static_assert(false_v<R>, "Bad return kind!");
            }
        }
        template<typename R = Word>
        inline R loadIO(RegisterIndex addr, Address offset) {
            auto loc = getRegisterValue<Address>(addr, offset); 
            if constexpr (std::is_same_v<R, QuadWord>) {
                auto lowest = static_cast<QuadWord>(loadIO<Address>(loc));
                auto lower = static_cast<QuadWord>(loadIO<Address>(loc+1)) << 16;
                auto higher = static_cast<QuadWord>(loadIO<Address>(loc+2)) << 32;
                auto highest = static_cast<QuadWord>(loadIO<Address>(loc+3)) << 48;
                return lowest | lower | higher | highest;
            } else if constexpr (std::is_same_v<R, DoubleWord>) {
                auto low = static_cast<DoubleWord>(loadIO<Address>(loc));
                auto high = static_cast<DoubleWord>(loadIO<Address>(loc+1)) << 16;
                return low | high;
            } else if constexpr (std::is_same_v<R, Word>) {
                return loadIO(loc);
            } else {
                static_assert(false_v<R>, "Bad return kind!");
            }
        }

    private:
        void invoke(const std::monostate&);
        // use tag dispatch to call the right routines
#define X(title, fmt) \
        void invoke(const title ## Instruction &);
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
        template<typename T>
        inline void setQuadRegisterValue(RegisterIndex lower, T value) noexcept {
            getQuadRegister(lower).put<T>(value);
        }
        template<typename T = DoubleWord>
        inline T getQuadRegisterValue(RegisterIndex lower) const noexcept {
            return getQuadRegister(lower).get<T>();
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
        // layout is actually 64kword * 4 but actually it is a tad more involved than that
        // in reality we are looking at 512kbyte of space but it is easiest if we just do
        // a 24-bit memory space. There are 256 64kbyte memory spaces that iris can see 
        // at any given time. Thus we have the following mapping (using only the upper 8 bits as
        // an address)
        // 0x00 -> Code0
        // 0x01 -> Code1
        // 0x02 -> Code2
        // 0x03 -> Code3
        // 0x04 -> Data0
        // 0x05 -> Data1
        // 0x06 -> Stack0
        // 0x07 -> Stack1
        // 0x08 -> IO0
        // 0x09 -> IO1
        // [0x0A, 0xFF] Unused
        //
        // To the programmer, the memory spaces are 16-bits in size but on word and double word boundaries
        // Thus a code address looks like this:
        // 0xFFFF -> 0x3FFFC (shift the incoming code address by two and add the upper 8 bits of the lowest
        // four code spaces starting address. Which is zero in this case)
        // 
        // A data address looks like this:
        // 0xFFFF -> 0x5FFFE (shift the incoming code address by one and add the upper 8 bits of the lowest
        // two data spaces starting address. Which is 0x040000 in this case)
        //
        // A stack address looks like this:
        // 0xFFFF -> 0x7FFFE (shift by one and add low stack space offset starting address, which is 0x060000)
        //
        // The io space follows the same design:
        // 0xFFFF -> 0x9FFFE (shift by one and add low io space offset starting address, which is 0x080000)
        //
        // However! Since iris operates on 16-bit words by default, there is absolutely no point in having this
        // synthetic design actually broken up into 8-bit byte sections. It should be 16-bit word groups. Thus
        // instead we would have the following 16-bit spaces:
        // 0x00 -> Code0
        // 0x01 -> Code1
        // 0x02 -> Data
        // 0x03 -> Stack
        // 0x04 -> IO
        //
        // Thus I have just laid out a potential design
        
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
