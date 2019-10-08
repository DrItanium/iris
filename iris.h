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
#include "opcodes.h"
namespace iris {
    template<typename T> constexpr auto IsBranchImmediateInstruction = false;
    template<typename T> constexpr auto UsesRelativeOffset = false;
    template<typename T> constexpr auto UsesLinkRegister = false;
    template<typename T> constexpr auto ManipulatesIP = false;
    template<typename T> constexpr auto DisallowsDivideByZero = false;
    template<typename T> constexpr auto IsIntegerOperation = false;
    template<typename T> constexpr auto IsOrdinalOperation = false;
    template<typename T> constexpr auto IsDivideOperation = false;
    template<typename T> constexpr auto IsRemainderOperation = false;
    template<> constexpr auto IsBranchImmediateInstruction<iris::BranchRelativeImmediateAndLinkInstruction> = true;
    template<> constexpr auto IsBranchImmediateInstruction<iris::BranchRelativeImmediateInstruction> = true;
    template<> constexpr auto IsBranchImmediateInstruction<iris::BranchImmediateAndLinkInstruction> = true;
    template<> constexpr auto IsBranchImmediateInstruction<iris::BranchImmediateInstruction> = true;
    template<> constexpr auto UsesRelativeOffset<iris::BranchRelativeImmediateInstruction> = true;
    template<> constexpr auto UsesRelativeOffset<iris::BranchRelativeImmediateAndLinkInstruction> = true;
    template<> constexpr auto UsesLinkRegister<iris::BranchRelativeImmediateAndLinkInstruction> = true;
    template<> constexpr auto UsesLinkRegister<iris::BranchImmediateAndLinkInstruction> = true;
    template<> constexpr auto ManipulatesIP<iris::MemoryMoveToIPInstruction> = true;
    template<> constexpr auto ManipulatesIP<iris::MemoryMoveFromIPInstruction> = true;
    template<> constexpr auto DisallowsDivideByZero<iris::ArithmeticRemainderSignedInstruction> = true;
    template<> constexpr auto DisallowsDivideByZero<iris::ArithmeticRemainderUnsignedInstruction> = true;
    template<> constexpr auto DisallowsDivideByZero<iris::ArithmeticDivideSignedInstruction> = true;
    template<> constexpr auto DisallowsDivideByZero<iris::ArithmeticDivideUnsignedInstruction> = true;
    template<> constexpr auto IsIntegerOperation<iris::ArithmeticRemainderSignedInstruction> = true;
    template<> constexpr auto IsIntegerOperation<iris::ArithmeticDivideSignedInstruction> = true;
    template<> constexpr auto IsOrdinalOperation<iris::ArithmeticRemainderUnsignedInstruction> = true;
    template<> constexpr auto IsOrdinalOperation<iris::ArithmeticDivideUnsignedInstruction> = true;
    template<> constexpr auto IsRemainderOperation<iris::ArithmeticRemainderSignedInstruction> = true;
    template<> constexpr auto IsRemainderOperation<iris::ArithmeticRemainderUnsignedInstruction> = true;
    template<> constexpr auto IsDivideOperation<iris::ArithmeticDivideSignedInstruction> = true;
    template<> constexpr auto IsDivideOperation<iris::ArithmeticDivideUnsignedInstruction> = true;

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
                } else if constexpr (std::is_same_v<V, DoubleWord>) {
                    _io.store(address, value);
                    _io.store(address + 1, value >> 16);
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
        inline void storeIO(RegisterIndex addr, const Register& reg, Address offset) { storeIO(addr, reg.get(), offset); }
        inline void storeData(RegisterIndex addr, const DoubleRegister& reg, Address offset) { storeData(addr, reg.get(), offset); }
        inline void storeData(RegisterIndex addr, const Register& reg, Address offset) { storeData(addr, reg.get(), offset); }
        inline void storeStack(RegisterIndex addr, const Register& reg) { storeStack(addr, reg.get()); }
        inline void storeCode(RegisterIndex addr, const DoubleRegister& reg, Address offset) { storeCode(addr, reg.get(), offset); }
        template<typename R = Word>
        inline R loadData(RegisterIndex addr, Address offset) {
            auto loc = getRegisterValue<Address>(addr, offset); 
            if constexpr (std::is_same_v<R, DoubleWord>) {
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
            if constexpr (std::is_same_v<R, DoubleWord>) {
                return loadCode(loc);
            } else {
                static_assert(false_v<R>, "Bad return kind!");
            }
        }
        template<typename R = Word>
        inline R loadIO(RegisterIndex addr, Address offset) {
            auto loc = getRegisterValue<Address>(addr, offset); 
            if constexpr (std::is_same_v<R, DoubleWord>) {
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
        void invoke(const iris::ErrorInstruction&);
        void invoke(const iris::MemoryCopyRegisterInstruction&);
        void invoke(const iris::MemoryAssignRegisterImmediateInstruction&);
        void invoke(const iris::MemorySwapRegistersInstruction&);
        void invoke(const iris::CompareGreaterThanOrEqualToUnsignedImmediate8Instruction&);
        void invoke(const iris::CompareGreaterThanOrEqualToSignedImmediate8Instruction&);
        void invoke(const iris::CompareLessThanOrEqualToUnsignedImmediate8Instruction&);
        void invoke(const iris::CompareLessThanOrEqualToSignedImmediate8Instruction&);
        void invoke(const iris::CompareLessThanUnsignedImmediate8Instruction&);
        void invoke(const iris::CompareLessThanSignedImmediate8Instruction&);
        void invoke(const iris::CompareGreaterThanUnsignedImmediate8Instruction&);
        void invoke(const iris::CompareGreaterThanSignedImmediate8Instruction&);
        void invoke(const iris::CompareNotEqualsImmediate8Instruction&);
        void invoke(const iris::CompareEqualsImmediate8Instruction&);
        void invoke(const iris::CompareGreaterThanOrEqualToUnsignedInstruction&);
        void invoke(const iris::CompareGreaterThanOrEqualToSignedInstruction&);
        void invoke(const iris::CompareLessThanOrEqualToUnsignedInstruction&);
        void invoke(const iris::CompareLessThanOrEqualToSignedInstruction&);
        void invoke(const iris::CompareLessThanUnsignedInstruction&);
        void invoke(const iris::CompareLessThanSignedInstruction&);
        void invoke(const iris::CompareGreaterThanUnsignedInstruction&);
        void invoke(const iris::CompareGreaterThanSignedInstruction&);
        void invoke(const iris::CompareNotEqualsInstruction&);
        void invoke(const iris::CompareEqualsInstruction&);
        void invoke(const iris::BranchSelectInstruction&);
        void invoke(const iris::BranchConditionalRegisterAndLinkInstruction&);
        void invoke(const iris::BranchRegisterAndLinkInstruction&);
        void invoke(const iris::BranchConditionalRegisterInstruction&);
        void invoke(const iris::BranchRegisterInstruction&);
        void invoke(const iris::BranchConditionalRelativeImmediateInstruction&);
        void invoke(const iris::BranchConditionalImmediateInstruction&);
        template<typename T, std::enable_if_t<IsBranchImmediateInstruction<std::decay_t<T>>, int> = 0>
        void invoke(const T & s) { 
            using K = std::decay_t<decltype(s)>; 
            auto [ link, offset] = s.arguments(); 
            if constexpr (UsesLinkRegister<K>) { 
                setRegisterValue(link, _ip.get() + 1); 
            } 
            if constexpr (UsesRelativeOffset<K>) { 
                _ip.put(_ip.get<SignedWord>() + offset); 
            } else { 
                _ip.put(offset); 
            } 
            _advanceIP = false; 
        }
        template<typename I, std::enable_if_t<ManipulatesIP<std::decay_t<I>>, int> = 0>
        void invoke(const I& input) noexcept {
            using K = std::decay_t<I>;
            auto [ reg ] = input.arguments();
            if constexpr (std::is_same_v<K, iris::MemoryMoveToIPInstruction>) {
                _ip.put(getRegisterValue(reg));
            } else if constexpr (std::is_same_v<K, iris::MemoryMoveFromIPInstruction>) {
                setRegisterValue(reg, _ip.get());
            } else {
                static_assert(false_v<I>, "Type not accepted!");
            }
        }
        template<typename T, std::enable_if_t<DisallowsDivideByZero<std::decay_t<T>>, int> = 0> 
        void invoke(const T& s) {
            using K = std::decay_t<T>;
            static_assert(IsOrdinalOperation<K> || IsIntegerOperation<K>);
            using D = std::conditional_t<IsOrdinalOperation<K>, Word, SignedWord>;
            auto [ dest, num, denom ] = s.arguments(); 
            D denominator = static_cast<D>(0);
            if constexpr (TreatArg2AsImmediate<K>) {
                denominator = static_cast<D>(denom);
            } else {
                denominator = getRegisterValue(denom);
            }
            if (denominator == 0) {
                throw DivideByZeroException();
            } else {
                if constexpr (auto numerator = getRegisterValue<D>(num); IsRemainderOperation<K>) {
                    setRegisterValue<D>(dest, numerator % denominator);
                } else if constexpr (IsDivideOperation<K>) {
                    setRegisterValue<D>(dest, numerator / denominator);
                } else {
                    static_assert(false_v<T>, "Unimplemented operation which disallows division by zero!");
                }
            }
        }
        template<typename T, std::enable_if_t<IsStackOperation<std::decay_t<T>>, int> = 0>
        void invoke(const T& s) {
            using K = std::decay_t<T>;
            auto [ sp, other ] = s.arguments();
            if constexpr (std::is_same_v<K, MemoryStackPopInstruction>) {
                // so stack grows downward
                // pops grow towards 0xFFFF
                // StackPop StackPointerRegister DestinationRegister
                setRegisterValue(other, loadStack(sp));
                incrementRegister(sp);
            } else if constexpr (std::is_same_v<K, MemoryStackPushInstruction> ||
                                 std::is_same_v<K, MemoryStackPushImmediateValueInstruction>) {
                // stack grows downward
                // StackPush StackPointerRegister SourceRegister|Imm16
                decrementRegister(sp);
                storeStack(sp, other);
            } else {
                static_assert(false_v<K>, "Unimplemented stack operation!");
            }
        }

        template<typename T, std::enable_if_t<IsDataOperation<std::decay_t<T>>, int> = 0>
        void invoke(const T& s) {
            using K = std::decay_t<T>;
            if constexpr (std::is_same_v<K, MemoryDataLoadWithOffsetInstruction>) {
                auto [ dest, loc, offset ] = s.arguments();
                setRegisterValue(dest, loadData(loc, offset));
            } else if constexpr (std::is_same_v<K, MemoryDataStoreWithOffsetInstruction>) {
                auto [ dest, value, offset ] = s.arguments();
                storeData(dest, value, offset);
            } else if constexpr (std::is_same_v<K, MemoryDataStoreImmediateValueInstruction>) {
                auto [ addr, imm16 ] = s.arguments();
                storeData(addr, imm16);
            } else {
                static_assert(false_v<K>, "Unimplemented stack operation!");
            }
        }
        template<typename T, std::enable_if_t<IsCodeOperation<std::decay_t<T>>, int> = 0>
        void invoke(const T& s) {
            using K = std::decay_t<T>;
            if constexpr (std::is_same_v<K, MemoryCodeLoadWithOffsetInstruction>) {
                // CodeLoad AddressRegister LowerRegister (implied UpperRegister = LowerRegister + 1)
                auto [addr, lower, offset] = s.arguments();
                setDoubleRegisterValue(lower, loadCode(addr, offset));
            } else if constexpr (std::is_same_v<K, MemoryCodeStoreWithOffsetInstruction>) {
                // CodeStore AddressRegister <= LowerRegister (upper register implied)
                auto [addr, lower, offset ] = s.arguments();
                storeCode(addr, lower, offset);
            } else {
                static_assert(false_v<K>, "Unimplemented code operation!");
            }
        }

    private:
        void cycle();
    private:
        DestinationRegister getDestinationRegister(RegisterIndex idx) noexcept;
        SourceRegister getSourceRegister(RegisterIndex idx) const noexcept;
        DoubleRegister getDoubleRegister(RegisterIndex start, RegisterIndex next) noexcept;
        const DoubleRegister getDoubleRegister(RegisterIndex start, RegisterIndex next) const noexcept;
        void invoke(DoubleWord bits);
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
