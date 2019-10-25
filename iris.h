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

class Core {
    public:
        static void terminateCore(Core&, Word);
        static Word readTerminateCell(Core&);
    public:
        Core() noexcept;
        ~Core() = default;
        void run();
        void mapIntoIOSpace(Address, std::tuple<MMIOReadFunction, MMIOWriteFunction>);
        void mapIntoIOSpace(Address, MMIOReadFunction);
        void mapIntoIOSpace(Address, MMIOWriteFunction);
        void mapIntoIOSpace(Address, MMIOReadFunction, MMIOWriteFunction);
        void terminateCycle();
    public: 
        template<typename A>
        Address extractAddress(A address, Address offset = 0) noexcept {
            if constexpr (std::is_same_v<A, Address>) {
                return address + offset;
            } else if constexpr (std::is_same_v<A, RegisterIndex>) {
                return getRegisterValue(address) + offset;
            } else {
                static_assert(false_v<A>, "Bad address kind!");
            }
        }

        template<typename T>
        Word extractValue(T value) noexcept {
            if constexpr (std::is_same_v<T, Word>) {
                return value;
            } else if constexpr (std::is_same_v<T, RegisterIndex>) {
                return getRegisterValue<Word>(value);
            } else {
                static_assert(false_v<T>, "Bad value kind!");
            }
        }

        template<typename T>
        DoubleWord loadCode(T addr, Address offset = 0) {
            return _code[extractAddress<T>(addr, offset)];
        }

        template<typename T>
        Word loadIO(T addr, Address offset = 0) {
            return _io.load(extractAddress<T>(addr, offset));
        }
        template<typename T>
        Word loadStack(T addr, Address offset = 0) {
            return _stack[extractAddress<T>(addr, offset)];
        }
        template<typename T>
        Word loadData(T addr, Address offset = 0) {
            return _data[extractAddress<T>(addr, offset)];
        }
        template<typename A, typename V>
        void storeStack(A address, V value, Address offset = 0) {
            _stack[extractAddress<A>(address, offset)] = extractValue<V>(value);
        }
        template<typename A, typename V>
        void storeData(A address, V value, Address offset = 0) {
            _data[extractAddress<A>(address, offset)] = extractValue<V>(value);
        }
        template<typename A, typename V>
        void storeCode(A address, V value, Address offset = 0) {
            if constexpr (auto addr = extractAddress<A>(address, offset); std::is_same_v<V, DoubleWord>) {
                _code[addr] = value;
            } else if constexpr (std::is_same_v<V, RegisterIndex>) {
                _code[addr] = getDoubleRegisterValue(value);
            } else {
                static_assert(false_v<V>, "Bad value kind!");
            }
        }
        template<typename A, typename V>
        void storeIO(A address, V value, Address offset = 0) {
            _io.store(extractAddress<A>(address, offset), extractValue<V>(value));
        }
    private:
        template<typename T, std::enable_if_t<IsErrorOperation<std::decay_t<T>>, int> = 0>
        void invoke(const T& s) {
            throw ErrorInstructionException();
        }

        inline void updateLinkRegister(RegisterIndex index) noexcept {
            setRegisterValue(index, _ip.get() + 1);
        }
        template<typename T>
        void branchTo(T addr) noexcept {
            if constexpr (std::is_same_v<T, RegisterIndex>) {
                _ip.put<Word>(getRegisterValue<Word>(addr));
            } else if constexpr (std::is_unsigned_v<T>) {
                _ip.put<Word>(addr);
            } else if constexpr (std::is_signed_v<T>) {
                // this becomes relative offset
                _ip.put<SignedWord>(addr + _ip.get<SignedWord>());
            } else {
                static_assert(false_v<T>, "Bad branch to kind!");
            }
            _advanceIP = false;
        }
        template<typename T, std::enable_if_t<IsBranchOperation<std::decay_t<T>>, int> = 0> 
        void invoke(const T& s) {
            using K = std::decay_t<T>;
            if constexpr (IsBranchImmediateInstruction<K>) {
                using O = std::conditional_t<UsesRelativeOffset<K>, SignedWord, UnsignedWord>;
                auto [ link, offset] = s.arguments(); 
                static_assert(std::is_same_v<O, decltype(offset)>);
                static_assert(!(UsesLinkRegister<K> && IsConditionalOperation<K>), 
                              "Impossible state, cannot have an immediate which is conditional and link at the same time!");
                auto performBranch = true;
                if constexpr (IsConditionalOperation<K>) {
                    performBranch = getRegisterValue<bool>(link);
                } 
                if (performBranch) {
                    if constexpr (UsesLinkRegister<K>) { 
                        updateLinkRegister(link);
                    } 
                    branchTo<O>(offset);
                }
            } else if constexpr (IsBranchRegisterInstruction<K>) {
                if constexpr (IsSelectOperation<K>) {
                    // BranchSelect ConditionalRegister TrueAddress FalseAddress
                    auto [ cond, onTrue, onFalse ] = s.arguments();
                    branchTo(getRegisterValue(getRegisterValue<bool>(cond) ? onTrue : onFalse));
                } else {
                    auto [ dest, cond, link ] = s.arguments();
                    if (getRegisterValue<bool>(cond)) {
                        updateLinkRegister(link);
                        branchTo(dest);
                    }
                }
            } else {
                static_assert(false_v<T>, "Bad branch kind!");
            }
        }

        template<typename T, std::enable_if_t<IsCompareOperation<std::decay_t<T>>, int> = 0>
        void invoke(const T& s) {
            using K = std::decay_t<T>;
            static_assert(MustBeIntegerOrOrdinalOperation<K>);
            using D = DeterminedNumericType<K>;
            auto [ dest, rsrc1, rsrc2 ] = s.arguments();
            D src2 = static_cast<D>(0);
            iris::Word result = false;
            if constexpr (TreatArg2AsImmediate<K>) {
                src2 = static_cast<D>(rsrc2);
            } else {
                src2 = getRegisterValue(rsrc2);
            }
            if (auto src1 = getRegisterValue<D>(rsrc1); src1 < src2) {
                result = 0b100;
            } else if (src1 == src2) {
                result = 0b010;
            } else {
                result = 0b001;
            }
            setRegisterValue(dest, result);
        }
        template<typename T, std::enable_if_t<IsMemoryOperation<std::decay_t<T>>, int> = 0>
        void invoke(const T& input) noexcept {
            using K = std::decay_t<T>;
            if constexpr (IsGPRManipulatorOperation<K>) {
                if constexpr (IsCopyRegisterOperation<K>) {
                    if (auto [dest, src] = input.arguments(); dest != src) {
                        setRegisterValue(dest, getRegisterValue(src));
                    }
                } else if constexpr (IsAssignRegisterImmediateOperation<K>) {
                    auto [dest, imm16] = input.arguments();
                    setRegisterValue(dest, imm16);
                } else {
                    static_assert(false_v<K>, "Unimplemented gpr manipulator");
                }
            } else if constexpr (IsStackOperation<K>) {
                if constexpr (auto [sp, other] = input.arguments(); IsPopOperation<K>) {
                    // so stack grows downward
                    // pops grow towards 0xFFFF
                    // StackPop StackPointerRegister DestinationRegister
                    setRegisterValue(other, loadStack(sp));
                    incrementRegister(sp); // sp should always point to the front
                } else if constexpr (IsPushOperation<K>) {
                    // stack grows downward
                    // StackPush StackPointerRegister SourceRegister|Imm16
                    decrementRegister(sp);
                    storeStack(sp, other);
                } else {
                    static_assert(false_v<K>, "Unimplemented stack operation!");
                }
            } else if constexpr (IsDataOperation<K>) {
                if constexpr (IsLoadOperation<K>) {
                    auto [ loc, dest, offset ] = input.arguments();
                    setRegisterValue(dest, loadData(loc, offset));
                } else if constexpr (IsStoreOperation<K>) {
                    auto [ dest, value, offset ] = input.arguments();
                    storeData(dest, value, offset);
                } else if constexpr (IsStoreImmediateOperation<K>) {
                    auto [ addr, imm16 ] = input.arguments();
                    storeData(addr, imm16);
                } else {
                    static_assert(false_v<K>, "Unimplemented stack operation!");
                }
            } else if constexpr (IsIOOperation<K>) {
                if constexpr (IsLoadOperation<K>) {
                    auto [ loc, dest, offset ] = input.arguments();
                    setRegisterValue(dest, loadIO(loc, offset));
                } else if constexpr (IsStoreOperation<K>) {
                    auto [ dest, value, offset ] = input.arguments();
                    storeIO(dest, value, offset);
                } else if constexpr (IsStoreImmediateOperation<K>) {
                    auto [ addr, imm16 ] = input.arguments();
                    storeIO(addr, imm16);
                } else {
                    static_assert(false_v<K>, "Unimplemented stack operation!");
                }
            } else if constexpr (IsCodeOperation<K>) {
                if constexpr (IsLoadOperation<K>) {
                    // CodeLoad AddressRegister LowerRegister (implied UpperRegister = LowerRegister + 1)
                    auto [addr, lower, offset] = input.arguments();
                    setDoubleRegisterValue(lower, loadCode(addr, offset));
                } else if constexpr (IsStoreOperation<K>) {
                    // CodeStore AddressRegister <= LowerRegister (upper register implied)
                    auto [addr, lower, offset ] = input.arguments();
                    storeCode(addr, lower, offset);
                } else {
                    static_assert(false_v<K>, "Unimplemented code operation!");
                }
            } else {
                static_assert(false_v<K>, "Unimplemented memory operation!");
            }
        }

        template<typename T, std::enable_if_t<IsArithmeticOperation<std::decay_t<T>>, int> = 0>
        void invoke(const T& s) {
            using K = std::decay_t<T>;
            static_assert(MustBeIntegerOrOrdinalOperation<K>);
            using D = DeterminedNumericType<K>;
            auto [ dest, rsrc1, rsrc2 ] = s.arguments();
            D src2 = static_cast<D>(0);
            D result = static_cast<D>(0);
            if constexpr (TreatArg2AsImmediate<K>) {
                src2 = static_cast<D>(rsrc2);
            } else {
                src2 = getRegisterValue(rsrc2);
            }
            if constexpr (Src2CannotBeZero<K>) {
                if (src2 == 0) {
                    throw DivideByZeroException();
                }
            }
            if constexpr (auto src1 = getRegisterValue<D>(rsrc1); IsAddOperation<K>) {
                result = src1 + src2;
            } else if constexpr (IsSubtractOperation<K>) {
                result = src1 - src2;
            } else if constexpr (IsMultiplyOperation<K>) {
                result = src1 * src2;
            } else if constexpr (IsRemainderOperation<K>) {
                result = src1 % src2;
            } else if constexpr (IsDivideOperation<K>) {
                result = src1 / src2;
            } else if constexpr (IsShiftLeftOperation<K>) {
                result = src1 << src2;
            } else if constexpr (IsShiftRightOperation<K>) {
                result = src1 >> src2;
            } else if constexpr (IsMinOperation<K>) {
                result = std::min(src1, src2);
            } else if constexpr (IsMaxOperation<K>) {
                result = std::max(src1, src2);
            } else {
                static_assert(false_v<K>, "Unimplemented arithmetic operation");
            }
            setRegisterValue<D>(dest, result);
        }
        template<typename T, std::enable_if_t<IsLogicalOperation<std::decay_t<T>>, int> = 0>
        void invoke(const T& s) {
            using K = std::decay_t<T>;
            if constexpr (IsBitwiseNotOperation<K>) {
                auto [ dest, src ] = s.arguments();
                setRegisterValue(dest, ~getRegisterValue(src));
            } else {
                auto [ dest, src1, src2 ] = s.arguments();
                auto first = getRegisterValue(src1);
                auto second = getRegisterValue(src2);
                Word result = 0;
                if constexpr (IsBitwiseAndOperation<K>) {
                    result = first & second;
                } else if constexpr (IsBitwiseOrOperation<K>) {
                    result = first | second;
                } else if constexpr (IsBitwiseXorOperation<K>) {
                    result = first ^ second;
                } else {
                    static_assert(false_v<K>, "Bad logical kind");
                }
                if constexpr (NotTheResult<K>) {
                    result = ~result;
                }
                setRegisterValue(dest, result);
            }
        }


    private:
        void cycle();
    private:
        Register& getDestinationRegister(RegisterIndex idx) noexcept;
        const Register& getSourceRegister(RegisterIndex idx) const noexcept;
        DoubleRegister getDoubleRegister(RegisterIndex start, RegisterIndex next) const noexcept;
        inline DoubleRegister getDoubleRegister(RegisterIndex start) const noexcept {
            return getDoubleRegister(start, static_cast<RegisterIndex>(static_cast<Byte>(start) + 1));
        }
    public:
        void invoke(DoubleWord bits);
    private:
        void incrementRegister(RegisterIndex idx, RegisterIndexNumericType times = 0) noexcept {
            // it is impossible to actually do zero here!
            if (UnsignedWord actualCount = static_cast<UnsignedWord>(times) + 1; actualCount == 1) {
                ++getDestinationRegister(idx);
            } else {
                setRegisterValue(idx, getRegisterValue(idx) + actualCount);
            }
        }
        void decrementRegister(RegisterIndex idx, RegisterIndexNumericType times = 0) noexcept {
            // it is impossible to actually do zero here!
            if (UnsignedWord actualCount = static_cast<UnsignedWord>(times) + 1; actualCount == 1) {
                --getDestinationRegister(idx);
            } else {
                setRegisterValue(idx, getRegisterValue(idx) - actualCount);
            }
        }
    public:
        template<typename T>
        void setDoubleRegisterValue(RegisterIndex lower, T value) noexcept {
            getDoubleRegister(lower).put<T>(value);
        }
        template<typename T = DoubleWord>
        T getDoubleRegisterValue(RegisterIndex lower) const noexcept {
            return getDoubleRegister(lower).get<T>();
        }
    public:
        template<typename T>
        void setRegisterValue(RegisterIndex idx, T value) noexcept {
            getDestinationRegister(idx).put(value);
        }
        template<typename T = Word>
        T getRegisterValue(RegisterIndex idx) const noexcept {
            return getSourceRegister(idx).get<T>();
        }
        template<typename T = Word>
        T getRegisterValue(RegisterIndex idx, T offset) const noexcept {
            return getRegisterValue<T>(idx) + offset;
        }
    public:
        constexpr auto getTerminateCell() const noexcept { return _terminateCell; }
        constexpr Word getIP() const noexcept { return _ip.get<Word>(); }
        void setIP(Word value) noexcept { _ip.put(value); }
        void resetExecutionStatus() noexcept { _executing = true; }
        constexpr auto getExecutingStatus() const noexcept { return _executing; }
        void setTerminateCell(Word value) noexcept { _terminateCell = value; }
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
