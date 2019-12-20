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
#include "types.h"
#include "register.h"
#include "opcodes.h"
#include "mem_bank.h"
namespace iris {
/**
 * A single iris machine, it is an abstract top level
 */
class Core {
    public:
        static void terminateCore(Core&, Word);
        static Word readTerminateCell(Core&);
    public:
        Core() noexcept = default;
        virtual ~Core() = default;
        void run();
        void terminateCycle();
    protected:
        virtual Ordinal loadFromMemory(Address address) noexcept = 0;
        virtual void storeToMemory(Address address, Ordinal value) noexcept = 0;
        virtual void storeToMemory(Address address, HalfOrdinal value) noexcept = 0;
        virtual void storeToMemory(Address address, QuarterOrdinal value) noexcept = 0;
        virtual void storeToMemory(Address address, Integer value) noexcept;
        virtual void storeToMemory(Address address, HalfInteger value) noexcept;
        virtual void storeToMemory(Address address, QuarterInteger value) noexcept;
    public:
        template<typename T>
        T load(Address address) noexcept {
            using K = std::decay_t<T>;
            if constexpr (Register temp(loadFromMemory(address)); std::is_same_v<K, Ordinal>) {
                return temp.get<Ordinal>();
            } else if constexpr (std::is_same_v<K, Integer>) {
                return temp.get<Integer>();
            } else if constexpr (std::is_same_v<K, bool>) {
                return temp.get<bool>();
            } else if constexpr (std::is_same_v<K, HalfOrdinal>) {
                return static_cast<HalfOrdinal>(temp.get<Ordinal>()); 
            } else if constexpr (std::is_same_v<K, HalfInteger>) {
                return static_cast<HalfInteger>(temp.get<Integer>()); 
            } else if constexpr (std::is_same_v<K, QuarterOrdinal>) {
                return static_cast<QuarterOrdinal>(temp.get<Ordinal>()); 
            } else if constexpr (std::is_same_v<K, QuarterInteger>) {
                return static_cast<QuarterInteger>(temp.get<Integer>()); 
            } else {
                static_assert(false_v<T>, "illegal kind requested!");
            }
        }
        template<typename T>
        void store(Address address, T value) noexcept {
            using K = std::decay_t<T>;
            if constexpr (std::is_same_v<K, bool>) {
                Register temp;
                temp.put(value);
                storeToMemory(address, temp.get<Ordinal>());
            } else {
                storeToMemory(address, value);
            }
        }
    private:
        template<typename T, std::enable_if_t<IsErrorOperation<std::decay_t<T>>, int> = 0>
        void invoke(const T& s) {
            raiseErrorInstruction();
        }

        inline void updateLinkRegister(RegisterIndex index) noexcept {
            putRegister(index, (((getIP() >> 2) + 1) << 2));
        }
        void branchTo(QuarterInteger offset) noexcept;
        void branchTo(HalfInteger offset) noexcept;
        void branchTo(Address exact) noexcept;
        void branchTo(RegisterIndex exact) noexcept;
        template<typename T>
        void branchTo(T addr) noexcept {
            if constexpr (std::is_same_v<T, RegisterIndex>) {
                setIP(retrieveRegister(addr, RequestOrdinal{}));
            } else if constexpr (std::is_unsigned_v<T>) {
                setIP(static_cast<Ordinal>(addr));
            } else if constexpr (std::is_signed_v<T>) {
                // this becomes relative offset
                setIP(static_cast<Integer>(static_cast<Integer>(addr) + static_cast<Integer>(getIP())));
            } else {
                static_assert(false_v<T>, "Bad branch to kind!");
            }
            doNotAdvanceIP();
        }
        template<EncodedInstruction T, std::enable_if_t<IsBranchInstruction<T>, int> = 0> 
        void invoke(const Instruction& s) {
            if constexpr (IsImmediateBranch<T>) {
                auto offset = s.getImm16();
                auto reg = s.getArg0();
                auto performBranch = true;
                if constexpr (IsConditionalOperation<T>) {
                    performBranch = (getRegisterValue<Ordinal>(reg) & BranchMask<T>) != 0;
                }
                if (performBranch) {
                    if constexpr (UsesLinkRegister<T>) {
                        updateLinkRegister(reg);
                    }
                    branchTo(static_cast<HalfInteger>(offset));
                }
            } else if constexpr (IsRegisterBranch<T>) {
                constexpr auto mask = BranchMask<T>;
                if constexpr (auto pattern = getRegisterValue<Ordinal>(s.getArg1()); IsConditionalBranchAndLink<T>) {
                    // need to now extract the mask as we will and it with the 
                    // contents of cond (src1)
                    if ((pattern & mask) != 0) {
                        updateLinkRegister(s.getArg2());
                        branchTo(s.getArg0());
                    }
                } else if constexpr (IsSelectOperation<T>) {
                    // arg0 - condition
                    // arg1 - onTrue
                    // arg2 - onFalse
                    branchTo(((pattern & mask) != 0) ?  s.getArg1() : s.getArg2());
                } else {
                    static_assert(false_v<T>, "Illegal branch register instruction");
                }
            } else {
                static_assert(false_v<T>, "Bad branch kind!");
            }
        }

        template<EncodedInstruction T, std::enable_if_t<IsCompareInstruction<T>, int> = 0>
        void invoke(const Instruction& s) {
            using D = DeterminedNumericType<T>;
            iris::Word result = false;
            if (auto src1 = getRegisterValue<D>(s.getArg1()),
                     src2 = getRegisterValue<D>(s.getArg2()); src1 < src2) {
                result = 0b100;
            } else if (src1 == src2) {
                result = 0b010;
            } else {
                result = 0b001;
            }
            putRegister(s.getArg0(), result);
        }
        template<EncodedInstruction T, std::enable_if_t<IsMemoryInstruction<T>, int> = 0>
        void store(Address addr, RegisterIndex value) {
            /// @todo finish this
        }
        template<EncodedInstruction T, std::enable_if_t<IsMemoryInstruction<T>, int> = 0>
        decltype(auto) load(Address addr) {
            /// @todo finish this
        }
        template<EncodedInstruction T, std::enable_if_t<IsMemoryInstruction<T>, int> = 0>
        void invoke(const Instruction& input) {
            if constexpr (!IsLoadOperation<T> && !IsStoreOperation<T>) {
                static_assert(false_v<T>, "Unimplemented memory operation!");
            } else {
                Address address = 0;
                if constexpr (!ArgumentIsImm16<T>) {
                    auto offset = input.getImm8();
                    address = getRegisterValue<Address>(input.getArg1()) + offset;
                } else {
                    address = input.getImm16();
                }
                if constexpr (IsStoreOperation<T>) {
                    store<T>(address, input.getArg0());
                } else if constexpr (IsLoadOperation<T>) {
                    setRegisterValue(input.getArg0(), load<T>(address));
                } else {
                    // redundant but complete solution
                    static_assert(false_v<T>, "Invalid memory operation kind! Should never get here!");
                }
            }
        }
        template<EncodedInstruction T, std::enable_if_t<IsArithmeticInstruction<T>, int> = 0>
        void invoke(const Instruction& s) {
            if constexpr (IsErrorOperation<T>) {
                // Error is considered an arithmetic operation
                raiseErrorInstruction();
            } else {
                using D = DeterminedNumericType<T>;
                auto src2 = getRegisterValue<D>(s.getArg2());
                if constexpr (Src2CannotBeZero<T>) {
                    if (src2 == 0) {
                        raiseDivideByZero();
                    }
                }
                auto result = getRegisterValue<D>(s.getArg1());
                if constexpr (IsAddOperation<T>) {
                    result += src2;
                } else if constexpr (IsSubtractOperation<T>) {
                    result -= src2;
                } else if constexpr (IsMultiplyOperation<T>) {
                    result *= src2;
                } else if constexpr (IsRemainderOperation<T>) {
                    result %= src2;
                } else if constexpr (IsDivideOperation<T>) {
                    result /= src2;
                } else if constexpr (IsShiftLeftOperation<T>) {
                    result <<= src2;
                } else if constexpr (IsShiftRightOperation<T>) {
                    result >>= src2;
                } else {
                    static_assert(false_v<T>, "Unimplemented arithmetic operation");
                }
                this->setRegisterValue(s.getArg0(), static_cast<D>(result));
            }
        }
        template<EncodedInstruction T, std::enable_if_t<IsBitwiseInstruction<T>, int> = 0>
        void invoke(const Instruction& s) {
            Ordinal computation = getRegisterValue(s.getArg1());
            if constexpr (IsNotOperation<T>) {
                if constexpr (ArgumentIsImm16<T>) {
                    // load immediate and overwrite computation
                    computation = s.getImm16();
                } 
                // perform the not
                computation = ~computation;
            } else if constexpr (auto p2 = getRegisterValue(s.getArg2()); IsAndOperation<T>) {
                computation &= p2;
            } else if constexpr (IsOrOperation<T>) {
                computation |= p2;
            } else if constexpr (IsXorOperation<T>) {
                computation ^= p2;
            } else {
                static_assert(false_v<T>, "Bad bitwise kind");
            }
            if constexpr (NotTheResult<T>) {
                // in the case of move and load immediate, this will reinvert
                // the bits back to what they should be, thus completing the
                // transfer of immediate or register contents. Goofy but
                // regular solution to this design
                computation = ~computation;
            }
            setRegisterValue(s.getArg0(), computation);
        }


    public:
        void invoke(EncodedInstruction bits);
    public:
        virtual void putRegister(RegisterIndex lower, Ordinal value) noexcept = 0;
        virtual void putRegister(RegisterIndex lower, Integer value) noexcept = 0;
        virtual void putRegister(RegisterIndex lower, bool value) noexcept = 0;
        class RequestOrdinal { };
        class RequestInteger { };
        class RequestBoolean { };
        template<typename T>
        using RequestType = std::conditional_t<
        std::is_same_v<std::decay_t<T>, Integer>, RequestInteger,
            std::conditional_t<std::is_same_v<T, bool>, RequestBoolean, RequestOrdinal>>;
        virtual Ordinal retrieveRegister(RegisterIndex ind, RequestOrdinal) const noexcept = 0;
        virtual Integer retrieveRegister(RegisterIndex ind, RequestInteger) const noexcept = 0;
        virtual bool retrieveRegister(RegisterIndex ind, RequestBoolean) const noexcept = 0;
    public:
        template<typename T>
        void setRegisterValue(RegisterIndex ind, T value) noexcept { 
            putRegister(ind, value); 
        }
        template<typename T>
        T getRegisterValue(RegisterIndex ind) noexcept {
            return retrieveRegister(ind, RequestType<T> { });
        }
    public:
        virtual Ordinal getTerminateCell() const noexcept = 0;
        virtual Ordinal getIP() const noexcept = 0;
        virtual void setIP(Ordinal value) noexcept = 0;
        virtual void setIP(Integer value) noexcept = 0;
        virtual void resetExecutionStatus() noexcept = 0;
        virtual bool getExecutingStatus() const noexcept = 0;
        virtual void setTerminateCell(Ordinal value) noexcept = 0;
        virtual void stopExecution() noexcept = 0;
    protected:
        virtual void advanceIP() noexcept = 0;
        virtual void doNotAdvanceIP() noexcept = 0;
        virtual void allowAdvanceIP() noexcept = 0;
        virtual bool shouldAdvanceIP() const noexcept = 0;
        virtual void raiseErrorInstruction() = 0;
        virtual void raiseDivideByZero() = 0;
        virtual void raiseBadOperation() = 0;
        /**
         * A wrapper around cycle which is responsible for 
         * handling any special logic cases such as catching exceptions, etc.
         */
        virtual void cycleHandler() = 0;
};


} // end namespace iris


#endif // end IRIS_H__
