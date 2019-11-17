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
#include "exceptions.h"
#include "register.h"
#include "opcodes.h"
namespace iris {
/**
 * A single iris machine, it is an abstract top level
 */
class Core {
    public:
        static void terminateCore(Core&, Word);
        static Word readTerminateCell(Core&);
    public:
        Core() noexcept;
        virtual ~Core() = default;
        void run();
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
    protected:
        virtual LongOrdinal loadFromCodeMemory(Address addr) = 0;
        virtual Ordinal loadFromDataMemory(Address addr) = 0;
        virtual Ordinal loadFromStackMemory(Address addr) = 0;
        virtual Ordinal loadFromIOMemory(Address addr) = 0;
        virtual void storeToCodeMemory(Address addr, LongOrdinal value) = 0;
        virtual void storeToDataMemory(Address addr, Ordinal value) = 0;
        virtual void storeToStackMemory(Address addr, Ordinal value) = 0;
        virtual void storeToIOMemory(Address addr, Ordinal value) = 0;
    public:
        template<typename T>
        LongOrdinal loadCode(T addr, Address offset = 0) {
            return loadFromCodeMemory(extractAddress<T>(addr, offset));
            //return _code[extractAddress<T>(addr, offset)];
        }

        template<typename T>
        Word loadIO(T addr, Address offset = 0) {
             return loadFromIOMemory(extractAddress<T>(addr, offset));
            //return _io.load(extractAddress<T>(addr, offset));
        }
        template<typename T>
        Word loadStack(T addr, Address offset = 0) {
            return loadFromStackMemory(extractAddress<T>(addr, offset));
            //return _stack[extractAddress<T>(addr, offset)];
        }
        template<typename T>
        Word loadData(T addr, Address offset = 0) {
            return loadFromDataMemory(extractAddress<T>(addr, offset));
        }
        template<typename A, typename V>
        void storeStack(A address, V value, Address offset = 0) {
            storeToStackMemory(extractAddress<A>(address, offset), extractValue<V>(value));
            //_stack[extractAddress<A>(address, offset)] = extractValue<V>(value);
        }
        template<typename A, typename V>
        void storeData(A address, V value, Address offset = 0) {
            storeToDataMemory(extractAddress<A>(address, offset), extractValue<V>(value));
            //_data[extractAddress<A>(address, offset)] = extractValue<V>(value);
        }
        template<typename A, typename V>
        void storeCode(A address, V value, Address offset = 0) {
            if constexpr (auto addr = extractAddress<A>(address, offset); std::is_same_v<V, LongOrdinal>) {
                storeToCodeMemory(addr, value);
                //_code[addr] = value;
            } else if constexpr (std::is_same_v<V, RegisterIndex>) {
                storeToCodeMemory(addr, getDoubleRegisterValue(value));
                //_code[addr] = getDoubleRegisterValue(value);
            } else {
                static_assert(false_v<V>, "Bad value kind!");
            }
        }
        template<typename A, typename V>
        void storeIO(A address, V value, Address offset = 0) {
            storeToIOMemory(extractAddress<A>(address, offset), extractValue<V>(value));
            //_io.store(extractAddress<A>(address, offset), extractValue<V>(value));
        }
    private:
        template<typename T, std::enable_if_t<IsErrorOperation<std::decay_t<T>>, int> = 0>
        void invoke(const T& s) {
            throw ErrorInstructionException();
        }

        inline void updateLinkRegister(RegisterIndex index) noexcept {
            setRegisterValue(index, static_cast<Ordinal>(getIP()  + 1));
        }
        template<typename T>
        void branchTo(T addr) noexcept {
            if constexpr (std::is_same_v<T, RegisterIndex>) {
                setIP(getRegisterValue<Ordinal>(addr));
                //_ip.put<Word>(getRegisterValue<Word>(addr));
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
            if constexpr (IsBranchImmediateInstruction<T>) {
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
                    if constexpr (IsRelativeJump<T>) {
                        branchTo<Integer>(offset);
                    } else {
                        branchTo<Ordinal>(offset);
                    }
                }
            } else if constexpr (IsBranchRegisterInstruction<T>) {
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

        template<EncodedInstruction T, std::enable_if_t<IsCompareOperation<T>, int> = 0>
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
            setRegisterValue(s.getArg0(), result);
        }
        template<EncodedInstruction T, std::enable_if_t<IsMemoryOperation<T>, int> = 0>
        void store(Address addr, RegisterIndex value) {
            if constexpr (IsSpaceCode<T>) {
                storeCode(addr, value);
            } else if constexpr (auto lower = getRegisterValue<Ordinal>(value); IsSpaceStack<T>) {
                storeStack(addr, lower);
            } else if constexpr (IsSpaceIO<T>) {
                storeIO(addr, lower);
            } else if constexpr (IsSpaceData<T>) {
                storeData(addr, lower);
            } else {
                static_assert(false_v<T>, "Unknown memory space!");
            }
        }
        template<EncodedInstruction T, std::enable_if_t<IsMemoryOperation<T>, int> = 0>
        decltype(auto) load(Address addr) {
            if constexpr (IsSpaceCode<T>) {
                return loadCode(addr);
            } else if constexpr (IsSpaceStack<T>) {
                return loadStack(addr);
            } else if constexpr (IsSpaceData<T>) {
                return loadData(addr);
            } else if constexpr (IsSpaceIO<T>) {
                return loadIO(addr);
            } else {
                static_assert(false_v<T>, "Unknown memory space!");
            }
        }
        template<EncodedInstruction T, std::enable_if_t<IsMemoryOperation<T>, int> = 0>
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
                    if constexpr (IsSpaceCode<T>) {
                        setDoubleRegisterValue(input.getArg0(), load<T>(address));
                    } else {
                        setRegisterValue(input.getArg0(), load<T>(address));
                    }
                } else {
                    // redundant but complete solution
                    static_assert(false_v<T>, "Invalid memory operation kind! Should never get here!");
                }
            }
        }

        template<EncodedInstruction T, std::enable_if_t<IsArithmeticOperation<T>, int> = 0>
        void invoke(const Instruction& s) {
            if constexpr (IsErrorOperation<T>) {
                // Error is considered an arithmetic operation
                throw ErrorInstructionException();
            } else {
                using D = DeterminedNumericType<T>;
                auto src2 = getRegisterValue<D>(s.getArg2());
                if constexpr (Src2CannotBeZero<T>) {
                    if (src2 == 0) {
                        throw DivideByZeroException();
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
        template<EncodedInstruction T, std::enable_if_t<IsLogicalOperation<T>, int> = 0>
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
        void invoke(LongOrdinal bits);
    public:
        virtual void putDoubleRegister(RegisterIndex lower, RegisterIndex upper, LongOrdinal value) noexcept = 0;
        virtual void putDoubleRegister(RegisterIndex lower, LongOrdinal value) noexcept = 0;
        virtual LongOrdinal retrieveDoubleRegister(RegisterIndex lower, RegisterIndex upper) const noexcept = 0;
        virtual LongOrdinal retrieveDoubleRegister(RegisterIndex lower) const noexcept = 0;
        virtual void putRegister(RegisterIndex lower, Ordinal value) noexcept = 0;
        virtual void putRegister(RegisterIndex lower, Integer value) noexcept = 0;
        void putRegister(RegisterIndex lower, bool value) noexcept {
            putRegister(lower, static_cast<Ordinal>(value ? 0xFFFF : 0x0000));
        }
        class RequestOrdinal { };
        class RequestInteger { };
        class RequestBoolean { };
        virtual Ordinal retrieveRegister(RegisterIndex ind, RequestOrdinal) const noexcept = 0;
        virtual Integer retrieveRegister(RegisterIndex ind, RequestInteger) const noexcept = 0;
        bool retrieveRegister(RegisterIndex ind, RequestBoolean) const noexcept { return retrieveRegister(ind, RequestOrdinal{}) != 0; }
    public:
        void setDoubleRegisterValue(RegisterIndex lower, LongOrdinal value) noexcept {
            putDoubleRegister(lower, value);
        }
        LongOrdinal getDoubleRegisterValue(RegisterIndex lower) const noexcept {
            return retrieveDoubleRegister(lower);
        }
    public:
        void setRegisterValue(RegisterIndex idx, Integer value) noexcept {
            putRegister(idx, value);
        }
        void setRegisterValue(RegisterIndex idx, Ordinal value) noexcept {
            putRegister(idx, value);
        }
        void setRegsiterValue(RegisterIndex idx, bool value) noexcept {
            putRegister(idx, static_cast<Ordinal>(value ? 0xFFFF : 0x0000));
        }
        template<typename T = Ordinal>
        T getRegisterValue(RegisterIndex idx) const noexcept {
            return retrieveRegister(idx, 
                    std::conditional_t<std::is_same_v<T, Integer>, RequestInteger,
                    std::conditional_t<
                    std::is_same_v<T, bool>,
                    RequestBoolean,
                    RequestOrdinal>> { });
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
        virtual void advanceIP() const noexcept = 0;
        virtual void doNotAdvanceIP() noexcept = 0;
        virtual void allowAdvanceIP() noexcept = 0;
        virtual bool shouldAdvanceIP() noexcept = 0;
    private:
        RegisterBank _regs;
};


} // end namespace iris


#endif // end IRIS_H__
