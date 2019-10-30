/**
 * @file 
 * Description of all the opcodes that make up the programmer facing aspects of iris
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
#ifndef IRIS_OPCODES_H__
#define IRIS_OPCODES_H__
#include <variant>
#include <optional>
#include <tuple>
#include "types.h"

namespace iris {
    constexpr EncodedInstruction operator "" _opcode(unsigned long long int conversion) noexcept { 
        return static_cast<iris::EncodedInstruction>(conversion) << 24;
    }
    constexpr EncodedInstruction operator "" _group(unsigned long long int conversion) noexcept { 
        return static_cast<iris::EncodedInstruction>(conversion) << 29;
    }
    constexpr auto GroupArithmetic = 0_group;
    constexpr auto GroupCompare    = 1_group;
    constexpr auto GroupMemory     = 2_group;
    constexpr auto GroupBranchImm  = 3_group;
    constexpr auto GroupBranchReg  = 4_group;
    constexpr auto GroupBitwise    = 5_group;
    constexpr auto GroupMask       = 7_group;
    static_assert(GroupMask == 0xE000'0000);
    template<EncodedInstruction group>
        constexpr auto isOfGroup(EncodedInstruction enc) noexcept {
            return (enc & GroupMask) == group;
        }
    constexpr auto isBitwiseInstruction(EncodedInstruction enc) noexcept { return isOfGroup<GroupBitwise>(enc); }
    constexpr auto isArithmeticInstruction(EncodedInstruction enc) noexcept { return isOfGroup<GroupArithmetic>(enc); }
    constexpr auto isCompareInstruction(EncodedInstruction enc) noexcept { return isOfGroup<GroupCompare>(enc); }
    constexpr auto isMemoryInstruction(EncodedInstruction enc) noexcept { return isOfGroup<GroupMemory>(enc); }
    constexpr auto isBranchImmInstruction(EncodedInstruction enc) noexcept { return isOfGroup<GroupBranchImm>(enc); }
    constexpr auto isBranchRegInstruction(EncodedInstruction enc) noexcept { return isOfGroup<GroupBranchReg>(enc); }
    constexpr auto isBranchInstruction(EncodedInstruction enc) noexcept { return isBranchImmInstruction(enc) || isBranchRegInstruction(enc); }

    template<EncodedInstruction enc>
        constexpr auto IsBitwiseInstruction = isBitwiseInstruction(enc);
    template<EncodedInstruction enc>
        constexpr auto IsBranchInstruction = isBranchInstruction(enc);
    template<EncodedInstruction enc>
        constexpr auto IsBranchImmInstruction = isBranchImmInstruction(enc);
    template<EncodedInstruction enc>
        constexpr auto IsBranchRegInstruction = isBranchRegInstruction(enc);
    template<EncodedInstruction enc>
        constexpr auto IsArithmeticInstruction = isArithmeticInstruction(enc);
    template<EncodedInstruction enc>
        constexpr auto IsCompareInstruction = isCompareInstruction(enc);
    template<EncodedInstruction enc>
        constexpr auto IsMemoryInstruction = isMemoryInstruction(enc);
    template<EncodedInstruction mask>
        constexpr auto flagSet(EncodedInstruction enc) noexcept {
            return (enc & mask) != 0;
        }
    template<EncodedInstruction mask>
        constexpr auto flagClear(EncodedInstruction enc) noexcept {
            return (enc & mask) == 0;
        }
    template<EncodedInstruction quantity, EncodedInstruction mask>
        constexpr auto fieldSetTo(EncodedInstruction enc) noexcept {
            return (enc & mask) == quantity;
        }
    template<EncodedInstruction enc, EncodedInstruction mask>
        constexpr auto FlagSet = flagSet<mask>(enc);
    template<EncodedInstruction enc, EncodedInstruction mask>
        constexpr auto FlagClear = flagClear<mask>(enc);
    template<EncodedInstruction enc, EncodedInstruction mask, EncodedInstruction quantity>
        constexpr auto FieldSetTo = fieldSetTo<quantity, mask>(enc);
    namespace bits {
        //-----------------------------------------------------------------------------
        // Arithmetic
        //-----------------------------------------------------------------------------
        // Fields are: 0b000,0,K,TTT
        // Where:
        // C = Major Operation
        // K = Integer? else Ordinal
        // T = Operation
        constexpr auto KindOrdinal             = 0b00000000_opcode;
        constexpr auto KindInteger             = 0b00000001_opcode;
        constexpr auto ArithmeticOperationMask = 0b00001110_opcode;
    } // end namespace bits
    template<EncodedInstruction enc>
    constexpr auto OperationCaresAboutSign = (IsArithmeticInstruction<enc> || IsCompareInstruction<enc>);
#define X(op, value) \
    namespace bits { \
        constexpr auto Operation ## op = value ; \
    } \
    template<EncodedInstruction enc> \
    constexpr auto Is ## op ## Operation = IsArithmeticInstruction<enc> && FieldSetTo<enc, bits::ArithmeticOperationMask, bits::Operation ## op>; 
    X(Error     , 0b00000000_opcode);
    X(Add       , 0b00000010_opcode);
    X(Subtract  , 0b00000100_opcode);
    X(Multiply  , 0b00000110_opcode);
    X(Divide    , 0b00001000_opcode);
    X(Remainder , 0b00001010_opcode);
    X(ShiftLeft , 0b00001100_opcode);
    X(ShiftRight, 0b00001110_opcode);
#undef X
    template<EncodedInstruction enc>
    constexpr auto IsIntegerOperation = OperationCaresAboutSign<enc> && FlagSet<enc, bits::KindInteger>;
    template<EncodedInstruction enc>
    constexpr auto IsOrdinalOperation = OperationCaresAboutSign<enc> && FlagClear<enc, bits::KindInteger>;
    namespace bits {
        //-----------------------------------------------------------------------------
        // Bitwise 
        //-----------------------------------------------------------------------------
        // format is: 0b101,0,A,N,TT
        // where:
        // N: Not the result?
        // T: The operation
        // A: Imm16 argument
        constexpr auto BitwiseOperationMask   = 0b00000011_opcode;
    }
#define X(op, value) \
    namespace bits { \
        constexpr auto Operation ## op = value ; \
    } \
    template<EncodedInstruction enc> \
    constexpr auto Is ## op ## Operation = IsBitwiseInstruction<enc> && FieldSetTo<enc, bits::BitwiseOperationMask, bits::Operation ## op>; 
    X(Not           , 0b00000000_opcode);
    X(And           , 0b00000001_opcode);
    X(Or            , 0b00000010_opcode);
    X(Xor           , 0b00000011_opcode);
#undef X
    namespace bits {
        constexpr auto NotTheResult           = 0b00000100_opcode;
        constexpr auto ArgumentIsImm16        = 0b00001000_opcode;
    } // end namespace bits
    template<EncodedInstruction enc>
    constexpr auto NotTheResult = IsBitwiseInstruction<enc> && FlagSet<enc, bits::NotTheResult>;
    namespace bits {
        //-----------------------------------------------------------------------------
        // Memory
        //-----------------------------------------------------------------------------
        // format is: 0b010,L,A,SS
        // where 
        // L: Load? else Store
        // A: Arg1 Is Imm? else register + (offset imm)
        constexpr auto LoadOperation          = 0b00001000_opcode;
        constexpr auto StoreOperation         = 0b00000000_opcode; 
        constexpr auto Arg1IsImm              = 0b00000100_opcode; 
        constexpr auto Arg1IsRegister         = 0b00000000_opcode; 
        constexpr auto SpaceMask              = 0b00000011_opcode;
    } // end namespace bits
    template<EncodedInstruction enc>
    constexpr auto ArgumentIsImm16 = (IsBitwiseInstruction<enc> && FlagSet<enc, bits::ArgumentIsImm16>) ||
                                     (IsMemoryInstruction<enc> && FlagSet<enc, bits::Arg1IsImm>);
#define X(op, value) \
    namespace bits { \
        constexpr auto Space ## op = value; \
    } \
    template<EncodedInstruction enc> \
    constexpr auto IsSpace ## op = IsMemoryInstruction<enc> && FieldSetTo<enc, bits::SpaceMask, bits::Space ## op >;
    X(Code,  0b00_opcode);
    X(Data,  0b01_opcode);
    X(Stack, 0b10_opcode);
    X(IO,    0b11_opcode);
#undef X
    namespace bits {
        //-----------------------------------------------------------------------------
        // Compare
        //-----------------------------------------------------------------------------
        // Format is 0b001,0000,K
        // Where K is Integer? else Ordinal
        //-----------------------------------------------------------------------------
        // Branches
        //-----------------------------------------------------------------------------
        // these code kinds and descriptions are taken from how the i960 works with 
        // conditions. The difference is that iris does not have a single condition code
        // register. Instead one of the gprs is used for that purpose since there is a
        // generous number of registers provided.
        // never branch
        constexpr auto BranchIfUnordered      = 0b00000000_opcode;
        constexpr auto BranchIfGreater        = 0b00000001_opcode;
        constexpr auto BranchIfEqual          = 0b00000010_opcode;
        constexpr auto BranchIfGreaterOrEqual = 0b00000011_opcode;
        constexpr auto BranchIfLess           = 0b00000100_opcode;
        constexpr auto BranchIfNotEqual       = 0b00000101_opcode;
        constexpr auto BranchIfLessOrEqual    = 0b00000110_opcode;
        constexpr auto BranchIfOrdered        = 0b00000111_opcode;
        constexpr auto BranchIfMask           = BranchIfOrdered;
    } // end namespace bits
    template<EncodedInstruction enc>
    constexpr auto BranchMask = (enc & bits::BranchIfMask);
    namespace bits {
        //-----------------------------------------------------------------------------
        // Branch Immediate
        //-----------------------------------------------------------------------------
        // Top level format is 0b011,C,xxxx
        // where 
        // C: Branch Conditional? else Link

        // if C is 1 then the format is: 0b011,1,R,YYY, this is known as the conditional immediate form
        // R: Relative? else Absolute
        // Y: Condition codes defined above

        // if C is 0 then the format is: 0b011,0,R,000, this is known as the link immediate form
        // R: Relative? else Absolute
        // there is not enough space in the encoding to allow conditional jump and link in a single instruction
        constexpr auto IsConditional = 0b00010000_opcode;
        constexpr auto IsLink        = 0b00000000_opcode;
        constexpr auto RelativeJump  = 0b00001000_opcode;
        constexpr auto AbsoluteJump  = 0b00000000_opcode;
    } // end namespace bits
    template<EncodedInstruction enc>
    constexpr auto IsRelativeJump = IsBranchImmInstruction<enc> && flagSet<bits::RelativeJump>(enc);
    template<EncodedInstruction enc>
    constexpr auto IsAbsoluteJump = IsBranchImmInstruction<enc> && flagClear<bits::RelativeJump>(enc);
    template<EncodedInstruction enc>
    constexpr auto IsLink = IsBranchImmInstruction<enc> && flagClear<bits::IsConditional>(enc);
    template<EncodedInstruction enc>
    constexpr auto IsConditional = IsBranchImmInstruction<enc> && flagSet<bits::IsConditional>(enc);
    namespace bits {
        //-----------------------------------------------------------------------------
        // Branch Register
        //-----------------------------------------------------------------------------
        // format is: 0b100,PP,YYY
        // P is a register action kind
        // 00 -> ConditionalBranchAndLink
        // 01 -> Select
        // 10 -> unused
        // 11 -> unused
        // Y is condition codes defined above
        constexpr auto IsConditionalBranchAndLink =  0b00000000_opcode;
        constexpr auto IsSelectOperation          =  0b00001000_opcode;
        constexpr auto BranchRegisterOperationMask = 0b00011000_opcode;
        //-----------------------------------------------------------------------------
    } // end namespace flags
    template<EncodedInstruction enc>
    constexpr auto IsConditionalBranchAndLink = IsBranchRegInstruction<enc> && FieldSetTo<enc, bits::BranchRegisterOperationMask, bits::IsConditionalBranchAndLink>;
    template<EncodedInstruction enc>
    constexpr auto IsSelectOperation = IsBranchRegInstruction<enc> && FieldSetTo<enc, bits::BranchRegisterOperationMask, bits::IsSelectOperation>;
    enum class Opcodes : EncodedInstruction {
        Error = 0,
#define X(t, _, o) t = o ,
#include "InstructionFormats.def"
#undef X
    };
    struct ErrorInstruction;
    using OpcodesNumericType = std::underlying_type_t<Opcodes>; 
    // forward declare the formats and perform sanity checks
#define X(name, __, o) \
    static_assert(o < LargestOpcode, "Operation " #name " is out of encoding space!"); \
    struct name  ## Instruction;
#include "InstructionFormats.def"
#undef X
    constexpr auto LargestOpcode = 0xFF00'0000;
    constexpr auto extractOpcode(EncodedInstruction inst) noexcept {
        return LargestOpcode & inst;
    }

    /**
     * The fields of an iris instruction are:
     * [0,7] Opcode
     * [8,15] Arg0 
     * [16,23] Arg1 
     * [24,31] Arg2 
     * [16,31] Immediate16
     * [24,31] Immediate8
     *
     * The formats are:
     * Opcode, Arg0, Arg1, Arg2(3reg)
     * Opcode, Arg0, Arg1, Immediate8 (2reg+i8)
     * Opcode, Arg0, Immediate16 (1reg + i16)
     * Opcode, Arg0, Arg1 (2reg)
     * Opcode, Arg0 (1reg)
     * Opcode, Immediate8 (i8)
     * Opcode, Immediate16 (i16)
     * Opcode (0arg)
     *
     * All of the fields are always in the same place as well. Thus, requesting a destination
     * as an imm16 will actually pull from the Immediate16 field. Group and Operation are actually
     * implied, the simulator just dispatches on the 8-bit code directly. The separation is 
     * purely there to separate instructions out. 
     */
    class Instruction {
        private:
            template<typename T = RegisterIndex>
                static constexpr T convertByteIndex(Byte result) noexcept {
                    if constexpr (std::is_same_v<T, RegisterIndex>) {
                        return static_cast<RegisterIndex>(result);
                    } else if constexpr (std::is_same_v<T, Byte>) {
                        return result;
                    } else if constexpr (std::is_same_v<T, SignedByte>) {
                        union temporary {
                            temporary(Byte u) : _u(u) { }
                            Byte _u;
                            SignedByte _s;
                        } ;
                        return temporary(result)._s;
                    } else {
                        static_assert(false_v<T>, "Illegal type requested!");
                    }
                }
        public:
            explicit constexpr Instruction(DoubleWord bits = 0) noexcept : _bits(bits) { }
            explicit constexpr Instruction(Opcodes opcode) noexcept : _bits(static_cast<decltype(_bits)>(opcode)) { }
            template<typename T>
                Instruction(Opcodes opcode, T arg0) noexcept : Instruction(opcode) {
                    setArg0(arg0);
                }
            template<typename T0, typename T1>
                Instruction(Opcodes opcode, T0 arg0, T1 arg1) noexcept : Instruction(opcode, arg0) {
                    setArg1(arg1);
                }
            template<typename T0, typename T1, typename T2>
                Instruction(Opcodes opcode, T0 arg0, T1 arg1, T2 arg2) noexcept : Instruction(opcode, arg0, arg1) {
                    setArg2(arg2);
                }
            constexpr Byte getLowestQuarter() const noexcept { 
                return decodeBits<DoubleWord, Byte, 0xFF, 0>(_bits); 
            }
            constexpr Byte getLowerQuarter() const noexcept { 
                return decodeBits<DoubleWord, Byte, 0xFF00, 8>(_bits);
            }
            constexpr Byte getHigherQuarter() const noexcept { 
                return decodeBits<DoubleWord, Byte, 0xFF0000, 16>(_bits);
            }
            constexpr Byte getHighestQuarter() const noexcept { 
                return decodeBits<DoubleWord, Byte, 0xFF000000, 24>(_bits);
            }
            constexpr Word getUpperHalf() const noexcept { return (_bits >> 16); }
            constexpr Word getLowerHalf() const noexcept { return _bits; }
            constexpr Byte getOpcodeIndex() const noexcept { return getLowestQuarter(); }
        private:
            template<typename T>
                static constexpr auto IsU8 = std::is_same_v<std::decay_t<T>, UnsignedByte>;
            template<typename T>
                static constexpr auto IsS8 = std::is_same_v<std::decay_t<T>, SignedByte>;
            template<typename T>
                static constexpr auto IsImm8 = IsU8<T> || IsS8<T>;
            template<typename T>
                static constexpr auto IsImm16 = std::is_same_v<std::decay_t<T>, Address>;
            template<typename T>
                static constexpr auto IsS16 = std::is_same_v<std::decay_t<T>, Offset16>;
        public:
            template<typename T = RegisterIndex>
                constexpr T getArg0() const noexcept { 
                    if constexpr (IsImm16<T>) {
                        return getImm16();
                    } else if constexpr (IsS16<T>) {
                        return static_cast<Offset16>(getImm16());
                    } else if constexpr (IsImm8<T>) {
                        return convertByteIndex<T>(getImm8());
                    } else {
                        return convertByteIndex<T>(getLowerQuarter());
                    }
                }
            template<typename T = RegisterIndex>
                constexpr T getArg1() const noexcept {
                    if constexpr (IsImm16<T>) {
                        return getImm16();
                    } else if constexpr (IsS16<T>) {
                        return static_cast<Offset16>(getImm16());
                    } else if constexpr (IsImm8<T>) {
                        return convertByteIndex<T>(getImm8());
                    } else {
                        return convertByteIndex<T>(getHigherQuarter());
                    }
                }
            template<typename T = RegisterIndex>
                constexpr T getArg2() const noexcept { 
                    if constexpr (IsImm16<T>) {
                        return getImm16();
                    } else if constexpr (IsS16<T>) {
                        return static_cast<Offset16>(getImm16());
                    } else if constexpr (IsImm8<T>) {
                        return convertByteIndex<T>(getImm8());
                    } else {
                        return convertByteIndex<T>(getHighestQuarter());
                    }
                }
            constexpr Byte getImm8() const noexcept { return getHighestQuarter(); }
            constexpr Word getImm16() const noexcept { return getUpperHalf(); }
            constexpr auto getRawBits() const noexcept { return _bits; }

        public:
            void setLowestQuarter(Byte value) noexcept;
            void setLowerQuarter(Byte value) noexcept;
            void setHigherQuarter(Byte value) noexcept;
            void setHighestQuarter(Byte value) noexcept;
            void setLowerHalf(UnsignedWord value) noexcept;
            void setUpperHalf(UnsignedWord value) noexcept;
            void setImm16(UnsignedWord value) noexcept;
            void setImm8(UnsignedByte value) noexcept;
            void setArg0(RegisterIndex value) noexcept;
            void setArg1(RegisterIndex value) noexcept;
            void setArg2(RegisterIndex value) noexcept;
            void setOpcode(Opcodes opcode) noexcept;
            void setOpcode(UnsignedByte value) noexcept;
            template<typename T>
                void setArg0(T value) {
                    using K = std::decay_t<T>;
                    if constexpr (std::is_same_v<K, RegisterIndex>) {
                        setArg0(value);
                    } else if constexpr (std::is_same_v<K, UnsignedByte>) {
                        setImm8(value);
                    } else if constexpr (std::is_same_v<K, SignedByte>) {
                        setImm8(static_cast<UnsignedByte>(value));
                    } else if constexpr (std::is_same_v<K, UnsignedWord>) {
                        setImm16(value);
                    } else if constexpr (IsS16<T>) {
                        setImm16(value);
                    } else {
                        static_assert(false_v<T>, "Illegal type!");
                    }
                }
            template<typename T>
                void setArg1(T value) {
                    using K = std::decay_t<T>;
                    if constexpr (std::is_same_v<K, RegisterIndex>) {
                        setArg1(value);
                    } else if constexpr (std::is_same_v<K, UnsignedByte>) {
                        setImm8(value);
                    } else if constexpr (std::is_same_v<K, SignedByte>) {
                        setImm8(static_cast<UnsignedByte>(value));
                    } else if constexpr (std::is_same_v<K, UnsignedWord>) {
                        setImm16(value);
                    } else if constexpr (IsS16<T>) {
                        setImm16(value);
                    } else {
                        static_assert(false_v<T>, "Illegal type!");
                    }
                }
            template<typename T>
                void setArg2(T value) {
                    using K = std::decay_t<T>;
                    if constexpr (std::is_same_v<K, RegisterIndex>) {
                        setArg2(value);
                    } else if constexpr (std::is_same_v<K, UnsignedByte>) {
                        setImm8(value);
                    } else if constexpr (std::is_same_v<K, SignedByte>) {
                        setImm8(static_cast<UnsignedByte>(value));
                    } else if constexpr (IsImm16<T>) {
                        setImm16(value);
                    } else {
                        static_assert(false_v<T>, "Illegal type!");
                    }
                }
        private:
            DoubleWord _bits;
    };

    template<Byte opcode>
        class DispatchableInstruction {
            private:
                template<Byte mask>
                    static constexpr bool CommonCompareBody = (opcode & mask) == mask;
            public:
                DispatchableInstruction() = delete;
                ~DispatchableInstruction() = delete;
                DispatchableInstruction(const DispatchableInstruction&) = delete;
                DispatchableInstruction(DispatchableInstruction&&) = delete;
                DispatchableInstruction operator=(const DispatchableInstruction&) = delete;
                DispatchableInstruction operator=(DispatchableInstruction&&) = delete;
            private:
                constexpr static Byte MajorOpcode_Branch = 0b1000'0000;
                constexpr static Byte ConditionalCode_NeverTake = MajorOpcode_Branch | 0b000;
                constexpr static Byte ConditionalCode_GreaterThan = MajorOpcode_Branch | 0b001;
                constexpr static Byte ConditionalCode_Equals = MajorOpcode_Branch | 0b010;
                constexpr static Byte ConditionalCode_LessThan = MajorOpcode_Branch | 0b100;
                constexpr static Byte ConditionalCode_LessThanOrEqual = ConditionalCode_LessThan | ConditionalCode_Equals;
                constexpr static Byte ConditionalCode_GreaterThanOrEqual = ConditionalCode_GreaterThan | ConditionalCode_Equals;
                constexpr static Byte ConditionalCode_Unconditional = ConditionalCode_GreaterThan | ConditionalCode_LessThan | ConditionalCode_Equals;
                constexpr static Byte ConditionalCode_NotEquals = ConditionalCode_LessThan | ConditionalCode_GreaterThan;
                constexpr static Byte BranchAction_TreatTargetAsRelative = MajorOpcode_Branch | 0b0'00'000;
                constexpr static Byte BranchAction_TreatTargetAsAbsolute = MajorOpcode_Branch | 0b1'00'000;
                constexpr static Byte BranchAction_RegisterForm = MajorOpcode_Branch | 0b0'0'000;
                constexpr static Byte BranchAction_ImmediateForm = MajorOpcode_Branch | 0b1'0'000;
            public:
#define X(name, mask) \
                static constexpr auto Is ## name ## Instruction = CommonCompareBody<mask>
                X(Branch, MajorOpcode_Branch);
                // conditional codes
                X(NeverTaken, ConditionalCode_NeverTake);
                X(GreaterThan, ConditionalCode_GreaterThan);
                X(LessThan, ConditionalCode_LessThan);
                X(GreaterThanOrEqual, ConditionalCode_GreaterThanOrEqual);
                X(LessThanOrEqual, ConditionalCode_LessThanOrEqual);
                X(Unconditional, ConditionalCode_Unconditional);
                X(NotEquals, ConditionalCode_NotEquals);
                X(Equals, ConditionalCode_Equals);
                X(RelativeAddress, BranchAction_TreatTargetAsRelative);
                X(AbsoluteAddress, BranchAction_TreatTargetAsAbsolute);
                X(RegisterForm, BranchAction_RegisterForm);
                X(ImmediateForm, BranchAction_ImmediateForm);
#undef X
            private:
                constexpr static Byte MajorOpcode_Common = 0b0000'0000;
                constexpr static Byte Common_Error = MajorOpcode_Common | 0b00'0000;
                constexpr static Byte Common_Arithmetic = MajorOpcode_Common | 0b01'0000;
                constexpr static Byte TreatArithmeticOperationAsInteger = Common_Arithmetic | 0b0000;
                constexpr static Byte TreatArithmeticOperationAsOrdinal = Common_Arithmetic | 0b1000;
                constexpr static Byte ArithmeticOperation_Add           = Common_Arithmetic | 0b000;
                constexpr static Byte ArithmeticOperation_Subtract      = Common_Arithmetic | 0b001;
                constexpr static Byte ArithmeticOperation_Multiply      = Common_Arithmetic | 0b010;
                constexpr static Byte ArithmeticOperation_Divide        = Common_Arithmetic | 0b011;
                constexpr static Byte ArithmeticOperation_Remainder     = Common_Arithmetic | 0b100;
                constexpr static Byte ArithmeticOperation_ShiftLeft     = Common_Arithmetic | 0b101;
                constexpr static Byte ArithmeticOperation_ShiftRight    = Common_Arithmetic | 0b110;
                constexpr static Byte Common_Bitwise = MajorOpcode_Common | 0b10'0000;
                constexpr static Byte BitwiseOperation_Not = Common_Bitwise | 0b00;
                constexpr static Byte BitwiseOperation_And = Common_Bitwise | 0b01;
                constexpr static Byte BitwiseOperation_Or  = Common_Bitwise | 0b10;
                constexpr static Byte BitwiseOperation_Xor = Common_Bitwise | 0b11;
                constexpr static Byte Common_Compare = MajorOpcode_Common | 0b10'0100;
                constexpr static Byte CompareOperation_Integer = Common_Compare | 0b0;
                constexpr static Byte CompareOperation_Ordinal = Common_Compare | 0b1;
            public:
#define X(name, mask) \
                static constexpr auto Is ## name ## Instruction = CommonCompareBody<mask>
                X(Common, MajorOpcode_Common);
                X(Error, Common_Error);
                X(Arithmetic, Common_Arithmetic);
                X(Add, ArithmeticOperation_Add);
                X(Subtract, ArithmeticOperation_Subtract);
                X(Multiply, ArithmeticOperation_Multiply);
                X(Divide, ArithmeticOperation_Divide);
                X(Remainder, ArithmeticOperation_Remainder);
                X(ShiftLeft, ArithmeticOperation_ShiftLeft);
                X(ShiftRight, ArithmeticOperation_ShiftRight);
                X(Bitwise, Common_Bitwise);
                X(Not, BitwiseOperation_Not);
                X(And, BitwiseOperation_And);
                X(Or, BitwiseOperation_Or);
                X(Xor, BitwiseOperation_Xor);
                X(Compare, Common_Compare);
#undef X
                static constexpr auto IsShiftInstruction = IsShiftLeftInstruction || IsShiftRightInstruction;
                static constexpr auto Src2CannotBeZero = IsDivideInstruction || IsRemainderInstruction;
                static constexpr auto IsIntegerInstruction = (CommonCompareBody<CompareOperation_Integer> ||
                        CommonCompareBody<TreatArithmeticOperationAsInteger>);
                static constexpr auto IsOrdinalInstruction = (CommonCompareBody<CompareOperation_Ordinal> ||
                        CommonCompareBody<TreatArithmeticOperationAsOrdinal>);
            private:
                constexpr static Byte MajorOpcode_Memory = 0b0100'0000;
                constexpr static Byte MemoryManipulateKind = MajorOpcode_Memory | 0b00'0000;
                constexpr static Byte MemoryLoadKind = MemoryManipulateKind | 0b00'1000;
                constexpr static Byte MemoryStoreKind = MemoryManipulateKind | 0b00'0000;
                constexpr static Byte MemoryManipulateCode = MemoryManipulateKind | 0b00;
                constexpr static Byte MemoryManipulateData = MemoryManipulateKind | 0b01;
                constexpr static Byte MemoryManipulateStack = MemoryManipulateKind | 0b10;
                constexpr static Byte MemoryManipulateIO = MemoryManipulateKind | 0b11;
                constexpr static Byte MemoryMiscKind = MajorOpcode_Memory | 0b00'1000;
                constexpr static Byte AssignRegisterImmediate = MemoryMiscKind | 0b000;
            public:
#define X(name, mask) \
                static constexpr auto Is ## name ## Instruction = CommonCompareBody<mask>
                // common ops
                // memory ops
                X(Memory, MajorOpcode_Memory);
                X(LoadStore, MemoryManipulateKind);
                X(MiscMemory, MemoryMiscKind);
                X(AssignRegister, AssignRegisterImmediate);
                X(MemoryStore, MemoryStoreKind);
                X(MemoryLoad, MemoryLoadKind);
#undef X
                static constexpr auto TargetsCodeSpace = CommonCompareBody<MemoryManipulateCode>;
                static constexpr auto TargetsIOSpace = CommonCompareBody<MemoryManipulateIO>;
                static constexpr auto TargetsStackSpace = CommonCompareBody<MemoryManipulateStack>;
                static constexpr auto TargetsDataSpace = CommonCompareBody<MemoryManipulateData>;
            private:
                constexpr static Byte MajorOpcode_Unused = 0b1100'0000;
        };

    template<Opcodes op>
        class ArgumentFormat {
            public:
                static constexpr OpcodesNumericType RawValue = static_cast<OpcodesNumericType>(op);
                static constexpr auto TargetOpcode = op;
                explicit constexpr ArgumentFormat(const Instruction&) { }
                constexpr ArgumentFormat() = default;
                constexpr auto getOpcode() const noexcept { return op; }
                constexpr auto getRawValue() const noexcept { return RawValue; }
        };
    template<typename T, Opcodes op>
        class ThreeArgumentsFormat : public ArgumentFormat<op> {
            public:
                using Parent = ArgumentFormat<op>;
                explicit constexpr ThreeArgumentsFormat(const Instruction& inst) : Parent(inst), _first(inst.getArg0()), _second(inst.getArg1()), _third(inst.getArg2<T>()) { }
                constexpr ThreeArgumentsFormat(RegisterIndex first, RegisterIndex second, T third) : _first(first), _second(second), _third(third) { }
                constexpr auto getFirst() const noexcept { return _first; }
                constexpr auto getSecond() const noexcept { return _second; }
                constexpr auto getThird() const noexcept { return _third; }
                constexpr std::tuple<RegisterIndex, RegisterIndex, T> arguments() const noexcept { return std::make_tuple(_first, _second, _third); }
                constexpr explicit operator Instruction() const noexcept { return { this->getOpcode(), _first, _second, _third }; }
            private:
                RegisterIndex _first;
                RegisterIndex _second;
                T _third;
        };
    template<typename T, Opcodes op>
        class TwoArgumentsFormat : public ArgumentFormat<op> {
            public:
                using Parent = ArgumentFormat<op>;
                explicit constexpr TwoArgumentsFormat(const Instruction& inst) : Parent(inst), _first(inst.getArg0()), _second(inst.getArg1<T>()) { }
                constexpr TwoArgumentsFormat(RegisterIndex first, T second) : _first(first), _second(second) { }
                constexpr auto getFirst() const noexcept { return _first; }
                constexpr auto getSecond() const noexcept { return _second; }
                constexpr std::tuple<RegisterIndex, T> arguments() const noexcept { return std::make_tuple(_first, _second); }
                constexpr explicit operator Instruction() const noexcept { return { this->getOpcode(), _first, _second }; }
            private:
                RegisterIndex _first;
                T _second;
        };
    template<typename T, Opcodes op>
        class OneArgumentFormat : public ArgumentFormat<op> {
            public:
                using Parent = ArgumentFormat<op>;
                explicit constexpr OneArgumentFormat(const Instruction& inst) : Parent(inst), _first(inst.getArg0<T>()) { }
                constexpr OneArgumentFormat(T first) : _first(first) { }
                constexpr auto getFirst() const noexcept { return _first; }
                constexpr std::tuple<T> arguments() const noexcept { return std::make_tuple(_first); }
                constexpr explicit operator Instruction() const noexcept { return { this->getOpcode(), _first }; }
            private:
                T _first;
        };
    template<Opcodes op>
        class ZeroArgumentFormat : public ArgumentFormat<op> { 
            public:
                using Parent = ArgumentFormat<op>;
                using Parent::Parent;
                constexpr explicit operator Instruction() const noexcept { return { this->getOpcode() }; }
        };
    template<Opcodes op>
        using ThreeRegisterFormat = ThreeArgumentsFormat<RegisterIndex, op>;
    template<Opcodes op>
        using TwoRegisterU8Format = ThreeArgumentsFormat<Byte, op>;
    template<Opcodes op>
        using TwoRegisterS8Format = ThreeArgumentsFormat<SignedByte, op>;
    template<Opcodes op>
        using TwoRegisterFormat = TwoArgumentsFormat<RegisterIndex, op>;
    template<Opcodes op>
        using OneRegisterU16Format = TwoArgumentsFormat<UnsignedWord, op>;
    template<Opcodes op>
        using OneRegisterS16Format = TwoArgumentsFormat<SignedWord, op>;
    template<Opcodes op>
        using OneRegisterU8Format = TwoArgumentsFormat<Byte, op>;
    template<Opcodes op>
        using OneRegisterS8Format = TwoArgumentsFormat<SignedByte, op>;
    template<Opcodes op>
        using OneRegisterFormat = OneArgumentFormat<RegisterIndex, op>;
    template<Opcodes op>
        using U16Format = OneArgumentFormat<UnsignedWord, op>;
    template<Opcodes op>
        using S16Format = OneArgumentFormat<Offset16, op>;
    template<Opcodes op>
        using U8Format = OneArgumentFormat<Byte, op>;
    template<Opcodes op>
        using S8Format = OneArgumentFormat<SignedByte, op>;

    template<typename T>
        constexpr auto IsThreeArgumentFormat = std::is_base_of_v<ThreeRegisterFormat<T::Opcode>, T> ||
        std::is_base_of_v<TwoRegisterS8Format<T::Opcode>, T> ||
        std::is_base_of_v<TwoRegisterU8Format<T::Opcode>, T>;
    template<typename T>
        constexpr auto TreatArg2AsImmediate =  (IsThreeArgumentFormat<T> && 
                (std::is_base_of_v<TwoRegisterU8Format<T::Opcode>, T> || 
                 std::is_base_of_v<TwoRegisterS8Format<T::Opcode>, T>)) ||
        (std::is_base_of_v<U8Format<T::Opcode>, T> ||
         std::is_base_of_v<S8Format<T::Opcode>, T>);


    template<typename T>
        constexpr auto UsesRelativeImmediateOffset = std::is_base_of_v<OneRegisterS16Format<T::Opcode>, T> || std::is_base_of_v<S16Format, T>;

    template<typename T>
        constexpr auto UsesAbsoluteImmediatePosition = std::is_base_of_v<OneRegisterU16Format<T::Opcode>, T> || std::is_base_of_v<U16Format, T>;

    template<typename T>
        constexpr auto IsSignedImmediate8Operation = (TreatArg2AsImmediate<T> && std::is_base_of_v<TwoRegisterS8Format<T::Opcode>, T>);

    template<typename T>
        constexpr auto IsUnsignedImmediate8Operation = (TreatArg2AsImmediate<T> && std::is_base_of_v<TwoRegisterU8Format<T::Opcode>, T>);

    template<typename T>
        constexpr auto IsIntegerOperation = IsSignedImmediate8Operation<T>;

    template<typename T>
        constexpr auto IsOrdinalOperation = IsUnsignedImmediate8Operation<T>;

    template<typename T>
        constexpr auto MustBeIntegerOrOrdinalOperation = (IsOrdinalOperation<std::decay_t<T>> || IsIntegerOperation<std::decay_t<T>>);

    template<typename T, std::enable_if_t<MustBeIntegerOrOrdinalOperation<T>, int> = 0>
        using DeterminedNumericType = std::conditional_t<IsOrdinalOperation<std::decay_t<T>>, UnsignedWord, SignedWord>;

    template<typename T> constexpr auto IsStackOperation = false;
    template<typename T> constexpr auto IsCodeOperation = false;
    template<typename T> constexpr auto IsDataOperation = false;
    template<typename T> constexpr auto IsIOOperation = false;
    template<typename T> constexpr auto IsBranchImmediateInstruction = false;
    template<typename T> constexpr auto UsesRelativeOffset = false;
    template<typename T> constexpr auto UsesLinkRegister = false;
    template<typename T> constexpr auto Src2CannotBeZero = false;
    template<typename T> constexpr auto IsDivideOperation = false;
    template<typename T> constexpr auto IsRemainderOperation = false;
    template<typename T> constexpr auto IsMemoryOperation = false;
    template<typename T> constexpr auto IsBranchOperation = false;
    template<typename T> constexpr auto IsArithmeticOperation = false;
    template<typename T> constexpr auto IsCompareOperation = false;
    template<typename T> constexpr auto IsAddOperation = false;
    template<typename T> constexpr auto IsSubtractOperation = false;
    template<typename T> constexpr auto IsMultiplyOperation = false;
    template<typename T> constexpr auto IsShiftLeftOperation = false;
    template<typename T> constexpr auto IsShiftRightOperation = false;
    template<typename T> constexpr auto IsAddImmediateOperation = false;
    template<typename T> constexpr auto IsSubtractImmediateOperation = false;
    template<typename T> constexpr auto IsMultiplyImmediateOperation = false;
    template<typename T> constexpr auto IsShiftLeftImmediateOperation = false;
    template<typename T> constexpr auto IsShiftRightImmediateOperation = false;
    template<typename T> constexpr auto IsLogicalOperation = false;
    template<typename T> constexpr auto IsBitwiseOrOperation = false;
    template<typename T> constexpr auto IsBitwiseNorOperation = false;
    template<typename T> constexpr auto IsBitwiseAndOperation = false;
    template<typename T> constexpr auto IsBitwiseNandOperation = false;
    template<typename T> constexpr auto IsBitwiseNotOperation = false;
    template<typename T> constexpr auto IsBitwiseXorOperation = false;
    template<typename T> constexpr auto NotTheResult = false;
    template<typename T> constexpr auto IsMaxOperation = false;
    template<typename T> constexpr auto IsMinOperation = false;
    template<typename T> constexpr auto IsGreaterThanOrEqualToOperation = false;
    template<typename T> constexpr auto IsGreaterThanOperation = false;
    template<typename T> constexpr auto IsLessThanOrEqualToOperation = false;
    template<typename T> constexpr auto IsLessThanOperation = false;
    template<typename T> constexpr auto IsEqualsOperation = false;
    template<typename T> constexpr auto IsNotEqualsOperation = false;
    template<typename T> constexpr auto IsGPRManipulatorOperation = false;
    template<typename T> constexpr auto IsPopOperation = false;
    template<typename T> constexpr auto IsPushOperation = false;
    template<typename T> constexpr auto IsCopyRegisterOperation = false;
    template<typename T> constexpr auto IsAssignRegisterImmediateOperation = false;
    template<typename T> constexpr auto IsLoadOperation = false;
    template<typename T> constexpr auto IsStoreOperation = false;
    template<typename T> constexpr auto IsStoreImmediateOperation = false;
    template<typename T> constexpr auto IsSelectOperation = false;
    template<typename T> constexpr auto IsConditionalOperation = false;
    template<typename T> constexpr auto IsErrorOperation = false;
    template<typename T> constexpr auto IsBranchRegisterInstruction = false;

    struct ErrorInstruction final : public ZeroArgumentFormat<Opcodes::Error> {
        static constexpr auto Opcode = Opcodes::Error; 
        using Parent = ZeroArgumentFormat<Opcodes::Error>;
        using Parent::Parent;
    };
    // define the actual instruction kinds
#define X(t, f, o) \
    struct t ## Instruction final : public f ## Format < Opcodes :: t > { \
        static constexpr auto Opcode = Opcodes:: t ; \
        using Parent = f ## Format < Opcodes:: t > ; \
        using Parent::Parent; \
    };
#include "InstructionFormats.def"
#undef X
#define DeclareProperty(op, prop) \
    template<> constexpr auto prop < op ## Instruction > = true; 
#include "InstructionProperties.def"
#undef DeclareProperty


} // end namespace iris
#endif // end IRIS_OPCODES_H__
