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
    constexpr auto GroupBranch     = 3_group;
    constexpr auto GroupBitwise    = 4_group;
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
    constexpr auto isBranchInstruction(EncodedInstruction enc) noexcept { return isOfGroup<GroupBranch>(enc); }

    template<EncodedInstruction enc>
    constexpr auto IsBitwiseInstruction = isBitwiseInstruction(enc);
    template<EncodedInstruction enc>
    constexpr auto IsLogicalOperation = IsBitwiseInstruction<enc>;
    template<EncodedInstruction enc>
    constexpr auto IsBranchInstruction = isBranchInstruction(enc);
    template<EncodedInstruction enc>
    constexpr auto IsArithmeticInstruction = isArithmeticInstruction(enc);
    template<EncodedInstruction enc>
    constexpr auto IsArithmeticOperation = IsArithmeticInstruction<enc>;
    template<EncodedInstruction enc>
    constexpr auto IsCompareInstruction = isCompareInstruction(enc);
    template<EncodedInstruction enc>
    constexpr auto IsCompareOperation = IsCompareInstruction<enc>;
    template<EncodedInstruction enc>
    constexpr auto IsMemoryInstruction = isMemoryInstruction(enc);
    template<EncodedInstruction enc>
    constexpr auto IsMemoryOperation = IsMemoryInstruction<enc>;
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
        constexpr auto KindOrdinal             = 0b000'0'000'0_opcode;
        constexpr auto KindInteger             = 0b000'0'000'1_opcode;
        constexpr auto ArithmeticOperationMask = 0b000'0'111'0_opcode;
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
    constexpr auto Src2CannotBeZero = IsDivideOperation<enc> || IsRemainderOperation<enc>;
    namespace bits {
        //-----------------------------------------------------------------------------
        // Bitwise 
        //-----------------------------------------------------------------------------
        // format is: 0b101,0,A,N,TT
        // where:
        // N: Not the result?
        // T: The operation
        // A: Imm16 argument
        constexpr auto BitwiseOperationMask   = 0b000'000'11_opcode;
    }
#define X(op, value) \
    namespace bits { \
        constexpr auto Operation ## op = value ; \
    } \
    template<EncodedInstruction enc> \
    constexpr auto Is ## op ## Operation = IsBitwiseInstruction<enc> && FieldSetTo<enc, bits::BitwiseOperationMask, bits::Operation ## op>; 
    X(Not           , 0b000'0'00'00_opcode);
    X(And           , 0b000'0'00'01_opcode);
    X(Or            , 0b000'0'00'10_opcode);
    X(Xor           , 0b000'0'00'11_opcode);
#undef X
    namespace bits {
        constexpr auto NotTheResult           = 0b000'0'0'1'00_opcode;
        constexpr auto ArgumentIsImm16        = 0b000'0'1'0'00_opcode;
    } // end namespace bits
    template<EncodedInstruction enc>
    constexpr auto NotTheResult = IsBitwiseInstruction<enc> && FlagSet<enc, bits::NotTheResult>;
    namespace bits {
        //-----------------------------------------------------------------------------
        // Memory
        //-----------------------------------------------------------------------------
        constexpr auto LoadOperation            = 0b000'1'00'0'0_opcode;
        constexpr auto StoreOperation           = 0b000'0'00'0'0_opcode;
        constexpr auto MemoryWidthMask          = 0b000'0'11'0'0_opcode;
        constexpr auto MemoryWidthWord          = 0b000'0'00'0'0_opcode;
        constexpr auto MemoryWidthHalf          = 0b000'0'01'0'0_opcode;
        constexpr auto MemoryWidthByte          = 0b000'0'10'0'0_opcode;
        constexpr auto MemoryWidthImmediate16   = 0b000'0'11'0'0_opcode;
        constexpr auto MemoryWidthReserved      = 0b000'0'11'0'0_opcode;
        constexpr auto DoNotUpdateSource        = 0b000'0'00'0'0_opcode;
        constexpr auto UpdateSource             = 0b000'0'00'1'0_opcode;
        constexpr auto TreatArg2AsSignedImm8    = 0b000'0'00'0'0_opcode;
        constexpr auto TreatArg2AsRegisterIndex = 0b000'0'00'0'1_opcode;
        constexpr auto DoNotShiftImmediateBy16  = 0b000'0'00'0'0_opcode;
        constexpr auto ShiftImmediateBy16       = 0b000'0'00'1'0_opcode;
    } // end namespace bits
    template<EncodedInstruction enc>
    constexpr auto IsLoadOperation = IsMemoryInstruction<enc> && FlagSet<enc, bits::LoadOperation>;
    template<EncodedInstruction enc>
    constexpr auto IsStoreOperation = IsMemoryInstruction<enc> && FlagClear<enc, bits::LoadOperation>;
    template<EncodedInstruction enc>
    constexpr auto MemoryWidthIsImmediate16 = IsLoadOperation<enc> && FieldSetTo<enc, bits::MemoryWidthMask, bits::MemoryWidthImmediate16>;
    template<EncodedInstruction enc>
    constexpr auto ArgumentIsImm16 = (IsBitwiseInstruction<enc> && FlagSet<enc, bits::ArgumentIsImm16>) ||
                                     MemoryWidthIsImmediate16<enc>;
    template<EncodedInstruction enc>
    constexpr auto UpdateSourceRegister = IsMemoryInstruction<enc> && FlagSet<enc, bits::UpdateSource>;
    template<EncodedInstruction enc>
    constexpr auto ShouldShiftImmediateBy16 = MemoryWidthIsImmediate16<enc> && FlagSet<enc, bits::ShiftImmediateBy16>;
    template<EncodedInstruction enc>
    constexpr auto MemoryWidthIsWord = IsMemoryInstruction<enc> && FieldSetTo<enc, bits::MemoryWidthMask, bits::MemoryWidthWord>;
    template<EncodedInstruction enc>
    constexpr auto MemoryWidthIsHalf = IsMemoryInstruction<enc> && FieldSetTo<enc, bits::MemoryWidthMask, bits::MemoryWidthHalf>;
    template<EncodedInstruction enc>
    constexpr auto MemoryWidthIsByte = IsMemoryInstruction<enc> && FieldSetTo<enc, bits::MemoryWidthMask, bits::MemoryWidthByte>;
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
        // Branch
        //-----------------------------------------------------------------------------
        constexpr auto ImmediateBranch             = 0b000'0'0'000_opcode;
        constexpr auto RegisterBranch              = 0b000'1'0'000_opcode;
        constexpr auto IsConditional               = 0b000'0'1'000_opcode;
        constexpr auto IsLink                      = 0b000'0'0'000_opcode;
        constexpr auto IsConditionalBranchAndLink  = 0b000'0'0'000_opcode;
        constexpr auto IsSelectOperation           = 0b000'0'1'000_opcode;
    } // end namespace bits
    template<EncodedInstruction enc>
    constexpr auto IsImmediateBranch = IsBranchInstruction<enc> && flagClear<bits::RegisterBranch>;
    template<EncodedInstruction enc>
    constexpr auto IsRegisterBranch = IsBranchInstruction<enc> && flagSet<bits::RegisterBranch>;
    template<EncodedInstruction enc>
    constexpr auto IsLink = IsBranchInstruction<enc> && flagClear<bits::IsConditional>(enc);
    template<EncodedInstruction enc>
    constexpr auto IsConditionalImmediate = IsBranchInstruction<enc> && flagSet<bits::IsConditional>(enc);
    template<EncodedInstruction enc>
    constexpr auto IsConditionalBranchAndLink = IsBranchInstruction<enc> && flagClear<bits::IsSelectOperation>(enc);
    template<EncodedInstruction enc>
    constexpr auto IsSelectOperation = IsBranchInstruction<enc> && flagSet<bits::IsSelectOperation>(enc);
    template<EncodedInstruction enc>
    constexpr auto UsesLinkRegister = IsLink<enc> || IsConditionalBranchAndLink<enc>;
    template<EncodedInstruction enc>
    constexpr auto IsConditionalOperation = IsConditionalBranchAndLink<enc> || 
                                            IsConditionalImmediate<enc> ||
                                            IsSelectOperation<enc>;

    template<EncodedInstruction enc>
    constexpr auto IsIntegerOperation = (OperationCaresAboutSign<enc> && FlagSet<enc, bits::KindInteger>);
    template<EncodedInstruction enc>
    constexpr auto IsOrdinalOperation = (OperationCaresAboutSign<enc> && FlagClear<enc, bits::KindInteger>);
    template<EncodedInstruction enc, std::enable_if_t<(OperationCaresAboutSign<enc>), int> = 0>
    using DeterminedNumericType = std::conditional_t<IsIntegerOperation<enc>, Integer, std::conditional_t<IsOrdinalOperation<enc>, Ordinal, void>>;
    // forward declare the formats and perform sanity checks
    constexpr auto LargestOpcode = 0xFF00'0000;
#define X(name, o) \
    static_assert(o <= LargestOpcode, "Operation " #name " is out of encoding space!");
#include "InstructionFormats.def"
#undef X
    constexpr auto extractOpcode(EncodedInstruction inst) noexcept {
        return LargestOpcode & inst;
    }

    /**
     * The fields of an iris instruction are:
     * [24,31] Opcode
     * [16,23] Arg0 
     * [08,15] Arg1 
     * [00,07] Arg2 
     * [00,15] Immediate16
     * [00,07] Immediate8
     *
     * The formats are:
     * 3REG: Opcode, Arg0, Arg1, Arg2
     * 2REG+I8: Opcode, Arg0, Arg1, I8
     * 1REG+I16: Opcode, Arg0, I16
     *
     * All other forms that are provided at a higher level condense down to these
     * three forms.
     */
    class Instruction {
        public:
            constexpr Instruction(EncodedInstruction opcode, Byte arg0, Byte arg1, Byte arg2) noexcept : _opcode(opcode), _arg0(arg0), _arg1(arg1), _arg2(arg2) { }
            constexpr Instruction(EncodedInstruction opcode, Byte arg0, Word imm16) noexcept : Instruction(opcode, arg0, static_cast<Byte>(imm16 >> 8), static_cast<Byte>(imm16)) { }
            explicit constexpr Instruction(EncodedInstruction enc = 0) noexcept : Instruction(extractOpcode(enc), ((0x00FF0000 & enc) >> 16), static_cast<Word>(enc)) { }
            constexpr Instruction(const Instruction& other) noexcept = default;

        public:
            constexpr auto getOpcode() const noexcept { return _opcode; }
            constexpr auto getArg0()  const noexcept { return static_cast<RegisterIndex>(_arg0); }
            constexpr auto getArg1()  const noexcept { return static_cast<RegisterIndex>(_arg1); }
            constexpr auto getArg2()  const noexcept { return static_cast<RegisterIndex>(_arg2); }
            constexpr auto getImm8()  const noexcept { return _arg2; }
            constexpr auto getImm16() const noexcept { return static_cast<Ordinal>((static_cast<Ordinal>(_arg2) | (static_cast<Ordinal>(_arg1) << 8))); } 
            constexpr EncodedInstruction getEncodedInstruction() const noexcept {
                return (_opcode) | (static_cast<EncodedInstruction>(_arg0) << 16) | static_cast<EncodedInstruction>(getImm16());
            }
        public:
            void setOpcode(HalfOrdinal value) noexcept {
                setOpcode(static_cast<EncodedInstruction>(value) << 24);
            }
            void setOpcode(EncodedInstruction inst) noexcept { _opcode = inst; }
            void setImm16(UnsignedWord value) noexcept {
                _arg1 = static_cast<Byte>(value >> 8);
                _arg2 = static_cast<Byte>(value);
            }

            void setArg0(RegisterIndex value) noexcept { _arg0 = std::to_integer<Byte>(value); }
            void setArg1(RegisterIndex value) noexcept { _arg1 = std::to_integer<Byte>(value); }
            void setArg2(RegisterIndex value) noexcept { _arg2 = std::to_integer<Byte>(value); }
            void setImm8(HalfOrdinal value) noexcept { _arg2 = value; }
        private:
            // break this into five different components
            EncodedInstruction _opcode;
            Byte _arg0;
            Byte _arg1;
            Byte _arg2;

    };

} // end namespace iris
#endif // end IRIS_OPCODES_H__
