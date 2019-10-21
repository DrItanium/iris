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
#ifndef IRIS_OPCODES_H__
#define IRIS_OPCODES_H__
#include <variant>
#include <optional>
#include <tuple>
#include "types.h"

namespace iris {
    constexpr Byte MajorOpcode_Common = 0b0000'0000;
    constexpr Byte Common_Error = MajorOpcode_Common | 0b00'0000;
    constexpr Byte Common_Arithmetic = MajorOpcode_Common | 0b01'0000;
    constexpr Byte TreatArithmeticOperationAsInteger = Common_Arithmetic | 0b0000;
    constexpr Byte TreatArithmeticOperationAsOrdinal = Common_Arithmetic | 0b1000;
    constexpr Byte ArithmeticOperation_Add           = Common_Arithmetic | 0b000;
    constexpr Byte ArithmeticOperation_Subtract      = Common_Arithmetic | 0b001;
    constexpr Byte ArithmeticOperation_Multiply      = Common_Arithmetic | 0b010;
    constexpr Byte ArithmeticOperation_Divide        = Common_Arithmetic | 0b011;
    constexpr Byte ArithmeticOperation_Remainder     = Common_Arithmetic | 0b100;
    constexpr Byte ArithmeticOperation_ShiftLeft     = Common_Arithmetic | 0b101;
    constexpr Byte ArithmeticOperation_ShiftRight    = Common_Arithmetic | 0b110;
    constexpr Byte Common_Bitwise = MajorOpcode_Common | 0b10'0000;
    constexpr Byte BitwiseOperation_Not = Common_Bitwise | 0b00;
    constexpr Byte BitwiseOperation_And = Common_Bitwise | 0b01;
    constexpr Byte BitwiseOperation_Or  = Common_Bitwise | 0b10;
    constexpr Byte BitwiseOperation_Xor = Common_Bitwise | 0b11;
    constexpr Byte Common_Compare = MajorOpcode_Common | 0b10'0100;
    constexpr Byte CompareOperation_Integer = Common_Compare | 0b0;
    constexpr Byte CompareOperation_Ordinal = Common_Compare | 0b1;
    constexpr Byte MajorOpcode_Memory = 0b0100'0000;
    constexpr Byte MemoryManipulateKind = MajorOpcode_Memory | 0b00'0000;
    constexpr Byte MemoryLoadKind = MemoryManipulateKind | 0b00'1000;
    constexpr Byte MemoryStoreKind = MemoryManipulateKind | 0b00'0000;
    constexpr Byte MemoryManipulateCode = MemoryManipulateKind | 0b00;
    constexpr Byte MemoryManipulateData = MemoryManipulateKind | 0b01;
    constexpr Byte MemoryManipulateStack = MemoryManipulateKind | 0b10;
    constexpr Byte MemoryManipulateIO = MemoryManipulateKind | 0b11;
#define X(space) \
    constexpr Byte MemoryManipulate_Load ## space = MemoryLoadKind | MemoryManipulate ## space ; \
    constexpr Byte MemoryManipulate_Store ## space = MemoryStoreKind | MemoryManipulate ## space
    X(Code);
    X(Data);
    X(Stack);
    X(IO);
#undef X
    constexpr Byte MemoryMiscKind = MajorOpcode_Memory | 0b00'1000;
    constexpr Byte AssignRegisterImmediate = MemoryMiscKind | 0b000;
    constexpr Byte MajorOpcode_Branch = 0b1000'0000;
    constexpr Byte ConditionalCode_NeverTake = MajorOpcode_Branch | 0b000;
    constexpr Byte ConditionalCode_GreaterThan = MajorOpcode_Branch | 0b001;
    constexpr Byte ConditionalCode_Equals = MajorOpcode_Branch | 0b010;
    constexpr Byte ConditionalCode_LessThan = MajorOpcode_Branch | 0b100;
    constexpr Byte ConditionalCode_LessThanOrEqual = ConditionalCode_LessThan | ConditionalCode_Equals;
    constexpr Byte ConditionalCode_GreaterThanOrEqual = ConditionalCode_GreaterThan | ConditionalCode_Equals;
    constexpr Byte ConditionalCode_Unconditional = ConditionalCode_GreaterThan | ConditionalCode_LessThan | ConditionalCode_Equals;
    constexpr Byte ConditionalCode_NotEquals = ConditionalCode_LessThan | ConditionalCode_GreaterThan;
    constexpr Byte BranchAction_TreatTargetAsRelative = MajorOpcode_Branch | 0b0'00'000;
    constexpr Byte BranchAction_TreatTargetAsAbsolute = MajorOpcode_Branch | 0b1'00'000;
    constexpr Byte BranchAction_RegisterForm = MajorOpcode_Branch | 0b0'0'000;
    constexpr Byte BranchAction_ImmediateForm = MajorOpcode_Branch | 0b1'0'000;
#define Y(sign, form, cc) \
        constexpr Byte Branch_ ## sign ## _ ## form ## _ ## cc = MajorOpcode_Branch | \
        ConditionalCode_ ## cc | \
        BranchAction_TreatTargetAs ## sign | \
        BranchAction_ ## form ## Form 
#define X(cc) \
    Y(Relative, Immediate, cc); \
    Y(Relative, Register, cc); \
    Y(Absolute, Immediate, cc); \
    Y(Absolute, Register, cc); 
    X(NeverTake);
    X(GreaterThan);
    X(Equals);
    X(LessThanOrEqual);
    X(GreaterThanOrEqual);
    X(Unconditional);
    X(NotEquals);
#undef X
#undef Y
    constexpr Byte MajorOpcode_Unused = 0b1100'0000;



    enum class ArithmeticOpcodes : Byte {
        IntegerError = 0b0000'0000,
        IntegerAdd,
        IntegerSubtract,
        IntegerMultiply,
        IntegerDivide,
        IntegerRemainder,
        IntegerShiftLeft,
        IntegerShiftRight,
        OrdinalError = 0b0000'1000,
        OrdinalAdd,
        OrdinalSubtract,
        OrdinalMultiply,
        OrdinalDivide,
        OrdinalRemainder,
        OrdinalShiftLeft,
        OrdinalShiftRight,
    };
    enum class Opcodes : UnsignedWord {
        Error = 0b0000'0000,
        IntegerAdd,
        IntegerSubtract,
        IntegerMultiply,
        IntegerDivide,
        IntegerRemainder,
        IntegerShiftLeft,
        IntegerShiftRight,
        OrdinalError = 0b0000'1000,
        OrdinalAdd,
        OrdinalSubtract,
        OrdinalMultiply,
        OrdinalDivide,
        OrdinalRemainder,
        OrdinalShiftLeft,
        OrdinalShiftRight,
#define X(t, _) t ,
#include "InstructionFormats.def"
#undef X
        Count,
    };
    constexpr auto MaximumOpcodeCount = (0xFF + 1);
    using OpcodesNumericType = std::underlying_type_t<Opcodes>; 
    static_assert(static_cast<OpcodesNumericType>(Opcodes::Count) <= MaximumOpcodeCount, "Too many opcodes defined!");
    // forward declare the formats
    struct ErrorInstruction;
#define X(t, f) struct t ## Instruction ; 
#include "InstructionFormats.def"
#undef X

using DecodedInstruction = std::variant<
            std::monostate, 
            ErrorInstruction
#define X(t, f) , t ## Instruction
#include "InstructionFormats.def"
#undef X
            >;

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
struct Instruction {
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
        constexpr std::optional<DecodedInstruction> decode() const noexcept;
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

static_assert(sizeof(Instruction) == sizeof(DoubleWord), "Instruction size mismatch large!");
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
#define X(t, f) \
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

constexpr std::optional<DecodedInstruction> Instruction::decode() const noexcept {
    // Since the opcode is stashed in the first byte we should switch on the 
    // undecoded byte. The group and kind is still but only at compile time.
    // This greatly cuts down on code complexity. When optimization is active 
    // we even get a huge performance boost too :)
    switch (getOpcodeIndex()) {
#define X(t, f) case t ## Instruction :: RawValue : return t ## Instruction ( *this ) ;
#include "InstructionFormats.def"
#undef X
        default:
            return std::nullopt;
    }
}

} // end namespace iris
#endif // end IRIS_OPCODES_H__
