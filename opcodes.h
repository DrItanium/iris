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
    enum class Opcodes : UnsignedWord {
#define X(g, o, f) g ## o ,
#include "InstructionFormats.def"
#undef X
        Count,
    };
    constexpr auto MaximumOpcodeCount = (0xFF + 1);
    using OpcodesNumericType = std::underlying_type_t<Opcodes>; 
    static_assert(static_cast<OpcodesNumericType>(Opcodes::Count) <= MaximumOpcodeCount, "Too many opcodes defined!");
    // forward declare the formats
#define X(g, o, f) struct g ## o ## Format ; 
#include "InstructionFormats.def"
#undef X

using DecodedInstruction = std::variant<
            std::monostate
#define X(g, o, f) , g ## o ## Format 
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
        explicit constexpr Instruction(DoubleWord bits) noexcept : _bits(bits) { }
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
        static constexpr auto IsImm16 = std::is_same_v<std::decay_t<T>, Word>;
    public:
        template<typename T = RegisterIndex>
        constexpr T getArg0() const noexcept { 
            if constexpr (IsImm16<T>) {
                return getImm16();
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
            } else if constexpr (IsImm8<T>) {
                return convertByteIndex<T>(getImm8());
            } else {
                return convertByteIndex<T>(getHighestQuarter());
            }
        }
        constexpr Byte getImm8() const noexcept { return getArg2<Byte>(); }
        constexpr Word getImm16() const noexcept { return getUpperHalf(); }
        constexpr std::optional<DecodedInstruction> decode() const noexcept;
    private:
        DoubleWord _bits;
};

static_assert(sizeof(Instruction) == sizeof(DoubleWord), "Instruction size mismatch large!");
template<Opcodes op>
class ArgumentFormat {
    public:
        static constexpr OpcodesNumericType RawValue = static_cast<OpcodesNumericType>(op);
        static constexpr auto TargetOpcode = op;
        explicit constexpr ArgumentFormat(const Instruction&) { };
        constexpr auto getOpcode() const noexcept { return op; }
        constexpr auto getRawValue() const noexcept { return RawValue; }
};
template<typename T, Opcodes op>
class ThreeArgumentsFormat : public ArgumentFormat<op> {
    public:
        using Parent = ArgumentFormat<op>;
        explicit constexpr ThreeArgumentsFormat(const Instruction& inst) : Parent(inst), _first(inst.getArg0()), _second(inst.getArg1()), _third(inst.getArg2<T>()) { }
        constexpr auto getFirst() const noexcept { return _first; }
        constexpr auto getSecond() const noexcept { return _second; }
        constexpr auto getThird() const noexcept { return _third; }
        constexpr std::tuple<RegisterIndex, RegisterIndex, T> arguments() const noexcept { return std::make_tuple(_first, _second, _third); }
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
        constexpr auto getFirst() const noexcept { return _first; }
        constexpr auto getSecond() const noexcept { return _second; }
        constexpr std::tuple<RegisterIndex, T> arguments() const noexcept { return std::make_tuple(_first, _second); }
    private:
        RegisterIndex _first;
        T _second;
};
template<typename T, Opcodes op>
class OneArgumentFormat : public ArgumentFormat<op> {
    public:
        using Parent = ArgumentFormat<op>;
        explicit constexpr OneArgumentFormat(const Instruction& inst) : Parent(inst), _first(inst.getArg0<T>()) { }
        constexpr auto getFirst() const noexcept { return _first; }
        constexpr std::tuple<T> arguments() const noexcept { return std::make_tuple(_first); }
    private:
        T _first;
};
template<Opcodes op>
class ZeroArgumentFormat : public ArgumentFormat<op> { 
    public:
        using Parent = ArgumentFormat<op>;
        using Parent::Parent;
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
using OneRegisterU16Format = TwoArgumentsFormat<Word, op>;
template<Opcodes op>
using OneRegisterU8Format = TwoArgumentsFormat<Byte, op>;
template<Opcodes op>
using OneRegisterS8Format = TwoArgumentsFormat<SignedByte, op>;
template<Opcodes op>
using OneRegisterFormat = OneArgumentFormat<RegisterIndex, op>;
template<Opcodes op>
using U16Format = OneArgumentFormat<Word, op>;
template<Opcodes op>
using U8Format = OneArgumentFormat<Byte, op>;
template<Opcodes op>
using S8Format = OneArgumentFormat<SignedByte, op>;

// define the actual instruction kinds
#define X(g, o, f) \
    struct g ## o ## Format final : public f ## Format < Opcodes :: g ## o > { \
        using Parent = f ## Format < Opcodes:: g ## o > ; \
        using Parent::Parent; \
    };
#include "InstructionFormats.def"
#undef X

constexpr std::optional<DecodedInstruction> Instruction::decode() const noexcept {
    // Since the opcode is stashed in the first byte we should switch on the 
    // undecoded byte. The group and kind is still but only at compile time.
    // This greatly cuts down on code complexity. When optimization is active 
    // we even get a huge performance boost too :)
    switch (getOpcodeIndex()) {
#define X(g, o, f) case g ## o ## Format :: RawValue : return g ## o ## Format ( *this ) ;
#include "InstructionFormats.def"
#undef X
        default:
            return std::nullopt;
    }
}

} // end namespace iris
#endif // end IRIS_OPCODES_H__
