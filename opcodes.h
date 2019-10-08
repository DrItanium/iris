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
#define X(t, _) t ,
#include "InstructionFormats.def"
#undef X
        Count,
    };
    constexpr auto MaximumOpcodeCount = (0xFF + 1);
    using OpcodesNumericType = std::underlying_type_t<Opcodes>; 
    static_assert(static_cast<OpcodesNumericType>(Opcodes::Count) <= MaximumOpcodeCount, "Too many opcodes defined!");
    // forward declare the formats
#define X(t, f) struct t ## Instruction ; 
#include "InstructionFormats.def"
#undef X

using DecodedInstruction = std::variant<
            std::monostate
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
        Instruction(Opcodes opcode) noexcept;
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
        constexpr Byte getImm8() const noexcept { return getArg2<Byte>(); }
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
        operator Instruction() const noexcept { return { this->getOpcode(), _first, _second, _third }; }
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
        operator Instruction() const noexcept { return { this->getOpcode(), _first, _second }; }
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
        operator Instruction() const noexcept { return { this->getOpcode(), _first }; }
    private:
        T _first;
};
template<Opcodes op>
class ZeroArgumentFormat : public ArgumentFormat<op> { 
    public:
        using Parent = ArgumentFormat<op>;
        using Parent::Parent;
        operator Instruction() const noexcept { return { this->getOpcode() }; }
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
constexpr auto TreatArg2AsImmediate = 
std::is_base_of_v<TwoRegisterU8Format<T::Opcode>, T> || 
std::is_base_of_v<TwoRegisterS8Format<T::Opcode>, T>;



// define the actual instruction kinds
#define X(t, f) \
    struct t ## Instruction final : public f ## Format < Opcodes :: t > { \
        static constexpr auto Opcode = Opcodes:: t ; \
        using Parent = f ## Format < Opcodes:: t > ; \
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
#define X(t, f) case t ## Instruction :: RawValue : return t ## Instruction ( *this ) ;
#include "InstructionFormats.def"
#undef X
        default:
            return std::nullopt;
    }
}

} // end namespace iris
#endif // end IRIS_OPCODES_H__
