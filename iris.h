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

namespace iris {
// false_v taken from https://quuxplusone.github.io/blog/2018/04/02/false-v/
template<typename...>
inline constexpr bool false_v = false;
using Word = uint16_t;
using SignedWord = int16_t;
using DoubleWord = uint32_t;
using DoubleSignedWord = int32_t;
using Byte = uint8_t;
using SignedByte = int8_t;
using RegisterIndex = std::byte;

class Register final {
    public:
        explicit constexpr Register(Word value = 0) noexcept : _value(value) { }
        constexpr Register(const Register& other) noexcept = default;
        constexpr Register(Register&& other) noexcept = default;
        ~Register() = default;
        Register& operator=(const Register& other) noexcept = default;
        Register& operator=(Register&& other) noexcept = default;
        constexpr Register& operator++() noexcept {
            ++_value;
            return *this;
        }
        constexpr Register& operator--() noexcept {
            --_value;
            return *this;
        }
        constexpr bool operator==(const Register& other) const noexcept { return other._value == _value; }
        constexpr bool operator==(SignedWord other) const noexcept      { return _signedValue == other; }
        constexpr bool operator==(Word other) const noexcept            { return _value == other; }
        constexpr bool operator!=(const Register& other) const noexcept { return other._value != _value; }
        constexpr bool operator!=(SignedWord other) const noexcept      { return other != _signedValue; }
        constexpr bool operator!=(Word other) const noexcept            { return other != _value; }
        constexpr bool operator<(const Register& other) const noexcept  { return _value < other._value; }
        constexpr bool operator<(SignedWord other) const noexcept       { return _signedValue < other; }
        constexpr bool operator<(Word other) const noexcept             { return _value < other; }
        constexpr bool operator<=(const Register& other) const noexcept { return _value <= other._value; }
        constexpr bool operator<=(SignedWord other) const noexcept      { return _signedValue <= other; }
        constexpr bool operator<=(Word other) const noexcept            { return _value <= other; }
        constexpr bool operator>(SignedWord other) const noexcept       { return _signedValue > other; }
        constexpr bool operator>(Word other) const noexcept             { return _value > other; }
        constexpr bool operator>(const Register& other) const noexcept  { return _value > other._value; }
        constexpr bool operator>=(SignedWord other) const noexcept      { return _signedValue >= other; }
        constexpr bool operator>=(Word other) const noexcept            { return _value >= other; }
        constexpr bool operator>=(const Register& other) const noexcept { return _value >= other._value; }
        template<typename T>
        T get() const noexcept {
            using K = std::decay_t<T>;
            if constexpr (std::is_same_v<K, Word>) {
                return _value;
            } else if constexpr (std::is_same_v<K, SignedWord>) {
                return _signedValue;
            } else {
                static_assert(false_v<T>, "Illegal type requested!");
            }
        }
        template<typename T>
        void put(T value) noexcept {
            using K = std::decay_t<T>;
            if constexpr (std::is_same_v<K, Word> || std::is_convertible_v<K, Word>) {
                _value = value;
            } else if constexpr (std::is_same_v<K, SignedWord> || std::is_convertible_v<K, SignedWord>) {
                _signedValue = value;
            } else {
                static_assert(false_v<T>, "Cannot assign (or convert) from provided type to Word or SignedWord!");
            }
        }
        explicit constexpr operator Word() const noexcept { return _value; }
        explicit constexpr operator SignedWord() const noexcept { return _signedValue; }
    private:
        union {
            Word _value;
            SignedWord _signedValue;
        };
};
using DestinationRegister = Register&;
using SourceRegister = const Register&;

enum class Group : Byte {
    Arithmetic,
    Memory,
    Branch,
    Compare,
    Arithmetic2,
    Count,
};
static_assert(static_cast<Byte>(Group::Count) <= 8, "Too many groups defined!");

template<typename E>
constexpr bool isLegal(E kind) noexcept {
    static_assert(std::is_enum_v<std::decay_t<E>>, "Kind provided must be an enum");
    using UT = std::underlying_type_t<std::decay_t<E>>;
    return static_cast<UT>(kind) < static_cast<UT>(E::Count);
}
enum class ArithmeticKind : Byte {
    Nop,
    AddSigned, 
    AddUnsigned,
    SubtractSigned, 
    SubtractUnsigned,
    MultiplySigned, 
    MultiplyUnsigned,
    DivideSigned, 
    DivideUnsigned,
    RemainderSigned, 
    RemainderUnsigned,
    ShiftLeftSigned, 
    ShiftLeftUnsigned,
    ShiftRightSigned, 
    ShiftRightUnsigned,
    BitwiseAnd,
    BitwiseOr,
    BitwiseNot,
    BitwiseXor,
    BitwiseNor,
    BitwiseNand,
    MaxSigned,
    MaxUnsigned,
    MinSigned,
    MinUnsigned,
    Increment,
    Decrement,
    Double,
    Halve,
    Count,
};
static_assert(static_cast<Byte>(ArithmeticKind::Count) <= 32, "Too many arithmetic operations!");

enum class Arithmetic2Kind : Byte {
    AddSignedImmediate, 
    AddUnsignedImmediate,
    SubtractSignedImmediate, 
    SubtractUnsignedImmediate,
    MultiplySignedImmediate, 
    MultiplyUnsignedImmediate,
    DivideSignedImmediate, 
    DivideUnsignedImmediate,
    RemainderSignedImmediate, 
    RemainderUnsignedImmediate,
    ShiftLeftSignedImmediate, 
    ShiftLeftUnsignedImmediate,
    ShiftRightSignedImmediate, 
    ShiftRightUnsignedImmediate,
    BitwiseAndImmediate, 
    BitwiseOrImmediate,
    BitwiseNotImmediate, 
    BitwiseXorImmediate,
    BitwiseNorImmediate, 
    BitwiseNandImmediate,
    Count,
};
static_assert(static_cast<Byte>(Arithmetic2Kind::Count) <= 32, "Too many arithmetic 2 operations!");
enum class MemoryKind : Byte {
    Move, Swap,
    Push, Pop, PushImmediate,
    LoadData, StoreData, 
    LoadDataImmediate, StoreDataImmediate,
    LoadCode, StoreCode,
    LoadIO, StoreIO, StoreIOImmediate,
    Count,
};
static_assert(static_cast<Byte>(MemoryKind::Count) <= 32, "Too many memory operations!");
enum class BranchKind : Byte {
    JumpImmediate, 
    JumpConditionalImmediate,
    JumpRelativeImmediate, 
    JumpConditionalRelativeImmediate,
    JumpRegister, 
    JumpConditionalRegister,
    JumpRegisterAndLink, 
    JumpImmediateAndLink,
    JumpConditionalRegisterAndLink,
    BranchSelect,
    Count,
};
static_assert(static_cast<Byte>(BranchKind::Count) <= 32, "Too many branch operations!");
enum class CompareKind : Byte {
    Equals, 
    NotEquals, 
    LessThanSigned, 
    LessThanUnsigned,
    GreaterThanSigned, 
    GreaterThanUnsigned,
    LessThanOrEqualToSigned, 
    LessThanOrEqualToUnsigned,
    GreaterThanOrEqualToSigned, 
    GreaterThanOrEqualToUnsigned,
    Spaceship,
    Count,
};
static_assert(static_cast<Byte>(CompareKind::Count) <= 32, "Too many branch operations!");
template<auto value>
struct BindConstantToType : std::integral_constant<decltype(value), value> {
    public:
        BindConstantToType() = delete;
        ~BindConstantToType() = delete;
        BindConstantToType(const BindConstantToType&) = delete;
        BindConstantToType(BindConstantToType&&) = delete;
        BindConstantToType& operator=(const BindConstantToType&) = delete;
        BindConstantToType& operator=(BindConstantToType&&) = delete;
};
template<Group group>
struct BindGroupToOperationKind : BindConstantToType<group> { };
template<typename T>
struct BindOperationToGroupKind { 
        BindOperationToGroupKind() = delete;
        ~BindOperationToGroupKind() = delete;
        BindOperationToGroupKind(const BindOperationToGroupKind&) = delete;
        BindOperationToGroupKind(BindOperationToGroupKind&&) = delete;
        BindOperationToGroupKind& operator=(const BindOperationToGroupKind&) = delete;
        BindOperationToGroupKind& operator=(BindOperationToGroupKind&&) = delete;
};

#define GroupToOperationKindBinding(g,t) \
    template<> \
    struct BindGroupToOperationKind<Group:: g> : BindConstantToType<Group:: g> { \
        using BoundType = t ; \
    }; \
    template<> \
    struct BindOperationToGroupKind<t> : BindConstantToType<Group:: g> { }
#define X(g) GroupToOperationKindBinding(g, g ## Kind)
X(Arithmetic);
X(Memory);
X(Branch);
X(Compare);
X(Arithmetic2);
#undef X
#undef GroupToOperationKindBinding 
    

template<Group group>
using GroupToOperationKind = typename BindGroupToOperationKind<group>::BoundType;
template<typename T>
constexpr auto OperationKindToGroup = BindOperationToGroupKind<T>::value;
template<auto value>
constexpr auto OperationValueToGroup = OperationKindToGroup<decltype(value)>;

template<typename T>
constexpr auto operationValueToGroup(T) noexcept {
    return OperationKindToGroup<T>;
}

static_assert(std::is_same_v<GroupToOperationKind<Group::Arithmetic>, ArithmeticKind>, "Group to operation kind sanity check failed");
static_assert(OperationKindToGroup<ArithmeticKind> == Group::Arithmetic, "Reverse type binding check failed!");
static_assert(OperationValueToGroup<ArithmeticKind::AddSigned> == Group::Arithmetic, "Reverse value binding check failed!");
static_assert(OperationKindToGroup<GroupToOperationKind<Group::Arithmetic>> == Group::Arithmetic, "Forward then reverse binding check failed!");

using OperationKinds = std::variant<
    GroupToOperationKind<Group::Arithmetic>,
    GroupToOperationKind<Group::Memory>,
    GroupToOperationKind<Group::Branch>,
    GroupToOperationKind<Group::Compare>,
    GroupToOperationKind<Group::Arithmetic2>>;
using Operation = std::optional<OperationKinds>;


template<Group g, GroupToOperationKind<g> op>
constexpr Byte EncodedOpcode = (static_cast<Byte>(g) & 0x7) | ((static_cast<Byte>(op) & 0x1F) << 3);

template<Group g>
using GroupToOpcodePair = std::tuple<decltype(g), GroupToOperationKind<g>>;

constexpr Byte decodeGroupIndex(Byte raw) noexcept {
    return raw & 0x7;
}
constexpr Group decodeGroup(Byte raw) noexcept {
    return static_cast<Group>(decodeGroupIndex(raw)); 
}

constexpr Byte decodeOperationIndex(Byte raw ) noexcept {
    return (raw & 0xF8) >> 3;
}
constexpr Operation decodeOperation(Byte raw) noexcept {
    switch (decodeGroup(raw)) {
#define X(k) case Group:: k : return static_cast<GroupToOperationKind<Group:: k>>(decodeOperationIndex(raw))
                X(Arithmetic);
                X(Memory);
                X(Branch);
                X(Compare);
                X(Arithmetic2);
#undef X
        default:
            return std::nullopt;
    }
}


template<Group g, GroupToOperationKind<g> op>
constexpr GroupToOpcodePair<g> MakeDecodedOpcode = std::make_tuple(g, op);

template<Byte raw>
constexpr auto DecodedOpcode = MakeDecodedOpcode<decodeGroup(raw), decodeOperation(raw)>;


/// @todo introduce compile time sanity checks to make sure that the index does not go out of range!
/**
 * The fields of an iris instruction are:
 * [0,7] Opcode
 * [8,15] Destination
 * [16,23] Source0
 * [24,31] Source1
 * [16,31] Immediate16
 * [24,31] Immediate8
 *
 * The formats are:
 * Opcode, Destination, Source0, Source1 (3reg)
 * Opcode, Destination, Source0, Immediate8 (2reg+i8)
 * Opcode, Destination, Immediate16 (1reg + i16)
 * Opcode, Destination, Source0 (2reg)
 * Opcode, Destination (1reg)
 * Opcode, Immediate8 (i8)
 * Opcode, Immediate16 (i16)
 * Opcode (0arg)
 *
 * All of the fields are always in the same place as well. Thus, requesting a destination
 * as an imm16 will actually pull from the Immediate16 field.
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
                union {
                    Byte value;
                    SignedByte sgned;
                } assignable;
                assignable.value = result;
                return assignable.sgned;
            } else {
                static_assert(false_v<T>, "Illegal type requested!");
            }
        }
    public:
        using OptionalDecodedOperation = std::optional<Operation>;
    public:
        explicit constexpr Instruction(DoubleWord bits) noexcept : _bits(bits) { }
        ~Instruction() = default;
        constexpr Byte getLowestQuarter() const noexcept { return _bits & 0xFF; }
        constexpr Byte getLowerQuarter() const noexcept { return (_bits & 0xFF00) >> 8; }
        constexpr Byte getHigherQuarter() const noexcept { return (_bits & 0xFF0000) >> 16; }
        constexpr Byte getHighestQuarter() const noexcept { return (_bits & 0xFF000000) >> 24; }
        constexpr Word getUpperHalf() const noexcept { return (_bits >> 16); }
        constexpr Word getLowerHalf() const noexcept { return _bits; }
        constexpr Byte getOpcodeIndex() const noexcept { return getLowestQuarter(); }
        constexpr Byte getGroupIndex() const noexcept { return iris::decodeGroupIndex(getOpcodeIndex()); }
        constexpr Byte getOperationIndex() const noexcept { return iris::decodeOperationIndex(getOpcodeIndex()); }
        constexpr Group decodeGroup() const noexcept { return iris::decodeGroup(getOpcodeIndex()); }
        constexpr Operation decodeOperation() const noexcept { return iris::decodeOperation(getOpcodeIndex()); }
    private:
        template<typename T>
        constexpr T innerGetIndex(Byte onDefault) const noexcept {
            // some extra logic must go into here for the formats to make sense
            // we always extract imm16 from the same location in the instruction
            if constexpr (std::is_same_v<T, Word>) {
                return getImm16();
            } else if constexpr (std::is_same_v<T, SignedWord>) {
                union {
                    Word u;
                    SignedWord s;
                } i16;
                i16.u = getImm16();
                return i16.s;
            } else if constexpr (std::is_same_v<T, Byte> || std::is_same_v<T, SignedByte>) {
                return convertByteIndex<T>(getImm8());
            } else {
                return convertByteIndex<T>(onDefault);
            }
        }
    public:
        template<typename T = RegisterIndex>
        constexpr T getDestinationIndex() const noexcept { 
            return innerGetIndex<T>(getLowerQuarter()); 
        }
        template<typename T = RegisterIndex>
        constexpr T getSource0Index() const noexcept {
            return innerGetIndex<T>(getHigherQuarter());
        }
        template<typename T = RegisterIndex>
        constexpr T getSource1Index() const noexcept { 
            return convertByteIndex<T>(getHighestQuarter()); 
        }
        constexpr Byte getImm8() const noexcept { return getSource1Index<Byte>(); }
        constexpr Word getImm16() const noexcept { return getUpperHalf(); }
        constexpr auto rawBits() const noexcept { return _bits; }
    private:
        DoubleWord _bits;
};

static_assert(sizeof(Instruction) == sizeof(DoubleWord), "Instruction size mismatch large!");
template<typename T>
class ThreeArgumentsFormat {
    public:
        ThreeArgumentsFormat(RegisterIndex a, RegisterIndex b, T c) : _first(a), _second(b), _third(c) { }
        ThreeArgumentsFormat(const Instruction& inst) : ThreeArgumentsFormat(inst.getDestinationIndex(), inst.getSource0Index(), inst.getSource1Index<T>()) { }
        ~ThreeArgumentsFormat() = default;
        constexpr auto getFirst() const noexcept { return _first; }
        constexpr auto getSecond() const noexcept { return _second; }
        constexpr auto getThird() const noexcept { return _third; }
    private:
        RegisterIndex _first;
        RegisterIndex _second;
        T _third;
};
template<typename T>
class TwoArgumentsFormat {
    public:
        TwoArgumentsFormat(RegisterIndex a, T b) : _first(a), _second(b) { }
        TwoArgumentsFormat(const Instruction& inst) : TwoArgumentsFormat(inst.getDestinationIndex(), inst.getSource0Index<T>()) { }
        ~TwoArgumentsFormat() = default;
        constexpr auto getFirst() const noexcept { return _first; }
        constexpr auto getSecond() const noexcept { return _second; }
    private:
        RegisterIndex _first;
        T _second;
};
template<typename T>
class OneArgumentFormat {
    public:
        explicit OneArgumentFormat(T a) : _first(a) { }
        explicit OneArgumentFormat(const Instruction& inst) : OneArgumentFormat(inst.getDestinationIndex<T>()) { }
        ~OneArgumentFormat() = default;
        constexpr auto getFirst() const noexcept { return _first; }
    private:
        T _first;
};
class ZeroArgumentFormat { };
using ThreeRegisterFormat = ThreeArgumentsFormat<RegisterIndex>;
using TwoRegisterU8Format = ThreeArgumentsFormat<Byte>;
using TwoRegisterS8Format = ThreeArgumentsFormat<SignedByte>;
using TwoRegisterFormat = TwoArgumentsFormat<RegisterIndex>;
using OneRegisterU16Format = TwoArgumentsFormat<Word>;
using OneRegisterS16Format = TwoArgumentsFormat<SignedWord>;
using OneRegisterFormat = OneArgumentFormat<RegisterIndex>;
using U16Format = OneArgumentFormat<Word>;
using S16Format = OneArgumentFormat<SignedWord>;
using U8Format = OneArgumentFormat<Byte>;
using S8Format = OneArgumentFormat<SignedByte>;
constexpr auto MemoryBankElementCount = (0xFFFF + 1);
constexpr auto RegisterCount = (0xFF + 1);



template<typename T, size_t capacity>
using NumericalStorageBank = std::array<T, capacity>;

using RegisterBank = NumericalStorageBank<Register, RegisterCount>;
/**
 * Generic template for defining a memory bank of 2^16 elements.
 * @tparam T the type of each memory bank cell
 */
template<typename T>
using MemoryBank = NumericalStorageBank<std::enable_if_t<std::is_integral_v<T>, T>, MemoryBankElementCount>;
/**
 * Separate memory space that holds the instructions the iris core executes;
 * DoubleWords are stored in this location.
 */
using CodeMemoryBank = MemoryBank<DoubleWord>;

/**
 * Location to store data values in an iris core; 2^16 words worth of storage
 * is provided by default.
 */
using DataMemoryBank = MemoryBank<Word>;
/**
 * Iris comes equipped with a full 2^16 words worth of stack space that is
 * accessed by separate instructions. It is only possible to do pushes and pops
 * to stack memory. However, the number of stack pointers is only limited by
 * the number of registers.
 */
using StackMemoryBank = MemoryBank<Word>;
/**
 * the MMIO space that is exposed to the program, one registers functions at
 * addresses into the space. Writing to an address which is not registers
 * results in nothing happening. Reading from an address will return all ones.
 */
class IOMemoryBank {
    public:
        IOMemoryBank() = default;
        ~IOMemoryBank() = default;
};

} // end namespace iris


#endif // end IRIS_H__
