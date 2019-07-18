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
#define CAT(a, b) PRIMITIVE_CAT(a, b)
#define PRIMITIVE_CAT(a, b) a ## b

namespace iris {
// false_v taken from https://quuxplusone.github.io/blog/2018/04/02/false-v/
template<typename...>
inline constexpr bool false_v = false;
using Word = uint16_t;
using SignedWord = int16_t;
using DoubleWord = uint32_t;
using SignedDoubleWord = int32_t;
using Byte = uint8_t;
using SignedByte = int8_t;
using RegisterIndex = std::byte;

class Register final {
    public:
        explicit constexpr Register(Word value = 0) noexcept : _storage(value) { }
        constexpr Register(const Register& other) noexcept = default;
        constexpr Register(Register&& other) noexcept = default;
        ~Register() = default;
        Register& operator=(const Register& other) noexcept = default;
        Register& operator=(Register&& other) noexcept = default;
        constexpr Register& operator++() noexcept {
            ++_storage._value;
            return *this;
        }
        constexpr Register& operator--() noexcept {
            --_storage._value;
            return *this;
        }
        constexpr bool operator==(const Register& other) const noexcept { return other.get<Word>() == get<Word>(); }
        constexpr bool operator==(SignedWord other) const noexcept      { return get<SignedWord>() == other; }
        constexpr bool operator==(Word other) const noexcept            { return get<Word>() == other; }
        constexpr bool operator!=(const Register& other) const noexcept { return other.get<Word>() != get<Word>(); }
        constexpr bool operator!=(SignedWord other) const noexcept      { return other != get<SignedWord>(); }
        constexpr bool operator!=(Word other) const noexcept            { return other != get<Word>(); }
        constexpr bool operator<(const Register& other) const noexcept  { return get<Word>() < other.get<Word>(); }
        constexpr bool operator<(SignedWord other) const noexcept       { return get<SignedWord>() < other; }
        constexpr bool operator<(Word other) const noexcept             { return get<Word>() < other; }
        constexpr bool operator<=(const Register& other) const noexcept { return get<Word>() <= other.get<Word>(); }
        constexpr bool operator<=(SignedWord other) const noexcept      { return get<SignedWord>() <= other; }
        constexpr bool operator<=(Word other) const noexcept            { return get<Word>() <= other; }
        constexpr bool operator>(SignedWord other) const noexcept       { return get<SignedWord>() > other; }
        constexpr bool operator>(Word other) const noexcept             { return get<Word>() > other; }
        constexpr bool operator>(const Register& other) const noexcept  { return get<Word>() > other.get<Word>(); }
        constexpr bool operator>=(SignedWord other) const noexcept      { return get<SignedWord>() >= other; }
        constexpr bool operator>=(Word other) const noexcept            { return get<Word>() >= other; }
        constexpr bool operator>=(const Register& other) const noexcept { return get<Word>() >= other.get<Word>(); }
        template<typename T>
        constexpr T get() const noexcept {
            using K = std::decay_t<T>;
            if constexpr (std::is_same_v<K, Word>) {
                return _storage._value;
            } else if constexpr (std::is_same_v<K, SignedWord>) {
                return _storage._signedValue;
            } else {
                static_assert(false_v<T>, "Illegal type requested!");
            }
        }
        template<typename T>
        constexpr void put(T value) noexcept {
            using K = std::decay_t<T>;
            if constexpr (std::is_same_v<K, Word> || std::is_convertible_v<K, Word>) {
                _storage._value = value;
            } else if constexpr (std::is_same_v<K, SignedWord> || std::is_convertible_v<K, SignedWord>) {
                _storage._signedValue = value;
            } else {
                static_assert(false_v<T>, "Cannot assign (or convert) from provided type to Word or SignedWord!");
            }
        }
        explicit constexpr operator Word() const noexcept { return get<Word>(); }
        explicit constexpr operator SignedWord() const noexcept { return get<SignedWord>(); }
    private:
        union BackingStore {
            constexpr BackingStore(Word v) : _value(v) { }
            Word _value;
            SignedWord _signedValue;
        } _storage;
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
    Error, // zero arguments
    // most of these operations are r8, r8, r8
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
    BitwiseNot, // r8, r8
    BitwiseXor,
    BitwiseNor,
    BitwiseNand,
    MaxSigned,
    MaxUnsigned,
    MinSigned,
    MinUnsigned,
    Increment, // r8, r8
    Decrement, // r8, r8
    Double, // r8, r8
    Halve, // r8, r8
    Count,
};
static_assert(static_cast<Byte>(ArithmeticKind::Count) <= 32, "Too many arithmetic operations!");

enum class Arithmetic2Kind : Byte {
    // r8, r8, i8 (signed or unsigned depends on the instruction)
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
    // shifting greater than or equal to 16 bits will result in zero
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
    /**
     * Copy the contents of a register to another register aka move
     */
    CopyRegister,
    /**
     * Swap the contents of two registers
     */
    SwapRegisters,
    /**
     * Assign an immediate value to a register aka load immediate
     */
    AssignRegisterImmediate,
    AssignRegisterSignedImmediate,
    // stack operations
    StackPush,
    StackPop,
    StackPushImmediateValue,
    // So when dealing with data operations we have some interesting stuff to think about
    // - There is always an offset that can either be a register or immediate
    // - Because it can be register or immediate offset, there will need to be 
    //   multiple forms of each instruction
    // - Code modifications do not take offsets into account, however an increment
    //   can happen automatically as part of the load store operation as well
    // - Offsets can either be signed or unsigned
    DataLoadWithSignedOffset, 
    DataStoreWithSignedOffset, 
    DataLoadWithUnsignedOffset,
    DataStoreWithUnsignedOffset,
    DataStoreImmediateValue, // store an imm16 into the target address
    CodeLoad,
    CodeStore,
    CodeLoadAndIncrement,
    CodeStoreAndIncrement,
    CodeLoadAndDecrement,
    CodeStoreAndDecrement,
    IOLoadWithSignedOffset,
    IOStoreWithSignedOffset,
    IOLoadWithUnsignedOffset,
    IOStoreWithUnsignedOffset,
    IOStoreImmediateValue, // store an imm16 into the target address in IO space
    Count,
};
static_assert(static_cast<Byte>(MemoryKind::Count) <= 32, "Too many memory operations!");
enum class BranchKind : Byte {
    Immediate,  // imm16
    ConditionalImmediate, // r8, u16
    RelativeImmediate, // s8
    ConditionalRelativeImmediate, // r8, s8
    Register,  // r8
    ConditionalRegister, // r8, r8 
    RegisterAndLink,  // r8, r8
    ImmediateAndLink, // r8 u16
    ConditionalRegisterAndLink, // r8, r8, r8
    Select,   // r8, r8, r8
    Count,
};
static_assert(static_cast<Byte>(BranchKind::Count) <= 32, "Too many branch operations!");
enum class CompareKind : Byte {
    // r8 r8 r8
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
    SpaceshipSigned,
    SpaceshipUnsigned,
    // r8 r8 i8
    EqualsImmediate8,
    NotEqualsImmediate8,
    LessThanSignedImmediate8,
    LessThanUnsignedImmediate8,
    GreaterThanSignedImmediate8, 
    GreaterThanUnsignedImmediate8,
    LessThanOrEqualToSignedImmediate8, 
    LessThanOrEqualToUnsignedImmediate8,
    GreaterThanOrEqualToSignedImmediate8, 
    GreaterThanOrEqualToUnsignedImmediate8,
    SpaceshipSignedImmediate8,
    SpaceshipUnsignedImmediate8,
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
struct BindOperationKind : BindConstantToType<group> { };
template<typename T>
struct BindOperationToGroupKind { 
        BindOperationToGroupKind() = delete;
        ~BindOperationToGroupKind() = delete;
        BindOperationToGroupKind(const BindOperationToGroupKind&) = delete;
        BindOperationToGroupKind(BindOperationToGroupKind&&) = delete;
        BindOperationToGroupKind& operator=(const BindOperationToGroupKind&) = delete;
        BindOperationToGroupKind& operator=(BindOperationToGroupKind&&) = delete;
};

#define OperationKindBinding(g,t) \
    template<> \
    struct BindOperationKind<Group:: g> : BindConstantToType<Group:: g> { \
        using BoundType = t ; \
    }; \
    template<> \
    struct BindOperationToGroupKind<t> : BindConstantToType<Group:: g> { }
#define X(g) OperationKindBinding(g, g ## Kind)
X(Arithmetic);
X(Memory);
X(Branch);
X(Compare);
X(Arithmetic2);
#undef X
#undef OperationKindBinding 
    

template<Group group>
using OperationKind = typename BindOperationKind<group>::BoundType;
template<typename T>
constexpr auto OperationKindToGroup = BindOperationToGroupKind<T>::value;
template<auto value>
constexpr auto OperationValueToGroup = OperationKindToGroup<decltype(value)>;

template<typename T>
constexpr auto operationValueToGroup(T) noexcept {
    return OperationKindToGroup<T>;
}

static_assert(std::is_same_v<OperationKind<Group::Arithmetic>, ArithmeticKind>, "Group to operation kind sanity check failed");
static_assert(OperationKindToGroup<ArithmeticKind> == Group::Arithmetic, "Reverse type binding check failed!");
static_assert(OperationValueToGroup<ArithmeticKind::AddSigned> == Group::Arithmetic, "Reverse value binding check failed!");
static_assert(OperationKindToGroup<OperationKind<Group::Arithmetic>> == Group::Arithmetic, "Forward then reverse binding check failed!");

using OperationKinds = std::variant<
    OperationKind<Group::Arithmetic>,
    OperationKind<Group::Memory>,
    OperationKind<Group::Branch>,
    OperationKind<Group::Compare>,
    OperationKind<Group::Arithmetic2>>;

template<Group g, OperationKind<g> op>
constexpr Byte EncodedOpcode = (static_cast<Byte>(g) & 0x7) | ((static_cast<Byte>(op) & 0x1F) << 3);

template<Group g>
using GroupToOpcodePair = std::tuple<decltype(g), OperationKind<g>>;

constexpr Byte decodeGroupIndex(Byte raw) noexcept {
    return raw & 0x7;
}
constexpr Group decodeGroup(Byte raw) noexcept {
    return static_cast<Group>(decodeGroupIndex(raw)); 
}

constexpr Byte decodeOperationIndex(Byte raw ) noexcept {
    return (raw & 0xF8) >> 3;
}

/// @todo introduce compile time sanity checks to make sure that the index does not go out of range!
/**
 * The fields of an iris instruction are:
 * [0,7] Opcode
 * [0,2] Group
 * [3,7] Operation
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
    private:
        template<typename T>
        constexpr T innerGetIndex(Byte onDefault) const noexcept {
            // some extra logic must go into here for the formats to make sense
            // we always extract imm16 from the same location in the instruction
            if constexpr (std::is_same_v<T, Word>) {
                return getImm16();
            } else if constexpr (std::is_same_v<T, SignedWord>) {
                union temporary {
                    temporary(Word input) : u(input) { }
                    Word u;
                    SignedWord s;
                };
                return temporary(getImm16()).s;
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
template<Group group, OperationKind<group> op>
class ArgumentFormat {
    public:
        static constexpr auto EncodedOpcode = iris::EncodedOpcode<group, op>;
        explicit constexpr ArgumentFormat(const Instruction&) { };
        constexpr auto getGroup() const noexcept { return group; }
        constexpr auto getOperation() const noexcept { return op; }

};
template<typename T, Group group, OperationKind<group> op>
class ThreeArgumentsFormat : public ArgumentFormat<group, op> {
    public:
        using Parent = ArgumentFormat<group, op>;
        static constexpr auto ArgumentCount = 3;
        explicit constexpr ThreeArgumentsFormat(const Instruction& inst) : Parent(inst), _first(inst.getDestinationIndex()), _second(inst.getSource0Index()), _third(inst.getSource1Index<T>()) { }
        constexpr auto getFirst() const noexcept { return _first; }
        constexpr auto getSecond() const noexcept { return _second; }
        constexpr auto getThird() const noexcept { return _third; }
        constexpr std::tuple<RegisterIndex, RegisterIndex, T> arguments() const noexcept { return std::make_tuple(_first, _second, _third); }
    private:
        RegisterIndex _first;
        RegisterIndex _second;
        T _third;
};
template<typename T, Group group, OperationKind<group> op>
class TwoArgumentsFormat : public ArgumentFormat<group, op> {
    public:
        using Parent = ArgumentFormat<group, op>;
        static constexpr auto ArgumentCount = 2;
        explicit constexpr TwoArgumentsFormat(const Instruction& inst) : Parent(inst), _first(inst.getDestinationIndex()), _second(inst.getSource0Index<T>()) { }
        constexpr auto getFirst() const noexcept { return _first; }
        constexpr auto getSecond() const noexcept { return _second; }
        constexpr std::tuple<RegisterIndex, T> arguments() const noexcept { return std::make_tuple(_first, _second); }
    private:
        RegisterIndex _first;
        T _second;
};
template<typename T, Group group, OperationKind<group> op>
class OneArgumentFormat : public ArgumentFormat<group, op> {
    public:
        using Parent = ArgumentFormat<group, op>;
        static constexpr auto ArgumentCount = 1;
        explicit constexpr OneArgumentFormat(const Instruction& inst) : Parent(inst), _first(inst.getDestinationIndex<T>()) { }
        constexpr auto getFirst() const noexcept { return _first; }
        constexpr std::tuple<T> arguments() const noexcept { return std::make_tuple(_first); }
    private:
        T _first;
};
template<Group group, OperationKind<group> op>
class ZeroArgumentFormat : public ArgumentFormat<group, op> { 
    public:
        using Parent = ArgumentFormat<group, op>;
        using Parent::Parent;
        static constexpr auto ArgumentCount = 0;
};
template<Group group, OperationKind<group> op>
using ThreeRegisterFormat = ThreeArgumentsFormat<RegisterIndex, group, op>;
template<Group group, OperationKind<group> op>
using TwoRegisterU8Format = ThreeArgumentsFormat<Byte, group, op>;
template<Group group, OperationKind<group> op>
using TwoRegisterS8Format = ThreeArgumentsFormat<SignedByte, group, op>;
template<Group group, OperationKind<group> op>
using TwoRegisterFormat = TwoArgumentsFormat<RegisterIndex, group, op>;
template<Group group, OperationKind<group> op>
using OneRegisterU16Format = TwoArgumentsFormat<Word, group, op>;
template<Group group, OperationKind<group> op>
using OneRegisterS16Format = TwoArgumentsFormat<SignedWord, group, op>;
template<Group group, OperationKind<group> op>
using OneRegisterU8Format = TwoArgumentsFormat<Byte, group, op>;
template<Group group, OperationKind<group> op>
using OneRegisterS8Format = TwoArgumentsFormat<SignedByte, group, op>;
template<Group group, OperationKind<group> op>
using OneRegisterFormat = OneArgumentFormat<RegisterIndex, group, op>;
template<Group group, OperationKind<group> op>
using U16Format = OneArgumentFormat<Word, group, op>;
template<Group group, OperationKind<group> op>
using S16Format = OneArgumentFormat<SignedWord, group, op>;
template<Group group, OperationKind<group> op>
using U8Format = OneArgumentFormat<Byte, group, op>;
template<Group group, OperationKind<group> op>
using S8Format = OneArgumentFormat<SignedByte, group, op>;

template<Byte value>
struct OperationToFormat final {
    OperationToFormat() = delete;
    ~OperationToFormat() = delete;
    OperationToFormat(const OperationToFormat&) = delete;
    OperationToFormat(OperationToFormat&&) = delete;
    OperationToFormat& operator=(const OperationToFormat&) = delete;
    OperationToFormat& operator=(OperationToFormat&&) = delete;
    using Type = std::monostate;
};
template<Byte value>
using OperationToFormat_t = typename OperationToFormat<value>::Type;

template<Byte value>
constexpr auto BoundToFormat = !std::is_same_v<OperationToFormat_t<value>, std::monostate>;

// define the actual instruction kinds
#define X(g, o, f) \
    struct g ## o ## Format final : public f ## Format < Group:: g , OperationKind<Group:: g>:: o > { \
        using Parent = f ## Format < Group:: g , OperationKind<Group:: g>:: o >; \
        using Parent::Parent; \
    }; \
    template<> \
struct OperationToFormat < g ## o ## Format :: EncodedOpcode > final { \
    OperationToFormat() = delete; \
    ~OperationToFormat() = delete; \
    OperationToFormat(const OperationToFormat&) = delete; \
    OperationToFormat(OperationToFormat&&) = delete; \
    OperationToFormat& operator=(const OperationToFormat&) = delete; \
    OperationToFormat& operator=(OperationToFormat&&) = delete; \
    using Type = g ## o ## Format ; \
};
#include "InstructionFormats.def"
#undef X


using DecodedInstruction = std::variant<
            std::monostate
#define X(g, o, f) , g ## o ## Format 
#include "InstructionFormats.def"
#undef X
            >;

template<typename T>
constexpr auto ArgumentCount = T::ArgumentCount;

template<typename T>
constexpr auto getArgumentCount(T) noexcept {
    return ArgumentCount<T>;
}

template<auto value>
struct OperationToArgumentFormat : public BindConstantToType<value> { 
    static_assert(std::is_enum_v<decltype(value)>, "Incoming value must be an enum type!");
};
#define X(g, o, f) \
    template<> \
    struct OperationToArgumentFormat<OperationKind<Group:: g>:: o> : \
    public BindConstantToType<OperationKind<Group:: g>:: o> { \
            using ArgumentFormat = g ## o ## Format; \
            static constexpr ArgumentFormat make(const Instruction& inst) noexcept { \
                return ArgumentFormat(inst); \
            } \
    };
#include "InstructionFormats.def"
#undef X

template<auto value>
using InstructionArgumentFormat = typename OperationToArgumentFormat<value>::ArgumentFormat;

constexpr std::optional<DecodedInstruction> decodeInstruction(const Instruction& inst) noexcept {
    switch (inst.getOpcodeIndex()) {
#define X(g, o, f) \
        case iris::EncodedOpcode<Group:: g, OperationKind<Group:: g >:: o>: \
             return OperationToFormat_t<iris::EncodedOpcode<Group:: g, OperationKind<Group:: g > :: o>>(inst);
#include "InstructionFormats.def"
#undef X
        default:
            return std::nullopt;
    }
}

#define X(g, o, f) \
    static_assert(std::is_same_v< \
            OperationToFormat_t< \
            iris::EncodedOpcode<Group:: g, \
            OperationKind<Group:: g > :: o >>, \
            g ## o ## Format >, "Define mismatch error!");
#include "InstructionFormats.def"
#undef X




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

class DoubleRegister final {
    public:
        constexpr DoubleRegister(Register& lower, Register& upper) : _lower(lower), _upper(upper) { }
        constexpr Word upperHalf() const noexcept { return _upper.get<Word>(); }
        constexpr Word lowerHalf() const noexcept { return _lower.get<Word>(); }
        template<typename T = DoubleWord>
        constexpr T get() const noexcept {
            if constexpr (std::is_same_v<T, DoubleWord>) {
                DoubleWord l = lowerHalf();
                DoubleWord u = upperHalf();
                return (u << 16) | l;
            } else if constexpr (std::is_same_v<T, SignedDoubleWord>) {
                union temporary {
                    constexpr temporary(DoubleWord v) : _v(v) { }
                    DoubleWord _v;
                    SignedDoubleWord _s;
                };
                return temporary(get<DoubleWord>())._s;
            } else {
                static_assert(false_v<T>, "Illegal type requested");
            }
        }
        void put(Word lower, Word upper) noexcept;
        template<typename T = DoubleWord>
        void put(T value) noexcept {
            if constexpr (std::is_same_v<T, DoubleWord>) {
                put(Word(value), Word(value >> 16));
            } else if constexpr (std::is_same_v<T, SignedDoubleWord>) {
                union temporary {
                    constexpr temporary(SignedDoubleWord v) : _v(v) { }
                    SignedDoubleWord _v;
                    DoubleWord _u;
                };
                put<DoubleWord>(temporary(value)._u);
            } else {
                static_assert(false_v<T>, "Illegal type requested!");
            }
        }
    private:
        Register& _lower;
        Register& _upper;
};
DoubleRegister makePair(RegisterBank& reg, RegisterIndex a, RegisterIndex b) noexcept;
DoubleRegister makePair(RegisterBank& reg, RegisterIndex a) noexcept;
class Core {
    private:
        // use tag dispatch to call the right routines
#define X(group, oper, fmt) \
        void invoke(const group ## oper ## Format &);
#include "InstructionFormats.def"
#undef X
    private:
        DestinationRegister unpackDestination(RegisterIndex idx) noexcept;
        SourceRegister unpackSourceRegister(RegisterIndex idx) const noexcept;
        inline void incrementRegister(RegisterIndex idx) noexcept {
            ++unpackDestination(idx);
        }
        inline void decrementRegister(RegisterIndex idx) noexcept {
            --unpackDestination(idx);
        }
        template<typename T>
        void setRegister(RegisterIndex idx, T value) noexcept {
            unpackDestination(idx).put(value);
        }
        template<typename T = Word>
        T getRegisterValue(RegisterIndex idx) const noexcept {
            return unpackSourceRegister(idx).get<T>();
        }
        void invoke(DoubleWord bits);
        RegisterBank _regs;
        CodeMemoryBank _code;
        DataMemoryBank _data;
        StackMemoryBank _stack;
        IOMemoryBank _io;

};

} // end namespace iris


#endif // end IRIS_H__
