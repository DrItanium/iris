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
class ArgumentFormat {
    public:
        explicit constexpr ArgumentFormat(const Instruction& inst) : _group(inst.decodeGroup()), _op(inst.decodeOperation()) { }
        constexpr auto getGroup() const noexcept { return _group; }
        constexpr auto getOperation() const noexcept { return _op; }
    private:
        Group _group;
        Operation _op;

};
template<typename T>
class ThreeArgumentsFormat : public ArgumentFormat {
    public:
        using Parent = ArgumentFormat;
        static constexpr auto ArgumentCount = 3;
        explicit constexpr ThreeArgumentsFormat(const Instruction& inst) : Parent(inst), _first(inst.getDestinationIndex()), _second(inst.getSource0Index()), _third(inst.getSource1Index<T>()) { }
        constexpr auto getFirst() const noexcept { return _first; }
        constexpr auto getSecond() const noexcept { return _second; }
        constexpr auto getThird() const noexcept { return _third; }
    private:
        RegisterIndex _first;
        RegisterIndex _second;
        T _third;
};
template<typename T>
class TwoArgumentsFormat : public ArgumentFormat {
    public:
        using Parent = ArgumentFormat;
        static constexpr auto ArgumentCount = 2;
        explicit constexpr TwoArgumentsFormat(const Instruction& inst) : Parent(inst), _first(inst.getDestinationIndex()), _second(inst.getSource0Index<T>()) { }
        constexpr auto getFirst() const noexcept { return _first; }
        constexpr auto getSecond() const noexcept { return _second; }
    private:
        RegisterIndex _first;
        T _second;
};
template<typename T>
class OneArgumentFormat : public ArgumentFormat {
    public:
        using Parent = ArgumentFormat;
        static constexpr auto ArgumentCount = 1;
        explicit constexpr OneArgumentFormat(const Instruction& inst) : Parent(inst), _first(inst.getDestinationIndex<T>()) { }
        constexpr auto getFirst() const noexcept { return _first; }
    private:
        T _first;
};
class ZeroArgumentFormat final : public ArgumentFormat { 
    public:
        using Parent = ArgumentFormat;
        using Parent::Parent;
        static constexpr auto ArgumentCount = 0;
};
using ThreeRegisterFormat = ThreeArgumentsFormat<RegisterIndex>;
using TwoRegisterU8Format = ThreeArgumentsFormat<Byte>;
using TwoRegisterS8Format = ThreeArgumentsFormat<SignedByte>;
using TwoRegisterFormat = TwoArgumentsFormat<RegisterIndex>;
using OneRegisterU16Format = TwoArgumentsFormat<Word>;
using OneRegisterS16Format = TwoArgumentsFormat<SignedWord>;
using OneRegisterU8Format = TwoArgumentsFormat<Byte>;
using OneRegisterS8Format = TwoArgumentsFormat<SignedByte>;
using OneRegisterFormat = OneArgumentFormat<RegisterIndex>;
using U16Format = OneArgumentFormat<Word>;
using S16Format = OneArgumentFormat<SignedWord>;
using U8Format = OneArgumentFormat<Byte>;
using S8Format = OneArgumentFormat<SignedByte>;

using DecodedInstruction = std::variant<
            ThreeRegisterFormat,
            TwoRegisterU8Format,
            TwoRegisterS8Format,
            TwoRegisterFormat,
            OneRegisterU16Format,
            OneRegisterS16Format,
            OneRegisterU8Format,
            OneRegisterS8Format,
            OneRegisterFormat,
            U16Format,
            S16Format,
            U8Format,
            S8Format,
            ZeroArgumentFormat>;

template<typename T>
constexpr auto ArgumentCount = T::ArgumentCount;

template<typename T>
constexpr auto getArgumentCount(T) noexcept {
    return ArgumentCount<T>;
}

static_assert(ArgumentCount<S8Format> == 1, "ArgumentCount sanity check failed!");
template<auto value>
struct OperationToArgumentFormat : public BindConstantToType<value> { 
    static_assert(std::is_enum_v<decltype(value)>, "Incoming value must be an enum type!");
};
#define X(g, o, f) \
    template<> \
    struct OperationToArgumentFormat<GroupToOperationKind<Group:: g>:: o> : \
    public BindConstantToType<GroupToOperationKind<Group:: g>:: o> { \
            using ArgumentFormat = f ## Format ; \
            static constexpr ArgumentFormat make(const Instruction& inst) noexcept { \
                return ArgumentFormat(inst); \
            } \
    };
#include "InstructionFormats.def"
#undef X

template<auto value>
using InstructionArgumentFormat = typename OperationToArgumentFormat<value>::ArgumentFormat;

#define X(g, o, f) \
    static_assert(std::is_same_v<InstructionArgumentFormat<GroupToOperationKind<Group:: g>::o>, f ## Format>, "Sanity check failed on format mismatch");
#include "InstructionFormats.def"
#undef X



#define CAT(a, b) PRIMITIVE_CAT(a, b)
#define PRIMITIVE_CAT(a, b) a ## b
constexpr std::optional<DecodedInstruction> decodeInstruction(const Instruction& inst) noexcept {
    if (auto op = inst.decodeOperation(); op) {
    return std::visit([&inst](auto&& value) -> std::optional<DecodedInstruction> {
                using K = std::decay_t<decltype(value)>;
#define MakeCase(op) case K :: op : return InstructionArgumentFormat<K :: op >(inst)
#define Y(g, op, f) PRIMITIVE_CAT(Action, g)(op, f)
#define X(g, op, f) Y(g, op, f)
                if constexpr (std::is_same_v<K, ArithmeticKind>) {
                switch (value) {
#define ActionArithmetic2(op, f)
#define ActionBranch(op, f)
#define ActionCompare(op, f)
#define ActionMemory(op, f)
#define ActionArithmetic(op, f) MakeCase(op);
#include "InstructionFormats.def"
#undef ActionArithmetic2
#undef ActionArithmetic 
#undef ActionBranch 
#undef ActionCompare 
#undef ActionMemory 
                    default:
                        return std::nullopt;
                }
                } else if constexpr (std::is_same_v<K, Arithmetic2Kind>) {
                switch (value) {
#define ActionArithmetic(op, f)
#define ActionBranch(op, f)
#define ActionCompare(op, f)
#define ActionMemory(op, f)
#define ActionArithmetic2(op, f) MakeCase(op);
#include "InstructionFormats.def"
#undef ActionArithmetic2
#undef ActionArithmetic 
#undef ActionBranch 
#undef ActionCompare 
#undef ActionMemory
                    default:
                        return std::nullopt;
                }
                } else if constexpr (std::is_same_v<K, BranchKind>) {
                switch (value) {
#define ActionArithmetic(op, f)
#define ActionBranch(op, f) MakeCase(op);
#define ActionCompare(op, f)
#define ActionMemory(op, f)
#define ActionArithmetic2(op, f) 
#include "InstructionFormats.def"
#undef ActionArithmetic2
#undef ActionArithmetic 
#undef ActionBranch 
#undef ActionCompare 
#undef ActionMemory
                    default:
                        return std::nullopt;
                }
                } else if constexpr (std::is_same_v<K, CompareKind>) {
                switch (value) {
#define ActionArithmetic(op, f)
#define ActionBranch(op, f) 
#define ActionCompare(op, f) MakeCase(op);
#define ActionMemory(op, f)
#define ActionArithmetic2(op, f) 
#include "InstructionFormats.def"
#undef ActionArithmetic2
#undef ActionArithmetic 
#undef ActionBranch 
#undef ActionCompare 
#undef ActionMemory
                    default:
                        return std::nullopt;
                }
                } else if constexpr (std::is_same_v<K, MemoryKind>) {
                switch (value) {
#define ActionArithmetic(op, f)
#define ActionBranch(op, f) 
#define ActionCompare(op, f) 
#define ActionMemory(op, f) MakeCase(op);
#define ActionArithmetic2(op, f) 
#include "InstructionFormats.def"
#undef ActionArithmetic2
#undef ActionArithmetic 
#undef ActionBranch 
#undef ActionCompare 
#undef ActionMemory
#undef X
#undef Y
#undef MakeCase
                    default:
                        return std::nullopt;
                }
                } else {
                    static_assert(false_v<K>, "Unimplemented type!");
                }
            }, *op);
    } else {
        return std::nullopt;
    }
}



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
