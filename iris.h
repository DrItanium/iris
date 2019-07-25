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
#include <functional>
#include <map>
#define CAT(a, b) PRIMITIVE_CAT(a, b)
#define PRIMITIVE_CAT(a, b) a ## b
#define NO_INSTANTIATE(kind) \
    kind () = delete; \
    ~ kind () = delete; \
    kind ( const kind & ) = delete; \
    kind ( kind && ) = delete; \
    kind& operator=(const kind &) = delete; \
    kind& operator=( kind &&) = delete;
namespace iris {
// false_v taken from https://quuxplusone.github.io/blog/2018/04/02/false-v/
template<typename...>
inline constexpr bool false_v = false;

/**
 *
 * Allow a separate lambda to be defined for each specific std::visit case.
 * Found on the internet in many places
 * @tparam Ts the functions that make up the dispatch logic
 */
template<typename ... Ts> 
struct overloaded : Ts... 
{
    using Ts::operator()...;
};

template<typename ... Ts>
overloaded(Ts...) -> overloaded<Ts...>;
using UnsignedWord = uint16_t;
using SignedWord = int16_t;
using UnsignedDoubleWord = uint32_t;
using SignedDoubleWord = int32_t;
using Word = UnsignedWord;
using DoubleWord = UnsignedDoubleWord;
using UnsignedByte = uint8_t;
using SignedByte = int8_t;
using Byte = UnsignedByte;
using RegisterIndex = std::byte;
using Address = UnsignedWord;
template<auto value>
struct BindConstantToType : std::integral_constant<decltype(value), value> {
        NO_INSTANTIATE(BindConstantToType);
};

// enumeration defines
#define BeginGroups enum class Group : Byte {
#define EndGroups Count, }; \
    static_assert(static_cast<Byte>(Group::Count) <= 8, "Too many groups defined!");
#define Group(v) v , 
#define BeginKind(kind) enum class kind ## Kind : Byte { 
#define EndKind(kind) Count, }; \
    static_assert(static_cast<Byte>( kind ## Kind :: Count) <= 32, "Too many " #kind " operations defined!");
#define GenerateArithmetic(o) o,
#define GenerateArithmetic2(o) o,
#define GenerateBranch(o) o,
#define GenerateMemory(o) o,
#define GenerateCompare(o) o,
#define GenerateDoubleRegister(o) o,
#define X(g, o, f) CAT(Generate, g)(o)
#include "InstructionFormats.def"
#undef GenerateArithmetic
#undef GenerateArithmetic2
#undef GenerateMemory
#undef GenerateBranch
#undef GenerateCompare
#undef GenerateDoubleRegister
#undef X
#undef BeginKind
#undef EndKind
#undef BeginGroups
#undef EndGroups
#undef Group

template<Group group>
struct BindOperationKind : BindConstantToType<group> { };
template<typename T>
struct BindOperationToGroupKind { 
    NO_INSTANTIATE(BindOperationToGroupKind);
};

#define BeginKind(_)
#define EndKind(_)
#define BeginGroups
#define EndGroups
#define OperationKindBinding(g,t) \
    template<> \
    struct BindOperationKind<Group:: g> : BindConstantToType<Group:: g> { \
        using BoundType = t ; \
    }; \
    template<> \
    struct BindOperationToGroupKind<t> : BindConstantToType<Group:: g> { };
#define Group(g) OperationKindBinding(g, g ## Kind)
#define X(g, o, f)
#include "InstructionFormats.def"
#undef X
#undef Group
#undef OperationKindBinding 
#undef BeginKind
#undef EndKind
#undef BeginGroups
#undef EndGroups
    

template<Group group>
using OperationKind = typename BindOperationKind<group>::BoundType;
template<typename T>
constexpr auto OperationKindToGroup = BindOperationToGroupKind<T>::value;
template<auto value>
constexpr auto OperationValueToGroup = OperationKindToGroup<decltype(value)>;

static_assert(std::is_same_v<OperationKind<Group::Arithmetic>, ArithmeticKind>, "Group to operation kind sanity check failed");
static_assert(OperationKindToGroup<ArithmeticKind> == Group::Arithmetic, "Reverse type binding check failed!");
static_assert(OperationValueToGroup<ArithmeticKind::AddSigned> == Group::Arithmetic, "Reverse value binding check failed!");
static_assert(OperationKindToGroup<OperationKind<Group::Arithmetic>> == Group::Arithmetic, "Forward then reverse binding check failed!");


template<Group g, OperationKind<g> op>
constexpr Byte EncodedOpcode = (static_cast<Byte>(g) & 0x7) | ((static_cast<Byte>(op) & 0x1F) << 3);

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
    NO_INSTANTIATE(OperationToFormat); 
    using Type = std::monostate;
};
template<Byte value>
using OperationToFormat_t = typename OperationToFormat<value>::Type;

template<Byte value>
constexpr auto BoundToFormat = !std::is_same_v<OperationToFormat_t<value>, std::monostate>;

// define the actual instruction kinds
#define BeginGroups
#define EndGroups
#define Group(_)
#define BeginKind(_)
#define EndKind(_)
#define X(g, o, f) \
    struct g ## o ## Format final : public f ## Format < Group:: g , OperationKind<Group:: g>:: o > { \
        using Parent = f ## Format < Group:: g , OperationKind<Group:: g>:: o >; \
        using Parent::Parent; \
    }; \
    template<> \
struct OperationToFormat < g ## o ## Format :: EncodedOpcode > final { \
    NO_INSTANTIATE(OperationToFormat); \
    using Type = g ## o ## Format ; \
}; \
    static_assert(std::is_same_v< \
            OperationToFormat_t< \
            iris::EncodedOpcode<Group:: g, \
            OperationKind<Group:: g > :: o >>, \
            g ## o ## Format >, "Define mismatch error!");
#include "InstructionFormats.def"
#undef X
#undef BeginKind
#undef EndKind
#undef BeginGroups
#undef EndGroups
#undef Group

using DecodedInstruction = std::variant<
            std::monostate
#define BeginGroups
#define EndGroups
#define Group(_)
#define BeginKind(_)
#define EndKind(_)
#define X(g, o, f) , g ## o ## Format 
#include "InstructionFormats.def"
#undef X
#undef BeginKind
#undef EndKind
#undef BeginGroups
#undef EndGroups
#undef Group
            >;

constexpr std::optional<DecodedInstruction> decodeInstruction(const Instruction& inst) noexcept {
    // Since the opcode is stashed in the first byte we should switch on the 
    // undecoded byte. The group and kind is still but only at compile time.
    // This greatly cuts down on code complexity. When optimization is active 
    // we even get a huge performance boost too :)
    switch (inst.getOpcodeIndex()) {
#define BeginGroups
#define EndGroups
#define Group(_)
#define BeginKind(_)
#define EndKind(_)
#define X(g, o, f) \
        case iris::EncodedOpcode<Group:: g, OperationKind<Group:: g >:: o>: \
             return OperationToFormat_t<iris::EncodedOpcode<Group:: g, OperationKind<Group:: g > :: o>>(inst);
#include "InstructionFormats.def"
#undef X
#undef BeginKind
#undef EndKind
#undef BeginGroups
#undef EndGroups
#undef Group
        default:
            return std::nullopt;
    }
}

constexpr auto MemoryBankElementCount = (0xFFFF + 1);
constexpr auto RegisterCount = (0xFF + 1);

class Register final {
    public:
        explicit constexpr Register(Word value = 0) noexcept : _storage(value) { }
        explicit constexpr Register(bool value) noexcept : _storage(value ? 0xFFFF : 0) { }
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
        constexpr bool operator==(bool other) const noexcept            { return get<bool>() == other; }
        constexpr bool operator!=(const Register& other) const noexcept { return other.get<Word>() != get<Word>(); }
        constexpr bool operator!=(SignedWord other) const noexcept      { return other != get<SignedWord>(); }
        constexpr bool operator!=(Word other) const noexcept            { return other != get<Word>(); }
        constexpr bool operator!=(bool other) const noexcept            { return other != get<bool>(); }
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
        template<typename T = Word>
        constexpr T get() const noexcept {
            using K = std::decay_t<T>;
            if constexpr (std::is_same_v<K, Word>) {
                return _storage._value;
            } else if constexpr (std::is_same_v<K, SignedWord>) {
                return _storage._signedValue;
            } else if constexpr (std::is_same_v<K, bool>) {
                return _storage._value != 0;
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
            } else if constexpr (std::is_same_v<K, bool> || std::is_convertible_v<K, bool>) {
                _storage._value = value ? 0xFFFF : 0;
            } else {
                static_assert(false_v<T>, "Cannot assign (or convert) from provided type to Word or SignedWord!");
            }
        }
        explicit constexpr operator Word() const noexcept { return get<Word>(); }
        explicit constexpr operator SignedWord() const noexcept { return get<SignedWord>(); }
        constexpr operator bool() const noexcept { return get<bool>(); }
    private:
        union BackingStore {
            constexpr BackingStore(Word v) : _value(v) { }
            Word _value;
            SignedWord _signedValue;
        } _storage;
};
using DestinationRegister = Register&;
using SourceRegister = const Register&;

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
class Core;
using StackMemoryBank = MemoryBank<Word>;
using MMIOWriteFunction = std::function<void(Core&, Word)>;
using MMIOReadFunction = std::function<Word(Core&)>;
struct MMIOEntry {
    public:
        MMIOEntry() = default;
        virtual ~MMIOEntry() = default;
        virtual void write(Core&, Word);
        virtual Word read(Core&);

};
struct LambdaMMIOEntry : MMIOEntry {
    public:
        LambdaMMIOEntry(MMIOReadFunction read = [](Core&) -> Word { return 0; }, MMIOWriteFunction write = [](Core&, Word) { });
        virtual ~LambdaMMIOEntry() = default;
        void write(Core&, Word value) override;
        Word read(Core&) override;
    private:
        MMIOReadFunction _read;
        MMIOWriteFunction _write;

};
struct CaptiveMMIOEntry : MMIOEntry {
    public:
        CaptiveMMIOEntry(MMIOEntry& other);
        virtual ~CaptiveMMIOEntry() = default;
        void write(Core&, Word) override;
        Word read(Core&) override;
    private:
        MMIOEntry& _other;
};
/**
 * Description of the io memory map to be installed into IO memory
 */
using IOMemoryMap = std::map<Address, 
                             std::variant<MMIOEntry, 
                                          std::tuple<MMIOReadFunction, MMIOWriteFunction>,
                                          MMIOReadFunction,
                                          MMIOWriteFunction>>;
/**
 * the MMIO space that is exposed to the program, one registers functions at
 * addresses into the space. Writing to an address which is not registers
 * results in nothing happening. Reading from an address will return all ones.
 */
class IOMemoryBank {
    public:
        using MMIOTable = NumericalStorageBank<MMIOEntry, MemoryBankElementCount>;
    public:
        IOMemoryBank(Core& c) : _core(c) { }
        ~IOMemoryBank() = default;
        Word load(Address);
        void store(Address, Word);
        void mapIntoMemory(Address, std::tuple<MMIOReadFunction, MMIOWriteFunction>);
        void mapIntoMemory(Address, MMIOReadFunction);
        void mapIntoMemory(Address, MMIOWriteFunction);
        void mapIntoMemory(Address, MMIOReadFunction, MMIOWriteFunction);
        void mapIntoMemory(Address, MMIOEntry&);
        void installMemoryMap(const IOMemoryMap&);
    private:
        Core& _core;
        IOMemoryBank::MMIOTable _storage;

};

class DoubleRegister final {
    public:
        static DoubleRegister makePair(RegisterBank& reg, RegisterIndex a, RegisterIndex b) noexcept;
        static DoubleRegister makePair(RegisterBank& reg, RegisterIndex a) noexcept;
        static const DoubleRegister makePair(const RegisterBank& reg, RegisterIndex a, RegisterIndex b) noexcept;
        static const DoubleRegister makePair(const RegisterBank& reg, RegisterIndex a) noexcept;
    public:
        constexpr DoubleRegister(Register& lower, Register& upper) : _lower(lower), _upper(upper) { }
        constexpr DoubleRegister(const Register& lower, const Register& upper) : _lower(const_cast<Register&>(lower)), _upper(const_cast<Register&>(upper)) { }
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
class Core {
    public:
        static void terminateCore(Core&, Word);
        static Word readTerminateCell(Core&);
        static void illegalWriteError(Core&, Word);
        static Word illegalReadError(Core&);
    public:
        Core();
        ~Core() = default;
        void run();
        void installIOMemoryMap(const IOMemoryMap& map);
        void terminateCycle();
    private:
        // use tag dispatch to call the right routines
#define BeginGroups
#define EndGroups
#define Group(_)
#define BeginKind(_)
#define EndKind(_)
#define X(group, oper, fmt) \
        void invoke(const group ## oper ## Format &);
#include "InstructionFormats.def"
#undef X
#undef BeginKind
#undef EndKind
#undef BeginGroups
#undef EndGroups
#undef Group
    private:
        void cycle();
    private:
        DestinationRegister getDestinationRegister(RegisterIndex idx) noexcept;
        SourceRegister getSourceRegister(RegisterIndex idx) const noexcept;
        DoubleRegister getDoubleRegister(RegisterIndex start, RegisterIndex next) noexcept;
        const DoubleRegister getDoubleRegister(RegisterIndex start, RegisterIndex next) const noexcept;
        void invoke(DoubleWord bits);
    private:
        inline const DoubleRegister getDoubleRegister(RegisterIndex start) const noexcept {
            return getDoubleRegister(start, static_cast<RegisterIndex>(static_cast<Byte>(start) + 1));
        }
        inline DoubleRegister getDoubleRegister(RegisterIndex start) noexcept {
            return getDoubleRegister(start, static_cast<RegisterIndex>( static_cast<Byte>(start) + 1));
        }
        inline void incrementRegister(RegisterIndex idx) noexcept {
            ++getDestinationRegister(idx);
        }
        inline void decrementRegister(RegisterIndex idx) noexcept {
            --getDestinationRegister(idx);
        }
        template<typename T>
        inline void setDoubleRegisterValue(RegisterIndex lower, RegisterIndex upper, T value) noexcept {
            getDoubleRegister(lower, upper).put<T>(value);
        }
        template<typename T>
        inline void setDoubleRegisterValue(RegisterIndex lower, T value) noexcept {
            getDoubleRegister(lower).put<T>(value);
        }
        template<typename T = DoubleWord>
        inline T getDoubleRegisterValue(RegisterIndex lower, RegisterIndex upper) const noexcept {
            return getDoubleRegister(lower, upper).get<T>();
        }
        template<typename T = DoubleWord>
        inline T getDoubleRegisterValue(RegisterIndex lower) const noexcept {
            return getDoubleRegister(lower).get<T>();
        }
        template<typename T>
        inline void setRegisterValue(RegisterIndex idx, T value) noexcept {
            getDestinationRegister(idx).put(value);
        }
        template<typename T = Word>
        inline T getRegisterValue(RegisterIndex idx) const noexcept {
            return getSourceRegister(idx).get<T>();
        }
        constexpr auto getTerminateCell() const noexcept { return _terminateCell; }
    private:
        RegisterBank _regs;
        CodeMemoryBank _code;
        DataMemoryBank _data;
        StackMemoryBank _stack;
        IOMemoryBank _io;
        Register _ip;
        bool _executing = false;
        bool _advanceIP = true;
        Word _terminateCell = 0;
};

} // end namespace iris
#undef NO_INSTANTIATE


#endif // end IRIS_H__
