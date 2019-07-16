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
    Other,
    Arithmetic2,
    Count,
};
static_assert(static_cast<Byte>(Group::Count) <= 8, "Too many groups defined!");
enum class ArithmeticKind : Byte {
    Nop,
    AddSigned, AddUnsigned,
    SubtractSigned, SubtractUnsigned,
    MultiplySigned, MultiplyUnsigned,
    DivideSigned, DivideUnsigned,
    RemainderSigned, RemainderUnsigned,
    ShiftLeftSigned, ShiftLeftUnsigned,
    ShiftRightSigned, ShiftRightUnsigned,
    BitwiseAnd, BitwiseOr,
    BitwiseNot, BitwiseXor,
    BitwiseNor, BitwiseNand,
    MaxSigned, MaxUnsigned,
    MinSigned, MinUnsigned,
    Increment, Decrement,
    Double, Halve,
    Count,
};
static_assert(static_cast<Byte>(ArithmeticKind::Count) <= 32, "Too many arithmetic operations!");
enum class Arithmetic2Kind : Byte {
    AddSignedImmediate, AddUnsignedImmediate,
    SubtractSignedImmediate, SubtractUnsignedImmediate,
    MultiplySignedImmediate, MultiplyUnsignedImmediate,
    DivideSignedImmediate, DivideUnsignedImmediate,
    RemainderSignedImmediate, RemainderUnsignedImmediate,
    ShiftLeftSignedImmediate, ShiftLeftUnsignedImmediate,
    ShiftRightSignedImmediate, ShiftRightUnsignedImmediate,
    BitwiseAndImmediate, BitwiseOrImmediate,
    BitwiseNotImmediate, BitwiseXorImmediate,
    BitwiseNorImmediate, BitwiseNandImmediate,
    Count,
};
static_assert(static_cast<Byte>(Arithmetic2Kind::Count) <= 32, "Too many arithmetic 2 operations!");

/// @todo introduce compile time sanity checks to make sure that the index does not go out of range!

struct Instruction {
    public:
        explicit constexpr Instruction(DoubleWord bits) noexcept : _bits(bits) { }
        ~Instruction() = default;
        constexpr Byte getOpcodeIndex() const noexcept { return _bits & 0xFF; }
        constexpr Byte getGroupKind() const noexcept { return getOpcodeIndex() & 0x7; }
        constexpr Byte getOperationKind() const noexcept { return (getOpcodeIndex() & 0xF8) >> 3; }
        constexpr Byte getDestinationIndex() const noexcept { return (_bits >> 8) & 0xFF; }
        constexpr Byte getSource0Index() const noexcept { return (_bits >> 16) & 0xFF; }
        constexpr Byte getSource1Index() const noexcept { return (_bits >> 24) & 0xFF; }
        constexpr Word getImm8() const noexcept { return getSource1Index(); }
        constexpr Word getImm16() const noexcept { return Word(_bits >> 16); }
        constexpr DoubleWord getImm24() const noexcept { return _bits >> 8; }
        constexpr auto rawBits() const noexcept { return _bits; }
    private:
        DoubleWord _bits;
};
static_assert(sizeof(Instruction) == sizeof(DoubleWord), "Instruction size mismatch large!");
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
