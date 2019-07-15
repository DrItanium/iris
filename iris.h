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
struct InstructionLogic {
    virtual ~InstructionLogic() = default;
    virtual void invoke() noexcept = 0;
    inline void operator()() noexcept { invoke(); }
};
using SourceStorage = std::variant<Word, SourceRegister>;
struct ThreeArgumentFormat : InstructionLogic {
    public:
        ThreeArgumentFormat(DestinationRegister& d, SourceRegister& s0, SourceStorage s1) : _dest(d), _src0(s0), _src1(s1) { }
        virtual ~ThreeArgumentFormat() = default;
    protected:
        DestinationRegister _dest;
        SourceRegister _src0;
        SourceStorage _src1;
};
struct TwoArgumentFormat : InstructionLogic {
    public:
        TwoArgumentFormat(DestinationRegister& d, SourceStorage s0) : _dest(d), _src0(s0) { }
        virtual ~TwoArgumentFormat() = default;
    protected:
        DestinationRegister _dest;
        SourceStorage _src0;
};
struct OneArgumentFormat : InstructionLogic {
    public:
        using Storage = std::variant<Word, DestinationRegister, SourceRegister>;
    public: 
        OneArgumentFormat(Storage storage) : _storage(storage) { }
        virtual ~OneArgumentFormat() = default;
    protected:
        Storage _storage;
};

struct Instruction {
    public:
        explicit constexpr Instruction(DoubleWord bits) noexcept : _bits(bits) { }
        ~Instruction() = default;
        constexpr Byte getGroupIndex() const noexcept { return _bits & 0x3; }
        constexpr Byte getOperationIndex() const noexcept { return (_bits >> 3) & 0x5; }
        constexpr Byte getDestinationIndex() const noexcept { return _bits & 0xFF; }
        constexpr Byte getSource0Index() const noexcept { return (_bits >> 8) & 0xFF; }
        constexpr Byte getSource1Index() const noexcept { return (_bits >> 16) & 0xFF; }
        constexpr Word getImm8() const noexcept { return getSource1Index(); }
        constexpr Word getImm16() const noexcept { return Word(_bits >> 16); }
        constexpr DoubleWord getImm24() const noexcept { return _bits >> 8; }
        constexpr auto rawBits() const noexcept { return _bits; }
    private:
        DoubleWord _bits;
};
static_assert(sizeof(Instruction) == sizeof(DoubleWord), "Instruction size mismatch large!");
constexpr auto MemoryBankElementCount = (0xFFFF + 1);

template<typename T>
using MemoryBank = std::array<T, MemoryBankElementCount>;
using CodeMemoryBank = MemoryBank<DoubleWord>;
using DataMemoryBank = MemoryBank<Word>;
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
