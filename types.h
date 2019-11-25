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
#ifndef IRIS_TYPES_H__
#define IRIS_TYPES_H__
#include <cstdint>
#include <type_traits>
#include <cstddef>

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
using Ordinal = uint16_t;
using Integer = int16_t;
using UnsignedWord = Ordinal;
using SignedWord = Integer;
using LongOrdinal = uint32_t;
using LongInteger = int32_t;
using UnsignedDoubleWord = LongOrdinal;
using SignedDoubleWord = LongInteger;
using Word = Ordinal;
using DoubleWord = LongOrdinal;
using HalfOrdinal = uint8_t;
using HalfInteger = int8_t;
using Byte = HalfOrdinal;
using RegisterIndex = std::byte;
using RegisterIndexNumericType = std::underlying_type_t<RegisterIndex>;
using Address = Ordinal;
using Offset16 = Integer;
using EncodedInstruction = LongOrdinal; 

template<typename T, typename R, T mask, T shift>
constexpr R decodeBits(T value) noexcept {
    auto partial = (value & mask);
    if constexpr (shift != 0) {
        partial = partial >> shift;
    }
    return static_cast<R>(partial);
}
template<typename T, typename R, T mask, T shift>
constexpr T encodeBits(T input, R value) noexcept {
    T component = static_cast<T>(value);
    if constexpr (shift != 0) {
        component <<= shift;
    }
    return static_cast<T>((input & ~mask) | (component & mask));
}
constexpr Ordinal getUpperHalf(LongOrdinal word) noexcept {
    return decodeBits<decltype(word), Ordinal, 0xFFFF'0000, 16>(word);
}
constexpr Ordinal getLowerHalf(LongOrdinal word) noexcept {
    return decodeBits<decltype(word), Ordinal, 0x0000'FFFF, 0>(word);
}

constexpr HalfOrdinal getUpperHalf(Ordinal word) noexcept {
    return decodeBits<decltype(word), HalfOrdinal, 0xFF00, 8>(word);
}
constexpr HalfOrdinal getLowerHalf(Ordinal word) noexcept {
    return decodeBits<decltype(word), HalfOrdinal, 0x00FF, 0>(word);
}
constexpr LongOrdinal setUpperHalf(LongOrdinal word, Ordinal value) noexcept {
    return encodeBits<decltype(word), decltype(value), 0xFFFF'0000, 16>(word, value);
}
constexpr LongOrdinal setLowerHalf(LongOrdinal word, Ordinal value) noexcept {
    return encodeBits<decltype(word), decltype(value), 0x0000'FFFF, 0>(word, value);
}

constexpr LongOrdinal makeLongOrdinal(Ordinal lower, Ordinal upper) noexcept {
    return static_cast<LongOrdinal>(lower) | (static_cast<LongOrdinal>(upper) << 16);
}
constexpr iris::DoubleWord makeDoubleWord(Word lower, Word upper) noexcept {
    return makeLongOrdinal(lower, upper);
}


} // end namespace iris
constexpr iris::RegisterIndex operator "" _reg(unsigned long long int conversion) noexcept { return iris::RegisterIndex{static_cast<iris::RegisterIndexNumericType>(conversion)}; }
iris::RegisterIndex operator "" _reg(const char* str, std::size_t size);
constexpr iris::RegisterIndex operator "" _dreg(unsigned long long int conversion) noexcept { return static_cast<iris::RegisterIndex>(conversion) & static_cast<iris::RegisterIndex>(0b1111110); }
constexpr iris::Integer operator "" _sw(unsigned long long int conv) noexcept { return static_cast<iris::Integer>(conv); }
constexpr iris::Integer operator "" _simm16(unsigned long long int conv) noexcept { return static_cast<iris::Integer>(conv); }
constexpr iris::Integer operator "" _s16(unsigned long long int conv) noexcept { return static_cast<iris::Integer>(conv); }
constexpr iris::Address operator "" _addr(unsigned long long int conversion) noexcept { return static_cast<iris::Address>(conversion); }
constexpr iris::Ordinal operator "" _uw(unsigned long long int conversion) noexcept { return static_cast<iris::Ordinal>(conversion); }
constexpr iris::Ordinal operator "" _imm16(unsigned long long int conversion) noexcept { return static_cast<iris::Ordinal>(conversion); }
constexpr iris::Ordinal operator "" _u16(unsigned long long int conversion) noexcept { return static_cast<iris::Ordinal>(conversion); }
constexpr iris::LongOrdinal operator "" _udw(unsigned long long int conversion) noexcept { return static_cast<iris::LongOrdinal>(conversion); }
constexpr iris::LongInteger operator "" _sdw(unsigned long long int conversion) noexcept { return static_cast<iris::LongInteger>(conversion); }




#endif // end IRIS_TYPES_H__
