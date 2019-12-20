/**
 * @file
 * Types for the 32-bit iris risc architecture
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
#include <cstddef>
#include <type_traits>

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
using Ordinal = uint32_t;
using Integer = int32_t;
using UnsignedWord = Ordinal;
using SignedWord = Integer;
using Word = Ordinal;
using HalfOrdinal = uint16_t;
using HalfInteger = int16_t;
using QuarterOrdinal = uint8_t;
using QuarterInteger = int8_t;
using Byte = QuarterOrdinal;
using RegisterIndex = std::byte;
using RegisterIndexNumericType = std::underlying_type_t<RegisterIndex>;
using Address = Ordinal;
using Offset16 = Integer;
using EncodedInstruction = Ordinal; 

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

constexpr QuarterOrdinal getUpperHalf(Ordinal word) noexcept {
    return decodeBits<decltype(word), HalfOrdinal, 0xFF00, 8>(word);
}
constexpr QuarterOrdinal getLowerHalf(Ordinal word) noexcept {
    return decodeBits<decltype(word), HalfOrdinal, 0x00FF, 0>(word);
}

constexpr auto RegisterCount = (0xFF + 1);


} // end namespace iris
constexpr iris::RegisterIndex operator "" _reg(unsigned long long int conversion) noexcept { return iris::RegisterIndex{static_cast<iris::RegisterIndexNumericType>(conversion)}; }
constexpr iris::Integer operator "" _int(unsigned long long int conv) noexcept { return static_cast<iris::Integer>(conv); }
constexpr iris::Address operator "" _addr(unsigned long long int conversion) noexcept { return static_cast<iris::Address>(conversion); }
constexpr iris::Ordinal operator "" _ord(unsigned long long int conversion) noexcept { return static_cast<iris::Ordinal>(conversion); }




#endif // end IRIS_TYPES_H__
