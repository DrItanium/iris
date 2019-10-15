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
#include <array>
#include <type_traits>
#include <cstddef>
#include <sstream>

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
using Offset16 = SignedWord;
using QuadOrdinal = uint64_t;

template<typename T>
constexpr auto isDoubleWide = false;
template<typename T>
constexpr auto isQuadWide = false;
template<typename T>
constexpr auto isRelativeOffset = false;

template<> constexpr auto isDoubleWide<UnsignedDoubleWord> = true;
template<> constexpr auto isDoubleWide<SignedDoubleWord> = true;
template<> constexpr auto isQuadWide<QuadOrdinal> = true;

template<> constexpr auto isRelativeOffset<SignedWord> = true;


template<typename T, typename R, T mask, T shift>
constexpr R decodeBits(T value) noexcept {
    if constexpr (shift != 0) {
        return static_cast<R>((value & mask) >> shift);
    } else {
        return static_cast<R>((value & mask));
    }
}
template<typename T, typename R, T mask, T shift>
constexpr T encodeBits(T input, R value) noexcept {
    if constexpr (shift != 0)  {
        return static_cast<T>((input & ~mask) | ((static_cast<T>(value) << shift) & mask));
    } else {
        return static_cast<T>((input & ~mask) | ((static_cast<T>(value)) & mask));
    }
}
constexpr UnsignedWord getUpperHalf(UnsignedDoubleWord word) noexcept {
    return decodeBits<decltype(word), UnsignedWord, 0xFFFF'0000, 16>(word);
}
constexpr UnsignedWord getLowerHalf(UnsignedDoubleWord word) noexcept {
    return decodeBits<decltype(word), UnsignedWord, 0x0000'FFFF, 0>(word);
}

constexpr UnsignedByte getUpperHalf(UnsignedWord word) noexcept {
    return decodeBits<decltype(word), UnsignedByte, 0xFF00, 8>(word);
}
constexpr UnsignedByte getLowerHalf(UnsignedWord word) noexcept {
    return decodeBits<decltype(word), UnsignedByte, 0x00FF, 0>(word);
}
constexpr UnsignedDoubleWord setUpperHalf(UnsignedDoubleWord word, UnsignedWord value) noexcept {
    return encodeBits<decltype(word), decltype(value), 0xFFFF'0000, 16>(word, value);
}
constexpr UnsignedDoubleWord setLowerHalf(UnsignedDoubleWord word, UnsignedWord value) noexcept {
    return encodeBits<decltype(word), decltype(value), 0x0000'FFFF, 0>(word, value);
}
constexpr auto MemoryBankElementCount = (0xFFFF + 1);
template<typename T, size_t capacity>
using NumericalStorageBank = std::array<T, capacity>;

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

constexpr iris::DoubleWord makeDoubleWord(Word lower, Word upper) noexcept {
    return static_cast<DoubleWord>(lower) | (static_cast<DoubleWord>(upper) << 16);
}

template<typename T, typename A>
constexpr auto IsSameOrConvertible = std::is_same_v<T, A> || std::is_convertible_v<T, A>;

} // end namespace iris
constexpr iris::RegisterIndex operator "" _reg(unsigned long long int conversion) noexcept { return iris::RegisterIndex{static_cast<std::underlying_type_t<iris::RegisterIndex>>(conversion)}; }
iris::RegisterIndex operator "" _reg(const char* str, std::size_t size);
constexpr iris::RegisterIndex operator "" _dreg(unsigned long long int conversion) noexcept { return static_cast<iris::RegisterIndex>(conversion) & static_cast<iris::RegisterIndex>(0b1111110); }
constexpr iris::SignedWord operator "" _sw(unsigned long long int conv) noexcept { return static_cast<iris::SignedWord>(conv); }
constexpr iris::SignedWord operator "" _simm16(unsigned long long int conv) noexcept { return static_cast<iris::SignedWord>(conv); }
constexpr iris::SignedWord operator "" _s16(unsigned long long int conv) noexcept { return static_cast<iris::SignedWord>(conv); }
constexpr iris::Address operator "" _addr(unsigned long long int conversion) noexcept { return static_cast<iris::Address>(conversion); }
constexpr iris::UnsignedWord operator "" _uw(unsigned long long int conversion) noexcept { return static_cast<iris::UnsignedWord>(conversion); }
constexpr iris::UnsignedWord operator "" _imm16(unsigned long long int conversion) noexcept { return static_cast<iris::UnsignedWord>(conversion); }
constexpr iris::UnsignedWord operator "" _u16(unsigned long long int conversion) noexcept { return static_cast<iris::UnsignedWord>(conversion); }
constexpr iris::UnsignedDoubleWord operator "" _udw(unsigned long long int conversion) noexcept { return static_cast<iris::UnsignedDoubleWord>(conversion); }
constexpr iris::SignedDoubleWord operator "" _sdw(unsigned long long int conversion) noexcept { return static_cast<iris::SignedDoubleWord>(conversion); }




#endif // end IRIS_TYPES_H__
