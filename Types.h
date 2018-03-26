/*
 * iris
 * Copyright (c) 2013-2018, Joshua Scoggins and Contributors
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
#ifndef TYPES_H__
#define TYPES_H__
#include <cstdint>
#include <climits>

#define INDIRECTOR(a, ...) PRIMITIVE_INDIRECTOR(a, __VA_ARGS__)
#define PRIMITIVE_INDIRECTOR(a, ...) a ## __VA_ARGS__
namespace iris {
	using byte = uint8_t;
	using Integer = int16_t;
	using Address = uint16_t;
	using Word = Address;
	using RawInstruction = uint32_t;
	using Address32 = uint32_t;

	template<typename T, typename R, T mask, T shift = 0>
	constexpr T encodeBits(T value, R newValue) noexcept {
		return (value & ~mask) | ((static_cast<T>(newValue) << shift) & mask);
	}
	template<typename T, typename R>
	constexpr T encodeBits(T value, R newValue, T mask, T shift = 0) noexcept {
		return (value & ~mask) | ((static_cast<T>(newValue) << shift) & mask);
	}


	template<typename T, typename R, T mask, T shift = 0>
	constexpr R decodeBits(T value) noexcept {
		return static_cast<R>((value & mask) >> shift);
	}

    template<typename T, typename R>
    constexpr R decodeBits(T value, T mask, T shift = 0) noexcept {
        return static_cast<R>((value & mask) >> shift);
    }

	template<typename T>
	constexpr std::size_t bitwidth = CHAR_BIT * sizeof(T);

	template<typename T, typename R = T>
	constexpr R binaryAnd(T a, T b) noexcept {
		return static_cast<R>(a & b);
	}
	
	template<>
	constexpr bool binaryAnd<bool, bool>(bool a, bool b) noexcept {
		return a && b;
	}
	
	template<typename T, typename R = T>
	constexpr R binaryOr(T a, T b) noexcept {
		return static_cast<R>(a | b);
	}
	
	template<>
	constexpr bool binaryOr<bool, bool>(bool a, bool b) noexcept {
		return a || b;
	}
	
	
	template<typename T, typename R = T>
	constexpr R binaryNot(T a) noexcept {
		return static_cast<R>(~a);
	}
	
	template<>
	constexpr bool binaryNot<bool, bool>(bool a) noexcept {
		return !a;
	}
	
	template<typename T, typename R = T>
	constexpr R binaryXor(T a, T b) noexcept {
		return static_cast<R>(a ^ b);
	}
	
	template<typename T, typename R = T>
	constexpr R binaryNand(T a, T b) noexcept {
		return static_cast<R>(~(a & b));
	}
	
	template<>
	constexpr bool binaryNand(bool a, bool b) noexcept {
		return !(a && b);
	}
	
	template<typename T, typename R = T>
	constexpr R binaryNor(T a, T b) noexcept {
		return ~(a | b);
	}
	
	template<>
	constexpr bool binaryNor(bool a, bool b) noexcept {
		return !(a || b);
	}
	
	template<typename T, typename R = T>
	constexpr R circularShiftLeft(T value, T shift) noexcept {
	    constexpr T width = bitwidth<T> - 1;
	    // taken from the wikipedia entry on circular shifts
	    return static_cast<R>((value << shift) | (value >> ((-shift) & width)));
	}
	
	template<typename T, typename R = T>
	constexpr R circularShiftRight(T value, T shift) noexcept {
	    constexpr T width = bitwidth<T> - 1;
	    // taken from the wikipedia entry on circular shifts
		return static_cast<R>( (value >> shift) | (value << ((-shift) & width)));
	}
	/**
	 * Checks to see if the given value is even.
	 * @param value the value to check evenness against
	 * @return boolean signifying that the given input is even.
	 * @tparam T the type of the thing to check to see if it is even.
	 */
	template<typename T>
	constexpr bool isEven(T value) noexcept {
	    return (value & 1) == 0;
	}
	
	/**
	 * Checks to see if the given value is odd.
	 * @param value the value to check oddness against
	 * @return boolean signifying that the given input is odd.
	 * @tparam T the type of the thing to check to see if it is odd.
	 */
	template<typename T>
	constexpr bool isOdd(T value) noexcept {
		return !isEven<T>(value);
	}

    template<typename T>
    struct AlwaysFalse : std::false_type { };

} // end namespace iris
#endif
