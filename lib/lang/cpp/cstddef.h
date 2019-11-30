/**
 * @file
 * cstddef interface
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

#ifndef IRIS_CSTDDEF_H__
#define IRIS_CSTDDEF_H__
#include "lang/cpp/platform.h"
#ifdef HAS_STL
#include <cstddef>
#else
extern "C" {
#include <stddef.h>
}
namespace std {
    using nullptr_t = decltype(nullptr);
    using ptrdiff_t = ::ptrdiff_t;
    using size_t = ::size_t;
    using max_align_t = ::max_align_t;

    enum class byte : unsigned char { };

    template<typename T> struct _ByteOperand { };
#define X(ty) \
    template<> struct _ByteOperand< ty > { using __Type = byte; }
    X(bool);
    X(char);
    X(signed char);
    X(unsigned char);
    X(short);
    X(unsigned short);
    X(int);
    X(unsigned int);
    X(long);
    X(unsigned long);
    X(long long);
    X(unsigned long long);
#undef X
    template<typename T>
    using _ByteOperand_t = typename _ByteOperand<T>::__Type;

    template<typename T>
    constexpr _ByteOperand_t<T>& operator<<=(byte& b, T shift) noexcept {
        b = byte(static_cast<unsigned char>(b) << shift);
        return b;
    }
    template<typename T>
    constexpr _ByteOperand_t<T> operator<<(byte b, T shift) noexcept {
        return byte(static_cast<unsigned char>(b) << shift);
    }
    template<typename T>
    constexpr _ByteOperand_t<T>& operator>>=(byte& b, T shift) noexcept {
        b = byte(static_cast<unsigned char>(b) >> shift);
        return b;
    }
    template<typename T>
    constexpr _ByteOperand_t<T> operator>>(byte b, T shift) noexcept {
        return byte(static_cast<unsigned char>(b) >> shift);
    }

    constexpr byte& operator|=(byte& l, byte r) noexcept {
        l = byte(static_cast<unsigned char>(l) | static_cast<unsigned char>(r));
        return l;
    }
    constexpr byte operator|(byte l, byte r) noexcept {
        return byte(static_cast<unsigned char>(l) | static_cast<unsigned char>(r));
    }
    constexpr byte& operator&=(byte& l, byte r) noexcept {
        l = byte(static_cast<unsigned char>(l) & static_cast<unsigned char>(r));
        return l;
    }
    constexpr byte operator&(byte l, byte r) noexcept {
        return byte(static_cast<unsigned char>(l) & static_cast<unsigned char>(r));
    }
    constexpr byte& operator^=(byte& l, byte r) noexcept {
        l = byte(static_cast<unsigned char>(l) ^ static_cast<unsigned char>(r));
        return l;
    }
    constexpr byte operator^(byte l, byte r) noexcept {
        return byte(static_cast<unsigned char>(l) ^ static_cast<unsigned char>(r));
    }
    constexpr byte operator~(byte l) noexcept {
        return byte(~static_cast<unsigned char>(l));
    }
    template<typename T>
    constexpr T to_integer(byte b) noexcept {
        return T(b);
    }
} // end namespace std
#endif // end defined(HAS_STL)

#endif // end IRIS_CSTDDEF_H__
