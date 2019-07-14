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
#ifndef IRIS_H__
#define IRIS_H__
#include <cstdint>
#include <variant>
#include <optional>
#include <list>

namespace iris {
using Word = uint16_t;
using SignedWord = int16_t;
using DoubleWord = uint32_t;
using DoubleSignedWord = int32_t;
using Byte = uint8_t;
using SignedByte = int8_t;

class Register final {
    public:
        explicit constexpr Register(Word value) noexcept : _value(value) { }
        explicit constexpr Register(SignedWord value) noexcept : _signedValue(value) { }
        constexpr Register(const Register& other) noexcept = default;
        constexpr Register(Register&& other) noexcept = default;
        ~Register() = default;
        constexpr Register& operator++() noexcept {
            ++_value;
            return *this;
        }
        constexpr Register& operator--() noexcept {
            --_value;
            return *this;
        }
        constexpr bool operator==(const Register& other) const noexcept {
            return other._value == _value;
        }
        constexpr bool operator!=(const Register& other) const noexcept {
            return other._value != _value;
        }
        constexpr bool operator<(SignedWord other) const noexcept { return _signedValue < other; }
        constexpr bool operator<(Word other) const noexcept { return _value < other; }
        constexpr bool operator<=(SignedWord other) const noexcept { return _signedValue <= other; }
        constexpr bool operator<=(Word other) const noexcept { return _value <= other; }
        constexpr bool operator>(SignedWord other) const noexcept { return _signedValue > other; }
        constexpr bool operator>(Word other) const noexcept { return _value > other; }
        constexpr bool operator>=(SignedWord other) const noexcept { return _signedValue >= other; }
        constexpr bool operator>=(Word other) const noexcept { return _value >= other; }
    private:
        union {
            Word _value;
            SignedWord _signedValue;
        };
};

} // end namespace iris


#endif // end IRIS_H__
