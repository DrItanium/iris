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
#ifndef IRIS_REGISTER_H__
#define IRIS_REGISTER_H__
#include "types.h"

namespace iris {

constexpr Word boolToWord(bool value) noexcept {
    return value ? 0xFFFF : 0;
}
class Register final {
    public:
        explicit constexpr Register(Word value = 0) noexcept : _storage(value) { }
        explicit constexpr Register(bool value) noexcept : _storage(value ? 0xFFFF : 0) { }
        constexpr Register(const Register& other) noexcept = delete;
        constexpr Register(Register&& other) noexcept = delete;
        ~Register() = default;
        Register& operator=(const Register& other) noexcept = delete;
        Register& operator=(Register&& other) noexcept = delete;
        constexpr Register& operator++() noexcept {
            if (!_hardwired) {
                ++_storage._value;
            }
            return *this;
        }
        constexpr Register& operator--() noexcept {
            if (!_hardwired) {
                --_storage._value;
            }
            return *this;
        }
        constexpr bool IsHardwired() const noexcept { return _hardwired; }
        void hardwireTo(Word value) noexcept {
            _hardwired = true;
            _storage._value = value;
        }

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
            if (!_hardwired) {
                if constexpr (std::is_same_v<K, Word> || std::is_convertible_v<K, Word>) {
                    _storage._value = value;
                } else if constexpr (std::is_same_v<K, SignedWord> || std::is_convertible_v<K, SignedWord>) {
                    _storage._signedValue = value;
                } else if constexpr (std::is_same_v<K, bool> || std::is_convertible_v<K, bool>) {
                    _storage._value = boolToWord(value);
                } else {
                    static_assert(false_v<T>, "Cannot assign (or convert) from provided type to Word or SignedWord!");
                }
            }
        }
        explicit constexpr operator Word() const noexcept { return get<Word>(); }
        explicit constexpr operator SignedWord() const noexcept { return get<SignedWord>(); }
        constexpr operator bool() const noexcept { return get<bool>(); }
    private:
        bool _hardwired = false;
        union BackingStore {
            constexpr BackingStore(Word v) : _value(v) { }
            Word _value;
            SignedWord _signedValue;
        } _storage;
};

constexpr auto RegisterCount = (0xFF + 1);
using RegisterBank = NumericalStorageBank<Register, RegisterCount>;

class DoubleRegister final {
    public:
        static DoubleRegister make(RegisterBank& reg, RegisterIndex a, RegisterIndex b) noexcept;
        static DoubleRegister make(RegisterBank& reg, RegisterIndex a) noexcept;
        static const DoubleRegister make(const RegisterBank& reg, RegisterIndex a, RegisterIndex b) noexcept;
        static const DoubleRegister make(const RegisterBank& reg, RegisterIndex a) noexcept;
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
            } else if constexpr (std::is_same_v<T, bool>) {
                return get<DoubleWord>() != 0;
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
            } else if constexpr (std::is_same_v<T, bool> || std::is_convertible_v<T, bool>) {
                put(boolToWord(value), boolToWord(value));
            } else {
                static_assert(false_v<T>, "Illegal type requested!");
            }
        }
        explicit constexpr operator UnsignedDoubleWord() const noexcept { return get<UnsignedDoubleWord>(); }
        explicit constexpr operator SignedDoubleWord() const noexcept { return get<SignedDoubleWord>(); }
        constexpr operator bool() const noexcept { return get<bool>(); }
        auto& operator++() noexcept {
            auto value = get();
            ++value;
            put(value);
            return *this;
        }
        auto& operator--() noexcept {
            auto value = get();
            --value;
            put(value);
            return *this;
        }
    private:
        Register& _lower;
        Register& _upper;
};
} // end namespace iris
#endif // end IRIS_REGISTER_H__
