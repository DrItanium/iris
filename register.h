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
#include "mem_bank.h"


namespace iris {
template<typename T, typename A>
constexpr auto IsSameOrConvertible = std::is_same_v<T, A> || std::is_convertible_v<T, A>;

class Register final {
    public:
        static inline constexpr Ordinal mask = 0xFFFF;
        static constexpr Ordinal boolToValue(bool value) noexcept {
            return value ? mask : 0;
        }
        using SignedType = Integer;
        using UnsignedType = Ordinal;
    public:
        explicit constexpr Register(Word value = 0) noexcept : _storage(value) { }
        explicit constexpr Register(bool value) noexcept : _storage(boolToValue(value)) { }
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

        template<typename Z = Word>
        constexpr Z get() const noexcept {
            using K = std::decay_t<Z>;
            if constexpr (std::is_same_v<K, Word>) {
                return _storage._value;
            } else if constexpr (std::is_same_v<K, SignedWord>) {
                return _storage._signedValue;
            } else if constexpr (std::is_same_v<K, bool>) {
                return _storage._value != 0;
            } else {
                static_assert(false_v<Z>, "Illegal type requested!");
            }
        }
        template<typename Z>
        constexpr void put(Z value) noexcept {
            using K = std::decay_t<Z>;
            if (!_hardwired) {
                if constexpr (IsSameOrConvertible<K, Word>) {
                    _storage._value = value;
                } else if constexpr (IsSameOrConvertible<K, SignedWord>) {
                    _storage._signedValue = value;
                } else if constexpr (IsSameOrConvertible<K, bool>) {
                    _storage._value = boolToValue(value);
                } else {
                    static_assert(false_v<Z>, "Cannot assign (or convert) from provided type to Word or SignedWord!");
                }
            }
        }
        explicit constexpr operator Word() const noexcept { return get<Word>(); }
        explicit constexpr operator SignedWord() const noexcept { return get<SignedWord>(); }
        constexpr operator bool() const noexcept { return get<bool>(); }
    private:
        bool _hardwired = false;
        union BackingStore {
            constexpr BackingStore(UnsignedType v) : _value(v) { }
            UnsignedType _value;
            SignedType _signedValue;
        } _storage;
};
constexpr auto RegisterCount = (0xFF + 1);

template<typename R>
class GenericDoubleRegister final {
    public:
#ifdef HAS_STL
        static GenericDoubleRegister<R> make(RegisterBank& reg, RegisterIndex a, RegisterIndex b) noexcept {
            if constexpr (std::is_same_v<Byte, RegisterIndexNumericType>) {
                return {reg[std::to_integer<Byte>(a)],
                        reg[std::to_integer<Byte>(b)] };
            } else {
                constexpr RegisterIndex registerBankMask = static_cast<RegisterIndex>(0xFF);
                return {reg[std::to_integer<Byte>(a & registerBankMask)], reg[std::to_integer<Byte>(b & registerBankMask)]};
            }
        }
        static GenericDoubleRegister<R> make(RegisterBank& reg, RegisterIndex a) noexcept {
            return make(reg, a, static_cast<RegisterIndex>(std::to_integer<Byte>(a) + 1));
        }
        static const GenericDoubleRegister<R> make(const RegisterBank& reg, RegisterIndex a, RegisterIndex b) noexcept {
            if constexpr (std::is_same_v<Byte, RegisterIndexNumericType>) {
                return {reg[std::to_integer<Byte>(a)],
                        reg[std::to_integer<Byte>(b)] };
            } else {
                constexpr RegisterIndex registerBankMask = static_cast<RegisterIndex>(0xFF);
                return {reg[std::to_integer<Byte>(a & registerBankMask)], reg[std::to_integer<Byte>(b & registerBankMask)]};
            }
        }
        static const GenericDoubleRegister<R> make(const RegisterBank& reg, RegisterIndex a) noexcept {
            return make(reg, a, static_cast<RegisterIndex>(std::to_integer<Byte>(a) + 1));
        }
#endif
    public:
        constexpr GenericDoubleRegister(R& lower, R& upper) : _lower(lower), _upper(upper) { }
        constexpr GenericDoubleRegister(const R& lower, const R& upper) : _lower(const_cast<R&>(lower)), _upper(const_cast<R&>(upper)) { }
        constexpr Word upperHalf() const noexcept { return _upper.template get<Word>(); }
        constexpr Word lowerHalf() const noexcept { return _lower.template get<Word>(); }
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
        void put(Word lower, Word upper) noexcept {
            _lower.put(lower);
            _upper.put(upper);
        }
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
            } else if constexpr (IsSameOrConvertible<T, bool>) {
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
        R& _lower;
        R& _upper;
};
using DoubleRegister = GenericDoubleRegister<Register>;
} // end namespace iris
#endif // end IRIS_REGISTER_H__
