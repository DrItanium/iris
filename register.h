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
        static inline constexpr Ordinal mask = 0xFFFF'FFFF;
        static constexpr Ordinal boolToValue(bool value) noexcept {
            return value ? mask : 0;
        }
    public:
        explicit constexpr Register(Ordinal value = 0) noexcept : _storage(value) { }
        explicit constexpr Register(bool value) noexcept : _storage(boolToValue(value)) { }
        constexpr Register(const Register& other) noexcept = delete;
        constexpr Register(Register&& other) noexcept = delete;
        ~Register() noexcept = default;
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
        void hardwireTo(Ordinal value) noexcept {
            _hardwired = true;
            _storage._value = value;
        }

        template<typename Z = Word>
        constexpr Z get() const noexcept {
            using K = std::decay_t<Z>;
            if constexpr (std::is_same_v<K, Ordinal>) {
                return _storage._value;
            } else if constexpr (std::is_same_v<K, Integer>) {
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
                if constexpr (IsSameOrConvertible<K, Ordinal>) {
                    _storage._value = value;
                } else if constexpr (IsSameOrConvertible<K, Integer>) {
                    _storage._signedValue = value;
                } else if constexpr (IsSameOrConvertible<K, bool>) {
                    _storage._value = boolToValue(value);
                } else {
                    static_assert(false_v<Z>, "Cannot assign (or convert) from provided type to Word or SignedWord!");
                }
            }
        }
        explicit constexpr operator Ordinal() const noexcept { return get<Ordinal>(); }
        explicit constexpr operator Integer() const noexcept { return get<Integer>(); }
        constexpr operator bool() const noexcept { return get<bool>(); }
    private:
        bool _hardwired = false;
        union BackingStore {
            constexpr BackingStore(Ordinal v) : _value(v) { }
            Ordinal _value;
            Integer _signedValue;
        } _storage;
};

using RegisterBank = NumericalStorageBank<Register, RegisterCount>;

} // end namespace iris
#endif // end IRIS_REGISTER_H__
