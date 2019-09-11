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
        constexpr Register(const Register& other) noexcept = default;
        constexpr Register(Register&& other) noexcept = default;
        ~Register() = default;
        Register& operator=(const Register& other) noexcept = default;
        Register& operator=(Register&& other) noexcept = default;
        constexpr Register& operator++() noexcept {
            ++_storage._value;
            return *this;
        }
        constexpr Register& operator--() noexcept {
            --_storage._value;
            return *this;
        }
        constexpr bool operator==(const Register& other) const noexcept { return other.get<Word>() == get<Word>(); }
        constexpr bool operator==(SignedWord other) const noexcept      { return get<SignedWord>() == other; }
        constexpr bool operator==(Word other) const noexcept            { return get<Word>() == other; }
        constexpr bool operator==(bool other) const noexcept            { return get<bool>() == other; }
        constexpr bool operator!=(const Register& other) const noexcept { return other.get<Word>() != get<Word>(); }
        constexpr bool operator!=(SignedWord other) const noexcept      { return other != get<SignedWord>(); }
        constexpr bool operator!=(Word other) const noexcept            { return other != get<Word>(); }
        constexpr bool operator!=(bool other) const noexcept            { return other != get<bool>(); }
        constexpr bool operator<(const Register& other) const noexcept  { return get<Word>() < other.get<Word>(); }
        constexpr bool operator<(SignedWord other) const noexcept       { return get<SignedWord>() < other; }
        constexpr bool operator<(Word other) const noexcept             { return get<Word>() < other; }
        constexpr bool operator<=(const Register& other) const noexcept { return get<Word>() <= other.get<Word>(); }
        constexpr bool operator<=(SignedWord other) const noexcept      { return get<SignedWord>() <= other; }
        constexpr bool operator<=(Word other) const noexcept            { return get<Word>() <= other; }
        constexpr bool operator>(SignedWord other) const noexcept       { return get<SignedWord>() > other; }
        constexpr bool operator>(Word other) const noexcept             { return get<Word>() > other; }
        constexpr bool operator>(const Register& other) const noexcept  { return get<Word>() > other.get<Word>(); }
        constexpr bool operator>=(SignedWord other) const noexcept      { return get<SignedWord>() >= other; }
        constexpr bool operator>=(Word other) const noexcept            { return get<Word>() >= other; }
        constexpr bool operator>=(const Register& other) const noexcept { return get<Word>() >= other.get<Word>(); }
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
        explicit constexpr operator Word() const noexcept { return get<Word>(); }
        explicit constexpr operator SignedWord() const noexcept { return get<SignedWord>(); }
        constexpr operator bool() const noexcept { return get<bool>(); }
    private:
        union BackingStore {
            constexpr BackingStore(Word v) : _value(v) { }
            Word _value;
            SignedWord _signedValue;
        } _storage;
};
using DestinationRegister = Register&;
using SourceRegister = const Register&;

constexpr auto RegisterCount = (0xFF + 1);
using RegisterBank = NumericalStorageBank<Register, RegisterCount>;

class QuadRegister final {
    public:
        static QuadRegister make(RegisterBank& reg, RegisterIndex a, RegisterIndex b, RegisterIndex c, RegisterIndex d) noexcept;
        static const QuadRegister make(const RegisterBank& reg, RegisterIndex a, RegisterIndex b, RegisterIndex c, RegisterIndex d) noexcept;
        static QuadRegister make(RegisterBank& reg, RegisterIndex a) noexcept;
        static const QuadRegister make(const RegisterBank& reg, RegisterIndex a) noexcept;
    public:
        constexpr QuadRegister(Register& lowest, Register& lower, Register& higher, Register& highest) noexcept : _lowest(lowest), _lower(lower), _higher(higher), _highest(highest) { }
        constexpr QuadRegister(const Register& lowest, 
                const Register& lower, 
                const Register& higher, 
                const Register& highest) noexcept : 
            _lowest(const_cast<Register&>(lowest)), 
            _lower(const_cast<Register&>(lower)), 
            _higher(const_cast<Register&>(higher)), 
            _highest(const_cast<Register&>(highest)) { }
        constexpr auto lowestWord() const noexcept { return _lowest.get(); }
        constexpr auto lowerWord() const noexcept { return _lower.get(); }
        constexpr auto higherWord() const noexcept { return _higher.get(); }
        constexpr auto highestWord() const noexcept { return _highest.get(); }
        constexpr auto lowerHalf() const noexcept { return DoubleWord(lowestWord()) | (DoubleWord(lowerWord()) << 16); }
        constexpr auto upperHalf() const noexcept { return DoubleWord(higherWord()) | (DoubleWord(highestWord()) << 16); }
        template<typename T = UnsignedQuadWord>
        constexpr T get() const noexcept {
            if constexpr (std::is_same_v<T, UnsignedQuadWord>) {
                UnsignedQuadWord l0 = lowestWord();
                UnsignedQuadWord l1 = lowerWord();
                UnsignedQuadWord h0 = higherWord();
                UnsignedQuadWord h1 = highestWord();
                return (l1 << 16) | l0 | (h0 << 32) | (h1 << 48);
            } else if constexpr (std::is_same_v<T, SignedQuadWord>) {
                union temporary {
                    constexpr temporary(UnsignedQuadWord v) : _v(v) { }
                    UnsignedQuadWord _v;
                    SignedQuadWord _s;
                };
                return temporary(get<UnsignedQuadWord>())._s;
            } else if constexpr (std::is_same_v<T, bool>) {
                return get<UnsignedQuadWord>() != 0;
            } else {
                static_assert(false_v<T>, "Illegal type requested");
            }
        }
        void put(Word lowest, Word lower, Word higher, Word highest) noexcept;
        void put(UnsignedDoubleWord lower, UnsignedDoubleWord upper) noexcept;
        template<typename T = UnsignedQuadWord>
        void put(T value) noexcept {
            if constexpr (std::is_same_v<T, UnsignedQuadWord>) {
                put(Word(value), Word(value >> 16), Word(value >> 32), Word(value >> 48));
            } else if constexpr (std::is_same_v<T, SignedQuadWord>) {
                union temporary {
                    constexpr temporary(SignedQuadWord v) : _v(v) { }
                    SignedQuadWord _v;
                    UnsignedQuadWord _u;
                };
                put<UnsignedQuadWord>(temporary(value)._u);
            } else if constexpr (std::is_same_v<T, bool> || std::is_convertible_v<T, bool>) {
                put(boolToWord(value), boolToWord(value), boolToWord(value), boolToWord(value));
            } else {
                static_assert(false_v<T>, "Illegal type requested!");
            }
        }
        explicit constexpr operator UnsignedQuadWord() const noexcept { return get<UnsignedQuadWord>(); }
        explicit constexpr operator SignedQuadWord() const noexcept { return get<SignedQuadWord>(); }
        constexpr operator bool() const noexcept { return get<bool>(); }
        constexpr auto& operator++() noexcept {
            auto value = get();
            ++value;
            put(value);
            return *this;
        }
        constexpr auto& operator--() noexcept {
            auto value = get();
            --value;
            put(value);
            return *this;
        }
        constexpr bool operator==(const QuadRegister& other) const noexcept { return other.get<UnsignedQuadWord>() == get<UnsignedQuadWord>(); }
        constexpr bool operator==(SignedQuadWord other) const noexcept      { return get<SignedQuadWord>() == other; }
        constexpr bool operator==(UnsignedQuadWord other) const noexcept            { return get<UnsignedQuadWord>() == other; }
        constexpr bool operator==(bool other) const noexcept            { return get<bool>() == other; }
        constexpr bool operator!=(const Register& other) const noexcept { return other.get<UnsignedQuadWord>() != get<UnsignedQuadWord>(); }
        constexpr bool operator!=(SignedQuadWord other) const noexcept      { return other != get<SignedQuadWord>(); }
        constexpr bool operator!=(UnsignedQuadWord other) const noexcept            { return other != get<UnsignedQuadWord>(); }
        constexpr bool operator!=(bool other) const noexcept            { return other != get<bool>(); }
        constexpr bool operator<(const Register& other) const noexcept  { return get<UnsignedQuadWord>() < other.get<UnsignedQuadWord>(); }
        constexpr bool operator<(SignedQuadWord other) const noexcept       { return get<SignedQuadWord>() < other; }
        constexpr bool operator<(UnsignedQuadWord other) const noexcept             { return get<UnsignedQuadWord>() < other; }
        constexpr bool operator<=(const Register& other) const noexcept { return get<UnsignedQuadWord>() <= other.get<UnsignedQuadWord>(); }
        constexpr bool operator<=(SignedQuadWord other) const noexcept      { return get<SignedQuadWord>() <= other; }
        constexpr bool operator<=(UnsignedQuadWord other) const noexcept            { return get<UnsignedQuadWord>() <= other; }
        constexpr bool operator>(SignedQuadWord other) const noexcept       { return get<SignedQuadWord>() > other; }
        constexpr bool operator>(UnsignedQuadWord other) const noexcept             { return get<UnsignedQuadWord>() > other; }
        constexpr bool operator>(const Register& other) const noexcept  { return get<UnsignedQuadWord>() > other.get<UnsignedQuadWord>(); }
        constexpr bool operator>=(SignedQuadWord other) const noexcept      { return get<SignedQuadWord>() >= other; }
        constexpr bool operator>=(UnsignedQuadWord other) const noexcept            { return get<UnsignedQuadWord>() >= other; }
        constexpr bool operator>=(const QuadRegister& other) const noexcept { return get<UnsignedQuadWord>() >= other.get<UnsignedQuadWord>(); }

    private:
        Register& _lowest;
        Register& _lower;
        Register& _higher;
        Register& _highest;

};
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
        constexpr auto& operator++() noexcept {
            auto value = get();
            ++value;
            put(value);
            return *this;
        }
        constexpr auto& operator--() noexcept {
            auto value = get();
            --value;
            put(value);
            return *this;
        }
        constexpr bool operator==(const DoubleRegister& other) const noexcept { return other.get<UnsignedDoubleWord>() == get<UnsignedDoubleWord>(); }
        constexpr bool operator==(SignedDoubleWord other) const noexcept      { return get<SignedDoubleWord>() == other; }
        constexpr bool operator==(UnsignedDoubleWord other) const noexcept            { return get<UnsignedDoubleWord>() == other; }
        constexpr bool operator==(bool other) const noexcept            { return get<bool>() == other; }
        constexpr bool operator!=(const Register& other) const noexcept { return other.get<UnsignedDoubleWord>() != get<UnsignedDoubleWord>(); }
        constexpr bool operator!=(SignedDoubleWord other) const noexcept      { return other != get<SignedDoubleWord>(); }
        constexpr bool operator!=(UnsignedDoubleWord other) const noexcept            { return other != get<UnsignedDoubleWord>(); }
        constexpr bool operator!=(bool other) const noexcept            { return other != get<bool>(); }
        constexpr bool operator<(const Register& other) const noexcept  { return get<UnsignedDoubleWord>() < other.get<UnsignedDoubleWord>(); }
        constexpr bool operator<(SignedDoubleWord other) const noexcept       { return get<SignedDoubleWord>() < other; }
        constexpr bool operator<(UnsignedDoubleWord other) const noexcept             { return get<UnsignedDoubleWord>() < other; }
        constexpr bool operator<=(const Register& other) const noexcept { return get<UnsignedDoubleWord>() <= other.get<UnsignedDoubleWord>(); }
        constexpr bool operator<=(SignedDoubleWord other) const noexcept      { return get<SignedDoubleWord>() <= other; }
        constexpr bool operator<=(UnsignedDoubleWord other) const noexcept            { return get<UnsignedDoubleWord>() <= other; }
        constexpr bool operator>(SignedDoubleWord other) const noexcept       { return get<SignedDoubleWord>() > other; }
        constexpr bool operator>(UnsignedDoubleWord other) const noexcept             { return get<UnsignedDoubleWord>() > other; }
        constexpr bool operator>(const Register& other) const noexcept  { return get<UnsignedDoubleWord>() > other.get<UnsignedDoubleWord>(); }
        constexpr bool operator>=(SignedDoubleWord other) const noexcept      { return get<SignedDoubleWord>() >= other; }
        constexpr bool operator>=(UnsignedDoubleWord other) const noexcept            { return get<UnsignedDoubleWord>() >= other; }
        constexpr bool operator>=(const DoubleRegister& other) const noexcept { return get<UnsignedDoubleWord>() >= other.get<UnsignedDoubleWord>(); }
    private:
        Register& _lower;
        Register& _upper;
};
} // end namespace iris
#endif // end IRIS_REGISTER_H__
