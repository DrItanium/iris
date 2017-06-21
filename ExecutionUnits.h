/*
 * syn
 * Copyright (c) 2013-2017, Joshua Scoggins and Contributors
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


// define Execution Units to cut down on the amount of repeated actions
#ifndef _SYN_XUNITS_H
#define _SYN_XUNITS_H
#include "Base.h"
#include "BaseArithmetic.h"
#include "IODevice.h"
#include <cmath>
namespace syn {

namespace FPU {
    enum class StandardOperations {
        Add,
        Subtract,
        Multiply,
        Divide,
        SquareRoot,
        Count,
    };
    template<typename Word, typename Return = Word, typename Operation = StandardOperations>
    Return performOperation(Operation op, Word a, Word b) {
        switch(op) {
            case Operation::Add:
                return syn::add<Word, Return>(a, b);
            case Operation::Subtract:
                return syn::sub<Word, Return>(a, b);
            case Operation::Multiply:
                return syn::mul<Word, Return>(a, b);
            case Operation::Divide:
                return syn::div<Word, Return>(a, b);
            case Operation::SquareRoot:
                return static_cast<Return>(sqrt(static_cast<double>(a)));
            default:
                throw syn::Problem("Undefined fpu operation!");
        }
    }
    template<StandardOperations op, typename Word, typename Return = Word>
    Return performOperation(Word a, Word b) {
        static_assert(!isErrorState(op), "Illegal FPU operation!");
        return performOperation<Word, Return, decltype(op)>(op, a, b);
    }
}

namespace ALU {
    enum class StandardOperations {
        Add,
        Subtract,
        Multiply,
        Divide,
        Remainder,
        ShiftLeft,
        ShiftRight,
        BinaryAnd,
        BinaryOr,
        UnaryNot,
        BinaryXor,
        BinaryNand,
        CircularShiftLeft,
        CircularShiftRight,
        Count,
    };

    template<typename Word, typename Return = Word, typename Operation = StandardOperations>
    Return performOperation(Operation op, Word a, Word b, syn::OnDivideByZero<Return> markDivideByZero = nullptr) {
        switch(op) {
            case Operation::Add:
                return syn::add<Word, Return>(a, b);
            case Operation::Subtract:
                return syn::sub<Word, Return>(a, b);
            case Operation::Multiply:
                return syn::mul<Word, Return>(a, b);
            case Operation::Divide:
                return syn::div<Word, Return>(a, b, markDivideByZero);
            case Operation::Remainder:
                return syn::rem<Word, Return>(a, b, markDivideByZero);
            case Operation::ShiftLeft:
                return syn::shiftLeft<Word, Return>(a, b);
            case Operation::ShiftRight:
                return syn::shiftRight<Word, Return>(a, b);
            case Operation::BinaryAnd:
                return syn::binaryAnd<Word, Return>(a, b);
            case Operation::BinaryOr:
                return syn::binaryOr<Word, Return>(a, b);
            case Operation::UnaryNot:
                return syn::binaryNot<Word, Return>(a);
            case Operation::BinaryXor:
                return syn::binaryXor<Word, Return>(a, b);
            case Operation::BinaryNand:
                return syn::binaryNand<Word, Return>(a, b);
            case Operation::CircularShiftLeft:
                return syn::circularShiftLeft<Word, Return>(a, b);
            case Operation::CircularShiftRight:
                return syn::circularShiftRight<Word, Return>(a, b);
            default:
                throw syn::Problem("Undefined ALU operation!");
        }
    }
    /**
     * Special version of performOperation where the StandardOperations is
     * hardcoded at compile time!
     */
    template<StandardOperations op, typename Word, typename Return = Word>
    constexpr Return performOperation(Word a, Word b, syn::OnDivideByZero<Return> markDivideByZero) noexcept {
        static_assert(!isErrorState(op), "Illegal operation!");
        return performOperation<Word, Return, decltype(op)>(op, a, b, markDivideByZero);
    }

    /**
     * Special version of performOperation where the StandardOperations is
     * hardcoded at compile time!
     */
    template<StandardOperations op, typename Word, typename Return = Word>
    constexpr Return performOperation(Word a, Word b) noexcept {
        return performOperation<op, Word, Return>(a, b, nullptr);
    }

} // end namespace ALU

namespace Comparator {
    enum class StandardOperations {
        Eq,
        Neq,
        LessThan,
        GreaterThan,
        LessThanOrEqualTo,
        GreaterThanOrEqualTo,
        // extended operations!
        BinaryAnd,
        BinaryOr,
        UnaryNot,
        BinaryXor,
        BinaryNand,
        ShiftLeft,
        ShiftRight,
        CircularShiftLeft,
        CircularShiftRight,
        BinaryNor,
        Count,
    };
    template<typename Word, typename Return = Word, typename Operation = StandardOperations>
    Return performOperation(Operation op, Word a, Word b) {
        switch(op) {
            case Operation::Eq:
                return syn::eq<Word, Return>(a, b);
            case Operation::Neq:
                return syn::neq<Word, Return>(a, b);
            case Operation::LessThan:
                return syn::lt<Word, Return>(a, b);
            case Operation::GreaterThan:
                return syn::gt<Word, Return>(a, b);
            case Operation::LessThanOrEqualTo:
                return syn::le<Word, Return>(a, b);
            case Operation::GreaterThanOrEqualTo:
                return syn::ge<Word, Return>(a, b);
            case Operation::BinaryAnd:
                return syn::binaryAnd<Word, Return>(a, b);
            case Operation::BinaryOr:
                return syn::binaryOr<Word, Return>(a, b);
            case Operation::UnaryNot:
                return syn::binaryNot<Word, Return>(a);
            case Operation::BinaryXor:
                return syn::binaryXor<Word, Return>(a, b);
            case Operation::BinaryNand:
                return syn::binaryNand<Word, Return>(a, b);
            case Operation::ShiftLeft:
                return syn::shiftLeft<Word, Return>(a, b);
            case Operation::ShiftRight:
                return syn::shiftRight<Word, Return>(a, b);
            case Operation::BinaryNor:
                return syn::binaryNor<Word, Return>(a, b);
            case Operation::CircularShiftLeft:
                return syn::circularShiftLeft<Word, Return>(a, b);
            case Operation::CircularShiftRight:
                return syn::circularShiftRight<Word, Return>(a, b);
            default:
                throw syn::Problem("Illegal compare operation!");
        }
    }
    template<StandardOperations op, typename Word, typename Return = Word>
    inline Return performOperation(Word a, Word b) {
        static_assert(!isErrorState(op), "Illegal operation!");
        return performOperation<Word, Return, decltype(op)>(op, a, b);
    }
    enum class BooleanOperations {
        Eq,
        Neq,
        BinaryAnd,
        BinaryOr,
        UnaryNot,
        BinaryXor,
        BinaryNand,
        BinaryNor,
        Count,
    };
    template<>
    inline bool performOperation<bool, bool, BooleanOperations>(BooleanOperations op, bool a, bool b) {
        using Operation = BooleanOperations;
        switch(op) {
            case Operation::Eq:
                return syn::eq<bool>(a, b);
            case Operation::Neq:
                return syn::neq<bool>(a, b);
            case Operation::BinaryAnd:
                return syn::binaryAnd<bool>(a, b);
            case Operation::BinaryOr:
                return syn::binaryOr<bool>(a, b);
            case Operation::BinaryXor:
                return syn::binaryXor<bool>(a, b);
            case Operation::UnaryNot:
                return syn::binaryNot<bool>(a);
            case Operation::BinaryNand:
                return syn::binaryNand<bool>(a, b);
            case Operation::BinaryNor:
                return syn::binaryNor<bool>(a, b);
            default:
                throw syn::Problem("Illegal boolean compare operation!");
        }
    }
    template<BooleanOperations op>
    bool performOperation(bool a, bool b) {
        static_assert(!isErrorState(op), "Illegal operation!");
        return performOperation<bool, bool, decltype(op)>(op, a, b);
    }
} // end namespace Comparator

template<typename Word, typename Address = Word>
class LoadStoreUnit : public AddressableIODevice<Word, Address> {
	public:
		using WordType = Word;
		using AddressType = Address;
		using Parent = AddressableIODevice<Word, Address>;
        using BadLoadHandler = std::function<Word&(Address)>;
        using BadStoreHandler = std::function<void(Address, Word)>;
        enum class Operation {
            Zero,
            Swap,
            Load,
            Store,
            Copy,
            Count,
        };
        static Word& defaultBadLoadHandler(Address addr) {
            static Word value = static_cast<Word>(0);
            throw syn::Problem("Provided address is not legal!");
            return value;
        }
        static void defaultBadStoreHandler(Address addr, Word value) {
            throw syn::Problem("Provided address is not legal!");
        }
	public:
		LoadStoreUnit(Address size, Address base = 0, BadLoadHandler onBadLoad = defaultBadLoadHandler, BadStoreHandler onBadStore = defaultBadStoreHandler) : Parent(base, size), _memory(std::move(std::make_unique<Word[]>(size))), _size(size), _onBadLoad(onBadLoad), _onBadStore(onBadStore) { }
		LoadStoreUnit() : LoadStoreUnit(0) { }

        void setLoadExceptionHandler(BadLoadHandler op) {
            _onBadLoad = op;
        }
        void setStoreExceptionHandler(BadStoreHandler op) {
            _onBadStore = op;
        }

		virtual ~LoadStoreUnit() { }
		inline void zero() noexcept {
			for (Address addr = 0; addr < _size; ++addr) {
				_memory[addr] = 0;
			}
		}
		inline Address getSize() const noexcept { return _size; }
		inline bool legalAddress(Address addr) const noexcept {
			return addr >= 0 && addr < _size;
		}
		void set(Address addr, Word value) {
			if (legalAddress(addr)) {
				_memory[addr] = value;
			} else {
                _onBadStore(addr, value);
			}
		}
		inline Word& retrieveMemory(Address addr) {
            return legalAddress(addr) ? _memory[addr] : _onBadLoad(addr);
		}
		virtual Word read(Address addr) override {
			return retrieveMemory(addr);
		}
		virtual void write(Address addr, Word value) override {
			set(addr, value);
		}
		Word& operator[](Address addr) {
			return retrieveMemory(addr);
		}
		void swap(Address a, Address b) {
			syn::swap<Word>(_memory[a], _memory[b]);
		}
		void copy(Address a, Address b) {
            set(a, read(b));
		}
		void install(std::istream& stream, std::function<Word(char*)> decode) {
			char buf[sizeof(Word)] = { 0 };
			for (Address i = 0; i < _size; ++i) {
				stream.read(buf, sizeof(Word));
				_memory[i] = decode(buf);
			}
		}
		void dump(std::ostream& stream, std::function<void(Word, char*)> encode) {
			char buf[sizeof(Word)] = { 0 };
			for (Address i = 0; i < _size; ++i) {
				encode(_memory[i], buf);
				stream.write(buf, sizeof(Word));
			}
		}
	private:
		std::unique_ptr<Word[]> _memory;
		Address _size;
        BadLoadHandler _onBadLoad;
        BadStoreHandler _onBadStore;
};

template<typename Word, typename Address, Address capacity>
class FixedSizeLoadStoreUnit : public LoadStoreUnit<Word, Address> {
    public:
        static constexpr auto count = capacity;
        using Parent = LoadStoreUnit<Word, Address>;
	public:
		FixedSizeLoadStoreUnit(Address base = 0, typename Parent::BadLoadHandler onBadLoad = Parent::defaultBadLoadHandler, typename Parent::BadStoreHandler onBadStore = Parent::defaultBadStoreHandler) : Parent(capacity, base, onBadLoad, onBadStore) { }
		virtual ~FixedSizeLoadStoreUnit() { }
};

template<typename T, T addressMask>
class Register {
    public:
        using AddressType = T;
        using Self = Register<T, addressMask>;
        static constexpr T getAddressMask() noexcept { return addressMask; }
        static constexpr T generateProperAddress(T value) noexcept { return syn::decodeBits<T, T, getAddressMask(), 0>(value); }
    public:
        Register(T value) noexcept : _value(value) { }
        Register() noexcept : _value(static_cast<T>(0)) { }
        Register(Register&& other) noexcept : _value(std::move(other._value)) { }
        Register(const Register& other) noexcept : _value(other.get()) { }
        virtual ~Register() { }
        inline T get() const noexcept { return _value; }
        inline void set(T value) noexcept { _value = generateProperAddress(value); }
        inline void increment(T value = 1) noexcept { set(get() + value); }
        inline void decrement(T value = 1) noexcept { set(get() - value); }

        inline Self& operator++() {
            increment();
            return *this;
        }
        inline Self& operator--() {
            decrement();
            return *this;
        }
        template<T index>
        bool getBit() const noexcept {
            static_assert(index < syn::bitwidth<T>, "Provided index is too large!");
            static_assert(index >= 0, "Provided index is less than zero!");
            return syn::getBit<T, index>(_value);
        }
        bool getBit(T index) const {
            if (index >= syn::bitwidth<T>) {
                throw syn::Problem("Provided index is too large!");
            } else if (index < 0) {
                throw syn::Problem("Provided index is less than zero!");
            } else {
                return syn::getBit<T>(_value, index);
            }
        }
        template<T index>
        void setBit(bool value) noexcept {
            static_assert(index < syn::bitwidth<T>, "Provided index is too large!");
            static_assert(index >= 0, "Provided index is less than zero!");
            set(syn::setBit<T, index>(_value, value));
        }
        void setBit(T index, bool value) noexcept {
            if (index >= syn::bitwidth<T>) {
                throw syn::Problem("Provided index is too large!");
            } else if (index < 0) {
                throw syn::Problem("Provided index is less than zero!");
            } else {
                _value = syn::setBit<T>(_value, value, index);
            }
        }
        template<T mask, T shift, typename R = T>
        inline R decode() const noexcept {
            return syn::decodeBits<T, R, mask, shift>(_value);
        }
        template<typename R = T>
        R decode(T mask, T shift) const noexcept {
            return syn::decodeBits<T, R>(_value, mask, shift);
        }
        template<typename Input, T mask, T shift>
        inline void encode(Input value) noexcept {
            set(syn::encodeBits<T, Input, mask, shift>(_value, value));
        }
        template<typename Input>
        inline void encode(Input value, T mask, T shift) noexcept {
            set(syn::encodeBits<T, Input>(_value, value, mask, shift));
        }
        inline bool operator<(const Self& b) const noexcept {
            return _value < b._value;
        }
        inline bool operator>(const Self& b) const noexcept {
            return _value > b._value;
        }
        inline bool operator<=(const Self& b) const noexcept {
            return _value <= b._value;
        }
        inline bool operator>=(const Self& b) const noexcept {
            return _value >= b._value;
        }
        inline Self& operator+=(const Self& other) const noexcept {
            increment(other._value);
            return *this;
        }
        inline Self& operator-=(const Self& other) const noexcept {
            decrement(other._value);
            return *this;
        }
        inline Self& operator*=(const Self& other) const noexcept {
            set(_value * other._value);
            return *this;
        }
        inline Self& operator/=(const Self& other) const noexcept {
            set(_value / other._value);
            return *this;
        }
        inline void swapBits(T index0, T index1) {
            if (index0 != index1) {
                constexpr auto width = syn::bitwidth<T>;
                if (index0 >= width || index1 >= width) {
                    throw syn::Problem("Provided index is not legal!");
                } else if (index1 < 0 || index0 < 0) {
                    throw syn::Problem("Provided index is less than zero!");
                } else {
                    auto tmp = getBit(index0);
                    setBit(index0, getBit(index1));
                    setBit(index1, tmp);
                }
            }
        }
    private:
        T _value;
};

} // end namespace syn
#endif // end _SYN_XUNITS_H
