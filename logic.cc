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
#include "iris.h"
#include "opcodes.h"

namespace iris {

void
Core::invoke(const std::monostate&) {
    throw UnimplementedOperationException();
}

void
Core::invoke(const iris::ErrorInstruction&) {
    throw ErrorInstructionException();
}

void
Core::invoke(const iris::MemoryCopyRegisterInstruction& s) {
    if (auto [dest, src] = s.arguments(); dest != src) {
        setRegisterValue(dest, getRegisterValue(src));
    }
}
void
Core::invoke(const iris::MemorySwapRegistersInstruction& s) {
    if (auto [ar, br] = s.arguments(); ar != br) {
        auto aValue = getRegisterValue<Word>(ar);
        setRegisterValue(ar, getRegisterValue(br));
        setRegisterValue(br, aValue);
    } 
}

void
Core::invoke(const iris::MemoryAssignRegisterImmediateInstruction& s) {
    auto [dest, imm16] = s.arguments();
    setRegisterValue(dest, imm16);
}
void
Core::invoke(const iris::MemoryCodeLoadWithOffsetInstruction& s) {
    // CodeLoad AddressRegister LowerRegister (implied UpperRegister = LowerRegister + 1)
    auto [addr, lower, offset] = s.arguments();
    setDoubleRegisterValue(lower, loadCode(addr, offset));
}
void
Core::invoke(const iris::MemoryCodeStoreWithOffsetInstruction& s) {
    // CodeStore AddressRegister <= LowerRegister (upper register implied)
    auto [addr, lower, offset ] = s.arguments();
    storeCode(addr, lower, offset);
}

void
Core::invoke(const iris::MemoryStackPopInstruction& s) {
    // so stack grows downward
    // pops grow towards 0xFFFF
    // StackPop StackPointerRegister DestinationRegister
    auto [stackPointer, destination] = s.arguments();
    setRegisterValue(destination, loadStack(stackPointer));
    incrementRegister(stackPointer);
}
void
Core::invoke(const iris::MemoryStackPushInstruction& s) {
    // stack grows downward
    // StackPush StackPointerRegister SourceRegister
    auto [stackPointer, src] = s.arguments();
    decrementRegister(stackPointer);
    storeStack(stackPointer, src);
}

void
Core::invoke(const iris::MemoryStackPushImmediateValueInstruction& s) {
    auto [stackPointer, imm16] = s.arguments();
    decrementRegister(stackPointer);
    storeStack(stackPointer, imm16);
}


void
Core::invoke(const iris::BranchSelectInstruction& s) {
    // BranchSelect ConditionalRegister TrueAddress FalseAddress
    auto [ cond, onTrue, onFalse] = s.arguments();
    _ip.put(getRegisterValue(getRegisterValue<bool>(cond) ? onTrue : onFalse));
    _advanceIP = false;
}
void
Core::invoke(const iris::CompareEqualsInstruction& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, getSourceRegister(src0) == getSourceRegister(src1));
}
void
Core::invoke(const iris::CompareNotEqualsInstruction& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, getSourceRegister(src0) != getSourceRegister(src1));
}
#define Y(name, op, kind) \
    void \
    Core::invoke(const iris:: Compare ## name ## kind ## Instruction & s) { \
        using T = kind ## Word ; \
        auto [ dest, src0, src1 ] = s.arguments(); \
        setRegisterValue(dest, \
                getRegisterValue<T>(src0) op \
                getRegisterValue<T>(src1)); \
    }
#define X(name, op) \
    Y(name, op, Signed); \
    Y(name, op, Unsigned);
X(LessThanOrEqualTo, <=);
X(LessThan, <);
X(GreaterThanOrEqualTo, >=);
X(GreaterThan, >);
#undef Y
#define Y(name, op, kind) \
    void \
    Core::invoke(const iris:: Compare ## name ## kind ## Immediate8Instruction & s) { \
        using T = kind ## Word ; \
        auto [ dest, src0, src1 ] = s.arguments(); \
        setRegisterValue(dest, \
                getRegisterValue<T>(src0) op \
                static_cast<T>(src1)); \
    }
X(LessThanOrEqualTo, <=);
X(LessThan, <);
X(GreaterThanOrEqualTo, >=);
X(GreaterThan, >);
#undef Y
#undef X


void
Core::invoke(const iris::CompareEqualsImmediate8Instruction& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, getRegisterValue(src0) == static_cast<Word>(src1));
}
void
Core::invoke(const iris::CompareNotEqualsImmediate8Instruction& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, getRegisterValue(src0) != static_cast<Word>(src1));
}
#define X(op, act) \
    void \
    Core::invoke ( const iris:: Arithmetic ## op ## SignedImmediateInstruction & s) { \
        using T = SignedWord; \
        auto [ dest, src0, s8 ] = s.arguments(); \
        setRegisterValue(dest, getRegisterValue<SignedWord>(src0) act static_cast<T>(s8)); \
    } \
    void \
    Core::invoke ( const iris:: Arithmetic ## op ## UnsignedImmediateInstruction & s) { \
        using T = UnsignedWord; \
        auto [ dest, src0, u8 ] = s.arguments(); \
        setRegisterValue(dest, getRegisterValue<Word>(src0) act static_cast<T>(u8)); \
    }
X(Multiply, *);
X(Add, +);
X(Subtract, -);
#undef X
#define X(op, act) \
    void \
    Core::invoke ( const iris:: Arithmetic ## op ## UnsignedImmediateInstruction & s) { \
        auto [ dest, src0, u8 ] = s.arguments(); \
        setRegisterValue(dest, getRegisterValue<Word>(src0) act static_cast<Word>(u8)); \
    }
X(ShiftLeft, <<);
X(ShiftRight, >>);
#undef X


void
Core::invoke(const iris::BranchConditionalImmediateInstruction& s) {
    auto [ cond, to ] = s.arguments();
    if (getRegisterValue<bool>(cond)) {
        _ip.put(to);
        _advanceIP = false;
    }
}

void
Core::invoke(const iris::BranchConditionalRegisterAndLinkInstruction& s) {
    auto [ dest, cond, link ] = s.arguments();
    if (getRegisterValue<bool>(cond)) {
        setRegisterValue(link, _ip.get() + 1);
        _ip.put(getRegisterValue(dest));
        _advanceIP = false;
    }
}

void
Core::invoke(const iris::MemoryDataLoadWithOffsetInstruction& s) {
    auto [ dest, loc, offset ] = s.arguments();
    setRegisterValue(dest, loadData(loc, offset));
}
void
Core::invoke(const iris::MemoryDataStoreImmediateValueInstruction& s) {
    auto [ addr, imm16 ] = s.arguments();
    storeData(addr, imm16);
}
void
Core::invoke(const iris::MemoryDataStoreWithOffsetInstruction& s) {
    auto [ dest, value, offset ] = s.arguments();
    storeData(dest, value, offset);
}
template<typename T> constexpr auto add(T a, T b) noexcept { return a + b; }
template<typename T> constexpr auto sub(T a, T b) noexcept { return a - b; }
template<typename T> constexpr auto mul(T a, T b) noexcept { return a * b; }
template<typename T> constexpr auto shl(T a, T b) noexcept { return a << b; }
template<typename T> constexpr auto shr(T a, T b) noexcept { return a >> b; }
#define Y(name, op, suffix, types) \
    void \
    Core::invoke( const iris:: Arithmetic ## name ## suffix & s ) { \
        auto [ dest, src0, src1 ] = s.arguments(); \
        setRegisterValue<types>(dest, op(getRegisterValue<types>(src0), getRegisterValue<types>(src1))); \
    }
#define X(name, op) \
    Y(name, op, SignedInstruction, SignedWord); \
    Y(name, op, UnsignedInstruction, Word)
X(Add, add);
X(Subtract, sub);
X(Multiply, mul);
X(ShiftRight, shr);
X(ShiftLeft, shl);
X(Min, std::min);
X(Max, std::max);
#undef X
#undef Y

void
Core::invoke(const iris::BranchRegisterInstruction& s) {
    auto [ dest ] = s.arguments();
    _ip.put(getRegisterValue(dest));
    _advanceIP = false;
}

void
Core::invoke(const iris::ArithmeticBitwiseNotInstruction& s) {
    auto [ dest, src0 ] = s.arguments();
    setRegisterValue(dest, ~getRegisterValue(src0));
}

template<typename T>
constexpr auto NotTheResult = false;

template<>
constexpr auto NotTheResult<ArithmeticBitwiseNorInstruction> = true;
template<>
constexpr auto NotTheResult<ArithmeticBitwiseNandInstruction> = true;

#define X(name, op) \
    void \
    Core::invoke(const iris::Arithmetic ## name ## Instruction & s) { \
        using K = std::decay_t<decltype(s)>; \
        auto [ dest, src0, src1 ] = s.arguments(); \
        auto result = (getRegisterValue(src0) op getRegisterValue(src1)); \
        if constexpr (NotTheResult<K>) { \
            result = ~result; \
        } \
        setRegisterValue(dest, result); \
    }

X(BitwiseNor, |);
X(BitwiseNand, &);
X(BitwiseAnd, &);
X(BitwiseOr, |);
X(BitwiseXor, ^);
#undef X
void
Core::invoke(const iris::BranchRegisterAndLinkInstruction& s) {
    auto [ address, link ] = s.arguments();
    setRegisterValue(link, _ip.get() + 1);
    _ip.put(getRegisterValue(address));
    _advanceIP = false;
}

void
Core::invoke(const iris::BranchConditionalRegisterInstruction& s) {
    if (auto [ dest, cond ] = s.arguments(); getRegisterValue<bool>(cond)) {
        _ip.put(getRegisterValue<Word>(dest));
        _advanceIP = false;
    }
}
void
Core::invoke(const iris::BranchConditionalRelativeImmediateInstruction& s) {
    if (auto [ cond, offset ] = s.arguments(); getRegisterValue<bool>(cond)) {
        _ip.put(_ip.get<SignedWord>() + offset);
        _advanceIP = false;
    }
}

void
Core::invoke(const iris::MemoryIOStoreImmediateValueInstruction& s) {
    auto [ dest, imm16 ] = s.arguments();
    storeIO(dest, imm16);
}

void
Core::invoke(const iris::MemoryIOLoadWithOffsetInstruction& s) {
    auto [ dest, addr, offset ] = s.arguments();
    setRegisterValue(dest, loadIO(addr, offset));
}
void
Core::invoke(const iris::MemoryIOStoreWithOffsetInstruction& s) {
    auto [ dest, value, offset ] = s.arguments();
    storeIO(dest, value, offset);
}
} // end namespace iris
