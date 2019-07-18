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

namespace iris {

void
DoubleRegister::put(Word lower, Word upper) noexcept {
    _lower.put(lower);
    _upper.put(upper);
}

DoubleRegister
makePair(RegisterBank& bank, RegisterIndex lower, RegisterIndex upper) noexcept {
    if constexpr (!std::is_same_v<Byte, std::underlying_type_t<RegisterIndex>>) {
        constexpr RegisterIndex registerBankMask = RegisterIndex(0xFF);
        return DoubleRegister(bank[Byte(lower & registerBankMask)], bank[Byte(upper & registerBankMask)]);
    } else {
        return DoubleRegister(bank[Byte(lower)], bank[Byte(upper)]);
    }
}

DoubleRegister
makePair(RegisterBank& bank, RegisterIndex lower) noexcept {
    auto conv = static_cast<Byte>(lower);
    return makePair(bank, lower, static_cast<RegisterIndex>(conv + 1));
}

void 
Core::invoke(DoubleWord ibits) {
    Instruction inst(ibits);
    if (auto operation = decodeInstruction(inst); operation) {
        std::visit([this](auto&& op) { 
                    using K = std::decay_t<decltype(op)>;
                    if constexpr (std::is_same_v<std::monostate, K>) {
                        throw "Unimplemented operation found!";
                    } else {
                        invoke(op);
                    }
                }, *operation);
    } else {
        throw "Bad operation!";
    }
}

void
Core::invoke(const iris::ArithmeticErrorFormat&) {
    throw "Error instruction!";
}

void
Core::invoke(const iris::ArithmeticHalveFormat& s) {
    auto [dest, src] = s.arguments();
    setRegister(dest, getRegisterValue(src) / 2);
}
void
Core::invoke(const iris::ArithmeticDoubleFormat & s) {
    auto [dest, src] = s.arguments();
    setRegister(dest, getRegisterValue(src) * 2);
}
void
Core::invoke(const iris::ArithmeticIncrementFormat& s) {
    if (auto [dest, src] = s.arguments(); dest == src) {
        incrementRegister(dest);
    } else {
        setRegister(dest, getRegisterValue(src) + 1);
    }
}
void
Core::invoke(const iris::ArithmeticDecrementFormat& s) {
    if (auto [dest, src] = s.arguments(); dest == src) {
        decrementRegister(dest);
    } else {
        setRegister(dest, getRegisterValue(src) - 1);
    }
}
void
Core::invoke(const iris::MemoryCopyRegisterFormat& s) {
    auto [dest, src] = s.arguments();
    setRegister(dest, getRegisterValue(src));
}
void
Core::invoke(const iris::MemorySwapRegistersFormat& s) {
    auto [ar, br] = s.arguments();
    auto aValue = getRegisterValue<Word>(ar);
    setRegister(ar, getRegisterValue(br));
    setRegister(br, aValue);
}

void
Core::invoke(const iris::MemoryAssignRegisterImmediateFormat& s) {
    auto [dest, imm16] = s.arguments();
    setRegister(dest, imm16);
}

void
Core::invoke(const iris::MemoryCodeLoadFormat& s) {
    // CodeLoad LowerRegister, UpperRegister <= AddressRegister 
    auto [lower, upper, addr] = s.arguments();
    makePair(_regs, lower, upper).put(_code[getRegisterValue(addr)]);
}
void
Core::invoke(const iris::MemoryCodeLoadAndDecrementFormat& s) {
    // CodeLoad LowerRegister, UpperRegister <= AddressRegister 
    auto [lower, upper, addr] = s.arguments();
    makePair(_regs, lower, upper).put(_code[getRegisterValue(addr)]);
    decrementRegister(addr);
}
void
Core::invoke(const iris::MemoryCodeLoadAndIncrementFormat& s) {
    // CodeLoad LowerRegister, UpperRegister <= AddressRegister 
    auto [lower, upper, addr] = s.arguments();
    makePair(_regs, lower, upper).put(_code[getRegisterValue(addr)]);
    incrementRegister(addr);
}
void
Core::invoke(const iris::MemoryCodeStoreFormat& s) {
    // CodeStore AddressRegister <= LowerRegister, UpperRegister
    auto [addr, lower, upper ] = s.arguments();
    _code[getRegisterValue(addr)] = makePair(_regs, lower, upper).get();
}

void
Core::invoke(const iris::MemoryCodeStoreAndDecrementFormat& s) {
    // CodeStore AddressRegister <= LowerRegister, UpperRegister
    auto [addr, lower, upper ] = s.arguments();
    _code[getRegisterValue(addr)] = makePair(_regs, lower, upper).get();
    decrementRegister(s.getFirst());
}
void
Core::invoke(const iris::MemoryCodeStoreAndIncrementFormat& s) {
    // CodeStore AddressRegister <= LowerRegister, UpperRegister
    auto [addr, lower, upper ] = s.arguments();
    _code[getRegisterValue(addr)] = makePair(_regs, lower, upper).get();
    incrementRegister(addr);
}

void
Core::invoke(const iris::MemoryStackPopFormat& s) {
    // so stack grows downward
    // pops grow towards 0xFFFF
    // StackPop StackPointerRegister DestinationRegister
    auto [stackPointer, destination] = s.arguments();
    setRegisterValue(destination, _stack[getRegisterValue(stackPointer)]);
    incrementRegister(stackPointer);
}
void
Core::invoke(const iris::MemoryStackPushFormat& s) {
    // stack grows downward
    // StackPush StackPointerRegister SourceRegister
    auto [stackPointer, src] = s.arguments();
    incrementRegister(stackPointer);
    _stack[getRegisterValue(stackPointer)] = getRegisterValue(src);
}

void
Core::invoke(const iris::MemoryStackPushImmediateValueFormat& s) {
    auto [stackPointer, imm16] = s.arguments();
    incrementRegister(stackPointer);
    _stack[getRegisterValue(stackPointer)] = imm16;
}



} // end namespace iris
