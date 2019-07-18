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
    setRegister(s.getFirst(), getRegisterValue(s.getSecond()) / 2);
}
void
Core::invoke(const iris::ArithmeticDoubleFormat & s) {
    setRegister(s.getFirst(), getRegisterValue(s.getSecond()) * 2);
}
void
Core::invoke(const iris::ArithmeticIncrementFormat& s) {
    if (DestinationRegister dest = unpackDestination(s.getFirst()); s.getFirst() == s.getSecond()) {
        ++dest;
    } else {
        dest.put<Word>(unpackSourceRegister(s.getSecond()).get<Word>() + 1);
    }
}
void
Core::invoke(const iris::ArithmeticDecrementFormat& s) {
    if (DestinationRegister dest = unpackDestination(s.getFirst()); s.getFirst() == s.getSecond()) {
        --dest;
    } else {
        dest.put<Word>(unpackSourceRegister(s.getSecond()).get<Word>() - 1);
    }
}
void
Core::invoke(const iris::MemoryCopyRegisterFormat& s) {
    setRegister(s.getFirst(), getRegisterValue(s.getSecond()));
}
void
Core::invoke(const iris::MemorySwapRegistersFormat& s) {
    auto firstValue = getRegisterValue<Word>(s.getFirst());
    setRegister(s.getFirst(), getRegisterValue(s.getSecond()));
    setRegister(s.getSecond(), firstValue);
}

void
Core::invoke(const iris::MemoryAssignRegisterImmediateFormat& s) {
    setRegister(s.getFirst(), s.getSecond());
}

void
Core::invoke(const iris::MemoryCodeLoadFormat& s) {
    // CodeLoad LowerRegister, UpperRegister <= AddressRegister 
    makePair(_regs, s.getFirst(), s.getSecond()).put(_code[getRegisterValue(s.getThird())]);
}
void
Core::invoke(const iris::MemoryCodeLoadAndDecrementFormat& s) {
    // CodeLoad LowerRegister, UpperRegister <= AddressRegister 
    makePair(_regs, s.getFirst(), s.getSecond()).put(_code[getRegisterValue(s.getThird())]);
    decrementRegister(s.getThird());
}
void
Core::invoke(const iris::MemoryCodeLoadAndIncrementFormat& s) {
    // CodeLoad LowerRegister, UpperRegister <= AddressRegister 
    makePair(_regs, s.getFirst(), s.getSecond()).put(_code[getRegisterValue(s.getThird())]);
    incrementRegister(s.getThird());
}
void
Core::invoke(const iris::MemoryCodeStoreFormat& s) {
    // CodeStore AddressRegister <= LowerRegister, UpperRegister
    _code[getRegisterValue(s.getFirst())] = makePair(_regs, s.getSecond(), s.getThird()).get();
}

void
Core::invoke(const iris::MemoryCodeStoreAndDecrementFormat& s) {
    // CodeStore AddressRegister <= LowerRegister, UpperRegister
    _code[getRegisterValue(s.getFirst())] = makePair(_regs, s.getSecond(), s.getThird()).get();
    decrementRegister(s.getFirst());
}
void
Core::invoke(const iris::MemoryCodeStoreAndIncrementFormat& s) {
    // CodeStore AddressRegister <= LowerRegister, UpperRegister
    _code[getRegisterValue(s.getFirst())] = makePair(_regs, s.getSecond(), s.getThird()).get();
    incrementRegister(s.getFirst());
}


} // end namespace iris
