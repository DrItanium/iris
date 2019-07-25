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
DoubleRegister::makePair(RegisterBank& bank, RegisterIndex lower, RegisterIndex upper) noexcept {
    if constexpr (!std::is_same_v<Byte, std::underlying_type_t<RegisterIndex>>) {
        constexpr RegisterIndex registerBankMask = RegisterIndex(0xFF);
        return DoubleRegister(bank[Byte(lower & registerBankMask)], bank[Byte(upper & registerBankMask)]);
    } else {
        return DoubleRegister(bank[Byte(lower)], bank[Byte(upper)]);
    }
}

const DoubleRegister
DoubleRegister::makePair(const RegisterBank& bank, RegisterIndex lower, RegisterIndex upper) noexcept {
    if constexpr (!std::is_same_v<Byte, std::underlying_type_t<RegisterIndex>>) {
        constexpr RegisterIndex registerBankMask = RegisterIndex(0xFF);
        return DoubleRegister(bank[Byte(lower & registerBankMask)], bank[Byte(upper & registerBankMask)]);
    } else {
        return DoubleRegister(bank[Byte(lower)], bank[Byte(upper)]);
    }
}

DoubleRegister
DoubleRegister::makePair(RegisterBank& bank, RegisterIndex lower) noexcept {
    auto conv = static_cast<Byte>(lower);
    return makePair(bank, lower, static_cast<RegisterIndex>(conv + 1));
}
const DoubleRegister
DoubleRegister::makePair(const RegisterBank& bank, RegisterIndex lower) noexcept {
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

DestinationRegister
Core::getDestinationRegister(RegisterIndex idx) noexcept {
    return _regs[static_cast<Byte>(idx)];
}
SourceRegister
Core::getSourceRegister(RegisterIndex idx) const noexcept {
    return _regs[static_cast<Byte>(idx)];
}
DoubleRegister
Core::getDoubleRegister(RegisterIndex lower, RegisterIndex upper) noexcept {
    return DoubleRegister::makePair(_regs, lower, upper);
}

const DoubleRegister
Core::getDoubleRegister(RegisterIndex lower, RegisterIndex upper) const noexcept {
    return DoubleRegister::makePair(_regs, lower, upper);
}

void
Core::invoke(const iris::ArithmeticErrorFormat&) {
    throw "Error instruction!";
}

void
Core::invoke(const iris::ArithmeticHalveFormat& s) {
    auto [dest, src] = s.arguments();
    setRegisterValue(dest, getRegisterValue(src) / 2);
}
void
Core::invoke(const iris::ArithmeticDoubleFormat & s) {
    auto [dest, src] = s.arguments();
    setRegisterValue(dest, getRegisterValue(src) * 2);
}
void
Core::invoke(const iris::ArithmeticIncrementFormat& s) {
    if (auto [dest, src] = s.arguments(); dest == src) {
        incrementRegister(dest);
    } else {
        setRegisterValue(dest, getRegisterValue(src) + 1);
    }
}
void
Core::invoke(const iris::ArithmeticDecrementFormat& s) {
    if (auto [dest, src] = s.arguments(); dest == src) {
        decrementRegister(dest);
    } else {
        setRegisterValue(dest, getRegisterValue(src) - 1);
    }
}
void
Core::invoke(const iris::MemoryCopyRegisterFormat& s) {
    if (auto [dest, src] = s.arguments(); dest != src) {
        setRegisterValue(dest, getRegisterValue(src));
    }
}
void
Core::invoke(const iris::MemorySwapRegistersFormat& s) {
    if (auto [ar, br] = s.arguments(); ar != br) {
        auto aValue = getRegisterValue<Word>(ar);
        setRegisterValue(ar, getRegisterValue(br));
        setRegisterValue(br, aValue);
    } 
}

void
Core::invoke(const iris::MemoryAssignRegisterImmediateFormat& s) {
    auto [dest, imm16] = s.arguments();
    setRegisterValue(dest, imm16);
}

void
Core::invoke(const iris::MemoryCodeLoadFormat& s) {
    // CodeLoad LowerRegister, UpperRegister <= AddressRegister 
    auto [lower, upper, addr] = s.arguments();
    setDoubleRegisterValue(lower, upper, _code[getRegisterValue(addr)]);
}
void
Core::invoke(const iris::MemoryCodeLoadAndDecrementFormat& s) {
    // CodeLoad LowerRegister, UpperRegister <= AddressRegister 
    auto [lower, upper, addr] = s.arguments();
    setDoubleRegisterValue(lower, upper, _code[getRegisterValue(addr)]);
    decrementRegister(addr);
}
void
Core::invoke(const iris::MemoryCodeLoadAndIncrementFormat& s) {
    // CodeLoad LowerRegister, UpperRegister <= AddressRegister 
    auto [lower, upper, addr] = s.arguments();
    setDoubleRegisterValue(lower, upper, _code[getRegisterValue(addr)]);
    incrementRegister(addr);
}
void
Core::invoke(const iris::MemoryCodeStoreFormat& s) {
    // CodeStore AddressRegister <= LowerRegister, UpperRegister
    auto [addr, lower, upper ] = s.arguments();
    _code[getRegisterValue(addr)] = getDoubleRegisterValue(lower, upper);
}

void
Core::invoke(const iris::MemoryCodeStoreAndDecrementFormat& s) {
    // CodeStore AddressRegister <= LowerRegister, UpperRegister
    auto [addr, lower, upper ] = s.arguments();
    _code[getRegisterValue(addr)] = getDoubleRegisterValue(lower, upper);
    decrementRegister(s.getFirst());
}
void
Core::invoke(const iris::MemoryCodeStoreAndIncrementFormat& s) {
    // CodeStore AddressRegister <= LowerRegister, UpperRegister
    auto [addr, lower, upper ] = s.arguments();
    _code[getRegisterValue(addr)] = getDoubleRegisterValue(lower, upper);
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
    decrementRegister(stackPointer);
    _stack[getRegisterValue(stackPointer)] = getRegisterValue(src);
}

void
Core::invoke(const iris::MemoryStackPushImmediateValueFormat& s) {
    auto [stackPointer, imm16] = s.arguments();
    incrementRegister(stackPointer);
    _stack[getRegisterValue(stackPointer)] = imm16;
}


void
Core::invoke(const iris::BranchSelectFormat& s) {
    // BranchSelect ConditionalRegister TrueAddress FalseAddress
    auto [ cond, onTrue, onFalse] = s.arguments();
    _ip.put(getRegisterValue(getRegisterValue<bool>(cond) ? onTrue : onFalse));
    _advanceIP = false;
}
void
Core::invoke(const iris::BranchImmediateFormat& s) {
    // BranchImmediate imm16
    _ip.put(s.getFirst());
    _advanceIP = false;
}
void
Core::invoke(const iris::CompareEqualsFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, getSourceRegister(src0) == getSourceRegister(src1));
}
void
Core::invoke(const iris::CompareNotEqualsFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, getSourceRegister(src0) != getSourceRegister(src1));
}
#define Y(name, op, kind) \
    void \
    Core::invoke(const iris:: Compare ## name ## kind ## Format & s) { \
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
    Core::invoke(const iris:: Compare ## name ## kind ## Immediate8Format & s) { \
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
Core::invoke(const iris::CompareEqualsImmediate8Format& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, getRegisterValue(src0) == static_cast<Word>(src1));
}
void
Core::invoke(const iris::CompareNotEqualsImmediate8Format& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, getRegisterValue(src0) != static_cast<Word>(src1));
}

#define X(op, act) \
    void \
    Core::invoke ( const iris:: Arithmetic2 ## op ## SignedImmediateFormat & s) { \
        auto [ dest, src0, s8 ] = s.arguments(); \
        setRegisterValue(dest, getRegisterValue<SignedWord>(src0) act static_cast<SignedWord>(s8)); \
    } \
    void \
    Core::invoke ( const iris:: Arithmetic2 ## op ## UnsignedImmediateFormat & s) { \
        auto [ dest, src0, u8 ] = s.arguments(); \
        setRegisterValue(dest, getRegisterValue<Word>(src0) act static_cast<Word>(u8)); \
    }
X(Multiply, *);
X(Add, +);
X(Subtract, -);
X(ShiftLeft, <<);
X(ShiftRight, >>);
#undef X

void
Core::invoke(const iris::Arithmetic2DivideSignedImmediateFormat& s) {
    if (auto [ dest, src0, s8 ] = s.arguments(); s8 == 0) {
        throw "Divide by zero!";
    } else {
        setRegisterValue(dest, getRegisterValue<SignedWord>(src0) / static_cast<SignedWord>(s8));
    }
}
void
Core::invoke(const iris::Arithmetic2DivideUnsignedImmediateFormat& s) {
    if (auto [ dest, src0, u8 ] = s.arguments(); u8 == 0) {
        throw "Divide by zero!";
    } else {
        setRegisterValue(dest, getRegisterValue<Word>(src0) / static_cast<Word>(u8));
    }
}
void
Core::invoke(const iris::Arithmetic2RemainderSignedImmediateFormat& s) {
    if (auto [ dest, src0, s8 ] = s.arguments(); s8 == 0) {
        throw "Remainder by zero!";
    } else {
        setRegisterValue(dest, getRegisterValue<SignedWord>(src0) % static_cast<SignedWord>(s8));
    }
}
void
Core::invoke(const iris::Arithmetic2RemainderUnsignedImmediateFormat& s) {
    if (auto [ dest, src0, u8 ] = s.arguments(); u8 == 0) {
        throw "Remainder by zero!";
    } else {
        setRegisterValue(dest, getRegisterValue<Word>(src0) % static_cast<Word>(u8));
    }
}

void
Core::invoke(const iris::Arithmetic2BitwiseOrImmediate8Format& s) {
    if (auto [ dest, src0, imm8 ] = s.arguments(); imm8 == 0) {
        // oring with zero acts as a move
        setRegisterValue(dest, getRegisterValue(src0));
    } else {
        setRegisterValue(dest, getRegisterValue(src0) | static_cast<Word>(imm8));
    }
}
void
Core::invoke(const iris::Arithmetic2BitwiseAndImmediate8Format& s) {
    if (auto [ dest, src0, imm8 ] = s.arguments(); imm8 == 0) {
        // anding with zero acts as a clear out
        setRegisterValue(dest, 0);
    } else {
        setRegisterValue(dest, getRegisterValue(src0) & static_cast<Word>(imm8));
    }
}
void
Core::invoke(const iris::Arithmetic2BitwiseXorImmediate8Format& s) {
    auto [ dest, src0, imm8 ] = s.arguments();
    setRegisterValue(dest, getRegisterValue(src0) ^ static_cast<Word>(imm8));
}
void
Core::invoke(const iris::Arithmetic2BitwiseNandImmediate8Format& s) {
    auto [ dest, src0, imm8 ] = s.arguments();
    setRegisterValue(dest, ~(getRegisterValue(src0) & static_cast<Word>(imm8)));
}
void
Core::invoke(const iris::Arithmetic2BitwiseNorImmediate8Format& s) {
    auto [ dest, src0, imm8 ] = s.arguments();
    setRegisterValue(dest, ~(getRegisterValue(src0) | static_cast<Word>(imm8)));
}
void
Core::invoke(const iris::Arithmetic2BitwiseOrImmediate16Format& s) {
    auto [ dest, imm16 ] = s.arguments();
    setRegisterValue(dest, getRegisterValue(dest) | imm16);
}
void
Core::invoke(const iris::Arithmetic2BitwiseAndImmediate16Format& s) {
    auto [ dest, imm16 ] = s.arguments();
    setRegisterValue(dest, getRegisterValue(dest) & imm16);
}
void
Core::invoke(const iris::Arithmetic2BitwiseXorImmediate16Format& s) {
    auto [ dest, imm16 ] = s.arguments();
    setRegisterValue(dest, getRegisterValue(dest) ^ imm16);
}
void
Core::invoke(const iris::Arithmetic2BitwiseNorImmediate16Format& s) {
    auto [ dest, imm16 ] = s.arguments();
    setRegisterValue(dest, ~(getRegisterValue(dest) | imm16));
}
void
Core::invoke(const iris::Arithmetic2BitwiseNandImmediate16Format& s) {
    auto [ dest, imm16 ] = s.arguments();
    setRegisterValue(dest, ~(getRegisterValue(dest) & imm16));
}

void
Core::invoke(const iris::ArithmeticMaxSignedFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, std::max(getRegisterValue<SignedWord>(src0), getRegisterValue<SignedWord>(src1)));
}
void
Core::invoke(const iris::ArithmeticMaxUnsignedFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, std::max(getRegisterValue<Word>(src0), getRegisterValue<Word>(src1)));
}
void
Core::invoke(const iris::ArithmeticMinSignedFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, std::min(getRegisterValue<SignedWord>(src0), getRegisterValue<SignedWord>(src1)));
}
void
Core::invoke(const iris::ArithmeticMinUnsignedFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, std::min(getRegisterValue<Word>(src0), getRegisterValue<Word>(src1)));
}
void
Core::invoke(const iris::Arithmetic2MaxSignedImmediateFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, std::max(getRegisterValue<SignedWord>(src0), static_cast<SignedWord>(src1)));
}
void
Core::invoke(const iris::Arithmetic2MaxUnsignedImmediateFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, std::max(getRegisterValue<Word>(src0), static_cast<Word>(src1)));
}
void
Core::invoke(const iris::Arithmetic2MinSignedImmediateFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, std::min(getRegisterValue<SignedWord>(src0), static_cast<SignedWord>(src1)));
}
void
Core::invoke(const iris::Arithmetic2MinUnsignedImmediateFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    setRegisterValue(dest, std::min(getRegisterValue<Word>(src0), static_cast<Word>(src1)));
}

void
Core::invoke(const iris::Arithmetic2MinImmediate16Format& s) {
    auto [ dest, imm16 ] = s.arguments();
    setRegisterValue(dest, std::min(getRegisterValue(dest), imm16));
}

void
Core::invoke(const iris::Arithmetic2MaxImmediate16Format& s) {
    auto [ dest, imm16 ] = s.arguments();
    setRegisterValue(dest, std::max(getRegisterValue(dest), imm16));
}

void
Core::invoke(const iris::BranchConditionalImmediateFormat& s) {
    auto [ cond, to ] = s.arguments();
    if (getRegisterValue<bool>(cond)) {
        _ip.put(to);
        _advanceIP = false;
    }
}

void
Core::invoke(const iris::BranchRegisterAndLinkFormat& s) {
    auto [ address, link ] = s.arguments();
    setRegisterValue(link, _ip.get() + 1);
    _ip.put(getRegisterValue(address));
    _advanceIP = false;
}
void
Core::invoke(const iris::BranchImmediateAndLinkFormat& s) {
    auto [ link, imm16 ] = s.arguments();
    setRegisterValue(link, _ip.get() + 1);
    _ip.put(imm16);
    _advanceIP = false;
}
void
Core::invoke(const iris::BranchConditionalRegisterAndLinkFormat& s) {
    auto [ dest, cond, link ] = s.arguments();
    if (getRegisterValue<bool>(cond)) {
        setRegisterValue(link, _ip.get() + 1);
        _ip.put(getRegisterValue(dest));
        _advanceIP = false;
    }
}

void
Core::invoke(const iris::MemoryDataLoadWithSignedOffsetFormat& s) {
    auto [ dest, loc, offset ] = s.arguments();
    setRegisterValue(dest, _data[static_cast<Word>(getRegisterValue<SignedWord>(loc) + static_cast<SignedWord>(offset))]);
}
void
Core::invoke(const iris::MemoryDataLoadWithUnsignedOffsetFormat& s) {
    auto [ dest, loc, offset ] = s.arguments();
    setRegisterValue(dest, _data[getRegisterValue<Word>(loc) + static_cast<Word>(offset)]);
}
void
Core::invoke(const iris::MemoryDataStoreImmediateValueFormat& s) {
    auto [ addr, imm16 ] = s.arguments();
    _data[getRegisterValue(addr)] = imm16;
}

void
Core::invoke(const iris::MemoryDataStoreWithSignedOffsetFormat& s) {
    auto [ dest, value, offset ] = s.arguments();
    auto address = static_cast<Word>(getRegisterValue<SignedWord>(dest) + static_cast<SignedWord>(offset));
    _data[address] = getRegisterValue(value);
}
void
Core::invoke(const iris::MemoryDataStoreWithUnsignedOffsetFormat& s) {
    auto [ dest, value, offset ] = s.arguments();
    auto address = getRegisterValue<Word>(dest) + static_cast<Word>(offset);
    _data[address] = getRegisterValue(value);
}
#define Y(name, op, suffix, types) \
    void \
    Core::invoke( const iris:: Arithmetic ## name ## suffix & s ) { \
        auto [ dest, src0, src1 ] = s.arguments(); \
        setRegisterValue<types>(dest, getRegisterValue<types>(src0) op getRegisterValue<types>(src1)); \
    }
#define X(name, op) \
    Y(name, op, SignedFormat, SignedWord); \
    Y(name, op, UnsignedFormat, Word)
X(Add, +);
X(Subtract, -);
X(Multiply, *);
X(ShiftRight, >>);
X(ShiftLeft, <<);
#undef X
#undef Y

void 
Core::invoke(const iris::ArithmeticDivideSignedFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    if (auto denominator = getRegisterValue<SignedWord>(src1); denominator == 0) {
        throw "Divide by zero!";
    } else {
        setRegisterValue<SignedWord>(dest, getRegisterValue<SignedWord>(src0) / denominator);
    }
}
void 
Core::invoke(const iris::ArithmeticDivideUnsignedFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    if (auto denominator = getRegisterValue<UnsignedWord>(src1); denominator == 0) {
        throw "Divide by zero!";
    } else {
        setRegisterValue<UnsignedWord>(dest, getRegisterValue<UnsignedWord>(src0) / denominator);
    }
}

void 
Core::invoke(const iris::ArithmeticRemainderSignedFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    if (auto denominator = getRegisterValue<SignedWord>(src1); denominator == 0) {
        throw "Remainder by zero!";
    } else {
        setRegisterValue<SignedWord>(dest, getRegisterValue<SignedWord>(src0) % denominator);
    }
}
void 
Core::invoke(const iris::ArithmeticRemainderUnsignedFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    if (auto denominator = getRegisterValue<UnsignedWord>(src1); denominator == 0) {
        throw "Remainder by zero!";
    } else {
        setRegisterValue<UnsignedWord>(dest, getRegisterValue<UnsignedWord>(src0) % denominator);
    }
}

void
Core::invoke(const iris::BranchRegisterFormat& s) {
    auto [ dest ] = s.arguments();
    _ip.put(getRegisterValue(dest));
    _advanceIP = false;
}
#define Y(name, op) \
    void \
    Core::invoke(const iris::Arithmetic ## name ## Format & s) { \
        auto [ dest, src0, src1 ] = s.arguments(); \
        setRegisterValue(dest, ~(getRegisterValue(src0) op getRegisterValue(src1))); \
    } \
    void \
    Core::invoke(const iris::DoubleRegisterDouble ## name ## Format & s) { \
        auto [ dest, src0, src1 ] = s.arguments(); \
        setDoubleRegisterValue(dest, ~(getDoubleRegisterValue(src0) op getDoubleRegisterValue(src1))); \
    }
#define X(name, op) \
    void \
    Core::invoke(const iris::Arithmetic ## name ## Format & s) { \
        auto [ dest, src0, src1 ] = s.arguments(); \
        setRegisterValue(dest, (getRegisterValue(src0) op getRegisterValue(src1))); \
    } \
    void \
    Core::invoke(const iris::DoubleRegisterDouble ## name ## Format & s) { \
        auto [ dest, src0, src1 ] = s.arguments(); \
        setDoubleRegisterValue(dest, (getDoubleRegisterValue(src0) op getDoubleRegisterValue(src1))); \
    }
#define Z(name, op) \
    void \
    Core::invoke(const iris::Arithmetic ## name ## Format & s) { \
        auto [ dest, src ] = s.arguments(); \
        setRegisterValue(dest, op(getRegisterValue(src))); \
    } \
    void \
    Core::invoke(const iris::DoubleRegisterDouble ## name ## Format & s) { \
        auto [ dest, src ] = s.arguments(); \
        setDoubleRegisterValue(dest, op(getDoubleRegisterValue(src))); \
    }

Y(BitwiseNor, |);
Y(BitwiseNand, &);
X(BitwiseAnd, &);
X(BitwiseOr, |);
X(BitwiseXor, ^);
Z(BitwiseNot, ~);
#undef Y
#undef X
#undef Z
void
Core::invoke(const iris::BranchRelativeImmediateFormat& s) {
    auto [ s8 ] = s.arguments();
    _ip.put<SignedWord>(_ip.get<SignedWord>() + s8);
    _advanceIP = false;
}

void
Core::invoke(const iris::BranchConditionalRegisterFormat& s) {
    if (auto [ dest, cond ] = s.arguments(); getRegisterValue<bool>(cond)) {
        _ip.put(getRegisterValue(dest));
        _advanceIP = false;
    }
}
void
Core::invoke(const iris::BranchConditionalRelativeImmediateFormat& s) {
    auto [ cond, offset ] = s.arguments();
    if (getRegisterValue<bool>(cond)) {
        _ip.put(_ip.get<SignedWord>() + offset);
        _advanceIP = false;
    }
}
void
Core::invoke(const iris::MemoryMoveToIPFormat& s) {
    auto [ src ] = s.arguments();
    _ip.put(getRegisterValue(src));
}
void
Core::invoke(const iris::MemoryMoveFromIPFormat& s) {
    auto [ dest ] = s.arguments();
    setRegisterValue(dest, _ip.get());
}
void
Core::invoke(const iris::MemoryAssignRegisterSignedImmediateFormat& s) {
    auto [ dest, s16 ] = s.arguments();
    setRegisterValue(dest, s16);
}

void
Core::invoke(const iris::MemoryIOStoreImmediateValueFormat& s) {
    auto [ dest, imm16 ] = s.arguments();
    _io.store(getRegisterValue<Address>(dest), imm16);
}

void
Core::invoke(const iris::MemoryIOLoadWithSignedOffsetFormat& s) {
    auto [ dest, addr, offset ] = s.arguments();
    setRegisterValue(dest, _io.load(static_cast<Address>( getRegisterValue<SignedWord>(addr) + offset )));
}

void
Core::invoke(const iris::MemoryIOStoreWithSignedOffsetFormat& s) {
    auto [ dest, value, offset ] = s.arguments();
    _io.store(getRegisterValue(dest) + offset, getRegisterValue(value));
}

void
Core::invoke(const iris::MemoryIOLoadWithUnsignedOffsetFormat& s) {
    auto [ dest, addr, offset ] = s.arguments();
    setRegisterValue(dest, _io.load(getRegisterValue(addr) + offset));
}
void
Core::invoke(const iris::MemoryIOStoreWithUnsignedOffsetFormat& s) {
    auto [ dest, value, offset ] = s.arguments();
    _io.store(getRegisterValue(dest) + offset, getRegisterValue(value));
}

Word
IOMemoryBank::load(Address address) {
    return _storage[address].read(_core);
}
void
IOMemoryBank::store(Address address, Word value) {
    _storage[address].write(_core, value);
}
void
IOMemoryBank::mapIntoMemory(Address address, MMIOReadFunction read, MMIOWriteFunction write) {
    _storage[address] = LambdaMMIOEntry(read, write);
}
void
IOMemoryBank::mapIntoMemory(Address address, MMIOEntry& entry) {
    _storage[address] = CaptiveMMIOEntry(entry);
}
void
IOMemoryBank::mapIntoMemory(Address addr, MMIOReadFunction r) {
    mapIntoMemory(addr, r, LambdaMMIOEntry::illegalWriteError);
}
void
IOMemoryBank::mapIntoMemory(Address addr, MMIOWriteFunction w) {
    mapIntoMemory(addr, LambdaMMIOEntry::illegalReadError, w);
}
void
IOMemoryBank::mapIntoMemory(Address addr, std::tuple<MMIOReadFunction, MMIOWriteFunction> t) {
    auto [ r, w ] = t;
    mapIntoMemory(addr, r, w);
}
#define Y(name, op, kind) \
    void \
    Core::invoke(const iris::DoubleRegisterDouble ## name ## kind ## Format & s) { \
        using T = kind ## DoubleWord ; \
        auto [ dest, src0, src1 ] = s.arguments(); \
        setDoubleRegisterValue<T>(dest, \
                getDoubleRegisterValue<T>(src0) op \
                getDoubleRegisterValue<T>(src1)); \
    }
#define X(name, op) \
    Y(name, op, Signed) \
    Y(name, op, Unsigned)
X(Add, +); 
X(Subtract, -);
X(Multiply, *);
X(ShiftLeft, <<);
X(ShiftRight, >>);
#undef Y
#define Y(name, op, kind) \
    void \
    Core::invoke(const iris::DoubleRegisterDouble ## name ## kind ## Format & s) { \
        using T = kind ## DoubleWord ; \
        auto [ dest, src0, src1 ] = s.arguments(); \
        setDoubleRegisterValue<T>(dest, op ( \
                getDoubleRegisterValue<T>(src0) , \
                getDoubleRegisterValue<T>(src1))); \
    }
X(Min, std::min);
X(Max, std::max);
#undef Y
#define Y(name, op, kind) \
    void \
    Core::invoke(const iris::DoubleRegisterDouble ## name ## kind ## Format & s) { \
        using T = kind ## DoubleWord ; \
        auto [ dest, src0, src1 ] = s.arguments(); \
        if (auto denominator = getDoubleRegisterValue < T > (src1); denominator != 0) { \
        setDoubleRegisterValue<T>(dest, \
                getDoubleRegisterValue<T>(src0) op \
                getDoubleRegisterValue<T>(src1)); \
        } else { \
            throw "Divide by zero!"; \
        } \
    }
X(Divide, /);
X(Remainder, %);
#undef X

void
Core::cycle() {
    // load an instruction from the current instruction pointer
    invoke(_code[_ip.get()]);
    if (_advanceIP) {
        ++_ip;
    }
    _advanceIP = true;
}

void
Core::run() {
    _executing = true;
    do {
        try {
            cycle();
        } catch (...) {
            /// @todo implement logic to handle edge cases such as divide by zero and other such handling
        }
    } while (_executing);
}

void
MMIOEntry::write(Core&, Word) {
    throw "Illegal IO write";
}

Word
MMIOEntry::read(Core&) {
    throw "Illegal IO read!";
    return 0;
}

void
LambdaMMIOEntry::write(Core& c, Word value) {
    _write(c, value);
}

Word
LambdaMMIOEntry::read(Core& c) {
    return _read(c);
}

void
CaptiveMMIOEntry::write(Core& c, Word value) {
    _other.write(c, value);
}

Word
CaptiveMMIOEntry::read(Core& c) {
    return _other.read(c);
}

LambdaMMIOEntry::LambdaMMIOEntry(MMIOReadFunction read, MMIOWriteFunction write) : _read(read), _write(write) { }
CaptiveMMIOEntry::CaptiveMMIOEntry(MMIOEntry& capture) : _other(capture) { }

void
Core::terminateCycle() {
    _executing = false;
}

void
Core::installIOMemoryMap(const IOMemoryMap& map) {
    _io.installMemoryMap(map);
}
void
IOMemoryBank::installMemoryMap(const IOMemoryMap& map) {
    for (const auto& entry : map) {
        auto [ addr, value ] = entry;
        std::visit([this, addr](auto&& value) { mapIntoMemory(addr, value); }, value);
    }
}
void
IOMemoryBank::mapIntoMemory(Address baseAddress, ComplexMemoryMapping fn) {
    fn(*this, baseAddress);
}

void
Core::terminateCore(Core& c, Word code) {
    c.terminateCycle();
    c._terminateCell = code;
}
Word
Core::readTerminateCell(Core& c) {
    return c._terminateCell;
}

} // end namespace iris
