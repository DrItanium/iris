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
Core::invoke(const iris::ErrorFormat&) {
    throw ErrorInstructionException();
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
Core::invoke(const iris::MemoryCodeLoadWithOffsetFormat& s) {
    // CodeLoad AddressRegister LowerRegister (implied UpperRegister = LowerRegister + 1)
    auto [addr, lower, offset] = s.arguments();
    setDoubleRegisterValue(lower, loadCode(addr, offset));
}
void
Core::invoke(const iris::MemoryCodeLoadAndDecrementFormat& s) {
    // CodeLoad AddressRegister LowerRegister (implied UpperRegister = LowerRegister + 1)
    auto [addr, lower, factor] = s.arguments();
    setDoubleRegisterValue(lower, loadCode(addr));
    decrementRegister(addr, factor);
}
void
Core::invoke(const iris::MemoryCodeLoadAndIncrementFormat& s) {
    // CodeLoad AddressRegister LowerRegister (implied UpperRegister = LowerRegister + 1)
    auto [addr, lower, factor] = s.arguments();
    setDoubleRegisterValue(lower, loadCode(addr));
    incrementRegister(addr, factor);
}
void
Core::invoke(const iris::MemoryCodeStoreWithOffsetFormat& s) {
    // CodeStore AddressRegister <= LowerRegister (upper register implied)
    auto [addr, lower, offset ] = s.arguments();
    storeCode(addr, lower, offset);
}

void
Core::invoke(const iris::MemoryCodeStoreAndDecrementFormat& s) {
    // CodeStore AddressRegister <= LowerRegister Offset ( An add of one is implied so the range is [0,257])
    auto [addr, lower, factor ] = s.arguments();
    storeCode(addr, lower);
    decrementRegister(addr, factor);
}
void
Core::invoke(const iris::MemoryCodeStoreAndIncrementFormat& s) {
    // CodeStore AddressRegister <= LowerRegister (implied UpperRegister)
    auto [addr, lower, factor ] = s.arguments();
    storeCode(addr, lower);
    incrementRegister(addr, factor);
}

void
Core::invoke(const iris::MemoryStackPopFormat& s) {
    // so stack grows downward
    // pops grow towards 0xFFFF
    // StackPop StackPointerRegister DestinationRegister
    auto [stackPointer, destination] = s.arguments();
    setRegisterValue(destination, loadStack(stackPointer));
    incrementRegister(stackPointer);
}
void
Core::invoke(const iris::MemoryStackPushFormat& s) {
    // stack grows downward
    // StackPush StackPointerRegister SourceRegister
    auto [stackPointer, src] = s.arguments();
    decrementRegister(stackPointer);
    storeStack(stackPointer, src);
}

void
Core::invoke(const iris::MemoryStackPushImmediateValueFormat& s) {
    auto [stackPointer, imm16] = s.arguments();
    incrementRegister(stackPointer);
    storeStack(stackPointer, imm16);
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
    Core::invoke ( const iris:: Arithmetic ## op ## SignedImmediateFormat & s) { \
        using T = SignedWord; \
        auto [ dest, src0, s8 ] = s.arguments(); \
        setRegisterValue(dest, getRegisterValue<SignedWord>(src0) act static_cast<T>(s8)); \
    } \
    void \
    Core::invoke ( const iris:: Arithmetic ## op ## UnsignedImmediateFormat & s) { \
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
    Core::invoke ( const iris:: Arithmetic ## op ## UnsignedImmediateFormat & s) { \
        auto [ dest, src0, u8 ] = s.arguments(); \
        setRegisterValue(dest, getRegisterValue<Word>(src0) act static_cast<Word>(u8)); \
    }
X(ShiftLeft, <<);
X(ShiftRight, >>);
#undef X

void
Core::invoke(const iris::ArithmeticDivideSignedImmediateFormat& s) {
    if (auto [ dest, src0, s8 ] = s.arguments(); s8 == 0) {
        throw DivideByZeroException();
    } else {
        setRegisterValue(dest, getRegisterValue<SignedWord>(src0) / static_cast<SignedWord>(s8));
    }
}
void
Core::invoke(const iris::ArithmeticDivideUnsignedImmediateFormat& s) {
    if (auto [ dest, src0, u8 ] = s.arguments(); u8 == 0) {
        throw DivideByZeroException();
    } else {
        setRegisterValue(dest, getRegisterValue<Word>(src0) / static_cast<Word>(u8));
    }
}
void
Core::invoke(const iris::ArithmeticRemainderSignedImmediateFormat& s) {
    if (auto [ dest, src0, s8 ] = s.arguments(); s8 == 0) {
        throw DivideByZeroException();
    } else {
        setRegisterValue(dest, getRegisterValue<SignedWord>(src0) % static_cast<SignedWord>(s8));
    }
}
void
Core::invoke(const iris::ArithmeticRemainderUnsignedImmediateFormat& s) {
    if (auto [ dest, src0, u8 ] = s.arguments(); u8 == 0) {
        throw DivideByZeroException();
    } else {
        setRegisterValue(dest, getRegisterValue<Word>(src0) % static_cast<Word>(u8));
    }
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
Core::invoke(const iris::MemoryDataLoadWithOffsetFormat& s) {
    auto [ dest, loc, offset ] = s.arguments();
    setRegisterValue(dest, loadData(loc, offset));
}
void
Core::invoke(const iris::MemoryDataStoreImmediateValueFormat& s) {
    auto [ addr, imm16 ] = s.arguments();
    storeData(addr, imm16);
}
void
Core::invoke(const iris::MemoryDataStoreWithOffsetFormat& s) {
    auto [ dest, value, offset ] = s.arguments();
    storeData(dest, value, offset);
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
        throw DivideByZeroException();
    } else {
        setRegisterValue<SignedWord>(dest, getRegisterValue<SignedWord>(src0) / denominator);
    }
}
void 
Core::invoke(const iris::ArithmeticDivideUnsignedFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    if (auto denominator = getRegisterValue<UnsignedWord>(src1); denominator == 0) {
        throw DivideByZeroException();
    } else {
        setRegisterValue<UnsignedWord>(dest, getRegisterValue<UnsignedWord>(src0) / denominator);
    }
}

void 
Core::invoke(const iris::ArithmeticRemainderSignedFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    if (auto denominator = getRegisterValue<SignedWord>(src1); denominator == 0) {
        throw DivideByZeroException();
    } else {
        setRegisterValue<SignedWord>(dest, getRegisterValue<SignedWord>(src0) % denominator);
    }
}
void 
Core::invoke(const iris::ArithmeticRemainderUnsignedFormat& s) {
    auto [ dest, src0, src1 ] = s.arguments();
    if (auto denominator = getRegisterValue<UnsignedWord>(src1); denominator == 0) {
        throw DivideByZeroException();
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
    Core::invoke(const iris::ArithmeticDouble ## name ## Format & s) { \
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
    Core::invoke(const iris::ArithmeticDouble ## name ## Format & s) { \
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
    Core::invoke(const iris::ArithmeticDouble ## name ## Format & s) { \
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
Core::invoke(const iris::MemoryIOStoreImmediateValueFormat& s) {
    auto [ dest, imm16 ] = s.arguments();
    storeIO(dest, imm16);
}

void
Core::invoke(const iris::MemoryIOLoadWithOffsetFormat& s) {
    auto [ dest, addr, offset ] = s.arguments();
    setRegisterValue(dest, loadIO(addr, offset));
}
void
Core::invoke(const iris::MemoryIOStoreWithOffsetFormat& s) {
    auto [ dest, value, offset ] = s.arguments();
    storeIO(dest, value, offset);
}
#define Y(name, op, kind) \
    void \
    Core::invoke(const iris::ArithmeticDouble ## name ## kind ## Format & s) { \
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
    Core::invoke(const iris::ArithmeticDouble ## name ## kind ## Format & s) { \
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
    Core::invoke(const iris::ArithmeticDouble ## name ## kind ## Format & s) { \
        using T = kind ## DoubleWord ; \
        auto [ dest, src0, src1 ] = s.arguments(); \
        if (auto denominator = getDoubleRegisterValue < T > (src1); denominator != 0) { \
        setDoubleRegisterValue<T>(dest, \
                getDoubleRegisterValue<T>(src0) op \
                getDoubleRegisterValue<T>(src1)); \
        } else { \
            throw DivideByZeroException(); \
        } \
    }
X(Divide, /);
X(Remainder, %);
#undef X
void
Core::invoke(const iris::MemoryDoubleIOLoadWithOffsetFormat& s) {
    auto [ addr, storage, offset ] = s.arguments();
    getDoubleRegister(storage).put(loadIO<DoubleWord>(addr, offset));
}
void
Core::invoke(const iris::MemoryDoubleDataLoadWithOffsetFormat& s) {
    auto [ addr, storage, offset ] = s.arguments();
    getDoubleRegister(storage).put(loadData<DoubleWord>(addr, offset));
}

void
Core::invoke(const iris::MemoryDoubleIOStoreWithOffsetFormat& s) {
    auto [ addr, storage, offset ] = s.arguments();
    storeIO(addr, getDoubleRegister(storage), offset);
}
void
Core::invoke(const iris::MemoryDoubleDataStoreWithOffsetFormat& s) {
    auto [ addr, storage, offset ] = s.arguments();
    storeData(addr, getDoubleRegister(storage), offset);
}
void
Core::invoke(const iris::MemoryQuadIOLoadWithOffsetFormat& s) {
    auto [ addr, storage, offset ] = s.arguments();
    setQuadRegisterValue(storage, loadIO<QuadWord>(addr, offset));
}
void
Core::invoke(const iris::MemoryQuadDataLoadWithOffsetFormat& s) {
    auto [ addr, storage, offset ] = s.arguments();
    setQuadRegisterValue(storage, loadData<QuadWord>(addr, offset));
}

void
Core::invoke(const iris::MemoryQuadCodeLoadWithOffsetFormat& s) {
    auto [ addr, storage, offset ] = s.arguments();

    setQuadRegisterValue(storage, loadCode<QuadWord>(addr, offset));
}

void
Core::invoke(const iris::MemoryQuadCodeStoreWithOffsetFormat& s) {
    auto [ addr, storage, offset ] = s.arguments();
    storeCode(addr, getQuadRegister(storage), offset);
}

void
Core::invoke(const iris::MemoryQuadDataStoreWithOffsetFormat& s) {
    auto [ addr, storage, offset ] = s.arguments();
    storeData(addr, getQuadRegister(storage), offset);
}
void
Core::invoke(const iris::MemoryQuadIOStoreWithOffsetFormat& s) {
    auto [ addr, storage, offset ] = s.arguments();
    storeIO(addr, getQuadRegister(storage), offset);
}
} // end namespace iris
