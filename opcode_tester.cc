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
#include "encoding.h"
#include <iostream>
#include <string>

template<typename T>
bool verifyResult(const std::string& failMsg, T got, T expected) noexcept {
    if (got != expected) {
        std::cout << failMsg << std::endl;
        std::cout << "\tGot: " << std::hex << got << std::endl;
        std::cout << "\tExpected: " << std::hex << expected << std::endl;
        return false;
    } else {
        return true;
    }
}
template<typename T, std::enable_if_t<!std::is_same_v<std::decay_t<T>, iris::RegisterIndex>, int> = 0>
bool verifyResult(const std::string& failMsg, iris::RegisterIndex got, T expected, iris::Core& c) noexcept {
    return verifyResult<T>(failMsg, c.getRegisterValue<T>(got), expected);
}
bool codeTests(iris::Core& c) {
    std::cout << "Code writing tests" << std::endl;
    std::cout << "\t1. Write and readback from code memory" << std::endl;
    for (iris::DoubleWord i = 0; i < 0x10000; ++i) {
        auto valueToWrite = ~i;
        auto address = static_cast<iris::Address>(i);
        c.storeCode(address, valueToWrite);
        if (!verifyResult("Write to code memory failed", c.loadCode(address), valueToWrite)) {
            return true;
        }
    }
    return false;

}
bool dataTests(iris::Core& c) {
    std::cout << "Data writing tests" << std::endl;
    std::cout << "\t1. Write and readback from data memory" << std::endl;
    for (iris::DoubleWord i = 0; i < 0x10000; ++i) {
        iris::Word valueToWrite(~i);
        iris::Word address(i);
        // data write
        c.storeData<iris::Address, iris::Word>(address, valueToWrite);
        if (!verifyResult("Write to data memory failed", c.loadData(address), valueToWrite)) {
            return true;
        }
    }
    return false;
}
bool stackTests(iris::Core& c) {
    std::cout << "Stack memory tests" << std::endl;
    std::cout << "\t1. Write and readback from stack memory" << std::endl;
    for (iris::DoubleWord i = 0; i < 0x10000; ++i) {
        iris::Address address(i);
        iris::Word valueToWrite(~(i + 12));
        // data write
        c.storeStack(address, valueToWrite);
        if (!verifyResult("Write to stack memory failed", c.loadStack(address), valueToWrite)) {
            return true;
        }
    }
    return false;
}
template<typename T>
void setRegisters(iris::Core& c, T r17, T r18, T r19) noexcept {
    c.setRegisterValue<T>(17_reg, r17);
    c.setRegisterValue<T>(18_reg, r18);
    c.setRegisterValue<T>(19_reg, r19);
}
template<typename T>
bool testAddOperation(iris::Core& c, T src1, T src2) noexcept {
    using K = std::decay_t<T>;
    static_assert(std::is_integral_v<std::decay_t<T>>);
    using D = std::conditional_t<std::is_signed_v<K>, iris::instructions::IntegerOperation, iris::instructions::OrdinalOperation>;
    setRegisters<K>(c, 0, src1, src2);
    auto check = src1 + src2;
    c.invoke(iris::instructions::opAdd(17_reg, 18_reg, 19_reg, D{}));
    return verifyResult<K>("add operation failed!", 17_reg, check, c);
}

template<typename T>
bool testSubtractOperation(iris::Core& c, T src1, T src2) noexcept {
    using K = std::decay_t<T>;
    static_assert(std::is_integral_v<std::decay_t<T>>);
    using D = std::conditional_t<std::is_signed_v<K>, iris::instructions::IntegerOperation, iris::instructions::OrdinalOperation>;
    setRegisters<K>(c, 0, src1, src2);
    auto check = src1 - src2;
    c.invoke(iris::instructions::opSubtract(17_reg, 18_reg, 19_reg, D{}));
    return verifyResult<K>("subtract operation failed!", 17_reg, check,c);
}

template<typename T>
bool testMultiplyOperation(iris::Core& c, T src1, T src2) noexcept {
    using K = std::decay_t<T>;
    static_assert(std::is_integral_v<std::decay_t<T>>);
    using D = std::conditional_t<std::is_signed_v<K>, iris::instructions::IntegerOperation, iris::instructions::OrdinalOperation>;
    setRegisters<K>(c, 0, src1, src2);
    auto check = src1 * src2;
    c.invoke(iris::instructions::opMultiply(17_reg, 18_reg, 19_reg, D{}));
    return verifyResult<K>("multiply operation failed!", 17_reg, check,c);
}

template<typename T>
bool testDivideOperation(iris::Core& c, T src1, T src2) noexcept {
    using K = std::decay_t<T>;
    static_assert(std::is_integral_v<std::decay_t<T>>);
    using D = std::conditional_t<std::is_signed_v<K>, iris::instructions::IntegerOperation, iris::instructions::OrdinalOperation>;
    setRegisters<K>(c, 0, src1, src2);
    auto check = src1 / src2;
    /// @todo do the divide by zero validation 
    c.invoke(iris::instructions::opDivide(17_reg, 18_reg, 19_reg, D{}));
    return verifyResult<K>("divide operation failed!", 17_reg, check, c);
}

template<typename T>
bool testRemainderOperation(iris::Core& c, T src1, T src2) noexcept {
    using K = std::decay_t<T>;
    static_assert(std::is_integral_v<std::decay_t<T>>);
    using D = std::conditional_t<std::is_signed_v<K>, iris::instructions::IntegerOperation, iris::instructions::OrdinalOperation>;
    setRegisters<K>(c, 0, src1, src2);
    auto check = src1 % src2;
    /// @todo do the divide by zero validation 
    c.invoke(iris::instructions::opRemainder(17_reg, 18_reg, 19_reg, D{}));
    return verifyResult<K>("remainder operation failed!", 17_reg, check, c);
}

template<typename T>
bool testShiftLeftOperation(iris::Core& c, T src1, T src2) noexcept {
    using K = std::decay_t<T>;
    static_assert(std::is_integral_v<std::decay_t<T>>);
    using D = std::conditional_t<std::is_signed_v<K>, iris::instructions::IntegerOperation, iris::instructions::OrdinalOperation>;
    setRegisters<K>(c, 0, src1, src2);
    auto check = src1 << src2;
    c.invoke(iris::instructions::opShiftLeft(17_reg, 18_reg, 19_reg, D{}));
    return verifyResult<K>("left shift operation failed!", 17_reg, check, c);
}

template<typename T>
bool testShiftRightOperation(iris::Core& c, T src1, T src2) noexcept {
    using K = std::decay_t<T>;
    static_assert(std::is_integral_v<std::decay_t<T>>);
    using D = std::conditional_t<std::is_signed_v<K>, iris::instructions::IntegerOperation, iris::instructions::OrdinalOperation>;
    setRegisters<K>(c, 0, src1, src2);
    auto check = src1 >> src2;
    c.invoke(iris::instructions::opShiftRight(17_reg, 18_reg, 19_reg, D{}));
    return verifyResult<K>("right shift operation failed!", 17_reg, check, c);
}

template<typename T>
bool testMaxOperation(iris::Core& c, T src1, T src2) noexcept {
    using K = std::decay_t<T>;
    static_assert(std::is_integral_v<std::decay_t<T>>);
    using D = std::conditional_t<std::is_signed_v<K>, iris::instructions::IntegerOperation, iris::instructions::OrdinalOperation>;
    setRegisters<K>(c, 0, src1, src2);
    auto check = std::max(src1, src2);
    c.invoke(iris::instructions::opMax(17_reg, 18_reg, 19_reg, D{}));
    return verifyResult<K>("max shift operation failed!", 17_reg, check, c);
}

template<typename T>
bool testMinOperation(iris::Core& c, T src1, T src2) noexcept {
    using K = std::decay_t<T>;
    static_assert(std::is_integral_v<std::decay_t<T>>);
    using D = std::conditional_t<std::is_signed_v<K>, iris::instructions::IntegerOperation, iris::instructions::OrdinalOperation>;
    setRegisters<K>(c, 0, src1, src2);
    auto check = std::min(src1, src2);
    c.invoke(iris::instructions::opMin(17_reg, 18_reg, 19_reg, D{}));
    return verifyResult<K>("min shift operation failed!", 17_reg, check, c);
}



bool testBitwiseNot(iris::Core& c, iris::UnsignedWord src1) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, 0);
    auto check = ~src1;
    c.invoke(iris::instructions::bitwiseNot(17_reg, 18_reg));
    return verifyResult<iris::Word>("bitwise not operation failed!", 17_reg, check, c);
}

bool testBitwiseAnd(iris::Core& c, iris::UnsignedWord src1, iris::UnsignedWord src2) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, src2);
    auto check = src1 & src2;
    c.invoke(iris::instructions::bitwiseAnd(17_reg, 18_reg, 19_reg));
    return verifyResult<iris::Word>("bitwise and operation failed!", 17_reg, check, c);
}

bool testBitwiseOr(iris::Core& c, iris::UnsignedWord src1, iris::UnsignedWord src2) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, src2);
    auto check = src1 | src2;
    c.invoke(iris::instructions::bitwiseOr(17_reg, 18_reg, 19_reg));
    return verifyResult<iris::Word>("bitwise or operation failed!", 17_reg, check, c);
}

bool testBitwiseXor(iris::Core& c, iris::UnsignedWord src1, iris::UnsignedWord src2) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, src2);
    auto check = src1 ^ src2;
    c.invoke(iris::instructions::bitwiseXor(17_reg, 18_reg, 19_reg));
    return verifyResult<iris::Word>("bitwise xor operation failed!", 17_reg, check, c);
}

bool testCopyRegister(iris::Core& c, iris::Word src1) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, 0);
    auto check = src1;
    c.invoke(iris::instructions::move(17_reg, 18_reg));
    return verifyResult<iris::Word>("copy register operation failed!", 17_reg, check, c);
}

bool testSwapRegisters(iris::Core& c, iris::Word dest, iris::Word src1) noexcept {
    setRegisters<decltype(src1)>(c, dest, src1, 0);
    c.invoke(iris::instructions::swap(17_reg, 18_reg));
    return verifyResult<iris::Word>("swap register operation failed!", 17_reg, src1, c) &&
           verifyResult<iris::Word>("swap register operation failed!", 18_reg, dest, c);
}
bool testAssignRegister(iris::Core& c, iris::Word src1) noexcept {
    setRegisters<decltype(src1)>(c, 0, 0, 0);
    c.invoke(iris::instructions::move(17_reg, src1));
    return verifyResult<iris::Word>("assign register operation failed!", 17_reg, src1, c);
}

bool testPushRegisterOperation(iris::Core& c, iris::Word src1) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, 0); // r17 is the stack pointer in this case
    c.invoke(iris::instructions::push(17_reg, 18_reg));
    return verifyResult<iris::Word>("push register operation failed!",
            c.getRegisterValue<iris::Word>(17_reg), 
            0 - 1) &&
          verifyResult<iris::Word>("push register operation failed!",
                  c.loadStack<iris::Word>(c.getRegisterValue(17_reg)),
                  src1);
}

bool testPushImmediateOperation(iris::Core& c, iris::Word src1) noexcept {
    setRegisters<decltype(src1)>(c, 0, 0, 0); // r17 is the stack pointer in this case
    c.invoke(iris::instructions::push(17_reg, src1));
    return verifyResult<iris::Word>("push immediate operation failed!",
            c.getRegisterValue<iris::Word>(17_reg), 
            0 - 1) &&
          verifyResult<iris::Word>("push immediate operation failed!",
                  c.loadStack<iris::Word>(c.getRegisterValue(17_reg)),
                  src1);
}

bool testPopOperation(iris::Core& c, iris::Word src1) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, 0);
    c.invoke(iris::instructions::push(17_reg, 18_reg));
    c.setRegisterValue(18_reg, 0);
    c.invoke(iris::instructions::pop(17_reg, 18_reg));
    return verifyResult<iris::Word>("pop operation failed!",
            c.getRegisterValue<iris::Word>(17_reg), 
            0) &&
          verifyResult<iris::Word>("pop operation failed!",
                  c.getRegisterValue(18_reg),
                  src1);
}

bool testBranchImmediateOperation(iris::Core& c, iris::Word src1) noexcept {
    c.setIP(0);
    c.invoke(iris::instructions::branch(src1));
    return verifyResult<iris::Word>("branch immediate operation failed",
            c.getIP(),
            src1);
}

bool testBranchRelativeImmediateOperation(iris::Core& c, iris::Offset16 src1) noexcept {
    c.setIP(0x20);
    c.invoke(iris::instructions::branch(src1));
    return verifyResult<iris::Word>("branch immediate operation failed",
            c.getIP(),
            0x20 + src1);
}


bool instructionTests(iris::Core& c) {
    std::cout << "Instruction related tests" << std::endl;
#define X(op) \
    if (! test ## op ## Operation <iris::Word> (c, 0xFDED, 2)) { return true; } \
    if (! test ## op ## Operation <iris::SignedWord> (c, -1, 2)) { return true; } 
    X(Add)
    X(Subtract)
    X(Multiply)
    X(Divide)
    X(Remainder)
    X(ShiftLeft)
    X(ShiftRight)
    X(Min)
    X(Max)
#undef X
    if (! testBitwiseNot(c, 0)) { return true; }
    if (! testBitwiseNot(c, 0xFFFF)) { return true; }
    if (! testBitwiseNot(c, 1)) { return true; }
#define X(op) \
    if (! testBitwise ## op (c, 0xFDED, 0x000F)) { return true; } \
    if (! testBitwise ## op (c, 0xFDED, 0x00F0)) { return true; } \
    if (! testBitwise ## op (c, 0xFDED, 0x0F00)) { return true; } \
    if (! testBitwise ## op (c, 0xFDED, 0xF000)) { return true; } \
    if (! testBitwise ## op (c, 0xFDED, 0x00FF)) { return true; } \
    if (! testBitwise ## op (c, 0xFDED, 0x0FF0)) { return true; } \
    if (! testBitwise ## op (c, 0xFDED, 0xFF00)) { return true; } \
    if (! testBitwise ## op (c, 0xFDED, 0x0FFF)) { return true; } \
    if (! testBitwise ## op (c, 0xFDED, 0xFFF0)) { return true; } \
    if (! testBitwise ## op (c, 0xFDED, 0xFFFF)) { return true; } \
    if (! testBitwise ## op (c, 0xFDED, 0xF00F)) { return true; } \
    if (! testBitwise ## op (c, 0xFDED, 0xFF0F)) { return true; }
    X(And)
    X(Or)
    X(Xor)
#undef X
    if (!testCopyRegister(c, 32)) { return true; }
    if (!testSwapRegisters(c, 32, 64)) { return true; }
    if (!testAssignRegister(c, 128)) { return true; }
    if (!testPushRegisterOperation(c, 0xFDED)) { return true; }
    if (!testPushImmediateOperation(c, 0xFDED)) { return true; }
    if (!testPopOperation(c, 0xFDED)) { return true; }
    if (!testBranchImmediateOperation(c, 0xFDED)) { return true; }
    if (!testBranchRelativeImmediateOperation(c, -1)) { return true; }

    return false;
}
int main(int, char* []) {
    iris::Core c;
    if (codeTests(c)) {
        return 1;
    } 
    if (dataTests(c)) {
        return 1;
    }
    if (stackTests(c)) {
        return 1;
    }
    if (instructionTests(c)) {
        return 1;
    }


    std::cout << "All tests passed!" << std::endl;
    return 0;
}
