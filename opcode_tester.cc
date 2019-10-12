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
bool testAddUnsignedOperation(iris::Core& c, iris::Word src1, iris::Word src2) noexcept {
    setRegisters<iris::Word>(c, 0, src1, src2);
    auto check = src1 + src2;
    c.invoke(iris::instructions::opAddUnsigned(17_reg, 18_reg, 19_reg));
    return verifyResult<iris::Word>("add operation failed!", c.getRegisterValue<iris::Word>(17_reg), check);
}

bool testAddSignedOperation(iris::Core& c, iris::SignedWord src1, iris::SignedWord src2) noexcept {
    setRegisters<iris::SignedWord>(c, 0, src1, src2);
    auto check = src1 + src2;
    c.invoke(iris::instructions::opAddSigned(17_reg, 18_reg, 19_reg));
    return verifyResult<iris::SignedWord>("add operation failed!", c.getRegisterValue<iris::SignedWord>(17_reg), check);
}

bool testSubtractUnsignedOperation(iris::Core& c, iris::Word src1, iris::Word src2) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, src2);
    auto check = src1 - src2;
    c.invoke(iris::instructions::opSubtractUnsigned(17_reg, 18_reg, 19_reg));
    return verifyResult<iris::Word>("subtract operation failed!", c.getRegisterValue<iris::Word>(17_reg), check);
}

bool testSubtractSignedOperation(iris::Core& c, iris::SignedWord src1, iris::SignedWord src2) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, src2);
    auto check = src1 - src2;
    c.invoke(iris::instructions::opSubtractSigned(17_reg, 18_reg, 19_reg));
    return verifyResult<iris::SignedWord>("subtract operation failed!", c.getRegisterValue<iris::SignedWord>(17_reg), check);
}

bool testMultiplyUnsignedOperation(iris::Core& c, iris::Word src1, iris::Word src2) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, src2);
    auto check = src1 * src2;
    c.invoke(iris::instructions::opMultiplyUnsigned(17_reg, 18_reg, 19_reg));
    return verifyResult<iris::Word>("multiply unsigned operation failed!", c.getRegisterValue<iris::Word>(17_reg), check);
}

bool testMultiplySignedOperation(iris::Core& c, iris::SignedWord src1, iris::SignedWord src2) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, src2);
    auto check = src1 * src2;
    c.invoke(iris::instructions::opMultiplySigned(17_reg, 18_reg, 19_reg));
    return verifyResult<iris::SignedWord>("multiply signed operation failed!", c.getRegisterValue<iris::SignedWord>(17_reg), check);
}

bool testDivideUnsignedOperation(iris::Core& c, iris::Word src1, iris::Word src2) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, src2);
    auto check = src1 / src2;
    c.invoke(iris::instructions::opDivideUnsigned(17_reg, 18_reg, 19_reg));
    /// @todo do the divide by zero validation 
    return verifyResult<iris::Word>("divide unsigned operation failed!", c.getRegisterValue<iris::Word>(17_reg), check);
}

bool testDivideSignedOperation(iris::Core& c, iris::SignedWord src1, iris::SignedWord src2) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, src2);
    auto check = src1 / src2;
    c.invoke(iris::instructions::opDivideSigned(17_reg, 18_reg, 19_reg));
    /// @todo do the divide by zero validation 
    return verifyResult<iris::SignedWord>("divide signed operation failed!", c.getRegisterValue<iris::SignedWord>(17_reg), check);
}

bool testRemainderUnsignedOperation(iris::Core& c, iris::Word src1, iris::Word src2) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, src2);
    auto check = src1 % src2;
    c.invoke(iris::instructions::opRemainderUnsigned(17_reg, 18_reg, 19_reg));
    /// @todo do the divide by zero validation 
    return verifyResult<iris::Word>("remainder unsigned operation failed!", c.getRegisterValue<iris::Word>(17_reg), check);
}

bool testRemainderSignedOperation(iris::Core& c, iris::SignedWord src1, iris::SignedWord src2) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, src2);
    auto check = src1 % src2;
    c.invoke(iris::instructions::opRemainderSigned(17_reg, 18_reg, 19_reg));
    /// @todo do the divide by zero validation 
    return verifyResult<iris::SignedWord>("remainder signed operation failed!", c.getRegisterValue<iris::SignedWord>(17_reg), check);
}

bool instructionTests(iris::Core& c) {
    std::cout << "Instruction related tests" << std::endl;
#define X(op) \
    if (! test ## op ## UnsignedOperation(c, 0xFDED, 2)) { return true; } \
    if (! test ## op ## SignedOperation(c, -1, 2)) { return true; } 
    X(Add)
    X(Subtract)
    X(Multiply)
    X(Divide)
    X(Remainder)
#undef X
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
