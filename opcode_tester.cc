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
#include <array>

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
enum class ArithmeticOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    ShiftLeft,
    ShiftRight,
};
template<typename T, ArithmeticOperation op>
bool testArithmeticOperation(iris::Core& c, T src1, T src2) noexcept {
    using K = std::decay_t<T>;
    static_assert(std::is_integral_v<K>);
    using D = std::conditional_t<std::is_signed_v<K>, iris::instructions::IntegerOperation, iris::instructions::OrdinalOperation>;
    setRegisters<K>(c, 0, src1, src2);
    K check = 0;
    iris::instructions::Bits instruction = 0;
    bool expectDivideByZero = false;
    std::stringstream ss;
    if (std::is_signed_v<K>) {
        ss << "integer ";
    } else {
        ss << "ordinal ";
    }
    if constexpr (op == ArithmeticOperation::Add) {
        check = src1 + src2;
        instruction = iris::instructions::opAdd(17_reg, 18_reg, 19_reg, D{});
        ss << "add ";
    } else if constexpr (op == ArithmeticOperation::Subtract) {
        check = src1 - src2;
        instruction = iris::instructions::opSubtract(17_reg, 18_reg, 19_reg, D{});
        ss << "subtract ";
    } else if constexpr (op == ArithmeticOperation::Multiply) {
        check = src1 * src2;
        instruction = iris::instructions::opMultiply(17_reg, 18_reg, 19_reg, D{});
        ss << "multiply ";
    } else if constexpr (op == ArithmeticOperation::Divide) {
        if (src2 != 0) {
            check = src1 / src2;
        } else {
            expectDivideByZero = true;
        }
        instruction = iris::instructions::opDivide(17_reg, 18_reg, 19_reg, D{});
        ss << "divide ";
    } else if constexpr (op == ArithmeticOperation::Remainder) {
        if (src2 != 0) {
            check = src1 % src2;
        } else {
            expectDivideByZero = true;
        }
        instruction = iris::instructions::opRemainder(17_reg, 18_reg, 19_reg, D{});
        ss << "remainder ";
    } else if constexpr (op == ArithmeticOperation::ShiftLeft) {
        check = src1 << src2;
        instruction = iris::instructions::opShiftLeft(17_reg, 18_reg, 19_reg, D{});
        ss << "shift left ";
    } else if constexpr (op == ArithmeticOperation::ShiftRight) {
        check = src1 >> src2;
        instruction = iris::instructions::opShiftRight(17_reg, 18_reg, 19_reg, D{});
        ss << "shift right ";
    } else {
        static_assert(iris::false_v<decltype(op)>, "Unimplemented arithmetic Operation");
    }
    ss << "operation failed!";
    auto str = ss.str();
    try {
        c.invoke(instruction);
        return verifyResult<T>(str, 17_reg, check, c);
    } catch (iris::DivideByZeroException&) {
        if (expectDivideByZero) {
            return true;
        } else {
            std::cout << str << std::endl;
            std::cout << "unexpected divide by zero happened!" << std::endl;
            return false;
        }
    }
}
enum class LogicalOperation {
    And,
    Or,
    Xor,
    Not,
};

template<LogicalOperation op>
bool testLogicalOperation(iris::Core& c, iris::UnsignedWord src1, iris::UnsignedWord src2 = 0) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, src2);
    iris::Word check = 0u;
    std::string msg;
    if constexpr (op == LogicalOperation::Not) {
        check = ~src1;
        c.invoke(iris::instructions::bitwiseNot(17_reg, 18_reg));
        msg = "bitwise not operation failed!";
    } else if constexpr (op == LogicalOperation::And) {
        check = src1 & src2;
        c.invoke(iris::instructions::bitwiseAnd(17_reg, 18_reg, 19_reg));
        msg = "bitwise and operation failed!";
    } else if constexpr (op == LogicalOperation::Or) {
        check = src1 | src2;
        c.invoke(iris::instructions::bitwiseOr(17_reg, 18_reg, 19_reg));
        msg = "bitwise or operation failed!";
    } else if constexpr (op == LogicalOperation::Xor) {
        check = src1 ^ src2;
        c.invoke(iris::instructions::bitwiseXor(17_reg, 18_reg, 19_reg));
        msg = "bitwise xor operation failed!";
    } else {
        static_assert(iris::false_v<decltype(op)>, "Bad logical operation kind!");
    }
    return verifyResult<iris::Word>(msg, 17_reg, check, c);
}


bool testCopyRegister(iris::Core& c, iris::Word src1) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, 0);
    auto check = src1;
    c.invoke(iris::instructions::move(17_reg, 18_reg));
    return verifyResult<iris::Word>("copy register operation failed!", 17_reg, check, c);
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
    return verifyResult<iris::Word>("branch immediate operation failed", c.getIP(), src1);
}

bool testBranchRegisterOperation(iris::Core& c, iris::Word src1) noexcept {
    setRegisters<decltype(src1)>(c, src1, 0, 0);
    c.setIP(0);
    c.invoke(iris::instructions::branch(17_reg));
    return verifyResult<iris::Word>("branch register operation failed", c.getIP(), src1);
}

bool testBranchConditionalRegisterOperation(iris::Core& c, iris::Word src1, iris::Word cond, iris::Word expect) noexcept {
    setRegisters<decltype(src1)>(c, src1, cond, 0);
    c.setIP(0);
    c.invoke(iris::instructions::branchConditional(18_reg, 17_reg));
    return verifyResult<iris::Word>("conditional branch register operation failed", c.getIP(), expect);
}
bool testBranchConditionalRegisterOperation(iris::Core& c, iris::Word src1, iris::Word cond) noexcept {
    return testBranchConditionalRegisterOperation(c, src1, cond, src1);
}


bool testBranchRelativeImmediateOperation(iris::Core& c, iris::Offset16 src1) noexcept {
    c.setIP(0x20);
    c.invoke(iris::instructions::branch(src1));
    auto check = 0x20 + src1;
    return verifyResult<iris::Word>("branch relative immediate operation failed", c.getIP(), check);
}

bool testMoveFromIP(iris::Core& c, iris::Address src1) noexcept {
    setRegisters<decltype(src1)>(c, 0, 0, 0);
    c.setIP(src1);
    c.invoke(iris::instructions::MemoryMoveFromIP({17_reg}));
    return verifyResult<iris::Word>("move from ip failed!", 17_reg, src1, c);
}

bool testMoveToIP(iris::Core& c, iris::Address src1) noexcept {
    setRegisters<decltype(src1)>(c, src1, 0, 0);
    c.setIP(0);
    c.invoke(iris::instructions::MemoryMoveToIP({17_reg}));
    return verifyResult<iris::Word>("move to ip failed!", c.getIP(), src1);
}

template<LogicalOperation op, bool testAllCombinations = true>
bool testLogicalOperationKind(iris::Core& c) noexcept {
    iris::DoubleWord start = 0;
    iris::DoubleWord end = 0x10000;
    if constexpr (!testAllCombinations) {
        if constexpr (op == LogicalOperation::Not) {
            start = 0xFFFF;
            end = 0x10002;
        } else {
            start = 0xFDED;
            end = start + 1;
        }
    }
    std::function<bool(iris::Word)> fn = nullptr;
    if constexpr (op == LogicalOperation::Not) {
        fn = [&c](iris::Word i) { return testLogicalOperation<op>(c, i); };
    } else {
        static constexpr iris::Word masks[] = {
            0x000F,
            0x00F0,
            0x0F00,
            0xF000,
            0x00FF,
            0x0FF0,
            0xFF00,
            0x0FFF,
            0xFFF0,
            0xFFFF,
            0xF00F,
            0xFF0F,
            0xF0FF,
            0xFDED,
        };
        fn = [&c](iris::Word i) {
            for (const auto& mask : masks) {
                if (!testLogicalOperation<op>(c,static_cast<iris::Word>(i), mask)) {
                    return false;
                }
            }
            return true;
        };
    }
    for (iris::DoubleWord i = start; i < end; ++i) {
        if (!fn(static_cast<iris::Word>(i))) {
            return false;
        }
    }
    return true;
}

bool testArithmeticOperationKinds() noexcept {
    static constexpr auto innerBody = [](iris::Core& c, iris::DoubleWord i, iris::DoubleWord j) noexcept {
                if (!testArithmeticOperation<iris::Word, ArithmeticOperation::Add>(c, static_cast<iris::Word>(i), static_cast<iris::Word>(j)) ||
                        !testArithmeticOperation<iris::SignedWord, ArithmeticOperation::Add>(c, static_cast<iris::SignedWord>(i), static_cast<iris::SignedWord>(j))) {
                    return false;
                }
                if (!testArithmeticOperation<iris::Word, ArithmeticOperation::Subtract>(c, static_cast<iris::Word>(i), static_cast<iris::Word>(j)) ||
                        !testArithmeticOperation<iris::SignedWord, ArithmeticOperation::Subtract>(c, static_cast<iris::SignedWord>(i), static_cast<iris::SignedWord>(j))) {
                    return false;
                }
                if (!testArithmeticOperation<iris::Word, ArithmeticOperation::Multiply>(c, static_cast<iris::Word>(i), static_cast<iris::Word>(j)) ||
                        !testArithmeticOperation<iris::SignedWord, ArithmeticOperation::Multiply>(c, static_cast<iris::SignedWord>(i), static_cast<iris::SignedWord>(j))) {
                    return false;
                }
                if (!testArithmeticOperation<iris::Word, ArithmeticOperation::Divide>(c, static_cast<iris::Word>(i), static_cast<iris::Word>(j)) ||
                        !testArithmeticOperation<iris::SignedWord, ArithmeticOperation::Divide>(c, static_cast<iris::SignedWord>(i), static_cast<iris::SignedWord>(j))) {
                    return false;
                }
                if (!testArithmeticOperation<iris::Word, ArithmeticOperation::Remainder>(c, static_cast<iris::Word>(i), static_cast<iris::Word>(j)) ||
                        !testArithmeticOperation<iris::SignedWord, ArithmeticOperation::Remainder>(c, static_cast<iris::SignedWord>(i), static_cast<iris::SignedWord>(j))) {
                    return false;
                }
                if (!testArithmeticOperation<iris::Word, ArithmeticOperation::ShiftLeft>(c, static_cast<iris::Word>(i), static_cast<iris::Word>(j)) ||
                        !testArithmeticOperation<iris::SignedWord, ArithmeticOperation::ShiftLeft>(c, static_cast<iris::SignedWord>(i), static_cast<iris::SignedWord>(j))) {
                    return false;
                }
                if (!testArithmeticOperation<iris::Word, ArithmeticOperation::ShiftRight>(c, static_cast<iris::Word>(i), static_cast<iris::Word>(j)) ||
                        !testArithmeticOperation<iris::SignedWord, ArithmeticOperation::ShiftRight>(c, static_cast<iris::SignedWord>(i), static_cast<iris::SignedWord>(j))) {
                    return false;
                }
                return true;
    };
    iris::Core c;
    for (auto i = 0; i < 0x100; ++i) {
        // eliminate chain walk downs by computing both i,j and j,i versions at the same time
        for (auto j = i; j < 0x100; ++j) {
            if (!innerBody(c, i, j)) {
                return false;
            }
            if (i != j) {
                if (!innerBody(c, j, i)) {
                    return false;
                }
            }
        }
    }
    return true;
}

bool instructionTests(iris::Core& c) {
    std::cout << "Instruction related tests" << std::endl;
    //-----------------------------------------------------------------------------
    // Arithmetic
    //-----------------------------------------------------------------------------
    if (!testArithmeticOperationKinds()) { return true; }
#if 0
    if (!testArithmeticOperationKind<ArithmeticOperation::Add>() ||
        !testArithmeticOperationKind<ArithmeticOperation::Subtract>() ||
        !testArithmeticOperationKind<ArithmeticOperation::Multiply>() ||
        !testArithmeticOperationKind<ArithmeticOperation::Divide>() ||
        !testArithmeticOperationKind<ArithmeticOperation::Remainder>() ||
        !testArithmeticOperationKind<ArithmeticOperation::ShiftLeft>() ||
        !testArithmeticOperationKind<ArithmeticOperation::ShiftRight>()) { return true; }
#endif

    //-----------------------------------------------------------------------------
    // Logical 
    //-----------------------------------------------------------------------------
    if ((! testLogicalOperationKind<LogicalOperation::Not>(c)) ||
        (! testLogicalOperationKind<LogicalOperation::And>(c)) ||
        (! testLogicalOperationKind<LogicalOperation::Or>(c)) ||
        (! testLogicalOperationKind<LogicalOperation::Xor>(c))) { return true; }
    //-----------------------------------------------------------------------------
    // Memory 
    //-----------------------------------------------------------------------------
    if (!testCopyRegister(c, 32) ||
        !testAssignRegister(c, 128) ||
        !testPushRegisterOperation(c, 0xFDED) ||
        !testPopOperation(c, 0xFDED) ||
        !testMoveFromIP(c, 0xFDED) ||
        !testMoveToIP(c, 0xFDED)) { return true; }
    /// @todo test the data, io, and code operations
    //-----------------------------------------------------------------------------
    // Branch 
    //-----------------------------------------------------------------------------
    if (!testBranchImmediateOperation(c, 0xFDED) ||
        !testBranchRelativeImmediateOperation(c, -1) ||
        !testBranchRegisterOperation(c, 0xFDED) ||
        !testBranchConditionalRegisterOperation(c, 0xFDED, 1, 0xFDED) ||
        !testBranchConditionalRegisterOperation(c, 0xFDED, 0, 0)) { return true; }
    /// @todo test the rest of the core branch kinds
    //-----------------------------------------------------------------------------
    // Compare 
    //-----------------------------------------------------------------------------
    /// @todo implement compare checks

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
