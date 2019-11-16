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
#include <functional>
#include <list>

using TestCaseBody = std::function<bool(iris::Core&)>;
using TestCase = std::tuple<std::string, TestCaseBody>;
using TestSuite = std::tuple<std::string, std::list<TestCase>>;
using TestSuites = std::list<TestSuite>;

bool executeTestSuite(const TestSuite& ts, iris::Core& c) noexcept {
    auto [ sname, suite ] = ts;
    std::cout << "Executing Test Suite: " << sname << std::endl;
    auto index = 0;
    for (auto const& tc : suite) {
        auto [title, op] = tc;
        std::cout << "\tExecuting Test Case " << index << ": " << title << std::endl;
        if (!op(c)) {
            return false;
        }
        ++index;
    }
    return true;
}
template<typename T>
bool verifyResult(T got, T expected, bool inverseExpectation = false) noexcept {
    auto assertionFailed = false;
    if (inverseExpectation) {
        assertionFailed = (got == expected); 
    } else {
        assertionFailed = (got != expected);
    }
    if (assertionFailed) {
        std::cout << "\tAssertion Failed!" << std::endl;
        std::cout << "\tGot: " << std::hex << got << std::endl;
        if (inverseExpectation) {
            std::cout << "\tExpected: not " << std::hex << expected << std::endl;
        } else {
            std::cout << "\tExpected: " << std::hex << expected << std::endl;
        }
        return false;
    } 
    return true;
}
template<typename T, std::enable_if_t<!std::is_same_v<std::decay_t<T>, iris::RegisterIndex>, int> = 0>
bool verifyResult(iris::RegisterIndex got, T expected, iris::Core& c) noexcept {
    return verifyResult<T>(c.getRegisterValue<T>(got), expected);
}
bool codeTests(iris::Core& c) {
    for (iris::DoubleWord i = 0; i < 0x10000; ++i) {
        auto valueToWrite = ~i;
        auto address = static_cast<iris::Address>(i);
        c.storeCode(address, valueToWrite);
        if (!verifyResult(c.loadCode(address), valueToWrite)) {
            return false;
        }
    }
    return true;

}
bool dataTests(iris::Core& c) {
    for (iris::DoubleWord i = 0; i < 0x10000; ++i) {
        iris::Word valueToWrite(~i);
        iris::Word address(i);
        // data write
        c.storeData<iris::Address, iris::Word>(address, valueToWrite);
        if (!verifyResult(c.loadData(address), valueToWrite)) {
            return false;
        }
    }
    return true;
}
bool stackTests(iris::Core& c) {
    for (iris::DoubleWord i = 0; i < 0x10000; ++i) {
        iris::Address address(i);
        iris::Word valueToWrite(~(i + 12));
        // data write
        c.storeStack(address, valueToWrite);
        if (!verifyResult(c.loadStack(address), valueToWrite)) {
            return false;
        }
    }
    return true;
}
template<typename T>
void setRegisters(iris::Core& c, T r17, T r18, T r19) noexcept {
    c.setRegisterValue<T>(20_reg, r17);
    c.setRegisterValue<T>(21_reg, r18);
    c.setRegisterValue<T>(22_reg, r19);
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
    if constexpr (op == ArithmeticOperation::Add) {
        check = src1 + src2;
        instruction = iris::instructions::opAdd(20_reg, 21_reg, 22_reg, D{});
    } else if constexpr (op == ArithmeticOperation::Subtract) {
        check = src1 - src2;
        instruction = iris::instructions::opSubtract(20_reg, 21_reg, 22_reg, D{});
    } else if constexpr (op == ArithmeticOperation::Multiply) {
        check = src1 * src2;
        instruction = iris::instructions::opMultiply(20_reg, 21_reg, 22_reg, D{});
    } else if constexpr (op == ArithmeticOperation::Divide) {
        if (src2 != 0) {
            check = src1 / src2;
        } else {
            expectDivideByZero = true;
        }
        instruction = iris::instructions::opDivide(20_reg, 21_reg, 22_reg, D{});
    } else if constexpr (op == ArithmeticOperation::Remainder) {
        if (src2 != 0) {
            check = src1 % src2;
        } else {
            expectDivideByZero = true;
        }
        instruction = iris::instructions::opRemainder(20_reg, 21_reg, 22_reg, D{});
    } else if constexpr (op == ArithmeticOperation::ShiftLeft) {
        check = src1 << src2;
        instruction = iris::instructions::opShiftLeft(20_reg, 21_reg, 22_reg, D{});
    } else if constexpr (op == ArithmeticOperation::ShiftRight) {
        check = src1 >> src2;
        instruction = iris::instructions::opShiftRight(20_reg, 21_reg, 22_reg, D{});
    } else {
        static_assert(iris::false_v<decltype(op)>, "Unimplemented arithmetic Operation");
    }
    try {
        c.invoke(instruction);
        return verifyResult<T>(20_reg, check, c);
    } catch (iris::DivideByZeroException&) {
        if (expectDivideByZero) {
            return true;
        } else {
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
    if constexpr (op == LogicalOperation::Not) {
        check = ~src1;
        c.invoke(iris::instructions::bitwiseNot(20_reg, 21_reg));
    } else if constexpr (op == LogicalOperation::And) {
        check = src1 & src2;
        c.invoke(iris::instructions::bitwiseAnd(20_reg, 21_reg, 22_reg));
    } else if constexpr (op == LogicalOperation::Or) {
        check = src1 | src2;
        c.invoke(iris::instructions::bitwiseOr(20_reg, 21_reg, 22_reg));
    } else if constexpr (op == LogicalOperation::Xor) {
        check = src1 ^ src2;
        c.invoke(iris::instructions::bitwiseXor(20_reg, 21_reg, 22_reg));
    } else {
        static_assert(iris::false_v<decltype(op)>, "Bad logical operation kind!");
    }
    return verifyResult<iris::Word>(20_reg, check, c);
}


bool testCopyRegister(iris::Core& c, iris::Word src1) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, 0);
    auto check = src1;
    c.invoke(iris::instructions::move(20_reg, 21_reg));
    return verifyResult<iris::Word>(20_reg, check, c);
}

bool testAssignRegister(iris::Core& c, iris::Word src1) noexcept {
    setRegisters<decltype(src1)>(c, 0, 0, 0);
    c.invoke(iris::instructions::move(20_reg, src1));
    return verifyResult<iris::Word>(20_reg, src1, c);
}

bool testPushRegisterOperation(iris::Core& c, iris::Word src1) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, 0); // r17 is the stack pointer in this case
    c.invoke(iris::instructions::push(20_reg, 21_reg));
    return verifyResult<iris::Word>(c.getRegisterValue<iris::Word>(20_reg), 0 - 1) &&
          verifyResult<iris::Word>(c.loadStack<iris::Word>(c.getRegisterValue(20_reg)), src1);
}

bool testPopOperation(iris::Core& c, iris::Word src1) noexcept {
    setRegisters<decltype(src1)>(c, 0, src1, 0);
    c.invoke(iris::instructions::push(20_reg, 21_reg));
    c.setRegisterValue(21_reg, 0);
    c.invoke(iris::instructions::pop(20_reg, 21_reg));
    return verifyResult<iris::Word>(c.getRegisterValue<iris::Word>(20_reg), 0) &&
          verifyResult<iris::Word>(c.getRegisterValue(21_reg), src1);
}
#if 0

bool testBranchImmediateOperation(iris::Core& c, iris::Word src1) noexcept {
    c.setIP(0);
    c.invoke(iris::instructions::branch(src1));
    return verifyResult<iris::Word>(c.getIP(), src1);
}
bool testBranchImmediateAndLinkOperation(iris::Core& c, iris::Word src1) noexcept {
    setRegisters<iris::Word>(c, 0, 0, 0);
    c.setIP(0);
    c.invoke(iris::instructions::call(20_reg, src1));
    return verifyResult<iris::Word>(c.getIP(), src1) &&
           verifyResult<iris::Word>(20_reg, 1, c);
}

bool testBranchRegisterOperation(iris::Core& c, iris::Word src1) noexcept {
    setRegisters<decltype(src1)>(c, src1, 0, 0);
    c.setIP(0);
    c.invoke(iris::instructions::branch(20_reg));
    return verifyResult<iris::Word>(c.getIP(), src1);
}

bool testBranchConditionalRegisterOperation(iris::Core& c, iris::Word src1, iris::Word cond, iris::Word expect) noexcept {
    setRegisters<decltype(src1)>(c, src1, cond, 0);
    c.setIP(0);
    c.invoke(iris::instructions::branchConditional(21_reg, 20_reg));
    return verifyResult<iris::Word>(c.getIP(), expect);
}

bool testBranchConditionalRegisterAndLinkOperation(iris::Core& c, iris::Word src1, iris::Word cond, iris::Word expect, iris::Word expectLink) noexcept {
    setRegisters<decltype(src1)>(c, src1, cond, 0);
    c.setIP(0);
    c.invoke(iris::instructions::branchConditionalAndLink(20_reg, 21_reg, 22_reg));
    return verifyResult<iris::Word>(c.getIP(), expect) &&
           verifyResult<iris::Word>(22_reg, expectLink, c);

}


bool testBranchRelativeImmediateOperation(iris::Core& c, iris::Offset16 src1) noexcept {
    c.setIP(0x20);
    c.invoke(iris::instructions::branch(src1));
    auto check = 0x20 + src1;
    return verifyResult<iris::Word>(c.getIP(), check);
}

bool testBranchRelativeImmediateAndLinkOperation(iris::Core& c, iris::Offset16 src1) noexcept {
    setRegisters<iris::Word>(c, 0, 0, 0);
    c.setIP(0x20);
    c.invoke(iris::instructions::call(20_reg, src1));
    auto check = 0x20 + src1;
    return verifyResult<iris::Word>(c.getIP(), check) &&
           verifyResult<iris::Word>(20_reg, 0x21, c);
}
#endif

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
template<ArithmeticOperation op, 
    iris::DoubleWord outerStart = 0,
    iris::DoubleWord outerEnd = 0x100,
    iris::DoubleWord innerStart = outerStart,
    iris::DoubleWord innerEnd = outerEnd>
bool testArithmeticOperationKinds(iris::Core& c) noexcept {
    for (auto i = outerStart; i < outerEnd; ++i) {
        for (auto j = innerStart; j < innerEnd; ++j) {
            if (!testArithmeticOperation<iris::Word, op>(c, static_cast<iris::Word>(i), static_cast<iris::Word>(j)) ||
                    !testArithmeticOperation<iris::SignedWord, op>(c, static_cast<iris::SignedWord>(i), static_cast<iris::SignedWord>(j))) {
                return false;
            }
        }
    }
    return true;
}
enum class CompareOperations {
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
};

template<CompareOperations op,
    typename T,
    iris::DoubleWord outerStart = 0,
    iris::DoubleWord outerEnd = 0x1000,
    iris::DoubleWord innerStart = outerStart,
    iris::DoubleWord innerEnd = outerEnd>
bool testCompareOperations(iris::Core& c) noexcept {
    using K = std::decay_t<T>;
    for (auto i = outerStart; i < outerEnd; ++i) {
        auto a = static_cast<K>(i);
        for (auto j = innerStart; j < innerEnd; ++j) {
            auto b= static_cast<K>(j);
            setRegisters<K>(c, 0, a, b);
            c.invoke(iris::instructions::compare<K>(20_reg, 21_reg, 22_reg));
            if constexpr (auto outcome = c.getRegisterValue<K>(20_reg); op == CompareOperations::Equals) {
                if (a == b) {
                    if (!verifyResult<K>(outcome & 0b010, 0b010)) {
                        return false;
                    }
                } else {
                    if (!verifyResult<K>(outcome & 0b010, 0)) {
                        return false;
                    }
                }
            } else if constexpr (op == CompareOperations::NotEquals) {
                if (a != b) {
                    if (!verifyResult<K>(outcome & 0b010, 0)) {
                        return false;
                    }
                } else {
                    if (!verifyResult<K>(outcome & 0b010, 0b010)) {
                        return false;
                    }
                }
            } else if constexpr (op == CompareOperations::LessThan) {
                if (a < b) {
                    if (!verifyResult<K>(outcome & 0b100, 0b100)) {
                        return false;
                    }
                } else {
                    if (!verifyResult<K>(outcome & 0b100, 0)) {
                        return false;
                    }
                }
            } else if constexpr (op == CompareOperations::GreaterThan) {
                if (a > b) {
                    if (!verifyResult<K>(outcome & 0b001, 0b001)) {
                        return false;
                    }
                } else {
                    if (!verifyResult<K>(outcome & 0b001, 0)) {
                        return false;
                    }
                }
            } else if constexpr (op == CompareOperations::LessThanOrEqualTo) {
                if (a <= b) {
                    if (!verifyResult<K>(outcome & 0b001, 0)) {
                        return false;
                    }
                } else {
                    if (!verifyResult<K>(outcome & 0b001, 0b001)) {
                        return false;
                    }
                }
            } else if constexpr (op == CompareOperations::GreaterThanOrEqualTo) {
                if (a >= b) {
                    if (!verifyResult<K>(outcome & 0b100, 0)) {
                        return false;
                    }
                } else {
                    if (!verifyResult<K>(outcome & 0b100, 0b100)) {
                        return false;
                    }
                }
            } else {
                static_assert(iris::false_v<decltype(op)>, "Unimplemented compare operation!");
            }
        }
    }
    return true;
}

template<typename T>
TestCaseBody setupFunction(std::function<bool(iris::Core&, T)> fn, T value) noexcept {
    return [fn, value](iris::Core& c) { return fn(c, value); };
}
template<typename T>
TestCaseBody setupFunction(std::function<bool(iris::Core&, T, T, T)> fn, T first, T second, T third) noexcept {
    return [fn, first, second, third](iris::Core& c) { return fn(c, first, second, third); };
}
template<typename T>
TestCaseBody setupFunction(std::function<bool(iris::Core&, T, T, T, T)> fn, T first, T second, T third, T fourth) noexcept {
    return [fn, first, second, third, fourth](iris::Core& c) { return fn(c, first, second, third, fourth); };
}
TestSuites suites {
    { "Arithmetic Operation Validation", {
            { "Add Signed and Unsigned", testArithmeticOperationKinds<ArithmeticOperation::Add> },
            { "Subtract Signed and Unsigned", testArithmeticOperationKinds<ArithmeticOperation::Subtract> },
            { "Multiply Signed and Unsigned", testArithmeticOperationKinds<ArithmeticOperation::Multiply> },
            { "Divide Signed and Unsigned", testArithmeticOperationKinds<ArithmeticOperation::Divide> },
            { "Remainder Signed and Unsigned", testArithmeticOperationKinds<ArithmeticOperation::Remainder> },
            { "Shift Left Signed and Unsigned", testArithmeticOperationKinds<ArithmeticOperation::ShiftLeft> },
            { "Shift Right Signed and Unsigned", testArithmeticOperationKinds<ArithmeticOperation::ShiftRight> },
        },

    },
    //{ "Branch Operation Validation", {
    //        { "Branch Absolute Immediate", setupFunction<iris::Word>(testBranchImmediateOperation, 0xFDED) },
    //        { "Branch Absolute Immediate and Link", setupFunction<iris::Word>(testBranchImmediateAndLinkOperation, 0xFDED) },
    //        { "Branch Relative Immediate", setupFunction<iris::SignedWord>(testBranchRelativeImmediateOperation, -1) },
    //        { "Branch Relative Immediate and Link", setupFunction<iris::SignedWord>(testBranchRelativeImmediateAndLinkOperation, -1) },
    //        { "Branch Absolute Register", setupFunction<iris::Word>(testBranchRegisterOperation, 0xFDED) },
    //        { "Branch Conditional Absolute Register (Branch Taken)", setupFunction<iris::Word>(testBranchConditionalRegisterOperation, 0xFDED, 1, 0xFDED) },
    //        { "Branch Conditional Absolute Register (Branch Not Taken)", setupFunction<iris::Word>(testBranchConditionalRegisterOperation, 0xFDED, 0, 0) },
    //        { "Branch Conditional Absolute Register And Link (Branch Taken)", setupFunction<iris::Word>(testBranchConditionalRegisterAndLinkOperation, 0xFDED, 1, 0xFDED, 1) },
    //        { "Branch Conditional Absolute Register And Link (Branch Not Taken)", setupFunction<iris::Word>(testBranchConditionalRegisterAndLinkOperation, 0xFDED, 0, 0, 0) },
    //    },
    //},
    { "Compare Ordinal Operation Validation", {
            { "Equals", testCompareOperation<CompareOperations::Equals, iris::Ordinal>},
            { "Not Equals", testCompareOperation<CompareOperations::NotEquals, iris::Ordinal>},
            { "Less Than", testCompareOperation<CompareOperations::LessThan, iris::Ordinal> },
            { "Less Than Or Equal", testCompareOperation<CompareOperations::LessThanOrEqualTo, iris::Ordinal>},
            { "Greater Than", testCompareOperation<CompareOperations::GreaterThan, iris::Ordinal> },
            { "Greater Than Or Equal", testCompareOperation<CompareOperations::GreaterThanOrEqualTo, iris::Ordinal> },
        },
    },
    { "Compare Integer Operation Validation", {
            { "Equals", testCompareOperation<CompareOperations::Equals, iris::SignedWord>},
            { "Not Equals", testCompareOperation<CompareOperations::NotEquals, iris::SignedWord>},
            { "Less Than", testCompareOperation<CompareOperations::LessThan, iris::SignedWord> },
            { "Less Than Or Equal", testCompareOperation<CompareOperations::LessThanOrEqualTo, iris::SignedWord>},
            { "Greater Than", testCompareOperation<CompareOperations::GreaterThan, iris::SignedWord> },
            { "Greater Than Or Equal", testCompareOperation<CompareOperations::GreaterThanOrEqualTo, iris::SignedWord> },
        },
    },
    { "Logical Operation Validation", {
            { "Logical Not Operation", testLogicalOperationKind<LogicalOperation::Not>},
            { "Logical And Operation", testLogicalOperationKind<LogicalOperation::And>},
            { "Logical Or Operation", testLogicalOperationKind<LogicalOperation::Or>},
            { "Logical Xor Operation", testLogicalOperationKind<LogicalOperation::Xor>},
        },
    },
    { "Miscellaneous Memory Operations", {
            { "Copy Register", setupFunction<iris::Word>(testCopyRegister,0xFDED) },
            { "Assign Register", setupFunction<iris::Word>(testAssignRegister, 0xFDED) },
        },
    },
    { "Code Space", {
            {"Write and readback from code memory", codeTests },
            { "Code Cell Write via instructions", 
                [](iris::Core& c) {
                    c.resetExecutionStatus();
                    setRegisters<iris::Word>(c, 0, 0xCDEF, 0x89AB);
                    c.invoke(iris::instructions::storeCode(20_reg, 21_reg, 0));
                    return verifyResult<iris::DoubleWord>(c.loadCode<iris::Address>(0), 0x89ABCDEF);
                }, 
            },
            { "Code Cell Write via instructions (non zero address)", 
                [](iris::Core& c) {
                    c.resetExecutionStatus();
                    setRegisters<iris::Word>(c, 0xFDED, 0xCDEF, 0x89AB);
                    c.invoke(iris::instructions::storeCode(20_reg, 21_reg, 0));
                    return verifyResult<iris::DoubleWord>(c.loadCode<iris::Address>(0xFDED), 0x89ABCDEF);
                }, 
            },
            { "Code cell read via instructions",
                [](iris::Core& c) {
                    c.resetExecutionStatus();
                    c.storeCode<iris::Address, iris::DoubleWord>(0, 0x89ABCDEF);
                    setRegisters<iris::Word>(c, 0, 0, 0);
                    c.invoke(iris::instructions::loadCode(20_reg, 21_reg, 0));
                    auto dr = c.getDoubleRegisterValue<iris::DoubleWord>(21_reg);
                    return verifyResult<iris::DoubleWord>(dr, 0x89ABCDEF);
                }, 
            },
            { "Code cell read via instructions (non zero address)",
                [](iris::Core& c) {
                    c.resetExecutionStatus();
                    c.storeCode<iris::Address, iris::DoubleWord>(0xfded, 0x89ABCDEF);
                    setRegisters<iris::Word>(c, 0xfded, 0, 0);
                    c.invoke(iris::instructions::loadCode(20_reg, 21_reg, 0));
                    auto dr = c.getDoubleRegisterValue<iris::DoubleWord>(21_reg);
                    return verifyResult<iris::DoubleWord>(dr, 0x89ABCDEF);
                }, 
            },
        },
    },
    { "Data Space", {
            {"Write and readback from data memory", dataTests},
            { "Data cell write via instructions", 
                [](iris::Core& c) {
                    c.resetExecutionStatus();
                    setRegisters<iris::Word>(c, 0, 0xCDEF, 0x89AB);
                    c.invoke(iris::instructions::storeData(20_reg, 21_reg, 0));
                    return verifyResult<iris::Word>(c.loadData<iris::Address>(0), 0xCDEF);
                }, 
            },
            { "Data cell write via instructions (non zero address)", 
                [](iris::Core& c) {
                    c.resetExecutionStatus();
                    setRegisters<iris::Word>(c, 0xFDED, 0xCDEF, 0x89AB);
                    c.invoke(iris::instructions::storeData(20_reg, 21_reg, 0));
                    return verifyResult<iris::Word>(c.loadData<iris::Address>(0xFDED), 0xCDEF);
                }, 
            },
            { "Data cell read via instructions",
                [](iris::Core& c) {
                    c.resetExecutionStatus();
                    c.storeData<iris::Address, iris::Word>(0, 0xCDEF);
                    setRegisters<iris::Word>(c, 0, 0, 0);
                    c.invoke(iris::instructions::loadData(20_reg, 21_reg));
                    return verifyResult<iris::Word>(21_reg, 0xCDEF, c);
                }, 
            },
            { "Data cell read via instructions (non zero address)",
                [](iris::Core& c) {
                    c.resetExecutionStatus();
                    c.storeData<iris::Address, iris::Word>(0xfded, 0xCDEF);
                    setRegisters<iris::Word>(c, 0xfded, 0, 0);
                    c.invoke(iris::instructions::loadData(20_reg, 21_reg));
                    return verifyResult<iris::Word>(21_reg, 0xCDEF, c);
                }, 
            },
        },
    },
    { "Stack Space", {
            {"Write and readback from stack memory", stackTests},
            { "Push Register onto stack", setupFunction<iris::Word>(testPushRegisterOperation, 0xFDED) },
            { "Pop Register from stack", setupFunction<iris::Word>(testPopOperation, 0xFDED) },
        },
    },
    { "IO Space", {
            { "Write to terminate cell!", 
                [](iris::Core& c) {
                    c.resetExecutionStatus();
                    setRegisters<iris::Word>(c, 0, 1, 0);
                    c.invoke(iris::instructions::storeIO(20_reg, 21_reg));
                    return verifyResult<iris::Word>(c.getTerminateCell(), 1) && verifyResult<bool>(c.getExecutingStatus(), false);
                }
            },
            { "Read from terminate cell",
               [](iris::Core& c) {
                    c.resetExecutionStatus();
                    c.setTerminateCell(0xFDED);
                    setRegisters<iris::Word>(c, 0, 0, 0);
                    c.invoke(iris::instructions::loadIO(20_reg, 21_reg, 0));
                    return verifyResult<iris::Word>(21_reg, 0xFDED, c);
               }
            },
        },
    },
};
int main(int, char* []) {
    iris::Core c;
    for (const auto & suite : suites) {
        if (!executeTestSuite(suite, c)) {
            std::cout << "tests failed!" << std::endl;
            return 1;
        }
    }
    std::cout << "all tests passed!" << std::endl;
    return 0;
}
