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
#ifndef IRIS_ENCODING_H__
#define IRIS_ENCODING_H__
#include "opcodes.h"
#include "exceptions.h"
namespace iris::instructions {
    using Bits = iris::EncodedInstruction;
    using ComplexBinaryInstruction = std::tuple<Bits, Bits>;
    class OrdinalOperation { };
    class IntegerOperation { };
    constexpr Instruction makeInstruction(EncodedInstruction opcode, Byte arg0, Byte arg1, Byte arg2) noexcept {
        return {opcode, arg0, arg1, arg2}; 
    }
    constexpr Instruction makeInstruction(EncodedInstruction opcode, Byte arg0, Word imm) noexcept {
        return {opcode, arg0, imm}; 
    }
#define X(title, opcode) \
    constexpr auto title (Byte arg0, Byte arg1, Byte arg2) noexcept { \
        return makeInstruction(static_cast<EncodedInstruction>((opcode)), arg0, arg1, arg2); \
    } \
    constexpr auto title (RegisterIndex arg0, RegisterIndex arg1, RegisterIndex arg2) noexcept { \
        return title (std::to_integer<Byte>(arg0), std::to_integer<Byte>(arg1), std::to_integer<Byte>(arg2)); \
    } \
    constexpr auto title (Byte arg0, Word imm) noexcept { \
        return makeInstruction(static_cast<EncodedInstruction>((opcode)), arg0, imm); \
    } \
    constexpr auto title (RegisterIndex arg0, Word imm) noexcept { \
        return title(std::to_integer<Byte>(arg0), imm); \
    } \
    constexpr auto title (RegisterIndex arg0, RegisterIndex arg1, Byte arg2) noexcept { \
        return title ( std::to_integer<Byte>(arg0), std::to_integer<Byte>(arg1), arg2); \
    }
#include "InstructionFormats.def"
#undef X
    template<typename T>
    constexpr auto compare(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept {
        using K = std::decay_t<T>;
        static_assert(std::is_integral_v<K>);
        if constexpr (std::is_signed_v<K>) {
            return CompareInteger(dest, src0, src1);
        } else if constexpr (std::is_unsigned_v<K>) {
            return CompareOrdinal(dest, src0, src1);
        } else {
            static_assert(iris::false_v<K>, "Bad compare type!");
        }
    }
    // single instruction aliases useful for ease of use
    constexpr auto zeroRegister(RegisterIndex targetRegister) noexcept                          { return Move(targetRegister, 0_reg, 0_reg); }
    constexpr auto compareWithZero(RegisterIndex dest, RegisterIndex src) noexcept              { return compare<iris::SignedWord>(dest, src, 0_reg); }
    template<typename T>
    constexpr auto move(RegisterIndex dest, T value) noexcept {
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address> || std::is_unsigned_v<K>) {
            return LoadImmediate(dest, value);
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return Move(dest, value, 0_reg);
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    constexpr auto swap(RegisterIndex a, RegisterIndex b, RegisterIndex temp) noexcept          { 
        return std::make_tuple(move(temp, a),
                               move(a, b),
                               move(b, temp));
    }
    template<typename T>
    constexpr auto call(RegisterIndex link, T value) noexcept {
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address> || std::is_unsigned_v<K>) {
            return BranchAbsoluteImmediateAndLink(link, value);
        } else if constexpr (std::is_same_v<K, Offset16> || std::is_signed_v<K>) {
            return BranchRelativeImmediateAndLink(link, value);
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return BranchIfOrderedToRegisterAndLink(value, 1_reg, link);
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    template<typename T>
    constexpr auto branch(T addr) noexcept {
        // unconditional branches are branches where the conditional register is hardwired
        // to 1
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address> || std::is_unsigned_v<K>) {
            return BranchIfOrderedToAbsoluteImmediate(1_reg, addr);
        } else if constexpr (std::is_same_v<K, Offset16> || std::is_signed_v<K>) {
            return BranchIfOrderedToRelativeImmediate(1_reg, addr);
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return BranchIfOrderedToRegisterAndLink(addr, 1_reg, 0_reg);
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    constexpr auto increment(RegisterIndex dest, RegisterIndex src, IntegerOperation) noexcept {
        return AddInteger(dest, src, 1_reg);
    }
    constexpr auto increment(RegisterIndex dest, IntegerOperation op) noexcept {
        return increment(dest, dest, op);
    }
    constexpr auto increment(RegisterIndex dest, RegisterIndex src, OrdinalOperation) noexcept {
        return AddOrdinal(dest, src, 1_reg);
    }
    constexpr auto increment(RegisterIndex dest, OrdinalOperation op) noexcept {
        return increment(dest, dest, op);
    }
    constexpr auto decrement(RegisterIndex dest, RegisterIndex src, IntegerOperation) noexcept {
        return SubtractInteger(dest, src, 1_reg);
    }
    constexpr auto decrement(RegisterIndex dest, IntegerOperation op) noexcept {
        return decrement(dest, dest, op);
    }
    constexpr auto decrement(RegisterIndex dest, RegisterIndex src, OrdinalOperation) noexcept {
        return SubtractOrdinal(dest, src, 1_reg);
    }
    constexpr auto decrement(RegisterIndex dest, OrdinalOperation op) noexcept {
        return decrement(dest, dest, op);
    }
    constexpr auto push(RegisterIndex sp, RegisterIndex value) noexcept {
        return std::make_tuple(
                decrement(sp, OrdinalOperation{}),
                StackStoreWithOffset(value, sp, 0_reg));
    }
    constexpr auto push(RegisterIndex sp, Ordinal value, RegisterIndex temporary) noexcept {
        return std::make_tuple(
                Move(temporary, value),
                decrement(sp, OrdinalOperation{}),
                StackStoreWithOffset(temporary, sp, 0_reg));
    }
    constexpr auto pop(RegisterIndex sp, RegisterIndex dest) noexcept {
        return std::make_tuple(
                StackLoadWithOffset(dest, sp, 0_reg),
                increment(sp, OrdinalOperation{}));
    }

    /// @todo figure out how to do immediate push and pop
    constexpr auto storeData(RegisterIndex dest, RegisterIndex value, Byte offset) noexcept { return DataStoreWithOffset(value, dest, offset); }
    constexpr auto storeData(RegisterIndex dest, RegisterIndex storage, Address value, Byte offset) noexcept { return std::make_tuple(move(storage, value), storeData(storage, dest, offset)); }
    constexpr auto storeData(RegisterIndex dest, RegisterIndex value) noexcept { return storeData(dest, value, 0); }
    constexpr auto loadData(RegisterIndex addr, RegisterIndex value, Byte offset) noexcept { return DataLoadWithOffset(value, addr, offset); }
    constexpr auto loadData(RegisterIndex addr, RegisterIndex value) noexcept { return loadData(addr, value, 0); }
    constexpr auto storeIO(RegisterIndex addr, RegisterIndex value, Byte offset) noexcept { return IOStoreWithOffset(value, addr, offset); }
    constexpr auto storeIO(RegisterIndex addr, RegisterIndex storage, Address value, Byte offset) noexcept { return std::make_tuple(move(storage, value), storeIO(storage, addr, offset)); }
    constexpr auto storeIO(RegisterIndex addr, RegisterIndex storage) noexcept { return storeIO(addr, storage, 0); }
    constexpr auto loadIO(RegisterIndex addr, RegisterIndex dest, Byte offset) noexcept { return IOLoadWithOffset(dest, addr, offset); }
    constexpr auto loadCode(RegisterIndex addr, RegisterIndex lower, Byte offset) noexcept { return CodeLoadWithOffset(lower, addr, offset); }
    constexpr auto loadCode(RegisterIndex addr, RegisterIndex lower) noexcept { return loadCode(addr, lower, 0); }
    constexpr auto storeCode(RegisterIndex addr, RegisterIndex lower, Byte offset) noexcept { return CodeStoreWithOffset(lower, addr, offset); }
    constexpr auto storeCode(RegisterIndex addr, RegisterIndex lower) noexcept { return storeCode(addr, lower, 0); }
    // arithmetic operations
    constexpr auto twoTimes(RegisterIndex dest, RegisterIndex src, OrdinalOperation) noexcept { return ShiftLeftOrdinal(dest, src, 1_reg); }
    constexpr auto twoTimes(RegisterIndex dest, RegisterIndex src, IntegerOperation) noexcept { return ShiftLeftInteger(dest, src, 1_reg); }
    constexpr auto twoTimes(RegisterIndex val, OrdinalOperation op) noexcept { return twoTimes(val, val, op); }
    constexpr auto twoTimes(RegisterIndex val, IntegerOperation op) noexcept { return twoTimes(val, val, op); }
    constexpr auto twoDivide(RegisterIndex dest, RegisterIndex src, OrdinalOperation) noexcept { return ShiftRightOrdinal(dest, src, 1_reg); }
    constexpr auto twoDivide(RegisterIndex dest, RegisterIndex src, IntegerOperation) noexcept { return ShiftRightInteger(dest, src, 1_reg); }
    constexpr auto twoDivide(RegisterIndex val, OrdinalOperation op) noexcept { return twoDivide(val, val, op); }
    constexpr auto twoDivide(RegisterIndex val, IntegerOperation op) noexcept { return twoDivide(val, val, op); }
    constexpr auto BitwiseNot(RegisterIndex dest, RegisterIndex src) noexcept { return BitwiseNot(dest, src, 0_reg); }
    constexpr auto invert(RegisterIndex dest, RegisterIndex src) noexcept { return BitwiseNot(dest, src); }
    constexpr auto invert(RegisterIndex dest)  noexcept { return invert(dest, dest); }
    constexpr auto square(RegisterIndex dest, RegisterIndex src)  noexcept { return MultiplyInteger(dest, src, src); }
    constexpr auto square(RegisterIndex dest) noexcept { return square(dest, dest); }
#define X(kind) \
    constexpr auto op ## kind (RegisterIndex dest, RegisterIndex src1, RegisterIndex src2, OrdinalOperation) noexcept { \
        return kind ## Ordinal(dest, src1, src2); \
    } \
    constexpr auto op ## kind (RegisterIndex dest, RegisterIndex src1, RegisterIndex src2, IntegerOperation) noexcept { \
        return kind ## Integer(dest, src1, src2); \
    } \
    constexpr auto op ## kind (RegisterIndex dest, RegisterIndex src, OrdinalOperation op) noexcept { \
        return op ## kind (dest, dest, src, op); \
    } \
    constexpr auto op ## kind (RegisterIndex dest, RegisterIndex src1, IntegerOperation op) noexcept { \
        return op ## kind (dest, dest, src1, op); \
    }
    X(Add);
    X(Subtract);
    X(Multiply);
    X(Divide);
    X(Remainder);
    X(ShiftLeft);
    X(ShiftRight);
#undef X
#if 0
    constexpr auto branchIfZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex dest) noexcept {
        return std::make_tuple(equalsZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfZero(RegisterIndex cond, RegisterIndex src0, Offset16 dest) noexcept {
        return std::make_tuple(equalsZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfZero(RegisterIndex cond, RegisterIndex src0, Address dest) noexcept {
        return std::make_tuple(equalsZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfNotZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex dest) noexcept {
        return std::make_tuple(notEqualsZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfNotZero(RegisterIndex cond, RegisterIndex src0, Offset16 dest) noexcept {
        return std::make_tuple(notEqualsZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfNotZero(RegisterIndex cond, RegisterIndex src0, Address dest) noexcept {
        return std::make_tuple(notEqualsZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfGreaterThanZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex dest) noexcept {
        return std::make_tuple(greaterThanZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfGreaterThanZero(RegisterIndex cond, RegisterIndex src0, Offset16 dest) noexcept {
        return std::make_tuple(greaterThanZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfGreaterThanZero(RegisterIndex cond, RegisterIndex src0, Address dest) noexcept {
        return std::make_tuple(greaterThanZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfLessThanZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex dest) noexcept {
        return std::make_tuple(lessThanZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfLessThanZero(RegisterIndex cond, RegisterIndex src0, Offset16 dest) noexcept {
        return std::make_tuple(lessThanZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfLessThanZero(RegisterIndex cond, RegisterIndex src0, Address dest) noexcept {
        return std::make_tuple(lessThanZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfGreaterThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex dest) noexcept {
        return std::make_tuple(greaterThanOrEqualToZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfGreaterThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, Offset16 dest) noexcept {
        return std::make_tuple(greaterThanOrEqualToZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfGreaterThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, Address dest) noexcept {
        return std::make_tuple(greaterThanOrEqualToZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfLessThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex dest) noexcept {
        return std::make_tuple(lessThanOrEqualToZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfLessThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, Offset16 dest) noexcept {
        return std::make_tuple(lessThanOrEqualToZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto branchIfLessThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, Address dest) noexcept {
        return std::make_tuple(lessThanOrEqualToZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr auto selectIfZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
        return std::make_tuple(equalsZero(cond, src0), select(cond, then, _else));
    }
    constexpr auto selectIfNotZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
        return std::make_tuple(notEqualsZero(cond, src0), select(cond, then, _else));
    }
    constexpr auto selectIfGreaterThanZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
        return std::make_tuple(greaterThanZero(cond, src0), select(cond, then, _else));
    }
    constexpr auto selectIfLessThanZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
        return std::make_tuple(lessThanZero(cond, src0), select(cond, then, _else));
    }
    constexpr auto selectIfGreaterThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
        return std::make_tuple(greaterThanOrEqualToZero(cond, src0), select(cond, then, _else));
    }
    constexpr auto selectIfLessThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept {
        return std::make_tuple(lessThanOrEqualToZero(cond, src0), select(cond, then, _else));
    }
#endif
    constexpr auto bitwiseNot(RegisterIndex dest, RegisterIndex src) noexcept { return BitwiseNot(dest, src); }
    constexpr auto bitwiseNot(RegisterIndex src) noexcept { return bitwiseNot(src, src); }
    constexpr auto bitwiseAnd(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { return BitwiseAnd(dest, src0, src1); }
    constexpr auto bitwiseAnd(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseAnd(dest, dest, src); }
    constexpr auto bitwiseOr(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { return BitwiseOr(dest, src0, src1); }
    constexpr auto bitwiseOr(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseOr(dest, dest, src); }
    constexpr auto bitwiseXor(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { return BitwiseXor(dest, src0, src1); }
    constexpr auto bitwiseXor(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseXor(dest, dest, src); }
    constexpr auto bitwiseNor(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { return BitwiseNor(dest, src0, src1); }
    constexpr auto bitwiseNor(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseNor(dest, dest, src); }
    constexpr auto bitwiseNand(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { return BitwiseNand(dest, src0, src1); }
    constexpr auto bitwiseNand(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseNand(dest, dest, src); }
    constexpr auto cube(RegisterIndex dest, RegisterIndex src, RegisterIndex temp) noexcept { return std::make_tuple(square(temp, src), MultiplyInteger(dest, src, temp)); } 
    /**
     * Compute quotient and remainder together
     */
    constexpr auto getDivRemainder(RegisterIndex quotient, RegisterIndex remainder, RegisterIndex numerator, RegisterIndex denominator, OrdinalOperation) noexcept {
        return std::make_tuple(DivideOrdinal(quotient, numerator, denominator), RemainderOrdinal(remainder, numerator, denominator));
    }
    constexpr auto getDivRemainder(RegisterIndex quotient, RegisterIndex remainder, RegisterIndex numerator, RegisterIndex denominator, IntegerOperation) noexcept {
        return std::make_tuple(DivideInteger(quotient, numerator, denominator), RemainderInteger(remainder, numerator, denominator));
    }
    constexpr auto indirectLoadData(RegisterIndex dest, RegisterIndex addr, Byte offset = 0) noexcept {
        return std::make_tuple(loadData(dest, addr, offset), loadData(dest, dest));
    }
    constexpr auto indirectStoreData(RegisterIndex dest, RegisterIndex addr, RegisterIndex temporary, Byte offset = 0) noexcept {
        return std::make_tuple(loadData(temporary, dest, offset),
                               storeData(temporary, addr));
    }
    constexpr auto ret(RegisterIndex link) noexcept                                              { return branch(link); }
#if 0
    constexpr auto swapStackTop(RegisterIndex sp, RegisterIndex t0, RegisterIndex t1) noexcept {
        return std::make_tuple(pop(sp, t0),
                               pop(sp, t1),
                               push(sp, t0),
                               push(sp, t1));
    }
    constexpr auto dropStackTop(RegisterIndex sp) noexcept {
        // hardwired zero will ignore the attempt to write
        return pop(sp, 0_reg);
    }
    constexpr auto rotateStack(RegisterIndex sp, RegisterIndex a, RegisterIndex b, RegisterIndex c) noexcept {
        // ( a b c -- c a b )
        return std::make_tuple(pop(sp, c),
                               pop(sp, b),
                               pop(sp, a),
                               push(sp, c),
                               push(sp, a),
                               push(sp, b));
    }
#endif
} // end namespace iris::instructions
#endif // end IRIS_ENCODING_H__
