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
#include <variant>
#include <vector>
#include <list>
#include <functional>
namespace iris::instructions {
    using Bits = UnsignedDoubleWord;
    /**
     * Encode an instruction with only its position to have access to. Most useful
     * to functions that are a member of the MultiInstructionExpression. It makes
     * it possible to capture this.
     */
    using DelayedBits = std::function<Bits(size_t)>;
#if 0
    class MultiInstructionExpression;
    /**
     * Encode an instruction while having parameter access to the expression that is meant to hold onto it
     */
    using ExternalDelayedBits = std::function<Bits(MultiInstructionExpression&, size_t)>;
    using ListContents = std::variant<Bits, DelayedBits, ExternalDelayedBits>;
    using List = std::vector<ListContents>;
#endif
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
    using AddressTypes = std::variant<RegisterIndex, Address>;
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
    constexpr auto storeData(RegisterIndex dest, RegisterIndex value, Byte offset) noexcept { return DataStoreWithOffset(dest, value, offset); }
    constexpr auto storeData(RegisterIndex dest, RegisterIndex storage, Address value, Byte offset) noexcept { return std::make_tuple(move(storage, value), storeData(dest, storage, offset)); }
    constexpr auto storeData(RegisterIndex dest, RegisterIndex value) noexcept { return storeData(dest, value, 0); }
    constexpr auto loadData(RegisterIndex addr, RegisterIndex value, Byte offset) noexcept { return DataLoadWithOffset(addr, value, offset); }
    constexpr auto loadData(RegisterIndex addr, RegisterIndex value) noexcept { return loadData(addr, value, 0); }
    constexpr auto storeIO(RegisterIndex addr, RegisterIndex value, Byte offset) noexcept { return IOStoreWithOffset(addr, value, offset); }
    constexpr auto storeIO(RegisterIndex addr, RegisterIndex storage, Address value, Byte offset) noexcept { return std::make_tuple(move(storage, value), storeIO(addr, storage, offset)); }
    constexpr auto storeIO(RegisterIndex addr, RegisterIndex storage) noexcept { return storeIO(addr, storage, 0); }
    constexpr auto loadIO(RegisterIndex addr, RegisterIndex dest, Byte offset) noexcept { return IOLoadWithOffset(addr, dest, offset); }
    constexpr auto loadCode(RegisterIndex addr, RegisterIndex lower, Byte offset) noexcept { return CodeLoadWithOffset(addr, lower, offset); }
    constexpr auto loadCode(RegisterIndex addr, RegisterIndex lower) noexcept { return loadCode(addr, lower, 0); }
    constexpr auto storeCode(RegisterIndex addr, RegisterIndex lower, Byte offset) noexcept { return CodeStoreWithOffset(addr, lower, offset); }
    constexpr auto storeCode(RegisterIndex addr, RegisterIndex lower) noexcept { return storeCode(addr, lower, 0); }
#if 0
    // arithmetic operations
    constexpr auto twoTimes(RegisterIndex dest, RegisterIndex src, OrdinalOperation) noexcept { return ArithmeticShiftLeftUnsigned({dest, src, 1_reg}); }
    constexpr auto twoTimes(RegisterIndex dest, RegisterIndex src, IntegerOperation) noexcept { return ArithmeticShiftLeftSigned({dest, src, 1_reg}); }
    constexpr auto twoTimes(RegisterIndex val, OrdinalOperation op) noexcept { return twoTimes(val, val, op); }
    constexpr auto twoTimes(RegisterIndex val, IntegerOperation op) noexcept { return twoTimes(val, val, op); }
    constexpr auto twoDivide(RegisterIndex dest, RegisterIndex src, OrdinalOperation) noexcept { return ArithmeticShiftRightUnsigned({dest, src, 1_reg}); }
    constexpr auto twoDivide(RegisterIndex dest, RegisterIndex src, IntegerOperation) noexcept { return ArithmeticShiftRightSigned({dest, src, 1_reg}); }
    constexpr auto twoDivide(RegisterIndex val, OrdinalOperation op) noexcept { return twoDivide(val, val, op); }
    constexpr auto twoDivide(RegisterIndex val, IntegerOperation op) noexcept { return twoDivide(val, val, op); }
    constexpr auto invert(RegisterIndex dest, RegisterIndex src) noexcept { return LogicalBitwiseNot({dest, src}); }
    constexpr auto invert(RegisterIndex dest)  noexcept { return invert(dest, dest); }
    constexpr auto square(RegisterIndex dest, RegisterIndex src)  noexcept { return ArithmeticMultiplySigned({dest, src, src}); }
    constexpr auto square(RegisterIndex dest) noexcept { return square(dest, dest); }
    constexpr auto increment(RegisterIndex target)  noexcept { return ArithmeticAddUnsigned({target, target, 1_reg}); }
    constexpr auto decrement(RegisterIndex target)  noexcept { return ArithmeticSubtractUnsigned({target, target, 1_reg}); }
#define X(kind) \
    constexpr auto op ## kind (RegisterIndex dest, RegisterIndex src1, RegisterIndex src2, OrdinalOperation) noexcept { \
        return Arithmetic ## kind ## Unsigned({dest, src1, src2}); \
    } \
    constexpr auto op ## kind (RegisterIndex dest, RegisterIndex src1, RegisterIndex src2, IntegerOperation) noexcept { \
        return Arithmetic ## kind ## Signed({dest, src1, src2}); \
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
    constexpr auto bitwiseNot(RegisterIndex dest, RegisterIndex src) noexcept { return LogicalBitwiseNot({dest, src}); }
    constexpr auto bitwiseNot(RegisterIndex src) noexcept { return bitwiseNot(src, src); }
    constexpr auto bitwiseAnd(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { return LogicalBitwiseAnd({dest, src0, src1}); }
    constexpr auto bitwiseAnd(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseAnd(dest, dest, src); }
    constexpr auto bitwiseOr(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { return LogicalBitwiseOr({dest, src0, src1}); }
    constexpr auto bitwiseOr(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseOr(dest, dest, src); }
    constexpr auto bitwiseXor(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { return LogicalBitwiseXor({dest, src0, src1}); }
    constexpr auto bitwiseXor(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseXor(dest, dest, src); }
    constexpr auto bitwiseNor(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { return std::make_tuple(bitwiseOr(dest, src0, src1), bitwiseNot(dest)); }
    constexpr auto bitwiseNor(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseNor(dest, dest, src); }
    constexpr auto bitwiseNand(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { return std::make_tuple(bitwiseAnd(dest, src0, src1), bitwiseNot(dest)); }
    constexpr auto bitwiseNand(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseNand(dest, dest, src); }
    constexpr auto cube(RegisterIndex dest, RegisterIndex src, RegisterIndex temp) noexcept { return std::make_tuple(square(temp, src), opMultiply(dest, src, temp, OrdinalOperation())); }
    /**
     * Compute quotient and remainder together
     */
    constexpr auto getDivRemainder(RegisterIndex quotient, RegisterIndex remainder, RegisterIndex numerator, RegisterIndex denominator, OrdinalOperation op) noexcept {
        return std::make_tuple(opDivide(quotient, numerator, denominator, op), opRemainder(remainder, numerator, denominator, op));
    }
    constexpr auto getDivRemainder(RegisterIndex quotient, RegisterIndex remainder, RegisterIndex numerator, RegisterIndex denominator, IntegerOperation op) noexcept {
        return std::make_tuple(opDivide(quotient, numerator, denominator, op), opRemainder(remainder, numerator, denominator, op));
    }
    constexpr auto indirectLoadData(RegisterIndex dest, RegisterIndex addr, Byte offset = 0) noexcept {
        return std::make_tuple(loadData(dest, addr, offset), loadData(dest, dest));
    }
    constexpr auto indirectStoreData(RegisterIndex dest, RegisterIndex addr, RegisterIndex temporary, Byte offset = 0) noexcept {
        return std::make_tuple(loadData(temporary, dest, offset),
                               storeData(temporary, addr));
    }
#endif
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
#if 0
    class MultiInstructionExpression {
        public:
            using Body = std::function<void(MultiInstructionExpression&)>;
        public:
            MultiInstructionExpression() = default;
            ~MultiInstructionExpression() = default;
            MultiInstructionExpression(Bits);
            MultiInstructionExpression(DelayedBits);
            MultiInstructionExpression(ExternalDelayedBits);
            MultiInstructionExpression(ComplexBinaryInstruction);
            void addInstruction(Bits);
            void addInstruction(DelayedBits);
            void addInstruction(ExternalDelayedBits);
            void addInstruction(MultiInstructionExpression&&);
            void addInstruction(Body b);
            template<typename ... Args>
            void addInstructions(Args&& ... instructions) {
                (addInstruction(std::forward<Args>(instructions)), ...);
            }
            template<typename Tuple, size_t... Is>
            void addInstruction(Tuple&& t, std::index_sequence<Is...>) {
                addInstructions(std::get<Is>(t)...);
            }
            auto size() const noexcept { return _instructions.size(); }
            bool tooLarge() const noexcept { return size() > 0xFFFF; }
            auto begin() { return _instructions.begin(); }
            auto end() { return _instructions.end(); }
            auto begin() const { return _instructions.cbegin(); }
            auto end() const { return _instructions.cend(); }

            /**
             * Pop the most recent value off of the data stack
             */
            size_t dataPop(); 

            /**
             * Push a value onto the data stack
             * @param value A size_t to be pushed onto the internal data stack
             */
            void dataPush(size_t value);
            /**
             * Push the current position onto the data stack
             */
            inline void mark() { dataPush(size()); }
            /**
             * Add the given delayed instruction to the expression and mark its
             * position. Use resolve to cause the
             * deferred function to be replaced with a final product. Failure to call
             * resolve for each defer may lead to shenanigans and incorrect behavior.
             * @param fn The function to install and 
             */
            void defer(DelayedBits fn);
            /**
             * Add the given delayed instruction to the expression and mark its
             * position. Use resolve to cause the
             * deferred function to be replaced with a final product. Failure to call
             * resolve for each defer may lead to shenanigans and incorrect behavior.
             * @param fn The function to install and 
             */
            void defer(ExternalDelayedBits fn);
            /**
             * Invoke the most recently deferred instruction and put the resultant 
             * value in the place of the generator function. The position on the
             * dataStack is also popped.
             */
            void resolve();
            /**
             * Register a desire to unconditionally jump forward relative to the 
             * current position via defer; At some point forwardJumpTarget must 
             * be called to complete the generation of the jump. 
             */
            void forwardJump();
            /**
             * Register a desire to conditionally jump forward relative to the
             * current position via defer; At some point forwardJumpTarget must
             * be called to complete the generation of the jump.
             * @param cond The register that is used for the conditional evaluation
             */
            void conditionalForwardJump(RegisterIndex cond);
            /**
             * Mark the implied next instruction inserted as the target of the most recent
             * forward jump request; Thus it must be done before the next instruction
             * inserted.
             */
            void forwardJumpTarget();
            /**
             * Mark the position where a backward jump will jump to; Most likely this will
             * be used for a loop of some kind where you jump back to the top.
             */
            void backwardJumpTarget();
            /**
             * Generate an unconditional jump instruction back to the most recent backwardJumpTarget.
             */
            void backwardJump();
            /**
             * Generate an conditional jump instruction back to the most recent backwardJumpTarget.
             * @param cond The register to use for the index
             */
            void conditionalBackwardJump(RegisterIndex cond);

            /**
             * Swap the top two elements of the data stack.
             * @throw Exception Too few elements to perform the swap with
             */
            void dataStackSwap();

            /**
             * Start an if condition statement
             */
            void ifComponent(RegisterIndex idx);
            /**
             * Complete the if statement jump after embedding an unconditional
             * jump request first
             */
            void elseComponent();
            /**
             * Stop a conditional statement
             */
            void thenComponent();

            void ifStatement(RegisterIndex cond, Body onTrue);
            void ifStatement(RegisterIndex cond, Body onTrue, Body onFalse);
            template<typename ... Args>
            void unconditionalLoop(Args&& ... body) {
                backwardJumpTarget();
                addInstructions(std::forward<Args>(body)...);
                backwardJump();
            }
            template<typename ... Args>
            void conditionalLoop(RegisterIndex cond, Args&& ... body) {
                backwardJumpTarget();
                addInstructions(std::forward<Args>(body)...);
                conditionalBackwardJump(cond);
            }
        private:
            List _instructions;
            std::list<size_t> _dataStack;
    };
#endif
} // end namespace iris::instructions
#endif // end IRIS_ENCODING_H__
