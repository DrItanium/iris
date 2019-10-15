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
    class MultiInstructionExpression;
    /**
     * Encode an instruction while having parameter access to the expression that is meant to hold onto it
     */
    using ExternalDelayedBits = std::function<Bits(MultiInstructionExpression&, size_t)>;
    using ListContents = std::variant<Bits, DelayedBits, ExternalDelayedBits>;
    using List = std::vector<ListContents>;
    using ComplexBinaryInstruction = std::tuple<Bits, Bits>;
    class OrdinalOperation { };
    class IntegerOperation { };
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
            void addInstruction(ComplexBinaryInstruction);
            void addInstruction(MultiInstructionExpression&&);
            void addInstruction(Body b);
            template<typename ... Args>
            void addInstructions(Args&& ... instructions) {
                (addInstruction(std::forward<Args>(instructions)), ...);
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
        private:
            List _instructions;
            std::list<size_t> _dataStack;
    };

#define X(title, fmt) \
    constexpr Bits title ( const title ## Instruction & i) noexcept { \
        return static_cast<Instruction>(i).getRawBits(); \
    }
#include "InstructionFormats.def"
#undef X
    using AddressTypes = std::variant<RegisterIndex, Address>;
    // single instruction aliases useful for ease of use
    constexpr Bits zeroRegister(RegisterIndex targetRegister) noexcept                          { return MemoryCopyRegister({targetRegister, 0_reg}); }
    constexpr Bits nop(RegisterIndex target = 0_reg) noexcept                                   { return MemoryCopyRegister({target, target}); }
    constexpr Bits greaterThanZero(RegisterIndex dest, RegisterIndex src)  noexcept             { return CompareGreaterThanSigned({dest, src, 0_reg}); }
    constexpr Bits greaterThanOrEqualToZero(RegisterIndex dest, RegisterIndex src) noexcept     { return CompareGreaterThanOrEqualToSigned({dest, src, 0_reg}); }
    constexpr Bits lessThanZero(RegisterIndex dest, RegisterIndex src) noexcept                 { return CompareLessThanSigned({dest, src, 0_reg}); }
    constexpr Bits lessThanOrEqualToZero(RegisterIndex dest, RegisterIndex src)  noexcept       { return CompareLessThanOrEqualToSigned({dest, src, 0_reg}); }
    constexpr Bits equalsZero(RegisterIndex dest, RegisterIndex src)  noexcept                  { return CompareEquals({dest, src, 0_reg}); }
    constexpr Bits notEqualsZero(RegisterIndex dest, RegisterIndex src)  noexcept               { return CompareNotEquals({dest, src, 0_reg}); }
    constexpr Bits swap(RegisterIndex a, RegisterIndex b) noexcept                              { return MemorySwapRegisters({a, b}); }
    constexpr Bits select(RegisterIndex cond, RegisterIndex then, RegisterIndex _else) noexcept { return BranchSelect({cond, then, _else}); }
    template<typename T>
    constexpr Bits call(RegisterIndex link, T value) noexcept {
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address> || std::is_unsigned_v<K>) {
            return BranchImmediateAndLink({link, value});
        } else if constexpr (std::is_same_v<K, Offset16> || std::is_signed_v<K>) {
            return BranchRelativeImmediateAndLink({link, static_cast<Offset16>(value)});
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return BranchRegisterAndLink({value, link });
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    template<typename T>
    constexpr Bits branchConditional(RegisterIndex cond, T addr) noexcept {
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address> || std::is_unsigned_v<K>) {
            return BranchConditionalImmediate({cond, static_cast<Address>(addr)});
        } else if constexpr (std::is_same_v<K, Offset16> || std::is_signed_v<K>) {
            return BranchConditionalRelativeImmediate({cond, static_cast<Offset16>(addr)});
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return BranchConditionalRegister({addr, cond});
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    template<typename T>
    constexpr Bits branch(T addr) noexcept {
        // unconditional branches are branches where the conditional register is hardwired
        // to 1
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address> || std::is_unsigned_v<K>) {
            return branchConditional(1_reg, static_cast<Address>(addr));
        } else if constexpr (std::is_same_v<K, Offset16> || std::is_signed_v<K>) {
            return branchConditional(1_reg, static_cast<Offset16>(addr));
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return branchConditional(1_reg, addr);
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    template<typename T>
    constexpr Bits move(RegisterIndex dest, T value) noexcept {
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address> || std::is_unsigned_v<K>) {
            if (value < 17) {
                // do a register transfer since [0,16] is held within [r0,r16]
                // in a hardwired context
                return MemoryCopyRegister({dest, static_cast<RegisterIndex>(value)});
            } else {
                return MemoryAssignRegisterImmediate({dest, value});
            }
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return MemoryCopyRegister({dest, value});
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    template<typename T>
    constexpr Bits push(RegisterIndex sp, T value) noexcept {
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address> || std::is_unsigned_v<K>) {
            return MemoryStackPushImmediateValue({sp, value});
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return MemoryStackPush({sp, value});
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    constexpr Bits pop(RegisterIndex sp, RegisterIndex dest) noexcept { return MemoryStackPop({sp, dest}); }
    constexpr Bits storeData(RegisterIndex dest, RegisterIndex value, UnsignedByte offset = 0) noexcept { return MemoryDataStoreWithOffset({dest, value, offset}); }
    constexpr Bits storeData(RegisterIndex dest, Address value) noexcept { return MemoryDataStoreImmediateValue({dest, value}); }
    constexpr Bits loadData(RegisterIndex addr, RegisterIndex value, UnsignedByte offset = 0) noexcept { return MemoryDataLoadWithOffset({addr, value, offset}); }
    constexpr Bits storeIO(RegisterIndex addr, RegisterIndex value, UnsignedByte offset = 0) noexcept { return MemoryIOStoreWithOffset({addr, value, offset}); }
    constexpr Bits storeIO(RegisterIndex addr, Address value) noexcept { return MemoryIOStoreImmediateValue({addr, value}); }
    constexpr Bits loadIO(RegisterIndex addr, RegisterIndex dest, UnsignedByte offset) noexcept { return MemoryIOLoadWithOffset({addr, dest, offset}); }
    constexpr Bits loadCode(RegisterIndex addr, RegisterIndex lower, UnsignedByte offset) noexcept { return MemoryCodeLoadWithOffset({addr, lower, offset}); }
    constexpr Bits storeCode(RegisterIndex addr, RegisterIndex lower, UnsignedByte offset) noexcept { return MemoryCodeStoreWithOffset({addr, lower, offset}); }
    constexpr Bits getIP(RegisterIndex dest) noexcept { return MemoryMoveFromIP({dest}); }
    constexpr Bits setIP(RegisterIndex dest) noexcept { return MemoryMoveToIP({dest}); }

    // arithmetic operations
    constexpr Bits twoTimes(RegisterIndex dest, RegisterIndex src, OrdinalOperation) noexcept { return ArithmeticShiftLeftUnsigned({dest, src, 1_reg}); }
    constexpr Bits twoTimes(RegisterIndex dest, RegisterIndex src, IntegerOperation) noexcept { return ArithmeticShiftLeftSigned({dest, src, 1_reg}); }
    constexpr Bits twoTimes(RegisterIndex val, OrdinalOperation op) noexcept { return twoTimes(val, val, op); }
    constexpr Bits twoTimes(RegisterIndex val, IntegerOperation op) noexcept { return twoTimes(val, val, op); }
    constexpr Bits twoDivide(RegisterIndex dest, RegisterIndex src, OrdinalOperation) noexcept { return ArithmeticShiftRightUnsigned({dest, src, 1_reg}); }
    constexpr Bits twoDivide(RegisterIndex dest, RegisterIndex src, IntegerOperation) noexcept { return ArithmeticShiftRightSigned({dest, src, 1_reg}); }
    constexpr Bits twoDivide(RegisterIndex val, OrdinalOperation op) noexcept { return twoDivide(val, val, op); }
    constexpr Bits twoDivide(RegisterIndex val, IntegerOperation op) noexcept { return twoDivide(val, val, op); }
    constexpr Bits invert(RegisterIndex dest, RegisterIndex src) noexcept { return LogicalBitwiseNot({dest, src}); }
    constexpr Bits invert(RegisterIndex dest)  noexcept { return invert(dest, dest); }
    constexpr Bits square(RegisterIndex dest, RegisterIndex src)  noexcept { return ArithmeticMultiplySigned({dest, src, src}); }
    constexpr Bits square(RegisterIndex dest) noexcept { return square(dest, dest); }
    constexpr Bits increment(RegisterIndex target)  noexcept { return ArithmeticAddUnsigned({target, target, 1_reg}); }
    constexpr Bits decrement(RegisterIndex target)  noexcept { return ArithmeticSubtractUnsigned({target, target, 1_reg}); }
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
    X(Max);
    X(Min);
#undef X
    constexpr ComplexBinaryInstruction branchIfZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex dest) noexcept {
        return std::make_tuple(equalsZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr ComplexBinaryInstruction branchIfZero(RegisterIndex cond, RegisterIndex src0, Offset16 dest) noexcept {
        return std::make_tuple(equalsZero(cond, src0), branchConditional(cond, dest));
    }
    constexpr ComplexBinaryInstruction branchIfZero(RegisterIndex cond, RegisterIndex src0, Address dest) noexcept {
        return std::make_tuple(equalsZero(cond, src0), branchConditional(cond, dest));
    }
    MultiInstructionExpression branchIfNotZero(RegisterIndex, AddressTypes) noexcept;
    MultiInstructionExpression branchIfGreaterThanZero(RegisterIndex, RegisterIndex, AddressTypes) noexcept ;
    MultiInstructionExpression branchIfLessThanZero(RegisterIndex, RegisterIndex, AddressTypes) noexcept ;
    MultiInstructionExpression branchIfGreaterThanOrEqualToZero(RegisterIndex, RegisterIndex, AddressTypes) noexcept ;
    MultiInstructionExpression branchIfLessThanOrEqualToZero(RegisterIndex, RegisterIndex, AddressTypes) noexcept ;
    MultiInstructionExpression selectIfZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept;
    MultiInstructionExpression selectIfNotZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept;
    MultiInstructionExpression selectIfGreaterThanZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept;
    MultiInstructionExpression selectIfLessThanZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept;
    MultiInstructionExpression selectIfGreaterThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept;
    MultiInstructionExpression selectIfLessThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept;
    constexpr Bits bitwiseNot(RegisterIndex dest, RegisterIndex src) noexcept { return LogicalBitwiseNot({dest, src}); }
    constexpr auto bitwiseNot(RegisterIndex src) noexcept { return bitwiseNot(src, src); }
    constexpr Bits bitwiseAnd(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { return LogicalBitwiseAnd({dest, src0, src1}); }
    constexpr auto bitwiseAnd(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseAnd(dest, dest, src); }
    constexpr Bits bitwiseOr(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { return LogicalBitwiseOr({dest, src0, src1}); }
    constexpr auto bitwiseOr(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseOr(dest, dest, src); }
    constexpr Bits bitwiseXor(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept { return LogicalBitwiseXor({dest, src0, src1}); }
    constexpr auto bitwiseXor(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseXor(dest, dest, src); }
    MultiInstructionExpression bitwiseNor(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept;
    inline auto bitwiseNor(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseNor(dest, dest, src); }
    MultiInstructionExpression bitwiseNand(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept;
    inline auto bitwiseNand(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseNand(dest, dest, src); }
    MultiInstructionExpression halt(RegisterIndex, Address = 0) noexcept;

    MultiInstructionExpression cube(RegisterIndex dest, RegisterIndex src, RegisterIndex temporary) noexcept;
    /**
     * Compute quotient and remainder together
     */
    MultiInstructionExpression getDivRemainder(RegisterIndex quotient, 
                         RegisterIndex remainder, 
                         RegisterIndex numerator,
                         RegisterIndex denominator) noexcept;
    MultiInstructionExpression indirectLoadData(RegisterIndex dest, RegisterIndex addr, UnsignedByte offset = 0) noexcept;
    MultiInstructionExpression indirectStoreData(RegisterIndex dest, RegisterIndex addr, RegisterIndex temporary, UnsignedByte offset = 0) noexcept;
    template<typename ... Args>
    MultiInstructionExpression unconditionalLoop(Args&& ... body) {
        MultiInstructionExpression me;
        me.backwardJumpTarget();
        me.addInstructions(std::forward<Args>(body)...);
        me.backwardJump();
        return me;
    }
    template<typename ... Args>
    MultiInstructionExpression conditionalLoop(RegisterIndex cond, Args&& ... body) {
        MultiInstructionExpression me;
        me.backwardJumpTarget();
        me.addInstructions(std::forward<Args>(body)...);
        me.conditionalBackwardJump(cond);
        return me;
    }
    constexpr Bits ret(RegisterIndex link) noexcept                                              { return branch(link); }
} // end namespace iris::instructions
#endif // end IRIS_ENCODING_H__
