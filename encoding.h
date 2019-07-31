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
namespace iris::instructions {
    using Bits = UnsignedDoubleWord;
    template<size_t Size, typename ... Args>
    constexpr auto count(std::tuple<Args...>) noexcept;
    
    template<typename T>
    constexpr auto count(T) noexcept {
        return 1;
    }
    template<typename ... Args>
    constexpr auto count(std::tuple<Args...> tup) noexcept {
        return count<std::tuple_size_v<decltype(tup)>, Args...>(tup);
    }
    template<typename A, typename B>
    constexpr auto count(std::pair<A, B> pair) noexcept {
        return count(std::tuple(pair));
    }
    
    template<size_t Size, typename ... Args>
    constexpr auto count(std::tuple<Args...> tup) noexcept {
        if constexpr (constexpr auto index = Size - 1; Size == 0) {
            return 0;
        } else {
            return count(std::get<index>(tup)) +
                   count<index, Args...>(tup);
        }
    }
    template<typename ... Args>
    constexpr auto count(Args&& ... tup) noexcept {
        // construct the types
        return count(std::make_tuple<Args...>(std::forward<Args>(tup)...));
    }
#define X(title, fmt) \
    Bits title ( const title ## Instruction &) noexcept;
#include "InstructionFormats.def"
#undef X
    using AddressTypes = std::variant<RegisterIndex, Address>;
    // single instruction aliases useful for ease of use
    Bits zeroRegister(RegisterIndex targetRegister) noexcept ;
    Bits nop(RegisterIndex target = 0_r) noexcept;
    Bits greaterThanZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits greaterThanOrEqualToZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits lessThanZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits lessThanOrEqualToZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits equalsZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits notEqualsZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits ret(RegisterIndex) noexcept ;
    template<typename T>
    Bits call(RegisterIndex link, T value) noexcept {
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
    Bits swap(RegisterIndex, RegisterIndex) noexcept;
    template<typename T>
    Bits branchConditional(RegisterIndex cond, T addr) noexcept {
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address> || std::is_unsigned_v<K>) {
            return BranchConditionalImmediate({cond, addr});
        } else if constexpr (std::is_same_v<K, Offset16> || std::is_signed_v<K>) {
            return BranchConditionalRelativeImmediate({cond, static_cast<Offset16>(addr)});
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return BranchConditionalRegister({addr, cond});
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    template<typename T>
    Bits branch(T addr) noexcept {
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address> || std::is_unsigned_v<K>) {
            return BranchImmediate({addr});
        } else if constexpr (std::is_same_v<K, Offset16> || std::is_signed_v<K>) {
            return BranchRelativeImmediate({static_cast<Offset16>(addr)});
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return BranchRegister({addr});
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    auto branchIfZero(RegisterIndex, RegisterIndex, AddressTypes) noexcept;
    auto branchIfNotZero(RegisterIndex, AddressTypes) noexcept ;
    auto branchIfGreaterThanZero(RegisterIndex, RegisterIndex, AddressTypes) noexcept ;
    auto branchIfLessThanZero(RegisterIndex, RegisterIndex, AddressTypes) noexcept ;
    auto branchIfGreaterThanOrEqualToZero(RegisterIndex, RegisterIndex, AddressTypes) noexcept ;
    auto branchIfLessThanOrEqualToZero(RegisterIndex, RegisterIndex, AddressTypes) noexcept ;
    Bits select(RegisterIndex cond, RegisterIndex then, RegisterIndex _else) noexcept;
    auto selectIfZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept;
    auto selectIfNotZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept;
    auto selectIfGreaterThanZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept;
    auto selectIfLessThanZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept;
    auto selectIfGreaterThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept;
    auto selectIfLessThanOrEqualToZero(RegisterIndex cond, RegisterIndex src0, RegisterIndex then, RegisterIndex _else) noexcept;
    template<typename T>
    Bits move(RegisterIndex dest, T value) noexcept {
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address> || std::is_unsigned_v<K>) {
            return MemoryAssignRegisterImmediate({dest, value});
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return MemoryCopyRegister({dest, value});
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    template<typename T>
    Bits push(RegisterIndex sp, T value) noexcept {
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address> || std::is_unsigned_v<K>) {
            return MemoryStackPushImmediateValue({sp, value});
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return MemoryStackPush({sp, value});
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    Bits pop(RegisterIndex sp, RegisterIndex dest) noexcept;
    Bits storeData(RegisterIndex dest, RegisterIndex value, UnsignedByte offset = 0) noexcept;
    Bits storeData(RegisterIndex dest, Address value) noexcept; 
    Bits loadData(RegisterIndex dest, RegisterIndex value, UnsignedByte offset = 0) noexcept;
    Bits storeIO(RegisterIndex dest, RegisterIndex value, UnsignedByte offset = 0) noexcept;
    Bits storeIO(RegisterIndex dest, Address value) noexcept; 
    Bits loadIO(RegisterIndex dest, RegisterIndex value, UnsignedByte offset = 0) noexcept;
    Bits storeCode(RegisterIndex dest, RegisterIndex lower, UnsignedByte offset = 0) noexcept;
    Bits loadCode(RegisterIndex dest, RegisterIndex lower, UnsignedByte offset = 0) noexcept;
    Bits storeCodeThenIncrement(RegisterIndex dest, RegisterIndex lower, UnsignedByte amount = 1) noexcept;
    Bits loadCodeThenIncrement(RegisterIndex dest, RegisterIndex lower, UnsignedByte amount = 1) noexcept;
    Bits storeCodeThenDecrement(RegisterIndex dest, RegisterIndex lower, UnsignedByte amount = 1) noexcept;
    Bits loadCodeThenDecrement(RegisterIndex dest, RegisterIndex lower, UnsignedByte amount = 1) noexcept;
    Bits getIP(RegisterIndex) noexcept;
    Bits setIP(RegisterIndex) noexcept;

    /// @todo Double and Quad memory operations

    // arithmetic operations
    Bits twoTimes(RegisterIndex dest, RegisterIndex src) noexcept ;
    Bits twoDivide(RegisterIndex dest, RegisterIndex src) noexcept ;
    Bits twoTimes(RegisterIndex targetRegister)  noexcept ;
    Bits twoDivide(RegisterIndex value)  noexcept ;
    Bits invert(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits invert(RegisterIndex dest)  noexcept ; 
    Bits square(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits square(RegisterIndex dest)  noexcept ;
    Bits increment(RegisterIndex target)  noexcept ;
    Bits decrement(RegisterIndex target)  noexcept ;
#define X(kind) \
    template<typename T> \
    Bits op ## kind ## Signed (RegisterIndex dest, RegisterIndex src0, T src1) noexcept { \
        using K = std::decay_t<T>; \
        if constexpr (std::is_same_v<K, SignedByte>) { \
            return Arithmetic ## kind ## SignedImmediate({dest, src0, src1}); \
        } else if constexpr (std::is_same_v<K, RegisterIndex>) { \
            return Arithmetic ## kind ## Signed({dest, src0, src1}); \
        } else { \
            static_assert(false_v<T>, "Bad kind to " #kind " with!"); \
        } \
    } \
    template<typename T> \
    inline auto op ## kind ## Signed (RegisterIndex dest, T src) noexcept { \
        return op ## kind ## Signed( dest, dest, src); \
    } \
    template<typename T> \
    Bits op ## kind ## Unsigned (RegisterIndex dest, RegisterIndex src0, T src1) noexcept { \
        using K = std::decay_t<T>; \
        if constexpr (std::is_same_v<K, UnsignedByte>) { \
            return Arithmetic ## kind ## UnsignedImmediate({dest, src0, src1}); \
        } else if constexpr (std::is_same_v<K, RegisterIndex>) { \
            return Arithmetic ## kind ## Unsigned({dest, src0, src1}); \
        } else { \
            static_assert(false_v<T>, "Bad kind to " #kind " with!"); \
        } \
    } \
    template<typename T> \
    inline auto op ## kind ## Unsigned (RegisterIndex dest, T src) noexcept { \
        return op ## kind ## Unsigned( dest, dest, src); \
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
    Bits bitwiseNot(RegisterIndex dest, RegisterIndex src) noexcept;
    inline auto bitwiseNot(RegisterIndex src) noexcept { return bitwiseNot(src, src); }
    Bits bitwiseAnd(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept;
    inline auto bitwiseAnd(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseAnd(dest, dest, src); }
    Bits bitwiseOr(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept;
    inline auto bitwiseOr(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseOr(dest, dest, src); }
    Bits bitwiseXor(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept;
    inline auto bitwiseXor(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseXor(dest, dest, src); }
    Bits bitwiseNor(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept;
    inline auto bitwiseNor(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseNor(dest, dest, src); }
    Bits bitwiseNand(RegisterIndex dest, RegisterIndex src0, RegisterIndex src1) noexcept;
    inline auto bitwiseNand(RegisterIndex dest, RegisterIndex src) noexcept { return bitwiseNand(dest, dest, src); }
    auto halt(RegisterIndex, Address = 0) noexcept;

    auto cube(RegisterIndex dest, RegisterIndex src, RegisterIndex temporary) noexcept;
    /**
     * Compute quotient and remainder together
     */
    auto getDivRemainder(RegisterIndex quotient, 
                         RegisterIndex remainder, 
                         RegisterIndex numerator,
                         RegisterIndex denominator) noexcept;
    auto indirectLoadData(RegisterIndex dest, RegisterIndex addr, UnsignedByte offset = 0) noexcept;
    auto indirectStoreData(RegisterIndex dest, RegisterIndex addr, RegisterIndex temporary, UnsignedByte offset = 0) noexcept;
    template<typename ... Types>
    using LoopReturnKind = std::variant<std::tuple<Bits, Bits>,
              std::tuple<std::tuple<Types...>, Bits>>;
    template<typename ... Types>
    LoopReturnKind<Types...>
    unconditionalLoop(std::tuple<Types...> tup) {
        if (auto leaves = count(tup); leaves == 0) {
            return std::make_tuple(nop(), branch(-1));
        } else if (leaves < 0x8000) {
            return std::make_tuple(tup, branch(-leaves));
        } else {
            throw Exception("loop is too large!");
        }
    }
    template<typename ... Args>
    auto unconditionalLoop(Args&& ... values) {
        return unconditionalLoop(std::make_tuple(std::forward<Args>(values)...));
    }
    template<typename ... Types>
    LoopReturnKind<Types...> 
    conditionalLoop(RegisterIndex condition, std::tuple<Types...> tup) {
        if (auto leaves = count(tup); leaves == 0) {
            return std::make_tuple(nop, branchConditional(condition, -1));
        } else if (leaves < 0x8000) {
            return std::make_tuple(tup, branchConditional(condition, -leaves));
        } else {
            throw Exception("loop is too large!");
        }
    }
    template<typename ... Args>
    auto conditionalLoop(RegisterIndex cond, Args&& ... values) {
        return conditionalLoop(cond, std::make_tuple(std::forward<Args>(values)...));
    }
} // end namespace iris::instructions
#endif // end IRIS_ENCODING_H__
