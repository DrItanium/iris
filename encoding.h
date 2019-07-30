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
#include <variant>
namespace iris::instructions {
    using Bits = UnsignedDoubleWord;
#define X(title, fmt) \
    Bits title ( const title ## Instruction &) noexcept;
#include "InstructionFormats.def"
#undef X
    using AddressTypes = std::variant<RegisterIndex, Address>;
    // single instruction aliases useful for ease of use
    Bits zeroRegister(RegisterIndex targetRegister) noexcept ;
    Bits nop(RegisterIndex target = 0_r) noexcept;
    Bits twoTimes(RegisterIndex dest, RegisterIndex src) noexcept ;
    Bits twoDivide(RegisterIndex dest, RegisterIndex src) noexcept ;
    Bits twoTimes(RegisterIndex targetRegister)  noexcept ;
    Bits twoDivide(RegisterIndex value)  noexcept ;
    Bits invert(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits invert(RegisterIndex dest)  noexcept ; 
    Bits square(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits square(RegisterIndex dest)  noexcept ;
    Bits greaterThanZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits greaterThanOrEqualToZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits lessThanZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits lessThanOrEqualToZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits equalsZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits notEqualsZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    Bits increment(RegisterIndex target)  noexcept ;
    Bits decrement(RegisterIndex target)  noexcept ;
    Bits ret(RegisterIndex) noexcept ;
    template<typename T>
    Bits call(RegisterIndex link, T value) noexcept {
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address>) {
            return BranchImmediateAndLink({link, value});
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return BranchRegisterAndLink({value, link });
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    Bits swap(RegisterIndex, RegisterIndex) noexcept;
    template<typename T>
    auto branchConditional(RegisterIndex cond, T addr) noexcept {
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address>) {
            return BranchConditionalImmediate({cond, addr});
        } else if constexpr (std::is_same_v<K, RegisterIndex>) {
            return BranchConditionalRegister({addr, cond});
        } else {
            static_assert(false_v<T>, "Bad kind to branch with!");
        }
    }
    template<typename T>
    auto branch(T addr) noexcept {
        using K = std::decay_t<T>;
        if constexpr (std::is_same_v<K, Address>) {
            return BranchImmediate({addr});
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
        if constexpr (std::is_same_v<K, Address>) {
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
        if constexpr (std::is_same_v<K, Address>) {
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

} // end namespace iris::instructions
#endif // end IRIS_ENCODING_H__
