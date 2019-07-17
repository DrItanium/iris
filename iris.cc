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

namespace iris {

void
DoubleRegister::put(Word lower, Word upper) noexcept {
    _lower.put(lower);
    _upper.put(upper);
}

DoubleRegister
makePair(RegisterBank& bank, RegisterIndex lower, RegisterIndex upper) noexcept {
    if constexpr (!std::is_same_v<Byte, std::underlying_type_t<RegisterIndex>>) {
        constexpr RegisterIndex registerBankMask = RegisterIndex(0xFF);
        return DoubleRegister(bank[Byte(lower & registerBankMask)], bank[Byte(upper & registerBankMask)]);
    } else {
        return DoubleRegister(bank[Byte(lower)], bank[Byte(upper)]);
    }
}

DoubleRegister
makePair(RegisterBank& bank, RegisterIndex lower) noexcept {
    auto conv = static_cast<Byte>(lower);
    return makePair(bank, lower, static_cast<RegisterIndex>(conv + 1));
}

void 
Core::invoke(DoubleWord ibits) {
    Instruction inst(ibits);
    if (auto operation = decodeInstruction(inst); operation) {
        std::visit([this](auto&& op) { invoke(op); });
    } else {
        throw "Bad operation!";
    }
}
void
Core::invoke(const ThreeRegisterFormat& fmt) {

#define CAT(a, b) PRIMITIVE_CAT(a, b)
#define PRIMITIVE_CAT(a, b) a ## b
constexpr std::optional<DecodedInstruction> decodeInstruction(const Instruction& inst) noexcept {
    if (auto op = inst.decodeOperation(); op) {
    return std::visit([&inst](auto&& value) -> std::optional<DecodedInstruction> {
                using K = std::decay_t<decltype(value)>;
#define MakeCase(op) case K :: op : return InstructionArgumentFormat<K :: op >(inst)
#define Y(g, op, f) PRIMITIVE_CAT(Action, g)(op, f)
#define X(g, op, f) Y(g, op, f)
                if constexpr (std::is_same_v<K, ArithmeticKind>) {
                switch (value) {
#define ActionArithmetic2(op, f)
#define ActionBranch(op, f)
#define ActionCompare(op, f)
#define ActionMemory(op, f)
#define ActionArithmetic(op, f) MakeCase(op);
#include "InstructionFormats.def"
#undef ActionArithmetic2
#undef ActionArithmetic 
#undef ActionBranch 
#undef ActionCompare 
#undef ActionMemory 
                    default:
                        return std::nullopt;
                }
                } else if constexpr (std::is_same_v<K, Arithmetic2Kind>) {
                switch (value) {
#define ActionArithmetic(op, f)
#define ActionBranch(op, f)
#define ActionCompare(op, f)
#define ActionMemory(op, f)
#define ActionArithmetic2(op, f) MakeCase(op);
#include "InstructionFormats.def"
#undef ActionArithmetic2
#undef ActionArithmetic 
#undef ActionBranch 
#undef ActionCompare 
#undef ActionMemory
                    default:
                        return std::nullopt;
                }
                } else if constexpr (std::is_same_v<K, BranchKind>) {
                switch (value) {
#define ActionArithmetic(op, f)
#define ActionBranch(op, f) MakeCase(op);
#define ActionCompare(op, f)
#define ActionMemory(op, f)
#define ActionArithmetic2(op, f) 
#include "InstructionFormats.def"
#undef ActionArithmetic2
#undef ActionArithmetic 
#undef ActionBranch 
#undef ActionCompare 
#undef ActionMemory
                    default:
                        return std::nullopt;
                }
                } else if constexpr (std::is_same_v<K, CompareKind>) {
                switch (value) {
#define ActionArithmetic(op, f)
#define ActionBranch(op, f) 
#define ActionCompare(op, f) MakeCase(op);
#define ActionMemory(op, f)
#define ActionArithmetic2(op, f) 
#include "InstructionFormats.def"
#undef ActionArithmetic2
#undef ActionArithmetic 
#undef ActionBranch 
#undef ActionCompare 
#undef ActionMemory
                    default:
                        return std::nullopt;
                }
                } else if constexpr (std::is_same_v<K, MemoryKind>) {
                switch (value) {
#define ActionArithmetic(op, f)
#define ActionBranch(op, f) 
#define ActionCompare(op, f) 
#define ActionMemory(op, f) MakeCase(op);
#define ActionArithmetic2(op, f) 
#include "InstructionFormats.def"
#undef ActionArithmetic2
#undef ActionArithmetic 
#undef ActionBranch 
#undef ActionCompare 
#undef ActionMemory
#undef X
#undef Y
#undef MakeCase
}

} // end namespace iris
