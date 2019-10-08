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

#include "register.h"

namespace iris {
void
DoubleRegister::put(Word lower, Word upper) noexcept {
    _lower.put(lower);
    _upper.put(upper);
}

DoubleRegister
DoubleRegister::make(RegisterBank& bank, RegisterIndex lower, RegisterIndex upper) noexcept {
    if constexpr (!std::is_same_v<Byte, std::underlying_type_t<RegisterIndex>>) {
        constexpr RegisterIndex registerBankMask = RegisterIndex(0xFF);
        return DoubleRegister(bank[Byte(lower & registerBankMask)], bank[Byte(upper & registerBankMask)]);
    } else {
        return DoubleRegister(bank[Byte(lower)], bank[Byte(upper)]);
    }
}

const DoubleRegister
DoubleRegister::make(const RegisterBank& bank, RegisterIndex lower, RegisterIndex upper) noexcept {
    if constexpr (!std::is_same_v<Byte, std::underlying_type_t<RegisterIndex>>) {
        constexpr RegisterIndex registerBankMask = RegisterIndex(0xFF);
        return DoubleRegister(bank[Byte(lower & registerBankMask)], bank[Byte(upper & registerBankMask)]);
    } else {
        return DoubleRegister(bank[Byte(lower)], bank[Byte(upper)]);
    }
}

DoubleRegister
DoubleRegister::make(RegisterBank& bank, RegisterIndex lower) noexcept {
    auto conv = static_cast<Byte>(lower);
    return make(bank, lower, static_cast<RegisterIndex>(conv + 1));
}
const DoubleRegister
DoubleRegister::make(const RegisterBank& bank, RegisterIndex lower) noexcept {
    auto conv = static_cast<Byte>(lower);
    return make(bank, lower, static_cast<RegisterIndex>(conv + 1));
}

constexpr RegisterIndex increment(RegisterIndex input, std::underlying_type_t<RegisterIndex> by = 1) noexcept {
    return static_cast<RegisterIndex>(static_cast<std::underlying_type_t<RegisterIndex>>(input) + by);
}
} // end namespace iris
