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
#include "opcodes.h"

namespace iris {
void
Instruction::setLowestQuarter(Byte value) noexcept {
    _bits = encodeBits<decltype(_bits), decltype(value), 0x00'00'00'FF, 0>(_bits, value);
}
void 
Instruction::setLowerQuarter(Byte value) noexcept {
    _bits = encodeBits<decltype(_bits), decltype(value), 0x00'00'FF'00, 8>(_bits, value);
}
void 
Instruction::setHigherQuarter(Byte value) noexcept {
    _bits = encodeBits<decltype(_bits), decltype(value), 0x00'FF'00'00, 16>(_bits, value);
}
void 
Instruction::setHighestQuarter(Byte value) noexcept {
    _bits = encodeBits<decltype(_bits), decltype(value), 0xFF'00'00'00, 24>(_bits, value);
}
void 
Instruction::setLowerHalf(UnsignedWord value) noexcept {
    _bits = iris::setLowerHalf(_bits, value);
}
void 
Instruction::setUpperHalf(UnsignedWord value) noexcept {
    _bits = iris::setUpperHalf(_bits, value);
}
void
Instruction::setImm16(UnsignedWord value) noexcept {
    setUpperHalf(value);
}
void
Instruction::setImm8(UnsignedByte value) noexcept {
    setHighestQuarter(value);
}
void
Instruction::setArg0(RegisterIndex value) noexcept {
    setLowerQuarter(static_cast<Byte>(value));
}
void
Instruction::setArg1(RegisterIndex value) noexcept {
    setHigherQuarter(static_cast<Byte>(value));
}
void
Instruction::setArg2(RegisterIndex value) noexcept {
    setHighestQuarter(static_cast<Byte>(value));
}
void
Instruction::setOpcode(Opcodes opcode) noexcept {
    setOpcode(static_cast<UnsignedByte>(opcode));
}
void
Instruction::setOpcode(UnsignedByte value) noexcept {
    setLowestQuarter(value);
}

Instruction::Instruction(Opcodes opcode) noexcept {
    setOpcode(opcode);
}


} // end namespace iris
