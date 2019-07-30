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

#include "encoding.h"

namespace iris::instructions {
#define X(title, fmt) \
    UnsignedDoubleWord \
    title ( const title ## Instruction & s) {\
        return ((Instruction)s).getRawBits(); \
    }
#include "InstructionFormats.def"
#undef X
auto
zeroRegister(RegisterIndex t) {
    return MemoryAssignRegisterImmediate({t, 0x0000_imm16});
}
auto
nop() {
    return MemorySwapRegisters({0_r, 0_r});
}
auto
twoTimes(RegisterIndex dest, RegisterIndex src) {
    return ArithmeticShiftLeftUnsignedImmediate({dest, src, 1});
}
auto
twoDivide(RegisterIndex dest, RegisterIndex src) {
    return ArithmeticShiftRightUnsignedImmediate({dest, src, 1});
}
auto 
call(RegisterIndex link, RegisterIndex reg) {
    return BranchRegisterAndLinkInstruction({ reg, link });
}
auto 
call(RegisterIndex link, Address imm16) {
    return BranchImmediateAndLinkInstruction({link, imm16});
}
auto 
ret(RegisterIndex link) {
    return iris::instructions::BranchRegister(link);
}

auto 
twoTimes(RegisterIndex targetRegister) {
    return twoTimes(targetRegister, targetRegister);
}
auto 
twoDivide(RegisterIndex value) {
    return twoDivide(value, value);
}
auto 
invert(RegisterIndex dest, RegisterIndex src) {
    return ArithmeticBitwiseNot({dest, src});
}
auto 
invert(RegisterIndex dest) { 
    return invert(dest, dest); 
}
auto 
square(RegisterIndex dest, RegisterIndex src) {
    return ArithmeticMultiplySigned({dest, src, src });
}
auto 
square(RegisterIndex dest) {
    return square(dest, dest);
}
auto 
greaterThanZero(RegisterIndex dest, RegisterIndex src) {
    return CompareGreaterThanSignedImmediate8({dest, src, 0});
}
auto 
lessThanZero(RegisterIndex dest, RegisterIndex src) {
    return CompareLessThanSignedImmediate8({dest, src, 0});
}
auto 
equalsZero(RegisterIndex dest, RegisterIndex src) {
    return CompareEqualsImmediate8Instruction({dest, src, 0});
}
auto 
notEqualsZero(RegisterIndex dest, RegisterIndex src) {
    return CompareNotEqualsImmediate8Instruction({dest, src, 0});
}
auto 
increment(RegisterIndex target) {
    return ArithmeticAddUnsignedImmediate({target, target, 1});
}
auto 
decrement(RegisterIndex target) {
    return ArithmeticAddSignedImmediate({target, target, -1});
}
auto 
branchIfZero(RegisterIndex temp, RegisterIndex src, RegisterIndex loc) {
    return std::make_tuple(
            equalsZero(temp, src),
            BranchConditionalRegister({loc, temp}));
}
auto 
branchIfZero(RegisterIndex temp, RegisterIndex src, Address loc) {
    return std::make_tuple(
            equalsZero(temp, src),
            BranchConditionalImmediate({temp, loc}));
}

} // end namespace iris::instructions
