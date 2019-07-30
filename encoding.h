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
namespace iris::instructions {
#define X(title, fmt) \
    UnsignedDoubleWord title ( const title ## Instruction &);
#include "InstructionFormats.def"
#undef X
    // single instruction aliases useful for ease of use
    UnsignedDoubleWord zeroRegister(RegisterIndex targetRegister);
    UnsignedDoubleWord nop();
    UnsignedDoubleWord twoTimes(RegisterIndex dest, RegisterIndex src);
    UnsignedDoubleWord twoDivide(RegisterIndex dest, RegisterIndex src);
    inline auto twoTimes(RegisterIndex targetRegister) {
        return twoTimes(targetRegister, targetRegister);
    }
    inline auto twoDivide(RegisterIndex value) {
        return twoDivide(value, value);
    }
    inline auto invert(RegisterIndex dest, RegisterIndex src) {
        return ArithmeticBitwiseNot({dest, src});
    }
    inline auto invert(RegisterIndex dest) { 
        return invert(dest, dest); 
    }
    inline auto square(RegisterIndex dest, RegisterIndex src) {
        return ArithmeticMultiplySigned({dest, src, src });
    }
    inline auto square(RegisterIndex dest) {
        return square(dest, dest);
    }
    inline auto greaterThanZero(RegisterIndex dest, RegisterIndex src) {
        return CompareGreaterThanSignedImmediate8({dest, src, 0});
    }
    inline auto lessThanZero(RegisterIndex dest, RegisterIndex src) {
        return CompareLessThanSignedImmediate8({dest, src, 0});
    }
    inline auto equalsZero(RegisterIndex dest, RegisterIndex src) {
        return CompareEqualsImmediate8Instruction({dest, src, 0});
    }
    inline auto notEqualsZero(RegisterIndex dest, RegisterIndex src) {
        return CompareNotEqualsImmediate8Instruction({dest, src, 0});
    }
    inline auto increment(RegisterIndex target) {
        return ArithmeticAddUnsignedImmediate({target, target, 1});
    }
    inline auto decrement(RegisterIndex target) {
        return ArithmeticAddSignedImmediate({target, target, -1});
    }
    inline auto ret(RegisterIndex link) {
        return iris::instructions::BranchRegister(link);
    }
    auto call(RegisterIndex, RegisterIndex);
    auto call(RegisterIndex, Address);
    auto branchIfZero(RegisterIndex, RegisterIndex, RegisterIndex);
    auto branchIfZero(RegisterIndex, RegisterIndex, Address);
} // end namespace iris::instructions
#endif // end IRIS_ENCODING_H__
