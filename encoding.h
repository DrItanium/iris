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
    UnsignedDoubleWord title ( const title ## Instruction &) noexcept;
#include "InstructionFormats.def"
#undef X
    // single instruction aliases useful for ease of use
    auto zeroRegister(RegisterIndex targetRegister) noexcept ;
    auto nop() noexcept ;
    auto twoTimes(RegisterIndex dest, RegisterIndex src) noexcept ;
    auto twoDivide(RegisterIndex dest, RegisterIndex src) noexcept ;
    auto twoTimes(RegisterIndex targetRegister)  noexcept ;
    auto twoDivide(RegisterIndex value)  noexcept ;
    auto invert(RegisterIndex dest, RegisterIndex src)  noexcept ;
    auto invert(RegisterIndex dest)  noexcept ; 
    auto square(RegisterIndex dest, RegisterIndex src)  noexcept ;
    auto square(RegisterIndex dest)  noexcept ;
    auto greaterThanZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    auto lessThanZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    auto equalsZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    auto notEqualsZero(RegisterIndex dest, RegisterIndex src)  noexcept ;
    auto increment(RegisterIndex target)  noexcept ;
    auto decrement(RegisterIndex target)  noexcept ;
    auto ret(RegisterIndex) noexcept ;
    auto call(RegisterIndex, RegisterIndex) noexcept ;
    auto call(RegisterIndex, Address) noexcept ;
    auto branchIfZero(RegisterIndex, RegisterIndex, RegisterIndex) noexcept ;
    auto branchIfZero(RegisterIndex, RegisterIndex, Address) noexcept ;
} // end namespace iris::instructions
#endif // end IRIS_ENCODING_H__
