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

#include "arduino_core.h"
#include "Arduino.h"

namespace iris {
ArduinoCore::ArduinoCore() noexcept {
    for (Ordinal i = 0; i < 256; ++i) {
        // setup the storage constants
        if (i < 17) {
            _regs[i].hardwireTo(i);
        }
    }
}

void
ArduinoCore::putRegister(RegisterIndex ind, Ordinal value) noexcept {
    _regs[std::to_integer<RegisterIndexNumericType>(ind)].put(value);
}

void
ArduinoCore::putRegister(RegisterIndex ind, Integer value) noexcept {
    _regs[std::to_integer<RegisterIndexNumericType>(ind)].put(value);
}

Ordinal
ArduinoCore::retrieveRegister(RegisterIndex ind, RequestOrdinal) const noexcept {
    return _regs[std::to_integer<RegisterIndexNumericType>(ind)].get<Ordinal>();
}

Integer
ArduinoCore::retrieveRegister(RegisterIndex ind, RequestInteger) const noexcept {
    return _regs[std::to_integer<RegisterIndexNumericType>(ind)].get<Integer>();
}

LongOrdinal
ArduinoCore::retrieveDoubleRegister(RegisterIndex lower, RegisterIndex upper) const noexcept {
    auto lv = static_cast<LongOrdinal>(retrieveRegister(lower, RequestOrdinal{}));
    auto uv = static_cast<LongOrdinal>(retrieveRegister(upper, RequestOrdinal{})) << 16;
    return lv | uv;
}
LongOrdinal
ArduinoCore::retrieveDoubleRegister(RegisterIndex lower) const noexcept {
    return retrieveDoubleRegister(lower, static_cast<RegisterIndex>(std::to_integer<RegisterIndexNumericType>(lower) + 1));
}

void
ArduinoCore::putDoubleRegister(RegisterIndex lower, RegisterIndex upper, LongOrdinal value) noexcept {
    putRegister(lower, static_cast<Ordinal>(value));
    putRegister(upper, static_cast<Ordinal>(value >> 16));
}

void
ArduinoCore::putDoubleRegister(RegisterIndex lower, LongOrdinal value) noexcept {
    putDoubleRegister(lower, static_cast<RegisterIndex>(std::to_integer<RegisterIndexNumericType>(lower) + 1), value);
}



}
