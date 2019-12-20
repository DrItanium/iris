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
#include "types.h"
#include "mem_core.h"
#include "opcodes.h"

namespace iris {
InMemoryCore::InMemoryCore() noexcept : Core() {
    // 0-16 are hardwired in r0-r16 (first 17 registers)
    for (int i = 0; i < 17; ++i) {
        _regs[i].hardwireTo(i);
    }
    // basic io reservations the processor will reserve
}

void
InMemoryCore::raiseDivideByZero() {
    throw DivideByZeroException();
}

void
InMemoryCore::raiseErrorInstruction() {
    throw ErrorInstructionException();
}

void
InMemoryCore::raiseBadOperation() {
    throw BadOperationException();
}

void
InMemoryCore::cycleHandler() {
    try {
        cycle();
    } catch(DivideByZeroException& ex) {
        /// @todo try to dispatch to an interrupt vector
    } catch(Exception& ex) {
        /// @todo implement logic to handle edge cases
    }
}

std::optional<CaptiveMemoryMappedDevice>
InMemoryCore::findDevice(Address address) noexcept {
    for (auto& device : _memoryMap) {
        if (device.respondsTo(address)) {
            return device;
        }
    }
    return std::nullopt;
}
Ordinal
InMemoryCore::loadFromMemory(Address address) noexcept {
    auto actualAddress = computeOrdinalAddress(address);
    if (auto device = findDevice(actualAddress); device) {
        return device->loadFull(actualAddress);
    } else {
        return 0;
    }
}
QuarterOrdinal
InMemoryCore::loadQuarterFromMemory(Address address) noexcept {
    auto actualAddress = computeHalfAddress(address);
    if (auto device = findDevice(actualAddress); device) {
        return device->loadHalf(actualAddress);
    } else {
        return 0;
    }
}

HalfOrdinal
InMemoryCore::loadHalfFromMemory(Address address) noexcept {
    if (auto device = findDevice(address); device) {
        return device->loadQuarter(address);
    } else {
        return 0;
    }
}

void 
InMemoryCore::storeToMemory(Address address, Ordinal value) noexcept {
    auto computedAddress = computeOrdinalAddress(address);
    if (auto device = findDevice(computedAddress); device) {
        device->store(computedAddress, value);
    }
}

void 
InMemoryCore::storeToMemory(Address address, HalfOrdinal value) noexcept {
    auto computedAddress = computeHalfAddress(address);
    if (auto device = findDevice(computedAddress); device) {
        device->storeHalf(computedAddress, value);
    }
}

void 
InMemoryCore::storeToMemory(Address address, QuarterOrdinal value) noexcept {
    if (auto device = findDevice(address); device) {
        device->storeQuarter(address, value);
    }
}

void 
InMemoryCore::mapDevice(MemoryMappedDevice::Ptr device) noexcept {
    _memoryMap.emplace_front(device);
    _memoryMap.front().map();
}

} // end namespace iris
