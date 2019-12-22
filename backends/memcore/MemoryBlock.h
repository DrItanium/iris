/**
 * @file
 * Memory block "device" for use with the in memory core
 * @copyright 
 * iris
 * Copyright (c) 2013-2020, Joshua Scoggins and Contributors
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
#ifndef IRIS_MEMCORE_MEMBLOCK_DEVICE_H__
#define IRIS_MEMCORE_MEMBLOCK_DEVICE_H__
#include "mem_core.h"
#include <types.h>
#include <array>
namespace iris {
    template<size_t capacity>
    class MemoryBlock : public RangedMemoryMappedDevice {
        public:
            MemoryBlock(Address start, Address end) : RangedMemoryMappedDevice(start, end) {
                // zero out memory
                for (decltype(capacity) i = 0; i < capacity; ++i) {
                    _storage[capacity] = 0;
                }
            }
            ~MemoryBlock() override = default;

        private:
            std::array<Byte, capacity> _storage;

    };

} // end namespace iris
#endif // end IRIS_MEMCORE_MEMBLOCK_DEVICE_H__
