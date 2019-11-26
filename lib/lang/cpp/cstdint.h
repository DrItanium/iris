/**
 * @file
 * cstdint interface
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

#ifndef IRIS_CSTDINT_H__
#define IRIS_CSTDINT_H__
#include "lang/cpp/platform.h"
#ifdef HAS_STL
#include <cstdint>
#else
extern "C" {
#include <stdint.h>
}
namespace std {
    using int8_t = ::int8_t;
    using int16_t = ::int16_t;
    using int32_t = ::int32_t;
    using int64_t = ::int64_t;
    using int_fast8_t =  ::int_fast8_t;
    using int_fast16_t = ::int_fast16_t;
    using int_fast32_t = ::int_fast32_t;
    using int_fast64_t = ::int_fast64_t;
    using uint_fast8_t =  ::uint_fast8_t;
    using uint_fast16_t = ::uint_fast16_t;
    using uint_fast32_t = ::uint_fast32_t;
    using uint_fast64_t = ::uint_fast64_t;
    using int_least8_t =  ::int_least8_t;
    using int_least16_t = ::int_least16_t;
    using int_least32_t = ::int_least32_t;
    using int_least64_t = ::int_least64_t;
    using uint_least8_t =  ::uint_least8_t;
    using uint_least16_t = ::uint_least16_t;
    using uint_least32_t = ::uint_least32_t;
    using uint_least64_t = ::uint_least64_t;
    using uint8_t = ::uint8_t;
    using uint16_t = ::uint16_t;
    using uint32_t = ::uint32_t;
    using uint64_t = ::uint64_t;
    using intmax_t = ::intmax_t;
    using intptr_t = ::intptr_t;
    using uintmax_t = ::uintmax_t;
    using uintptr_t = ::uintptr_t;
} // end namespace std
#endif // end defined(HAS_STL)

#endif // end IRIS_CSTDINT_H__
