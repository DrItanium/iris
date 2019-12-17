/**
 * @file
 * platform detection routines
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

#ifndef IRIS_PLATFORM_H__
#define IRIS_PLATFORM_H__
#ifdef __AVR__
#define TARGET_AVR
#endif
#ifdef __arm__
#define TARGET_ARM
#endif

#ifdef ARDUINO
#define ARDUINO_ENABLED
#endif

#if defined(TARGET_ARM) && defined(ARDUINO_ENABLED)
#define TARGET_ARM_ARDUINO
#endif 



#ifndef TARGET_AVR
#define HAS_STL
#endif

// So that YCM analyzes my implementation
#ifdef YCM_VERIFY_WRAPPER_IMPL
#   ifdef HAS_STL
#      undef HAS_STL
#   endif
#endif

#if defined(TARGET_ARM) && defined(ARDUINO_ENABLED) 
#define CUSTOM_NEW_IMPL_REQUIRED
#endif 

constexpr bool platformIsArduino() noexcept {
#ifdef ARDUINO_ENABLED
    return true;
#else
    return false;
#endif
}

constexpr bool platformIsAVR() noexcept {
#ifdef TARGET_AVR
    return true;
#else
    return false;
#endif
}

constexpr bool targetIsAtTiny85() noexcept {
    if constexpr (platformIsAVR()) {
#ifdef __AVR_ATtiny85__
        return true;
#else
        return false;
#endif
    } else {
        return false;
    }
}

constexpr bool targetIsArm() noexcept {
#ifdef TARGET_ARM
    return true;
#else 
    return false;
#endif
}

constexpr bool platformIsAmd64() noexcept {
#ifdef __x86_64__
    return true;
#else
    return false;
#endif
}

#endif // end IRIS_PLATFORM_H__
