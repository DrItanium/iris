/**
 * @file
 * limits wrapper implementation
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

#ifndef IRIS_LIMITS_H__
#define IRIS_LIMITS_H__
#include <lang/cpp/platform.h>

#ifdef HAS_STL
#include <limits>
#else
namespace std {
    template<typename T> 
    class numeric_limits {
        public:
            static constexpr bool is_specialized = false;

            static constexpr T min() noexcept { return T(); }
            static constexpr T max() noexcept { return T(); }
            static constexpr T lowest() noexcept { return T(); }

            static constexpr int digits       = 0;
            static constexpr int digits10     = 0;
            static constexpr int max_digits10 = 0;

            static constexpr bool is_signed  = false;
            static constexpr bool is_integer = false;
            static constexpr bool is_exact   = false;
            static constexpr int radix = 0;
            static constexpr T epsilon() noexcept { return T(); }
            static constexpr T round_error() noexcept { return T(); }
            /// @todo continue here
    };
    enum float_round_style {
        round_indeterminate       = -1,
        round_toward_zero         =  0,
        round_to_nearest          =  1,
        round_toward_infinity     =  2,
        round_toward_neg_infinity =  3,
    };

    enum float_denorm_style {
        denorm_indeterminate = -1,
        denorm_absent        =  0,
        denorm_present       =  1,
    };

} // end namespace std
#endif
#endif // end IRIS_LIMITS_H__
