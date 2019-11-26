/**
 * @file
 * type_traits interface based off of possible implementation sections at en.cppreference.com
 * and influences from libstdc++ in other cases. 
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

#ifndef IRIS_TYPE_TRAITS_H__
#define IRIS_TYPE_TRAITS_H__
#include <lang/cpp/platform.h>

#ifdef HAS_STL
#include <type_traits>
#else
#include <lang/cpp/cstddef.h>
namespace std {
    template<typename T, T v>
    struct integral_constant {
        static constexpr T value = v;
        using value_type = T;
        using type = integral_constant;
        constexpr operator value_type() const noexcept { return value; }
        constexpr value_type operator()() const noexcept { return value; }
    };

    template<bool B>
    using bool_constant = integral_constant<bool, B>;

    using true_type = bool_constant<true>;
    using false_type = bool_constant<false>;

    template<typename T, typename U>
    struct is_same : std::false_type { };

    template<typename T>
    struct is_same<T, T> : std::true_type { };

    template<typename T, typename U>
    inline constexpr bool is_same_v = std::is_same<T, U>::value;

    template<typename T> struct remove_const { using type = T; };
    template<typename T> struct remove_const<const T> { using type = T; };
    template<typename T>
    using remove_const_t = typename remove_const<T>::type;
    template<typename T> struct remove_volatile { using type = T; };
    template<typename T> struct remove_volatile<volatile T> { using type = T; };
    template<typename T>
    using remove_volatile_t = typename remove_volatile<T>::type;

    template<typename T>
    struct remove_cv {
       using type = typename std::remove_volatile<typename std::remove_const<T>::type>::type;
    };

    template<typename T>
    using remove_cv_t = typename remove_cv<T>::type;



    template<typename T>
    struct is_void : std::is_same<void, typename std::remove_cv<T>::type> { };



    template<typename T>
    inline constexpr bool is_void_v = std::is_void<T>::value;

    static_assert(is_void_v<void>);
    static_assert(!is_void_v<int>);

    template<typename T>
    struct is_null_pointer : std::is_same<std::nullptr_t, std::remove_cv_t<T>> { };
    template<typename T>
    inline constexpr bool is_null_pointer_v = std::is_null_pointer<T>::value;

    static_assert(is_null_pointer_v<std::nullptr_t>, "nullptr is supposed to be a null_pointer");
    static_assert(!is_null_pointer_v<int>, "int is not supposed to be a null_pointer");

    /// @todo implement is_integral
    /// @todo implement is_floating_point
    
    template<typename T>
    struct is_array : std::false_type { };

    template<typename T>
    struct is_array<T[]> : std::true_type { };

    template<typename T, std::size_t N>
    struct is_array<T[N]> : std::true_type { };

    template<typename T>
    inline constexpr bool is_array_v = is_array<T>::value;
    static_assert(!std::is_array_v<int>, "int is not an array!");
    static_assert(std::is_array_v<int[]>, "int[] is an array!");
    static_assert(std::is_array_v<int[3]>, "int[3] is an array!");

    /// @todo implement is_enum
    /// @todo implement is_union
    /// @todo implement is_class
    
    // Implementation taken from https://en.cppreference.com/w/cpp/types/is_function
    template<typename>
    struct is_function : std::false_type { };

    template<typename R, typename ... Args>
    struct is_function<R(Args...)> : std::true_type { };

    // specialization for variadic functions such as std::printf
    template<typename R, typename ... Args>
    struct is_function<R(Args......)> : std::true_type { };


    // make the preprocessor generate all of this
#define X(qual) \
    template<class R, class... Args> \
    struct is_function<R(Args...) qual > : std::true_type { }; \
    template<class R, class... Args> \
    struct is_function<R(Args......) qual > : std::true_type { }
    X(const);
    X(volatile);
    X(const volatile);
    X(&);
    X(const &);
    X(volatile &);
    X(const volatile &);
    X(&&);
    X(const &&);
    X(volatile &&);
    X(const volatile &&);
    X(noexcept);
    X(const noexcept);
    X(volatile noexcept);
    X(const volatile noexcept);
    X(& noexcept);
    X(const & noexcept);
    X(volatile & noexcept);
    X(const volatile & noexcept);
    X(&& noexcept);
    X(const && noexcept);
    X(volatile && noexcept);
    X(const volatile && noexcept);
#undef X

    template<typename T>
    inline constexpr bool is_function_v = is_function<T>::value;

}
#endif

#endif // end IRIS_TYPE_TRAITS_H__
