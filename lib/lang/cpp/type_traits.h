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
   
    template<typename T>
    struct is_floating_point : std::integral_constant<
                               bool,
                               std::is_same<float, typename std::remove_cv<T>::type>::value ||
                               std::is_same<double, typename std::remove_cv<T>::type>::value ||
                               std::is_same<long double, typename std::remove_cv<T>::type>::value
                               > { };

    template<typename T>
    inline constexpr bool is_floating_point_v = is_floating_point<T>::value;
    
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

    template<typename T> struct IsPointerHelper : std::false_type { }; 
    template<typename T> struct IsPointerHelper<T*> : std::true_type { }; 
    template<typename T> struct is_pointer : IsPointerHelper<typename std::remove_cv<T>::type> { };

    template<typename T>
    inline constexpr bool is_pointer_v = is_pointer<T>::value;

    static_assert(!std::is_pointer_v<int>);
    static_assert(!std::is_pointer_v<const int>);
    static_assert(std::is_pointer_v<int*>);
    static_assert(std::is_pointer_v<int**>);
    static_assert(std::is_pointer_v<const int*>);
    static_assert(std::is_pointer_v<const int* const *>);

    template<typename T> struct is_lvalue_reference : std::false_type { };
    template<typename T> struct is_lvalue_reference<T&> : std::true_type { };

    template<typename T>
    inline constexpr bool is_lvalue_reference_v = is_lvalue_reference<T>::value;

    template<typename T> struct is_rvalue_reference : std::false_type { };
    template<typename T> struct is_rvalue_reference<T&&> : std::true_type { };

    template<typename T>
    inline constexpr bool is_rvalue_reference_v = is_rvalue_reference<T>::value;

    /// @todo implement is_member_object_pointer
    /// @todo implement is_member_function_pointer
    /// @todo implement is_fundamental
    /// @todo implement is_arithmetic
    /// @todo implement is_scalar
    /// @todo implement is_compound
    template<typename T> struct is_reference : std::false_type { };
    template<typename T> struct is_reference<T&> : std::true_type { };
    template<typename T> struct is_reference<T&&> : std::true_type { };

    template<typename T>
    inline constexpr bool is_reference_v = is_reference<T>::value;

    /// @todo implement is_member_pointer
    
    template<typename T> struct is_const : std::false_type { };
    template<typename T> struct is_const<const T> : std::true_type { };
    template<typename T>
    inline constexpr bool is_const_v = is_const<T>::value;
    template<typename T> struct is_volatile : std::false_type { };
    template<typename T> struct is_volatile<volatile T> : std::true_type { };
    template<typename T>
    inline constexpr bool is_volatile_v = is_volatile<T>::value;

    /// @todo implement is_trivial
    /// @todo implement is_trivially_copyable
    /// @todo implement is_standard_layout 
    /// @todo implement is_pod
    /// @todo implement has_unique_object_representations
    /// @todo implement is_empty
    /// @todo implement is_polymorphic
    /// @todo implement is_abstract
    /// @todo implement is_final
    /// @todo implement is_aggregate
    /// @todo implement is_signed
    /// @todo implement is_unsigned
    /// @todo implement is_constructible, is_trivially_constructible  and is_nothrow_constructible
    /// @todo implement is_default_constructible, is_trivially_default_constructible, is_nothrow_default_constructible
    /// @todo implement is_copy_constructible, is_trivially_copy_constructible  and is_nothrow_copy_constructible
    /// @todo implement is_move_constructible, is_trivially_move_constructible  and is_nothrow_move_constructible
    /// @todo implement is_assignable, is_trivially_assignable  and is_nothrow_assignable
    /// @todo implement is_copy_assignable, is_trivially_copy_assignable  and is_nothrow_copy_assignable
    /// @todo implement is_move_assignable, is_trivially_move_assignable  and is_nothrow_move_assignable
    /// @todo implement is_destructible, is_trivially_destructible  and is_nothrow_destructible
    /// @todo implement has_virtual_destructor
    /// @todo implement is_swappable, is_swappable_with, is_nothrow_swappable_with, is_nothrow_swappable
    /// @todo implement alignment_of
    /// @todo implement rank
    /// @todo implement extent
    /// @todo implement is_base_of
    /// @todo implement is_convertible and is_nothrow_convertible
    /// @todo implement is_layout_compatible
    /// @todo implement is_invocable
    /// @todo implement is_invocable_r
    /// @todo implement is_nothrow_invocable
    /// @todo implement is_nothrow_invocable_r
    template<typename T> struct add_cv { using type = const volatile T; };
    template<typename T> struct add_const { using type = const T; };
    template<typename T> struct add_volatile { using type = volatile T; };

    template<typename T> 
    using add_cv_t = typename add_cv<T>::type;
    template<typename T> 
    using add_const_t = typename add_const<T>::type;
    template<typename T> 
    using add_volatile_t = typename add_volatile<T>::type;
}
#endif

#endif // end IRIS_TYPE_TRAITS_H__
