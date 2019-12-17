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
#include "platform.h"

#ifdef HAS_STL
#include <type_traits>
#else
#include "cstddef.h"
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

    template<typename T> struct _IsIntegral : std::false_type { };
#define X(type) \
    template<> struct _IsIntegral< type > : std::true_type { }
    X(bool);
    X(char);
    X(signed char);
    X(unsigned char);
    X(short);
    X(unsigned short);
    X(int);
    X(unsigned int);
    X(long);
    X(unsigned long);
    X(long long);
    X(unsigned long long);
    X(float);
    X(double);
    X(long double);
#undef X
    template<typename T>
    struct is_integral : _IsIntegral<typename remove_cv<T>::type>::type { };

    template<typename T>
    inline constexpr bool is_integral_v = is_integral<T>::value;

   
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

    // adding support for is_enum,is_union, and is_class requires using gcc builtins
    template<typename T> struct is_enum : integral_constant<bool, __is_enum(T)> { };
    template<typename T> inline constexpr bool is_enum_v = is_enum<T>::value; 
    template<typename T> struct is_union : integral_constant<bool, __is_union(T)> { };
    template<typename T> inline constexpr bool is_union_v = is_union<T>::value; 
    template<typename T> struct is_class : integral_constant<bool, __is_class(T)> { };
    template<typename T> inline constexpr bool is_class_v = is_class<T>::value; 

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


    template<typename T> struct is_arithmetic : std::integral_constant<bool, std::is_integral<T>::value || std::is_floating_point<T>::value> { };

    template<typename T>
    inline constexpr bool is_arithmetic_v = is_arithmetic<T>::value;

    template<typename T> struct is_fundamental : std::integral_constant<bool, is_arithmetic_v<T> || is_void_v<T> || is_same<nullptr_t, typename remove_cv<T>::type>::value > { };

    template<typename T>
    inline constexpr bool is_fundamental_v = is_fundamental<T>::value;


    template<typename T>
    struct is_compound : std::integral_constant<bool, !is_fundamental_v<T>> { };

    template<typename T>
    inline constexpr bool is_compound_v = is_compound<T>::value;

    template<typename T> struct is_reference : std::false_type { };
    template<typename T> struct is_reference<T&> : std::true_type { };
    template<typename T> struct is_reference<T&&> : std::true_type { };

    template<typename T>
    inline constexpr bool is_reference_v = is_reference<T>::value;

    template<typename T> struct _IsMemberPointerHelper: std::false_type { };
    template<typename T, typename U> struct _IsMemberPointerHelper <T U::*> : std::true_type { };

    template<typename T>
    struct is_member_pointer : _IsMemberPointerHelper<typename std::remove_cv<T>::type> { };

    template<typename T>
    inline constexpr bool is_member_pointer_v = is_member_pointer<T>::value;

    template<typename T> struct _IsMemberFunctionPointerHelper : std::false_type { }; 
    template<typename T, class U> struct _IsMemberFunctionPointerHelper<T U::*> : std::is_function<T> { }; 

    template<typename T>
    struct is_member_function_pointer : _IsMemberFunctionPointerHelper<std::remove_cv_t<T>> { };

    template<typename T>
    inline constexpr bool is_member_function_pointer_v = is_member_function_pointer<T>::value;

    template<typename T>
    struct is_member_object_pointer : std::integral_constant<bool, std::is_member_pointer<T>::value && !std::is_member_function_pointer<T>::value> { };

    template<typename T>
    inline constexpr bool is_member_object_pointer_v = is_member_object_pointer<T>::value;
    template<typename T> struct is_const : std::false_type { };
    template<typename T> struct is_const<const T> : std::true_type { };
    template<typename T>
    inline constexpr bool is_const_v = is_const<T>::value;
    template<typename T> struct is_volatile : std::false_type { };
    template<typename T> struct is_volatile<volatile T> : std::true_type { };
    template<typename T>
    inline constexpr bool is_volatile_v = is_volatile<T>::value;

    template<typename T> struct add_cv { using type = const volatile T; };
    template<typename T> struct add_const { using type = const T; };
    template<typename T> struct add_volatile { using type = volatile T; };

    template<typename T> 
    using add_cv_t = typename add_cv<T>::type;
    template<typename T> 
    using add_const_t = typename add_const<T>::type;
    template<typename T> 
    using add_volatile_t = typename add_volatile<T>::type;

    template<typename T> struct remove_reference { using type = T; };
    template<typename T> struct remove_reference<T&> { using type = T; };
    template<typename T> struct remove_reference<T&&> { using type = T; };

    template<typename T>
    using remove_reference_t = typename remove_reference<T>::type;

    
    template<typename T> struct remove_pointer { using type = T; };
    template<typename T> struct remove_pointer<T*> { using type = T; };
    template<typename T> struct remove_pointer<T* const> { using type = T; };
    template<typename T> struct remove_pointer<T* const volatile> { using type = T; };

    template<typename T>
    using remove_pointer_t = typename remove_pointer<T>::type;
    template<typename T> struct remove_extent { using type = T; };
    template<typename T> struct remove_extent<T[]> { using type = T; };
    template<typename T, std::size_t N> struct remove_extent<T[N]> { using type = T; };

    template<typename T>
    using remove_extent_t = typename remove_extent<T>::type;

    template<typename T> struct remove_all_extents { using type = T; };
    template<typename T> struct remove_all_extents<T[]> { using type = typename remove_all_extents<T>::type; };
    template<typename T, std::size_t N> struct remove_all_extents<T[N]> { using type = typename remove_all_extents<T>::type; };

    template<typename T>
    using remove_all_extents_t = typename remove_all_extents<T>::type;

    template<typename T> struct remove_cvref { using type = remove_cv_t<remove_reference_t<T>>; };

    template<typename T>
    using remove_cvref_t = typename remove_cvref<T>::type;

    template<bool B, typename T = void>
    struct enable_if { };

    template<typename T>
    struct enable_if<true, T> { using type = T; };

    template<bool B, typename T = void>
    using enable_if_t = typename enable_if<B, T>::type;

    template<bool B, typename T, typename F>
    struct conditional { using type = T; };

    template<typename T, typename F>
    struct conditional<false, T, F> { using type = F; };

    template<bool B, typename T, typename F>
    using conditional_t = typename conditional<B,T,F>::type;


    
    template<typename...>
    using void_t = void;

    template<typename...> struct conjunction : std::true_type { };
    template<typename B> struct conjunction<B> : B { };

    template<typename B, typename... Bn>
    struct conjunction<B, Bn...>
        : conditional_t<bool(B::value), conjunction<Bn...>, B> { };
    template<typename... B>
    inline constexpr bool conjunction_v = conjunction<B...>::value;
    
    template<typename...> struct disjunction : std::false_type { };
    template<typename B> struct disjunction<B> : B { };

    template<typename B, typename... Bn>
    struct disjunction<B, Bn...>
        : conditional_t<bool(B::value), disjunction<Bn...>, B> { };
    template<typename... B>
    inline constexpr bool disjunction_v = disjunction<B...>::value;

    template<typename T> struct negation : std::bool_constant<!bool(T::value)> { };

    template<typename T>
    inline constexpr bool negation_v = negation<T>::value;

    template<typename T>
    struct is_scalar : std::integral_constant<bool, 
    is_arithmetic_v<T> || is_enum_v<T> 
                       || is_pointer_v<T>
                       || is_member_pointer_v<T>
                       || is_null_pointer_v<T>> { };

    template<typename T>
    inline constexpr bool is_scalar_v = is_scalar<T>::value;

    template<typename T>
    struct is_object : std::integral_constant<bool, is_scalar_v<T> || is_array_v<T> || is_union_v<T> || is_class_v<T>> { };

    template<typename T>
    inline constexpr bool is_object_v = is_object<T>::value;

    template<typename T>
    struct alignment_of : integral_constant<std::size_t, alignof(T)> { };

    template<typename T>
    inline constexpr std::size_t alignment_of_v = alignment_of<T>::value;

    template<typename T> struct rank : integral_constant<std::size_t, 0> { };
    template<typename T> struct rank<T[]> : integral_constant<std::size_t, rank<T>::value + 1> { };
    template<typename T, std::size_t N> struct rank<T[N]> : integral_constant<std::size_t, rank<T>::value + 1> { };

    template<typename T>
    inline constexpr std::size_t rank_v = rank<T>::value;

    static_assert(rank_v<int> == 0);
    static_assert(rank_v<int[]> == 1);
    static_assert(rank_v<int[9]> == 1);
    static_assert(rank_v<int[9][9]> == 2);
    static_assert(rank_v<int[][9][9]> == 3);

    template<typename T, unsigned N = 0> struct extent : integral_constant<std::size_t, 0> { };
    template<typename T> struct extent<T[], 0> : integral_constant<std::size_t, 0> { };
    template<typename T, unsigned N> struct extent<T[], N> : extent<T, N-1> { };
    template<typename T, std::size_t I> struct extent<T[I], 0> : integral_constant<std::size_t, I> { };
    template<typename T, std::size_t I, unsigned N> struct extent<T[I], N> : extent<T, N-1> { };

    template<typename T, unsigned N = 0>
    inline constexpr std::size_t extent_v = extent<T, N>::value;

    static_assert(extent_v<int[3]> == 3);
    static_assert(extent_v<int[3][4], 0> == 3);
    static_assert(extent_v<int[3][4], 1> == 4);
    static_assert(extent_v<int[3][4], 2> == 0);
    static_assert(extent_v<int[]> == 0);

    // declval implementation
    template<typename T, typename U = T&&> U __declval(int);
    template<typename T> T __declval(long);
    template<typename T> auto declval() noexcept -> decltype(__declval<T>(0));

    namespace details {
        template<typename B> std::true_type isBaseOfTestFunction(const volatile B*);
        template<typename B> std::false_type isBaseOfTestFunction(const volatile void*);

        template<typename B, typename D>
        using __IsBaseOf = decltype(isBaseOfTestFunction<B>(std::declval<D*>()));

        template<typename B, typename D, typename = void>
        struct __IsBaseOf2 : std::true_type { };
        template<typename B, typename D>
        struct __IsBaseOf2<B, D, std::void_t<__IsBaseOf<B, D>>> : __IsBaseOf<B, D> { };


    } // end namespace details

    template<typename Base, typename Derived>
    struct is_base_of : conditional_t<is_class_v<Base> && is_class_v<Derived>,
                                      details::__IsBaseOf2<Base, Derived>,
                                      false_type> { };

    template<typename Base, typename Derived>
    inline constexpr bool is_base_of_v = is_base_of<Base, Derived>::value;

    namespace details {
        template<typename T> struct TypeIdentity { using type = T; }; 

        template<typename T> auto tryAddLValueReference(int) -> TypeIdentity<T&>;
        template<typename T> auto tryAddLValueReference(...) -> TypeIdentity<T>;
        template<typename T> auto tryAddRValueReference(int) -> TypeIdentity<T&&>;
        template<typename T> auto tryAddRValueReference(...) -> TypeIdentity<T>;
    } // end namespace details

    template<typename T> struct add_lvalue_reference : decltype(details::tryAddLValueReference<T>(0)) { };
    template<typename T> struct add_rvalue_reference : decltype(details::tryAddRValueReference<T>(0)) { };

    template<typename T> 
    using add_lvalue_reference_t = typename add_lvalue_reference<T>::type;
    template<typename T> 
    using add_rvalue_reference_t = typename add_rvalue_reference<T>::type;


    namespace details {
        template<typename T>
        auto tryAddPointer(int) -> TypeIdentity<typename std::remove_reference<T>::type*>;
        template<typename T>
        auto tryAddPointer(...) -> TypeIdentity<T>;
    } // end namespace details
    template<typename T>
    struct add_pointer : decltype(details::tryAddPointer<T>(0)) { };

    template<typename T>
    using add_pointer_t = typename add_pointer<T>::type;

    template<typename T>
    struct decay {
        private:
            using U = typename remove_reference<T>::type;
        public:
            using type = typename conditional<
                            is_array_v<U>,
                            typename remove_extent<U>::type*,
                            typename conditional<
                                is_function_v<U>,
                                typename add_pointer<U>::type,
                                typename remove_cv<U>::type
                                    >::type>::type;

    };
    template<typename T>
    using decay_t = typename decay<T>::type;
    static_assert(is_same_v<decay_t<const int>, int>);
    static_assert(is_same_v<decay_t<const volatile int>, int>);
    static_assert(is_same_v<decay_t<const volatile int>, int>);
    static_assert(is_same_v<decay_t<const volatile int&>, int>);
    static_assert(is_same_v<decay_t<const volatile int&&>, int>);
    static_assert(is_same_v<decay_t<int[2]>, int*>);
    static_assert(is_same_v<decay_t<int(int)>, int(*)(int)>);
#define X(kind) \
    template<typename T> struct is_ ## kind : integral_constant<bool, __is_ ## kind (T) > { }; \
    template<typename T> inline constexpr bool is_ ## kind ## _v = is_ ## kind <T> :: value
    X(empty);
    X(polymorphic);
    X(abstract);
    X(literal_type);
    X(trivially_copyable);
    X(trivial);
    X(standard_layout);
    X(final);
    X(pod);
#undef X
    template<typename T> struct is_aggregate : bool_constant<__is_aggregate(remove_cv_t<T>)> { };
    template<typename T> inline constexpr bool is_aggregate_v = is_aggregate<T>::value;
    namespace details {
        template<typename T, bool = is_arithmetic<T>::value>
        struct IsSigned : false_type { };

        template<typename T>
        struct IsSigned<T, true> : integral_constant<bool, T(-1) < T(0)> { };

    } // end namespace details
    template<typename T> struct is_signed : details::IsSigned<T>::type { };
    template<typename T> inline constexpr bool is_signed_v = is_signed<T>::value;
    template<typename T> struct is_unsigned : bool_constant<is_arithmetic_v<T> && !is_signed_v<T>> { };
    template<typename T> inline constexpr bool is_unsigned_v = is_unsigned<T>::value;

    namespace details {
        template<typename T, bool = is_enum<T>::value> struct UnderlyingType { using type = __underlying_type(T); };
        template<typename T> struct UnderlyingType<T, false> { };

    } // end namespace details

    template<typename T> struct underlying_type : details::UnderlyingType<T> { };
    template<typename T>
    using underlying_type_t = typename underlying_type<T>::type;



    template<typename T>
    struct has_unique_object_representations : public bool_constant<__has_unique_object_representations(remove_cv_t<remove_all_extents_t<T>>)> { };

    template<typename T>
    inline constexpr bool has_unique_object_representations_v = has_unique_object_representations<T>::value;

    template<typename T> struct has_virtual_destructor : bool_constant<__has_virtual_destructor(T)> { };
    template<typename T> inline constexpr bool has_virtual_destructor_v = has_virtual_destructor<T>::value;

    template<typename T, typename ... Args> struct is_constructible : bool_constant<__is_constructible(T, Args...)> { };
    template<typename T, typename ... Args> inline constexpr bool is_constructible_v = is_constructible<T>::value;
    template<typename T> struct is_default_constructible : is_constructible<T>::type { };
    template<typename T> inline constexpr bool is_default_constructible_v = is_default_constructible<T>::value;
    template<typename T> struct is_copy_constructible : is_constructible<T, const T&>::type { };
    template<typename T> inline constexpr bool is_copy_constructible_v = is_copy_constructible<T>::value;
    template<typename T> struct is_move_constructible : is_constructible<T, T&&>::type { };
    template<typename T> inline constexpr bool is_move_constructible_v = is_move_constructible<T>::value;
    template<typename T, typename U> struct is_assignable : bool_constant<__is_assignable(T, U)> { };
    template<typename T, typename U> inline constexpr bool is_assignable_v = is_assignable<T, U>::value;
    /// @todo implement aligned_storage
    /// @todo implement aligned_union
    /// @todo implement make_signed
    /// @todo implement make_unsigned
    /// @todo implement is_trivially_constructible  and is_nothrow_constructible
    /// @todo implement is_trivially_default_constructible, is_nothrow_default_constructible
    /// @todo implement is_trivially_copy_constructible  and is_nothrow_copy_constructible
    /// @todo implement is_trivially_move_constructible  and is_nothrow_move_constructible
    /// @todo implement is_trivially_assignable  and is_nothrow_assignable
    /// @todo implement is_copy_assignable, is_trivially_copy_assignable  and is_nothrow_copy_assignable
    /// @todo implement is_move_assignable, is_trivially_move_assignable  and is_nothrow_move_assignable
    /// @todo implement is_destructible, is_trivially_destructible  and is_nothrow_destructible
    /// @todo implement is_swappable, is_swappable_with, is_nothrow_swappable_with, is_nothrow_swappable
    /// @todo implement is_nothrow_convertible
    /// @todo implement is_invocable
    /// @todo implement is_invocable_r
    /// @todo implement is_nothrow_invocable
    /// @todo implement is_nothrow_invocable_r
    /// @todo implement common_type
    /// @todo implement invoke_result
    namespace details {
        template<typename T> struct MakeUnsigned              { using type = T; };
        template<>           struct MakeUnsigned<char>        { using type = unsigned char; };
        template<>           struct MakeUnsigned<signed char> { using type = unsigned char; };
        template<>           struct MakeUnsigned<short>       { using type = unsigned short; };
        template<>           struct MakeUnsigned<int>         { using type = unsigned int; };
        template<>           struct MakeUnsigned<long>        { using type = unsigned long; };
        template<>           struct MakeUnsigned<long long>   { using type = unsigned long long; };
        /// @todo implement support for enums

        template<typename T> struct MakeSigned                     { using type = T; };
        template<>           struct MakeSigned<unsigned char>      { using type = signed char; };
        template<>           struct MakeSigned<char>               { using type = signed char; };
        template<>           struct MakeSigned<unsigned short>     { using type = signed short; };
        template<>           struct MakeSigned<unsigned int>       { using type = signed int; };
        template<>           struct MakeSigned<unsigned long>      { using type = signed long; };
        template<>           struct MakeSigned<unsigned long long> { using type = signed long long; };
        /// @todo implement support for enums
    } // end namespace details
    namespace details {
        template<typename From, typename To, 
            bool = is_void_v<From> ||
                   is_function_v<To> ||
                   is_array_v<To>> 
        struct IsConvertible {
            using type = typename is_void<To>::type;
        };

        template<typename From, typename To>
        struct IsConvertible<From, To, false> {
            private:
                template<typename To1>
                static void TestAux(To1) noexcept;

                template<typename From1, typename To1, typename = decltype(TestAux<To1>(std::declval<From1>()))>
                static true_type Test(int);

                template<typename, typename>
                static false_type Test(...);
            public:
                using type = decltype(Test<From, To>(0));
        };
    } // end namespace details

    template<typename From, typename To>
    struct is_convertible : public details::IsConvertible<From, To>::type { };

    template<typename From, typename To>
    inline constexpr bool is_convertible_v = is_convertible<From, To>::value;
}
#endif

#endif // end IRIS_TYPE_TRAITS_H__
