// Copyright 2024 Gareth Cross
#pragma once
#include <tuple>
#include <variant>

#include "wf/type_list.h"

namespace wf {

// Struct for getting argument types and return types from a function pointer or lambda.
// This specialization is for lambdas.
template <typename T>
struct function_traits : public function_traits<decltype(&T::operator())> {};

// This specialization is for member functions (like operator() on a lambda).
template <typename ClassType, typename Ret, typename... Args>
struct function_traits<Ret (ClassType::*)(Args...) const> {
  constexpr static auto arity = sizeof...(Args);

  // Return type of the function.
  using return_type = Ret;

  // The arg type list.
  using args_list = type_list<Args...>;

  // Get the i'th argument type.
  template <std::size_t i>
  using args_list_element_t = typename std::tuple_element<i, std::tuple<Args...>>::type;
};

// This specialization is for general function pointers.
template <typename Ret, typename... Args>
struct function_traits<Ret (*)(Args...)> {
  constexpr static auto arity = sizeof...(Args);

  // Return type of the function.
  using return_type = Ret;

  // The arg type list.
  using args_list = type_list<Args...>;

  // Get the i'th argument type.
  template <std::size_t i>
  using args_list_element_t = typename std::tuple_element<i, std::tuple<Args...>>::type;
};

// Enable if to check that two types are the same.
template <typename A, typename B, typename Type = void>
using enable_if_same_t = std::enable_if_t<std::is_same_v<A, B>, Type>;

// Enable if to check that two types are not the same.
template <typename A, typename B, typename Type = void>
using enable_if_not_same_t = std::enable_if_t<!std::is_same_v<A, B>, Type>;

namespace detail {
template <typename T, typename List, typename = void>
struct is_invocable : std::false_type {};
template <typename T, typename... Args>
struct is_invocable<T, type_list<Args...>,
                    decltype(std::declval<T>()(std::declval<Args>()...), void())> : std::true_type {
};

}  // namespace detail

// True if the object `T` can be invoked with the specified args list.
// std::is_invocable causes issues on gcc 12, so we define our own.
template <typename T, typename... Args>
constexpr bool is_invocable_v = detail::is_invocable<T, type_list<Args...>>::value;

// True if the type is a tuple.
template <typename T>
constexpr bool is_tuple_v = false;
template <typename... Ts>
constexpr bool is_tuple_v<std::tuple<Ts...>> = true;

// True if the type is a variant.
template <typename T>
constexpr bool is_variant_v = false;
template <typename... Ts>
constexpr bool is_variant_v<std::variant<Ts...>> = true;

}  // namespace wf