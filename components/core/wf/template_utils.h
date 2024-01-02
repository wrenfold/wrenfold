// Copyright 2022 Gareth Cross
#pragma once
#include <optional>
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

// Template to check if the `operator()` method is implemented.
template <typename T, typename, typename = void>
constexpr bool has_call_operator_v = false;

// Specialization that is activated when the Apply method exists:
template <typename T, typename Argument>
constexpr bool has_call_operator_v<T, Argument,
                                   decltype(std::declval<T>()(std::declval<Argument>()), void())> =
    true;

// Template to check if the `operator()` method is implemented for two types.
template <typename T, typename, typename, typename = void>
constexpr bool has_binary_call_operator_v = false;

// Specialization that is activated when the binary operator() method exists.
template <typename T, typename Argument1, typename Argument2>
constexpr bool has_binary_call_operator_v<
    T, Argument1, Argument2,
    decltype(std::declval<T>()(std::declval<Argument1>(), std::declval<Argument2>()), void())> =
    true;

// True if the type is a tuple.
template <typename T>
constexpr bool is_tuple_v = false;
template <typename... Ts>
constexpr bool is_tuple_v<std::tuple<Ts...>> = true;

// True if the type is an optional.
template <typename T>
constexpr bool is_optional_v = false;
template <typename T>
constexpr bool is_optional_v<std::optional<T>> = true;

// True if the type is a variant.
template <typename T>
constexpr bool is_variant_v = false;
template <typename... Ts>
constexpr bool is_variant_v<std::variant<Ts...>> = true;

namespace detail {
template <class... Ts>
struct overloaded_struct : Ts... {
  using Ts::operator()...;
};

// A user-defined deduction guide for `overloaded_struct`.
template <class... Ts>
overloaded_struct(Ts...) -> overloaded_struct<Ts...>;
}  // namespace detail

// Create a struct w/ multiple operator() methods (determined by `funcs`).
template <typename... Funcs>
auto make_overloaded(Funcs&&... funcs) {
  return detail::overloaded_struct{std::forward<Funcs>(funcs)...};
}

// Visit a variant w/ the lambdas `funcs`. Whichever lambda matches the variant type is invoked.
// If the last lambda is `auto`, it will match any remaining types.
template <typename... Funcs, typename Variant>
auto overloaded_visit(Variant&& var, Funcs&&... funcs) {
  return std::visit(make_overloaded(std::forward<Funcs>(funcs)...), std::forward<Variant>(var));
}

namespace detail {

// True if all types are void.
template <typename... Ts>
constexpr bool all_void_v = std::conjunction_v<std::is_same<void, Ts>...>;

// True if any type is void.
template <typename... Ts>
constexpr bool any_void_v = std::disjunction_v<std::is_same<void, Ts>...>;

template <typename F, std::size_t... I>
auto index_seq_for(F&& f, std::index_sequence<I...>) {
  // Either every invocation returns void, or none of them return void.
  static_assert(all_void_v<std::invoke_result_t<F, std::integral_constant<std::size_t, I>>...> ||
                    !any_void_v<std::invoke_result_t<F, std::integral_constant<std::size_t, I>>...>,
                "Cannot place void type into a tuple.");

  if constexpr (all_void_v<std::invoke_result_t<F, std::integral_constant<std::size_t, I>>...>) {
    // All return types are void.
    (std::invoke(std::forward<F>(f), std::integral_constant<std::size_t, I>{}), ...);
  } else {
    // Not all return types are void, return a tuple.
    // Use initializer list syntax to ensure order of execution is left to right.
    return std::tuple<std::invoke_result_t<F, std::integral_constant<std::size_t, I>>...>{
        std::invoke(std::forward<F>(f), std::integral_constant<std::size_t, I>{})...};
  }
}

template <bool Enumerate, typename F, typename... Tuples, std::size_t... I>
auto zip_tuples_impl(F&& f, std::index_sequence<I...> seq, Tuples&&... tuples) {
  if constexpr (sizeof...(I) > 0) {
    return index_seq_for(
        [&f, &tuples...](auto integral_constant) {
          constexpr std::size_t index = integral_constant();  // std::integral_constant::operator()
          if constexpr (Enumerate) {
            return std::invoke(f, integral_constant,
                               std::get<index>(std::forward<Tuples>(tuples))...);
          } else {
            return std::invoke(f, std::get<index>(std::forward<Tuples>(tuples))...);
          }
        },
        seq);
  } else {
    // Return empty tuple when all input tuples are empty.
    // Suppress unused variable warnings:
    (void)f;
    (void)seq;
    ((void)tuples, ...);
    return std::tuple<>{};
  }
}

}  // namespace detail

// Zip together `N` tuples and invoke the provided callable on each element of the zip.
// The smallest tuple determines the highest iterated element. If the callable returns non-void,
// a new tuple will be returned containing the returned values.
template <typename F, typename... Tuples>
auto zip_tuples(F&& f, Tuples&&... tuples) {
  constexpr std::size_t min_len = std::min({std::tuple_size_v<std::decay_t<Tuples>>...});
  constexpr bool enumerate = false;
  return detail::zip_tuples_impl<enumerate>(std::forward<F>(f), std::make_index_sequence<min_len>(),
                                            std::forward<Tuples>(tuples)...);
}

// Same as `zip_tuples`, but the first argument to the callable will be an integer_constant
// indicating which element of the tuple is being iterated.
template <typename F, typename... Tuples>
auto zip_enumerate_tuples(F&& f, Tuples&&... tuples) {
  constexpr std::size_t min_len = std::min({std::tuple_size_v<std::decay_t<Tuples>>...});
  constexpr bool enumerate = true;
  return detail::zip_tuples_impl<enumerate>(std::forward<F>(f), std::make_index_sequence<min_len>(),
                                            std::forward<Tuples>(tuples)...);
}

}  // namespace wf
