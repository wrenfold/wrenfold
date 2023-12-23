// Copyright 2022 Gareth Cross
#pragma once
#include <functional>
#include <optional>
#include <tuple>
#include <variant>

namespace wf {

// Does nothing but act as a list of types:
template <typename... Ts>
struct type_list {};

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
constexpr bool has_call_operator_v<
    T, Argument, decltype(std::declval<T>()(std::declval<const Argument>()), void())> = true;

// Template to check if the `operator()` method is implemented for two types.
template <typename T, typename, typename, typename = void>
constexpr bool has_binary_call_operator_v = false;

// Specialization that is activated when the binary operator() method exists.
template <typename T, typename Argument1, typename Argument2>
constexpr bool has_binary_call_operator_v<
    T, Argument1, Argument2,
    decltype(std::declval<T>()(std::declval<const Argument1>(), std::declval<const Argument2>()),
             void())> = true;

// Size of type list.
template <typename T>
struct type_list_size;
template <typename... Ts>
struct type_list_size<type_list<Ts...>> {
  static constexpr std::size_t value = sizeof...(Ts);
};
template <typename List>
constexpr std::size_t type_list_size_v = type_list_size<List>::value;

// Check if a type is in a type_list.
template <typename T, typename... Ts>
struct type_list_contains_type : std::disjunction<std::is_same<T, Ts>...> {};
template <typename T, typename... Ts>
struct type_list_contains_type<T, type_list<Ts...>> : type_list_contains_type<T, Ts...> {};
template <typename T, typename... Ts>
constexpr bool type_list_contains_type_v = type_list_contains_type<T, Ts...>::value;

// enable-if that enables when `T` is in a list of types.
template <typename T, typename... Ts>
struct enable_if_contains_type : std::enable_if<type_list_contains_type_v<T, Ts...>> {};
template <typename T, typename... Ts>
struct enable_if_contains_type<T, type_list<Ts...>>
    : std::enable_if<type_list_contains_type_v<T, Ts...>> {};
template <typename T, typename... Ts>
using enable_if_contains_type_t = typename enable_if_contains_type<T, Ts...>::type;

// enable-if that enables when `T` is _not_ in a list of types.
template <typename T, typename... Ts>
struct enable_if_does_not_contain_type : std::enable_if<!type_list_contains_type_v<T, Ts...>> {};
template <typename T, typename... Ts>
struct enable_if_does_not_contain_type<T, type_list<Ts...>>
    : std::enable_if<!type_list_contains_type_v<T, Ts...>> {};
template <typename T, typename... Ts>
using enable_if_does_not_contain_type_t = typename enable_if_does_not_contain_type<T, Ts...>::type;

// Helper to append a type to the front of a type list.
template <typename, typename>
struct append_front_to_type_list;
template <typename T, typename... Args>
struct append_front_to_type_list<T, type_list<Args...>> {
  using type = type_list<T, Args...>;
};
template <typename T, typename List>
using append_front_to_type_list_t = typename append_front_to_type_list<T, List>::type;

// Get the head of a type list.
template <typename T>
struct head_of_type_list;
template <typename Head, typename... Ts>
struct head_of_type_list<type_list<Head, Ts...>> {
  using type = Head;
};
template <typename T>
using head_of_type_list_t = typename head_of_type_list<T>::type;

// Concatenate two type lists together into one.
template <typename, typename>
struct concatenate_type_lists;
template <typename... As, typename... Bs>
struct concatenate_type_lists<type_list<As...>, type_list<Bs...>> {
  using type = type_list<As..., Bs...>;
};
template <typename A, typename B>
using concatenate_type_lists_t = typename concatenate_type_lists<A, B>::type;

// Get the index of a type in a type list.
template <typename T, typename U = void, typename... Types>
constexpr std::size_t index_of_type_helper() {
  return std::is_same<T, U>::value ? 0 : 1 + index_of_type_helper<T, Types...>();
}
template <typename T, typename List>
struct index_of_type;
template <typename T, typename... Ts>
struct index_of_type<T, type_list<Ts...>> {
  constexpr static std::size_t value = index_of_type_helper<T, Ts...>();
};
template <typename T, typename List>
constexpr std::size_t index_of_type_v = index_of_type<T, List>::value;

// Get the N'th element of a type list.
template <std::size_t N, typename... Ts>
struct type_list_element;

template <std::size_t N, typename T, typename... Ts>
struct type_list_element<N, type_list<T, Ts...>> {
  using type = typename type_list_element<N - 1, type_list<Ts...>>::type;
};
template <typename T, typename... Ts>
struct type_list_element<0, type_list<T, Ts...>> {
  using type = T;
};
template <std::size_t N, typename List>
using type_list_element_t = typename type_list_element<N, List>::type;

template <typename T>
struct type_list_from_variant;
template <typename T>
using type_list_from_variant_t = typename type_list_from_variant<T>::type;
template <typename... Ts>
struct type_list_from_variant<std::variant<Ts...>> {
  using type = type_list<Ts...>;
};

// This template iterates over a type_list and creates a new type_list of return types that occur
// when `Callable` is invoked with each type in the input type list. Duplicates may occur.
template <typename Callable, typename...>
struct call_operator_return_types;
template <typename Callable, typename... Ts>
using call_operator_return_types_t = typename call_operator_return_types<Callable, Ts...>::type;

template <typename Callable>
struct call_operator_return_types<Callable> {
  using type = type_list<>;  //  This is the end of the recursion.
};

template <typename Callable, typename Head, typename... Tail>
struct call_operator_return_types<Callable, Head, Tail...> {
  // Check if the callable accepts this type, and determine the invoke result.
  using candidate_type =
      typename std::conditional_t<has_call_operator_v<Callable, Head>,
                                  std::invoke_result_t<Callable, Head>, std::nullptr_t>;

  // Don't append nullptr_t, this is the signal for an invocation that isn't valid (no operator()).
  using type = typename std::conditional_t<
      !std::is_same_v<candidate_type, std::nullptr_t>,
      append_front_to_type_list_t<candidate_type, call_operator_return_types_t<Callable, Tail...>>,
      call_operator_return_types_t<Callable, Tail...>>;
};

// Specialization that accepts a type-list.
template <typename Callable, typename... Ts>
struct call_operator_return_types<Callable, type_list<Ts...>> {
  // Build the list of possible turn types.
  using list = call_operator_return_types_t<Callable, Ts...>;
  // Get the first one (TODO: Check they all match!)
  using type = head_of_type_list_t<list>;
};

// Select `Indices` elements from a tuple. Returns a new tuple with just those elements.
template <typename Tuple, std::size_t... Indices>
auto select_from_tuple(Tuple&& tuple, std::index_sequence<Indices...>) {
  return std::tuple<std::tuple_element_t<Indices, std::remove_reference_t<Tuple>>...>(
      std::get<Indices>(std::forward<Tuple>(tuple))...);
}

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

}  // namespace wf
