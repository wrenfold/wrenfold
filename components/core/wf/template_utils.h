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
  return std::is_same_v<T, U> ? 0 : 1 + index_of_type_helper<T, Types...>();
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

template <typename T>
struct type_list_from_tuple;
template <typename T>
using type_list_from_tuple_t = typename type_list_from_tuple<T>::type;
template <typename... Ts>
struct type_list_from_tuple<std::tuple<Ts...>> {
  using type = type_list<Ts...>;
};

// Perform a map on a type list, and produce a new type list.
template <template <typename...> typename Map, typename T>
struct type_list_map;
template <template <typename...> typename Map, typename... Ts>
struct type_list_map<Map, type_list<Ts...>> {
  using type = type_list<Map<Ts>...>;
};
template <template <typename...> typename Map, typename T>
using type_list_map_t = typename type_list_map<Map, T>::type;

namespace detail {
template <template <typename...> class Filter>
constexpr auto type_list_filter_(type_list<>) -> type_list<>;

template <template <typename...> class Filter, typename T, typename... Ts>
constexpr auto type_list_filter_(type_list<T, Ts...>) {
  using remainder = decltype(type_list_filter_<Filter>(type_list<Ts...>{}));
  if constexpr (Filter<T>::value) {
    return append_front_to_type_list_t<T, remainder>{};
  } else {
    return remainder{};
  }
}
}  // namespace detail

template <template <typename...> class Filter, typename T>
struct type_list_filter;
template <template <typename...> class Filter, typename... Ts>
struct type_list_filter<Filter, type_list<Ts...>> {
  using type = decltype(detail::type_list_filter_<Filter>(type_list<Ts...>{}));
};
template <template <typename...> typename Filter, typename T>
using type_list_filter_t = typename type_list_filter<Filter, T>::type;

// See `filter_type_sequence`.
template <template <typename...> class Predicate, std::size_t... S, std::size_t I, std::size_t... R,
          typename T, typename... Ts>
constexpr auto filter_type_sequence(type_list<T, Ts...>, std::index_sequence<S...>,
                                    std::index_sequence<I, R...>) {
  static_assert(sizeof...(Ts) == sizeof...(R));
  if constexpr (sizeof...(R) > 0) {
    if constexpr (Predicate<T>::value) {
      return filter_type_sequence<Predicate>(type_list<Ts...>{}, std::index_sequence<S..., I>{},
                                             std::index_sequence<R...>{});
    } else {
      return filter_type_sequence<Predicate>(type_list<Ts...>{}, std::index_sequence<S...>{},
                                             std::index_sequence<R...>{});
    }
  } else {
    if constexpr (Predicate<T>::value) {
      return std::index_sequence<S..., I>{};
    } else {
      return std::index_sequence<S...>{};
    }
  }
}

// Filter a type list with the given predicate template. Returns an std::index_sequence that
// includes indices of the selected types form the original list.
template <template <typename...> class Predicate, typename... Ts>
constexpr auto filter_type_sequence(type_list<Ts...> list) {
  return filter_type_sequence<Predicate>(list, std::index_sequence<>{},
                                         std::make_index_sequence<sizeof...(Ts)>());
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
