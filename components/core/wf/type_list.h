// Copyright 2024 Gareth Cross
#pragma once
#include <tuple>
#include <type_traits>
#include <variant>

// Utilities for manipulating the `type_list` type.
namespace wf {

// Does nothing but act as a list of types:
template <typename... Ts>
struct type_list {};

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
struct type_list_contains : std::disjunction<std::is_same<T, Ts>...> {};
template <typename T, typename... Ts>
struct type_list_contains<T, type_list<Ts...>> : type_list_contains<T, Ts...> {};
template <typename T, typename... Ts>
constexpr bool type_list_contains_v = type_list_contains<T, Ts...>::value;

// enable-if that enables when `T` is in a list of types.
template <typename T, typename... Ts>
struct enable_if_contains_type : std::enable_if<type_list_contains_v<T, Ts...>> {};
template <typename T, typename... Ts>
struct enable_if_contains_type<T, type_list<Ts...>>
    : std::enable_if<type_list_contains_v<T, Ts...>> {};
template <typename T, typename... Ts>
using enable_if_contains_type_t = typename enable_if_contains_type<T, Ts...>::type;

// enable-if that enables when `T` is _not_ in a list of types.
template <typename T, typename... Ts>
struct enable_if_does_not_contain_type : std::enable_if<!type_list_contains_v<T, Ts...>> {};
template <typename T, typename... Ts>
struct enable_if_does_not_contain_type<T, type_list<Ts...>>
    : std::enable_if<!type_list_contains_v<T, Ts...>> {};
template <typename T, typename... Ts>
using enable_if_does_not_contain_type_t = typename enable_if_does_not_contain_type<T, Ts...>::type;

// Helper to append a type to the front of a type list.
template <typename, typename>
struct type_list_push_front;
template <typename T, typename... Args>
struct type_list_push_front<T, type_list<Args...>> {
  using type = type_list<T, Args...>;
};
template <typename T, typename List>
using type_list_push_front_t = typename type_list_push_front<T, List>::type;

// Get the front/head of a type list.
template <typename T>
struct type_list_front;
template <typename Head, typename... Ts>
struct type_list_front<type_list<Head, Ts...>> {
  using type = Head;
};
template <typename T>
using type_list_front_t = typename type_list_front<T>::type;

// Concatenate two type lists together into one.
template <typename, typename>
struct type_list_concatenate;
template <typename... As, typename... Bs>
struct type_list_concatenate<type_list<As...>, type_list<Bs...>> {
  using type = type_list<As..., Bs...>;
};
template <typename A, typename B>
using type_list_concatenate_t = typename type_list_concatenate<A, B>::type;

// Get the index of a type in a type list.
template <typename T, typename U = void, typename... Types>
constexpr std::size_t index_of_type_helper() {
  return std::is_same_v<T, U> ? 0 : 1 + index_of_type_helper<T, Types...>();
}
template <typename T, typename List>
struct type_list_index;
template <typename T, typename... Ts>
struct type_list_index<T, type_list<Ts...>> {
  static_assert(type_list_contains_v<T, type_list<Ts...>>,
                "Specified type list does contain the query type.");
  constexpr static std::size_t value = index_of_type_helper<T, Ts...>();
};
template <typename T, typename List>
constexpr std::size_t type_list_index_v = type_list_index<T, List>::value;

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
    return type_list_push_front_t<T, remainder>{};
  } else {
    return remainder{};
  }
}
}  // namespace detail

// Filter a type list w/ the provided predicate.
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

}  // namespace wf
