// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <string>
#include <variant>
#include <vector>

#include "wf/utility/traits.h"

namespace wf {

// Describe the relative order of two expressions (a, b)
// This is for operations like sorting, not for expressing mathematical relations.
// Can eventually replace this with spaceship operator in C++20.
enum class relative_order : int {
  // a < b
  less_than = -1,
  // a == b
  equal = 0,
  // a > b
  greater_than = 1,
};

// Convert `relative_order` to string view.
constexpr std::string_view string_from_relative_order(const relative_order order) noexcept {
  switch (order) {
    case relative_order::less_than:
      return "less_than";
    case relative_order::equal:
      return "equal";
    case relative_order::greater_than:
      return "greater_than";
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Order object of type `T`.
// Implementations should expose `relative_order operator()(const T& a, const T& b) const`.
template <typename T, typename = void>
struct order_struct;

// True if type `T` implements `order_struct<T>`.
template <typename T, typename = void>
struct is_orderable : std::false_type {};
template <typename T>
struct is_orderable<T, std::enable_if_t<is_invocable_v<order_struct<T>, const T&, const T&>>>
    : std::is_same<std::invoke_result_t<order_struct<T>, const T&, const T&>, relative_order> {};
template <typename T>
constexpr bool is_orderable_v = is_orderable<T>::value;

// Determine relative ordering between two objects of type `T`.
template <typename T>
relative_order determine_order(const T& a, const T& b) {
  return order_struct<T>{}(a, b);
}

// Pick `relative_order` using comparison operators. Type must implement `<`.
// TODO: Use spaceship when switching to c++20.
template <typename T>
constexpr relative_order order_by_comparison(const T& a, const T& b) noexcept(noexcept(a < b)) {
  if (a < b) {
    return relative_order::less_than;
  } else if (b < a) {
    return relative_order::greater_than;
  }
  return relative_order::equal;
}

// Determine relative lexicographical order of two sequences, given iterators and a comparison
// predicate. `compare` should return `relative_order`.
template <typename ItA, typename ItB, typename Compare>
constexpr relative_order lexicographical_order(ItA begin_a, ItA end_a, ItB begin_b, ItB end_b,
                                               Compare&& compare) {
  for (; begin_a != end_a && begin_b != end_b; ++begin_a, ++begin_b) {
    if (const relative_order order = compare(*begin_a, *begin_b);
        order == relative_order::less_than) {
      return relative_order::less_than;
    } else if (order == relative_order::greater_than) {
      return relative_order::greater_than;
    }
  }
  if (begin_a == end_a && begin_b != end_b) {
    return relative_order::less_than;  // `a` is shorter:
  } else if (begin_a != end_a && begin_b == end_b) {
    return relative_order::greater_than;  // `b` is shorter
  }
  return relative_order::equal;  //  they are equal
}

// Version of `lexicographical_compare` that accepts containers directly.
template <typename A, typename B, typename Compare>
constexpr relative_order lexicographical_order(const A& a, const B& b, Compare&& compare) {
  return lexicographical_order(std::begin(a), std::end(a), std::begin(b), std::end(b),
                               std::forward<Compare>(compare));
}

// Compute `relative_order` for integral types.
template <typename T>
struct order_struct<T, std::enable_if_t<std::is_integral_v<T>>> {
  constexpr relative_order operator()(T a, T b) const noexcept {
    return wf::order_by_comparison(a, b);
  }
};

// Compute `relative_order` for std::string.
template <>
struct order_struct<std::string> {
  relative_order operator()(const std::string& a, const std::string& b) const noexcept {
    return wf::lexicographical_order(a.begin(), a.end(), b.begin(), b.end(), order_struct<char>{});
  }
};

// Compute `relative_order` for std::vector. (Lexicographical order by elements).
template <typename T>
struct order_struct<std::vector<T>> {
  relative_order operator()(const std::vector<T>& a, const std::vector<T>& b) const {
    return wf::lexicographical_order(a, b, order_struct<T>{});
  }
};

// Inherit to implement ordering of a variant.
template <typename T>
struct order_variant;
template <typename... Ts>
struct order_variant<std::variant<Ts...>> {
  relative_order operator()(const std::variant<Ts...>& a, const std::variant<Ts...>& b) const {
    if (a.index() < b.index()) {
      return relative_order::less_than;
    } else if (a.index() > b.index()) {
      return relative_order::greater_than;
    }
    return std::visit(
        [&](const auto& first) -> relative_order {
          // Cast is safe because we know the types match:
          using T = std::decay_t<decltype(first)>;
          return order_struct<T>{}(first, std::get<T>(b));
        },
        a);
  }
};

namespace detail {

// Helper for writing chains of comparisons. This is to make it easier to order by
// multiple fields in order:
class comparison_chain {
 public:
  constexpr explicit comparison_chain(const relative_order order) noexcept : previous_(order) {}

  // Implicit cast to `relative_order` so we can return this directly from functions.
  constexpr operator relative_order() const noexcept { return previous_; }  // NOLINT

  // Add another comparison to the chain. If `T` implements order_struct we use that, otherwise
  // we fall back to trying to use `<` operator.
  template <typename T>
  constexpr comparison_chain and_then_by(const T& a, const T& b) const {
    if (previous_ != relative_order::equal) {
      // Only add to the chain if we haven't already determined the order.
      return comparison_chain(previous_);
    }
    if constexpr (is_orderable_v<T>) {
      return comparison_chain(determine_order(a, b));
    } else {
      return comparison_chain(order_by_comparison(a, b));
    }
  }

  constexpr bool is_less_than() const noexcept { return previous_ == relative_order::less_than; }

 private:
  relative_order previous_;
};

}  // namespace detail

// Start a comparison chain by constructing `comparison_chain`. `a` and `b` are the
// first comparison of the chain.
template <typename T>
constexpr auto order_by(const T& a, const T& b) {
  if constexpr (is_orderable_v<T>) {
    return detail::comparison_chain(determine_order(a, b));
  } else {
    return detail::comparison_chain(order_by_comparison(a, b));
  }
}

}  // namespace wf
