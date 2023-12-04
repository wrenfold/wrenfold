// Copyright 2023 Gareth Cross
#include "wf/expression.h"
#include "wf/expressions/all_expressions.h"

namespace math {

// Visitor that can be used to sort expressions by determining their relative order.
struct order_visitor {
  template <typename T>
  relative_order operator()(const T& a, const T& b) const {
    return compare(a, b);
  }

  // This visitor applies to any two members of `OrderOfTypes` that are _not_ the same type.
  template <typename Numeric,
            typename = enable_if_contains_type_t<Numeric, Float, Integer, Rational, Constant>>
  constexpr relative_order compare(const Numeric& a, const Numeric& b) const noexcept {
    if (a < b) {
      return relative_order::less_than;
    } else if (b < a) {
      return relative_order::greater_than;
    }
    return relative_order::equal;
  }

  relative_order compare(const CastBool& a, const CastBool& b) const {
    return expression_order(a.arg(), b.arg());
  }

  relative_order compare(const Conditional& a, const Conditional& b) const {
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  constexpr relative_order compare(const Infinity&, const Infinity&) const noexcept {
    return relative_order::equal;
  }

  relative_order compare(const Addition& a, const Addition& b) const {
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  relative_order compare(const Derivative& a, const Derivative& b) const {
    if (a.order() < b.order()) {
      return relative_order::less_than;
    } else if (a.order() > b.order()) {
      return relative_order::greater_than;
    }
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  relative_order compare(const Multiplication& a, const Multiplication& b) const {
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  relative_order compare(const Power& a, const Power& b) const {
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  relative_order compare(const Function& a, const Function& b) const {
    // First compare by name:
    const int name_comp = a.function_name().compare(b.function_name());
    if (name_comp > 0) {
      return relative_order::greater_than;
    } else if (name_comp < 0) {
      return relative_order::less_than;
    }
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  relative_order compare(const Relational& a, const Relational& b) const {
    if (a.operation() < b.operation()) {
      return relative_order::less_than;
    } else if (a.operation() > b.operation()) {
      return relative_order::greater_than;
    }
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  constexpr relative_order compare(const Undefined&, const Undefined&) const noexcept {
    return relative_order::equal;
  }

  relative_order compare(const Variable& a, const Variable& b) const {
    if (a.identifier() < b.identifier()) {
      return relative_order::less_than;
    } else if (a.is_identical_to(b)) {
      return relative_order::equal;
    }
    return relative_order::greater_than;
  }

  // Order two sequences lexicographically.
  template <typename Iterator>
  static relative_order lexicographical_compare(Iterator begin_a, Iterator end_a, Iterator begin_b,
                                                Iterator end_b) {
    for (; begin_a != end_a && begin_b != end_b; ++begin_a, ++begin_b) {
      const relative_order result = expression_order(*begin_a, *begin_b);
      if (result == relative_order::less_than) {
        return relative_order::less_than;
      } else if (result == relative_order::greater_than) {
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
};

template <typename... Ts>
static constexpr auto get_type_order_indices(type_list<Ts...>) {
  using order_of_types =
      type_list<Float, Integer, Rational, Constant, Infinity, Variable, Multiplication, Addition,
                Power, Function, Relational, Conditional, CastBool, Derivative, Undefined>;

  // Every type in the approved type list must appear here, or we get a compile error:
  static_assert(type_list_size<order_of_types>::value == type_list_size<ExpressionTypeList>::value);

  return std::array<std::size_t, sizeof...(Ts)>{index_of_type_v<Ts, order_of_types>...};
}

relative_order expression_order(const Expr& a, const Expr& b) {
  // An array where element [i] is the position of the type with index `i` within
  // our preferred canonical ordering.
  static constexpr auto order = get_type_order_indices(ExpressionTypeList{});

  const auto index_a = order[a.type_index()];
  const auto index_b = order[b.type_index()];
  if (index_a < index_b) {
    return relative_order::less_than;
  } else if (index_a > index_b) {
    return relative_order::greater_than;
  }

  // Otherwise we have the same type:
  return visit(a, [&b](const auto& a_typed) -> relative_order {
    using Ta = std::decay_t<decltype(a_typed)>;
    const auto& b_typed = cast_unchecked<Ta>(b);
    return order_visitor{}(a_typed, b_typed);
  });
}

}  // namespace math
