// Copyright 2023 Gareth Cross
#include "expression.h"
#include "expressions/all_expressions.h"

namespace math {

// Visitor that can be used to sort expressions by determining their relative order.
struct OrderVisitor {
  template <typename T>
  RelativeOrder operator()(const T& a, const T& b) const {
    return compare(a, b);
  }

  // This visitor applies to any two members of `OrderOfTypes` that are _not_ the same type.
  template <typename Numeric,
            typename = enable_if_contains_type_t<Numeric, Float, Integer, Rational, Constant>>
  constexpr RelativeOrder compare(const Numeric& a, const Numeric& b) const noexcept {
    if (a < b) {
      return RelativeOrder::LessThan;
    } else if (b < a) {
      return RelativeOrder::GreaterThan;
    }
    return RelativeOrder::Equal;
  }

  RelativeOrder compare(const CastBool& a, const CastBool& b) const {
    return expression_order(a.arg(), b.arg());
  }

  RelativeOrder compare(const Conditional& a, const Conditional& b) const {
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  constexpr RelativeOrder compare(const Infinity&, const Infinity&) const noexcept {
    return RelativeOrder::Equal;
  }

  RelativeOrder compare(const Addition& a, const Addition& b) const {
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  RelativeOrder compare(const Derivative& a, const Derivative& b) const {
    if (a.order() < b.order()) {
      return RelativeOrder::LessThan;
    } else if (a.order() > b.order()) {
      return RelativeOrder::GreaterThan;
    }
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  RelativeOrder compare(const Multiplication& a, const Multiplication& b) const {
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  RelativeOrder compare(const Power& a, const Power& b) const {
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  RelativeOrder compare(const Function& a, const Function& b) const {
    // First compare by name:
    const int name_comp = a.function_name().compare(b.function_name());
    if (name_comp > 0) {
      return RelativeOrder::GreaterThan;
    } else if (name_comp < 0) {
      return RelativeOrder::LessThan;
    }
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  RelativeOrder compare(const Relational& a, const Relational& b) const {
    if (a.operation() < b.operation()) {
      return RelativeOrder::LessThan;
    } else if (a.operation() > b.operation()) {
      return RelativeOrder::GreaterThan;
    }
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  constexpr RelativeOrder compare(const Undefined&, const Undefined&) const noexcept {
    return RelativeOrder::Equal;
  }

  RelativeOrder compare(const Variable& a, const Variable& b) const {
    if (a.identifier() < b.identifier()) {
      return RelativeOrder::LessThan;
    } else if (a.is_identical_to(b)) {
      return RelativeOrder::Equal;
    }
    return RelativeOrder::GreaterThan;
  }

  // Order two sequences lexicographically.
  template <typename Iterator>
  static RelativeOrder lexicographical_compare(Iterator begin_a, Iterator end_a, Iterator begin_b,
                                               Iterator end_b) {
    for (; begin_a != end_a && begin_b != end_b; ++begin_a, ++begin_b) {
      const RelativeOrder result = expression_order(*begin_a, *begin_b);
      if (result == RelativeOrder::LessThan) {
        return RelativeOrder::LessThan;
      } else if (result == RelativeOrder::GreaterThan) {
        return RelativeOrder::GreaterThan;
      }
    }
    if (begin_a == end_a && begin_b != end_b) {
      return RelativeOrder::LessThan;  // `a` is shorter:
    } else if (begin_a != end_a && begin_b == end_b) {
      return RelativeOrder::GreaterThan;  // `b` is shorter
    }
    return RelativeOrder::Equal;  //  they are equal
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

RelativeOrder expression_order(const Expr& a, const Expr& b) {
  // An array where element [i] is the position of the type with index `i` within
  // our preferred canonical ordering.
  static constexpr auto order = get_type_order_indices(ExpressionTypeList{});

  const auto index_a = order[a.type_index()];
  const auto index_b = order[b.type_index()];
  if (index_a < index_b) {
    return RelativeOrder::LessThan;
  } else if (index_a > index_b) {
    return RelativeOrder::GreaterThan;
  }

  // Otherwise we have the same type:
  return visit(a, [&b](const auto& a_typed) -> RelativeOrder {
    using Ta = std::decay_t<decltype(a_typed)>;
    const auto& b_typed = cast_unchecked<Ta>(b);
    return OrderVisitor{}(a_typed, b_typed);
  });
}

}  // namespace math
