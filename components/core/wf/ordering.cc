// Copyright 2023 Gareth Cross
#include "wf/expression.h"
#include "wf/expressions/all_expressions.h"
#include "wf/visitor_impl.h"

namespace wf {

// Visitor that can be used to sort expressions by determining their relative order.
struct order_visitor {
  template <typename T>
  relative_order operator()(const T& a, const T& b) const {
    return compare(a, b);
  }

  // This visitor applies to any two members of `OrderOfTypes` that are _not_ the same type.
  template <typename Numeric,
            typename = enable_if_contains_type_t<Numeric, float_constant, integer_constant,
                                                 rational_constant, symbolic_constant>>
  constexpr relative_order compare(const Numeric& a, const Numeric& b) const noexcept {
    if (a < b) {
      return relative_order::less_than;
    } else if (b < a) {
      return relative_order::greater_than;
    }
    return relative_order::equal;
  }

  relative_order compare(const cast_bool& a, const cast_bool& b) const {
    return expression_order(a.arg(), b.arg());
  }

  relative_order compare(const conditional& a, const conditional& b) const {
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  constexpr relative_order compare(const complex_infinity&,
                                   const complex_infinity&) const noexcept {
    return relative_order::equal;
  }

  relative_order compare(const addition& a, const addition& b) const {
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  relative_order compare(const derivative& a, const derivative& b) const {
    if (a.order() < b.order()) {
      return relative_order::less_than;
    } else if (a.order() > b.order()) {
      return relative_order::greater_than;
    }
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  relative_order compare(const multiplication& a, const multiplication& b) const {
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  relative_order compare(const power& a, const power& b) const {
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  relative_order compare(const function& a, const function& b) const {
    // First compare by name:
    const int name_comp = a.function_name().compare(b.function_name());
    if (name_comp > 0) {
      return relative_order::greater_than;
    } else if (name_comp < 0) {
      return relative_order::less_than;
    }
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  relative_order compare(const relational& a, const relational& b) const {
    if (a.operation() < b.operation()) {
      return relative_order::less_than;
    } else if (a.operation() > b.operation()) {
      return relative_order::greater_than;
    }
    return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }

  constexpr relative_order compare(const undefined&, const undefined&) const noexcept {
    return relative_order::equal;
  }

  relative_order compare(const variable& a, const variable& b) const {
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
      type_list<float_constant, integer_constant, rational_constant, symbolic_constant,
                complex_infinity, variable, multiplication, addition, power, function, relational,
                conditional, cast_bool, derivative, undefined>;

  // Every type in the approved type list must appear here, or we get a compile error:
  static_assert(type_list_size<order_of_types>::value == sizeof...(Ts));

  return std::array<std::size_t, sizeof...(Ts)>{type_list_index_v<Ts, order_of_types>...};
}

relative_order expression_order(const Expr& a, const Expr& b) {
  // An array where element [i] is the position of the type with index `i` within
  // our preferred canonical ordering.
  using all_types = Expr::storage_type::types;
  static constexpr auto order = get_type_order_indices(all_types{});

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

}  // namespace wf
