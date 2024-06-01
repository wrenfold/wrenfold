// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/expression.h"

#include "wf/constants.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"
#include "wf/plain_formatter.h"
#include "wf/tree_formatter.h"
#include "wf/utility/assertions.h"

namespace wf {

scalar_expr::scalar_expr(const std::string_view name, const number_set set)
    : scalar_expr(std::in_place_type_t<variable>{}, std::string{name}, set) {}

static scalar_expr simplify_rational(rational_constant r) {
  if (const auto as_int = r.try_convert_to_integer(); as_int.has_value()) {
    return scalar_expr(as_int->value());
  }
  return scalar_expr(std::in_place_type_t<rational_constant>{}, r);
}

scalar_expr::scalar_expr(const rational_constant r) : scalar_expr(simplify_rational(r)) {}

scalar_expr::scalar_expr(const float_constant f)
    : scalar_expr(scalar_expr::from_float(f.value())) {}

scalar_expr::scalar_expr(const integer_constant i)
    : scalar_expr(scalar_expr::from_int(i.value())) {}

scalar_expr scalar_expr::from_complex(const double a, const double b) {
  return scalar_expr(a) + scalar_expr(b) * constants::imaginary_unit;
}

scalar_expr scalar_expr::from_float(const double x) {
  if (std::isnan(x)) {
    return constants::undefined;
  } else if (std::isinf(x)) {
    // Not exactly true, since floating point is closer to the affine extension
    // of the real numbers. But we don't have +/- real infinity.
    return constants::complex_infinity;
  }
  if (!std::isfinite(x)) {
    throw wf::domain_error("Floating point values must be finite: {}", x);
  }
  return make_expr<float_constant>(x);
}

scalar_expr scalar_expr::from_int(const checked_int x) {
  if (x == 0) {
    return constants::zero;
  } else if (x == 1) {
    return constants::one;
  } else if (x == -1) {
    return constants::negative_one;
  }
  return make_expr<integer_constant>(x);
}

std::string scalar_expr::to_string() const {
  plain_formatter formatter{};
  formatter(*this);
  return formatter.take_output();
}

std::string scalar_expr::to_expression_tree_string() const {
  tree_formatter_visitor formatter{};
  formatter(*this);
  return formatter.take_output();
}

scalar_expr scalar_expr::operator-() const {
  return multiplication::from_operands({constants::negative_one, *this});
}

scalar_expr operator+(const scalar_expr& a, const scalar_expr& b) {
  // See note on absl::Span() constructor, the lifetimes here are valid.
  // We are constructing an initializer_list.
  return addition::from_operands({a, b});
}

scalar_expr operator-(const scalar_expr& a, const scalar_expr& b) {
  return a + multiplication::from_operands({constants::negative_one, b});
}

scalar_expr operator*(const scalar_expr& a, const scalar_expr& b) {
  return multiplication::from_operands({a, b});
}

scalar_expr operator/(const scalar_expr& a, const scalar_expr& b) {
  auto one_over_b = power::create(b, constants::negative_one);
  return multiplication::from_operands({a, one_over_b});
}

boolean_expr operator<(const scalar_expr& a, const scalar_expr& b) {
  return relational::create(relational_operation::less_than, a, b);
}

boolean_expr operator>(const scalar_expr& a, const scalar_expr& b) {
  return relational::create(relational_operation::less_than, b, a);
}

boolean_expr operator<=(const scalar_expr& a, const scalar_expr& b) {
  return relational::create(relational_operation::less_than_or_equal, a, b);
}

boolean_expr operator>=(const scalar_expr& a, const scalar_expr& b) {
  return relational::create(relational_operation::less_than_or_equal, b, a);
}

boolean_expr operator==(const scalar_expr& a, const scalar_expr& b) {
  return relational::create(relational_operation::equal, a, b);
}

template <typename... AllTypes, typename... OrderedTypes>
static constexpr auto get_type_order_indices(type_list<AllTypes...>,
                                             type_list<OrderedTypes...>) noexcept {
  using all_types_list = type_list<AllTypes...>;
  using ordered_list = type_list<OrderedTypes...>;

  // The lists should contain the same elements, just in a different order:
  static_assert(sizeof...(AllTypes) == sizeof...(OrderedTypes));
  static_assert(std::conjunction_v<type_list_contains<OrderedTypes, all_types_list>...>,
                "A type in OrderedTypes is not present in AllTypes.");
  static_assert(std::conjunction_v<type_list_contains<AllTypes, ordered_list>...>,
                "A type in AllTypes is not present in OrderedTypes.");

  // Return array mapping from [index in expression_variant] --> [index in ordered list].
  return std::array<uint16_t, sizeof...(AllTypes)>{
      static_cast<uint16_t>(type_list_index_v<AllTypes, ordered_list>)...};
}

relative_order order_struct<scalar_expr>::compare(const scalar_expr& a,
                                                  const scalar_expr& b) const {
  using order_of_types =
      type_list<float_constant, integer_constant, rational_constant, symbolic_constant,
                complex_infinity, imaginary_unit, variable, multiplication, addition, power,
                function, unevaluated, conditional, iverson_bracket, compound_expression_element,
                derivative, undefined>;
  static constexpr auto order =
      get_type_order_indices(scalar_expr::storage_type::types{}, order_of_types{});

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
    static_assert(is_orderable_v<Ta>, "Type does not implement order_struct.");

    const auto& b_typed = get_unchecked<const Ta>(b);
    return order_struct<Ta>{}(a_typed, b_typed);
  });
}

// Visitor to determine mathematical precedence.
struct precedence_visitor {
  template <typename T>
  constexpr precedence operator()(const T& value) const noexcept {
    if constexpr (std::is_same_v<multiplication, T>) {
      return precedence::multiplication;
    } else if constexpr (std::is_same_v<addition, T>) {
      return precedence::addition;
    } else if constexpr (std::is_same_v<power, T>) {
      return precedence::power;
    } else if constexpr (std::is_same_v<rational_constant, T>) {
      return precedence::multiplication;
    } else if constexpr (std::is_same_v<relational, T>) {
      return precedence::relational;
    } else if constexpr (type_list_contains_v<T, integer_constant, rational_constant,
                                              float_constant>) {
      return value.is_negative() ? precedence::multiplication : precedence::none;
    } else {
      return precedence::none;
    }
  }
};

precedence get_precedence(const scalar_expr& expr) { return visit(expr, precedence_visitor{}); }

scalar_expr make_unique_variable_symbol(number_set set) {
  return make_expr<variable>(unique_variable(), set);
}

}  // namespace wf
