// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/expression.h"
#include "wf/expressions/numeric_expressions.h"

namespace wf {

// Power operation: base**exponent
class power {
 public:
  static constexpr std::string_view name_str = "Power";
  static constexpr bool is_leaf_node = false;

  power(scalar_expr base, scalar_expr exponent) noexcept
      : children_{std::move(base), std::move(exponent)} {}

  // Implement ExpressionImpl::Map
  template <typename Operation>
  scalar_expr map_children(Operation&& operation) const {
    return power::create(operation(base()), operation(exponent()));
  }

  constexpr const auto& children() const noexcept { return children_; }

  // Create a new power.
  // Will apply rules to simplify automatically.
  static scalar_expr create(scalar_expr base, scalar_expr exp);

  constexpr const scalar_expr& base() const noexcept { return children_[0]; }
  constexpr const scalar_expr& exponent() const noexcept { return children_[1]; }

  constexpr auto begin() const noexcept { return children_.begin(); }
  constexpr auto end() const noexcept { return children_.end(); }

 protected:
  std::array<scalar_expr, 2> children_;
};

// Inspect the expression `base**exp`. If it simplifies or reduces to something _other_ than
// pow(base, exp), return that expression.
std::optional<scalar_expr> pow_maybe_simplify(const scalar_expr& base, const scalar_expr& exp);

// Convert an expression to a base/exponent pair.
std::pair<scalar_expr, scalar_expr> as_base_and_exp(const scalar_expr& expr);

// Convert a rational exponent to the whole integer part and the remainder.
// If the exponent is negative, we add to the whole integer part so that the rational part
// can be positive (i.e. we eliminate "absurd" rationals).
constexpr std::pair<checked_int, rational_constant> factorize_rational_exponent(
    const rational_constant& r) {
  const checked_int integer_part = r.numerator() / r.denominator();
  const rational_constant fractional_part_signed{r.numerator() % r.denominator(), r.denominator()};
  if (r.numerator() >= 0 || fractional_part_signed.is_zero()) {
    return std::make_pair(integer_part, fractional_part_signed);
  } else {
    // If negative, we subtract one from the integer part and make the rational part positive:
    return std::make_pair(integer_part - 1, fractional_part_signed + rational_constant{1, 1});
  }
}

template <>
struct hash_struct<power> {
  std::size_t operator()(const power& pow) const noexcept {
    return hash_args(0, pow.base(), pow.exponent());
  }
};

template <>
struct is_identical_struct<power> {
  bool operator()(const power& a, const power& b) const {
    return are_identical(a.base(), b.base()) && are_identical(a.exponent(), b.exponent());
  }
};

template <>
struct order_struct<power> {
  relative_order operator()(const power& a, const power& b) const {
    return wf::lexicographical_order(a, b, order_struct<scalar_expr>{});
  }
};

}  // namespace wf
