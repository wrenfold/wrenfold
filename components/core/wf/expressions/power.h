// Copyright 2022 Gareth Cross
#pragma once
#include "wf/expression.h"
#include "wf/expressions/numeric_expressions.h"

namespace math {

// Power operation: base**exponent
class power {
 public:
  static constexpr std::string_view name_str = "Power";
  static constexpr bool is_leaf_node = false;

  power(Expr base, Expr exponent) : children_{std::move(base), std::move(exponent)} {}

  // Base and exponent must match.
  bool is_identical_to(const power& other) const {
    return base().is_identical_to(other.base()) && exponent().is_identical_to(other.exponent());
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void for_each(Operation&& operation) const {
    std::for_each(children_.begin(), children_.end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    return power::create(operation(base()), operation(exponent()));
  }

  // Create a new power.
  // Will apply rules to simplify automatically.
  static Expr create(Expr a, Expr b);

  constexpr const Expr& base() const noexcept { return children_[0]; }
  constexpr const Expr& exponent() const noexcept { return children_[1]; }

  constexpr auto begin() const noexcept { return children_.begin(); }
  constexpr auto end() const noexcept { return children_.end(); }

 protected:
  std::array<Expr, 2> children_;
};

// Convert an expression to a base/exponent pair.
std::pair<Expr, Expr> as_base_and_exp(const Expr& expr);

// Convert a rational exponent to the whole integer part and the remainder.
// If the exponent is negative, we add to the whole integer part so that the rational part
// can be positive (i.e. we eliminate "absurd" rationals).
inline constexpr std::pair<integer_constant, rational_constant> factorize_rational_exponent(
    const rational_constant& r) {
  const integer_constant integer_part{r.numerator() / r.denominator()};
  const rational_constant fractional_part_signed{r.numerator() % r.denominator(), r.denominator()};
  if (r.numerator() >= 0) {
    return std::make_pair(integer_part, fractional_part_signed);
  } else {
    // If negative, we subtract one from the integer part and make the rational part positive:
    return std::make_pair(integer_constant{integer_part.get_value() - 1},
                          fractional_part_signed + rational_constant{1, 1});
  }
}

template <>
struct hash_struct<power> {
  std::size_t operator()(const power& pow) const {
    return hash_args(0, pow.base(), pow.exponent());
  }
};

}  // namespace math
