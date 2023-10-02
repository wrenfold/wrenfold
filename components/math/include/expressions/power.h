// Copyright 2022 Gareth Cross
#pragma once
#include "expression.h"
#include "expressions/numeric_expressions.h"

namespace math {

// Power operation: base^exponent
class Power {
 public:
  static constexpr std::string_view NameStr = "Power";
  static constexpr bool IsLeafNode = false;

  Power(Expr base, Expr exponent) : base_(std::move(base)), exponent_(std::move(exponent)) {}

  // Base and exponent must match.
  bool is_identical_to(const Power& other) const {
    return base_.is_identical_to(other.base_) && exponent_.is_identical_to(other.exponent_);
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void iterate(Operation operation) const {
    operation(base_);
    operation(exponent_);
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    return Power::create(operation(base_), operation(exponent_));
  }

  // Create a new power.
  // Will apply rules to simplify automatically.
  static Expr create(const Expr& a, const Expr& b);

  const Expr& base() const { return base_; }
  const Expr& exponent() const { return exponent_; }

 protected:
  Expr base_;
  Expr exponent_;
};

// Convert an expression to a base/exponent pair.
std::pair<Expr, Expr> as_base_and_exp(const Expr& expr);

// Convert a rational exponent to the whole integer part and the remainder.
// If the exponent is negative, we add to the whole integer part so that the rational part
// can be positive (i.e. we eliminate "absurd" rationals).
inline constexpr std::pair<Integer, Rational> factorize_rational_exponent(const Rational& r) {
  const Integer integer_part{r.numerator() / r.denominator()};
  const Rational fractional_part_signed{r.numerator() % r.denominator(), r.denominator()};
  if (r.numerator() >= 0) {
    return std::make_pair(integer_part, fractional_part_signed);
  } else {
    // If negative, we subtract one from the integer part and make the rational part positive:
    return std::make_pair(Integer{integer_part.get_value() - 1},
                          fractional_part_signed + Rational{1, 1});
  }
}

template <>
struct Hash<Power> {
  std::size_t operator()(const Power& pow) const { return HashArgs(0, pow.base(), pow.exponent()); }
};

}  // namespace math
