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
  bool IsIdenticalTo(const Power& other) const {
    return base_.IsIdenticalTo(other.base_) && exponent_.IsIdenticalTo(other.exponent_);
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void Iterate(Operation operation) const {
    operation(base_);
    operation(exponent_);
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr Map(Operation&& operation) const {
    return Power::Create(operation(base_), operation(exponent_));
  }

  // Create a new power.
  // Will apply rules to simplify automatically.
  static Expr Create(const Expr& a, const Expr& b);

  const Expr& Base() const { return base_; }
  const Expr& Exponent() const { return exponent_; }

 protected:
  Expr base_;
  Expr exponent_;
};

// Convert an expression to a base/exponent pair.
std::pair<Expr, Expr> AsBaseAndExponent(const Expr& expr);

// Convert a rational exponent to the whole integer part and the remainder.
// If the exponent is negative, we add to the whole integer part so that the rational part
// can be positive (i.e. we eliminate "absurd" rationals).
inline std::pair<Integer, Rational> FactorizeRationalExponent(const Rational& r) {
  const Integer integer_part{r.Numerator() / r.Denominator()};
  const Rational fractional_part_signed{r.Numerator() % r.Denominator(), r.Denominator()};
  if (r.Numerator() >= 0) {
    return std::make_pair(integer_part, fractional_part_signed);
  } else {
    // If negative, we subtract one from the integer part and make the rational part positive:
    return std::make_pair(Integer{integer_part.GetValue() - 1},
                          fractional_part_signed + Rational{1, 1});
  }
}

template <>
struct Hash<Power> {
  std::size_t operator()(const Power& pow) const { return HashArgs(0, pow.Base(), pow.Exponent()); }
};

}  // namespace math
