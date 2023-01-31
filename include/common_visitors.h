// Copyright 2022 Gareth Cross
#pragma once
#include <algorithm>

#include "expressions/multiplication.h"
#include "expressions/numeric_expressions.h"
#include "expressions/power.h"
#include "visitor_impl.h"

// This file is intended to contain common utility visitors.
namespace math {

template <typename Derived>
class NAryOp;

// Visitor that returns true for numerical values, or powers of numerical values.
struct IsNumericVisitor {
  using ReturnType = bool;
  constexpr static VisitorPolicy Policy = VisitorPolicy::NoError;
  constexpr bool Apply(const Float&) const { return true; }
  constexpr bool Apply(const Integer&) const { return true; }
  constexpr bool Apply(const Rational&) const { return true; }
  bool Apply(const Power& pow) const {
    return VisitStruct(pow.Base(), IsNumericVisitor{}).value_or(false) &&
           VisitStruct(pow.Exponent(), IsNumericVisitor{}).value_or(false);
  }
};

inline bool IsNumeric(const Expr& expr) {
  return VisitStruct(expr, IsNumericVisitor{}).value_or(false);
}

// Visitor that identifies negative numeric constants, or products of numeric constants that will be
// negative.
struct IsNegativeNumberVisitor {
  using ReturnType = bool;
  constexpr static VisitorPolicy Policy = VisitorPolicy::NoError;

  // Numerics < 0 are all negative.
  bool Apply(const Integer& num) const { return num.GetValue() < 0; }
  bool Apply(const Float& f) const { return f.GetValue() < 0; }
  bool Apply(const Rational& r) const { return r.Numerator() < 0; }

  // Multiplications can be negative-like, if the product of all the constant terms is negative.
  bool Apply(const Multiplication& m) const {
    const std::size_t count = std::count_if(m.begin(), m.end(), [](const Expr& expr) {
      return VisitStruct(expr, IsNegativeNumberVisitor{}).value_or(false);
    });
    // odd = negative, even = positive
    return static_cast<bool>(count & 1);
  }
};

inline bool IsNegativeNumber(const Expr& expr) {
  return VisitStruct(expr, IsNegativeNumberVisitor{}).value_or(false);
}

// Visitor to determine mathematical precedence.
struct PrecedenceVisitor {
  static constexpr VisitorPolicy Policy = VisitorPolicy::NoError;
  using ReturnType = Precedence;

  constexpr Precedence Apply(const Multiplication&) const { return Precedence::Multiplication; }
  constexpr Precedence Apply(const Addition&) const { return Precedence::Addition; }
  constexpr Precedence Apply(const Power&) const { return Precedence::Power; }
  constexpr Precedence Apply(const Rational&) const { return Precedence::Multiplication; }
};

inline Precedence GetPrecedence(const Expr& expr) {
  return VisitStruct(expr, PrecedenceVisitor{}).value_or(Precedence::None);
}

}  // namespace math
