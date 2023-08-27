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
  using Policy = VisitorPolicy::CompileError;

  template <typename T>
  bool Apply(const T& arg) const {
    if constexpr (ContainsTypeHelper<T, Float, Integer, Rational>) {
      return true;
    } else if constexpr (std::is_same_v<T, Power>) {
      return IsNumeric(arg.Base()) && IsNumeric(arg.Exponent());
    } else {
      return false;
    }
  }
};

inline bool IsNumeric(const Expr& expr) { return VisitStruct(expr, IsNumericVisitor{}); }

// Visitor that identifies negative numeric constants, or products of numeric constants that will be
// negative.
struct IsNegativeNumberVisitor {
  using ReturnType = bool;
  using Policy = VisitorPolicy::CompileError;

  // Numerics < 0 are all negative.
  bool Apply(const Integer& num) const { return num.GetValue() < 0; }
  bool Apply(const Float& f) const { return f.GetValue() < 0; }
  bool Apply(const Rational& r) const { return r.Numerator() < 0; }

  // Multiplications can be negative-like, if the product of all the constant terms is negative.
  bool Apply(const Multiplication& m) const {
    const std::size_t count = std::count_if(m.begin(), m.end(), [](const Expr& expr) {
      return VisitStruct(expr, IsNegativeNumberVisitor{});
    });
    // odd = negative, even = positive
    return static_cast<bool>(count & 1);
  }

  template <typename T>
  constexpr bool Apply(const T&) const {
    return false;
  }
};

// TODO: This probably deserves a better name, since it doesn't just check for numbers.
inline bool IsNegativeNumber(const Expr& expr) {
  return VisitStruct(expr, IsNegativeNumberVisitor{});
}

}  // namespace math
