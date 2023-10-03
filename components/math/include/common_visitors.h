// Copyright 2022 Gareth Cross
#pragma once
#include <algorithm>

#include "expressions/multiplication.h"
#include "expressions/numeric_expressions.h"
#include "expressions/power.h"
#include "visitor_impl.h"

// This file is intended to contain common utility visitors.
namespace math {

// Visitor that returns true for numerical values, or powers of numerical values.
struct IsNumericVisitor {
  template <typename T>
  bool operator()(const T& arg) const {
    if constexpr (list_contains_type_v<T, Float, Integer, Rational>) {
      return true;
    } else if constexpr (std::is_same_v<T, Power>) {
      return is_numeric(arg.base()) && is_numeric(arg.exponent());
    } else {
      return false;
    }
  }
};

inline bool is_numeric(const Expr& expr) { return visit(expr, IsNumericVisitor{}); }

// Visitor that identifies negative numeric constants, or products of numeric constants that will be
// negative.
struct IsNegativeNumberVisitor {
  // Numerics < 0 are all negative.
  bool operator()(const Integer& num) const { return num.get_value() < 0; }
  bool operator()(const Float& f) const { return f.get_value() < 0; }
  bool operator()(const Rational& r) const { return r.numerator() < 0; }

  // Multiplications can be negative-like, if the product of all the constant terms is negative.
  bool operator()(const Multiplication& m) const {
    const std::size_t count = std::count_if(m.begin(), m.end(), [](const Expr& expr) {
      return visit(expr, IsNegativeNumberVisitor{});
    });
    // odd = negative, even = positive
    return static_cast<bool>(count & 1);
  }

  template <typename T>
  constexpr bool operator()(const T&) const {
    return false;
  }
};

// TODO: This probably deserves a better name, since it doesn't just check for numbers.
inline bool is_negative_number(const Expr& expr) { return visit(expr, IsNegativeNumberVisitor{}); }

}  // namespace math
