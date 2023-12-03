// Copyright 2022 Gareth Cross
#pragma once
#include <algorithm>

#include "wf/expressions/multiplication.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/power.h"
#include "wf/visitor_impl.h"

// This file is intended to contain common utility visitors.
namespace math {

// Visitor that returns true for numerical values, or powers of numerical values.
struct IsNumericVisitor {
  template <typename T>
  bool operator()(const T& arg) const {
    if constexpr (type_list_contains_type_v<T, Float, Integer, Rational>) {
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
  constexpr bool operator()(const Integer& i) const noexcept { return i.is_negative(); }
  constexpr bool operator()(const Float& f) const noexcept { return f.is_negative(); }
  constexpr bool operator()(const Rational& r) const noexcept { return r.is_negative(); }

  template <typename T>
  constexpr bool operator()(const T&) const noexcept {
    return false;
  }

  // Multiplications can be negative-like, if the product of all the constant terms is negative.
  bool operator()(const Multiplication& m) const {
    const std::size_t count = std::count_if(m.begin(), m.end(), [](const Expr& expr) {
      return visit(expr, IsNegativeNumberVisitor{});
    });
    // odd = negative, even = positive
    return static_cast<bool>(count & 1);
  }
};

// TODO: This probably deserves a better name, since it doesn't just check for numbers.
inline bool is_negative_number(const Expr& expr) { return visit(expr, IsNegativeNumberVisitor{}); }

}  // namespace math
