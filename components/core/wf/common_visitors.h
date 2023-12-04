// Copyright 2022 Gareth Cross
#pragma once
#include <algorithm>

#include "wf/expressions/multiplication.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/power.h"
#include "wf/visitor_impl.h"

// This file is intended to contain common utility visitors.
namespace wf {

// Visitor that returns true for numerical values, or powers of numerical values.
struct is_numeric_visitor {
  template <typename T>
  bool operator()(const T& arg) const {
    if constexpr (type_list_contains_type_v<T, float_constant, integer_constant,
                                            rational_constant>) {
      return true;
    } else if constexpr (std::is_same_v<T, power>) {
      return is_numeric(arg.base()) && is_numeric(arg.exponent());
    } else {
      return false;
    }
  }
};

inline bool is_numeric(const Expr& expr) { return visit(expr, is_numeric_visitor{}); }

// Visitor that identifies negative numeric constants, or products of numeric constants that will be
// negative.
struct is_negative_number_visitor {
  constexpr bool operator()(const integer_constant& i) const noexcept { return i.is_negative(); }
  constexpr bool operator()(const float_constant& f) const noexcept { return f.is_negative(); }
  constexpr bool operator()(const rational_constant& r) const noexcept { return r.is_negative(); }

  template <typename T>
  constexpr bool operator()(const T&) const noexcept {
    return false;
  }

  // Multiplications can be negative-like, if the product of all the constant terms is negative.
  bool operator()(const multiplication& m) const {
    const std::size_t count = std::count_if(m.begin(), m.end(), [](const Expr& expr) {
      return visit(expr, is_negative_number_visitor{});
    });
    // odd = negative, even = positive
    return static_cast<bool>(count & 1);
  }
};

// TODO: This probably deserves a better name, since it doesn't just check for numbers.
inline bool is_negative_number(const Expr& expr) {
  return visit(expr, is_negative_number_visitor{});
}

}  // namespace wf
