// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <functional>
#include <vector>

#include "wf/expression.h"

namespace wf {

// True for numerical values and powers where base and exponent are both numerical values.
bool is_numeric(const scalar_expr& expr);

// Visitor that identifies negative numeric constants, or products of numeric constants that will be
// negative when evaluated.
bool is_negative_number(const scalar_expr& expr);

// Return all expressions that match the provided predicate.
// The predicate is invoked on each unique sub-expression exactly once.
std::vector<scalar_expr> get_expressions_by_predicate(
    const scalar_expr& expr, std::function<bool(const scalar_expr&)> predicate);

// Visitor that checks if an expression contains a `target` sub-expression (typically a variable).
// Returns true if `target` appears exactly as a sub-expression in anything we visit.
template <typename T>
class is_function_of_visitor {
 public:
  explicit constexpr is_function_of_visitor(const T& target) noexcept : target_(target) {}

  template <typename U>
  bool operator()(const U& x) const;

 private:
  const T& target_;
};

// Apply `is_function_of_visitor` to see if `func` is a function of exact expression `target`.
bool is_function_of(const scalar_expr& func, const scalar_expr& target);

}  // namespace wf
