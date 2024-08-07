// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <unordered_set>
#include <vector>

#include "wf/expression.h"
#include "wf/expressions/variable.h"

namespace wf {

// True for numerical values and powers where base and exponent are both numerical values.
bool is_numeric(const scalar_expr& expr);

// Visitor that identifies negative numeric constants, or products of numeric constants that will be
// negative when evaluated.
bool is_negative_number(const scalar_expr& expr);

// Visitor that recursively traverses the expression tree and extracts all `variable` expressions.
class get_variables_visitor {
 public:
  template <typename T>
  void operator()(const T& arg);

  // Move resulting vector of variables out of the visitor.
  std::vector<variable> take_output() && { return std::move(variables_); }

 private:
  std::unordered_set<scalar_expr, hash_struct<scalar_expr>, is_identical_struct<scalar_expr>>
      visited_;
  std::vector<variable> variables_;
};

// Get all variable expressions that appear in a scalar-valued expression tree.
std::vector<variable> get_variables(const scalar_expr& expr);

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
