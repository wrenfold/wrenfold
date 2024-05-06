// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <cstddef>
#include <functional>

#include "wf/any_expression.h"
#include "wf/compound_expression.h"
#include "wf/expression.h"
#include "wf/expression_cache.h"
#include "wf/matrix_expression.h"

namespace wf {

class counter_visitor;

// Visitor that identifies common sub-expressions. The input is broken down into a series of
// substitutions of the form (var, expression) that can be performed to rebuild the original
// expression. For example:
//
//   f = (x*y) + cos(x*y)
//
// Can be converted to:
//       v0 = x*y
//        f = v0 + cos(v0)
//
// Note that this version of CSE is intended as a tool for debugging the symbolic expression tree.
// It is _not_ the CSE we use at code-generation time.
class cse_visitor {
 public:
  using variable_function = std::function<scalar_expr(std::size_t)>;

  cse_visitor(const counter_visitor& counter, variable_function make_variable,
              std::size_t min_occurrences);

  // Apply to any expression type.
  template <typename T, typename = enable_if_inherits_expression_base_t<T>>
  T operator()(const T& expr);

  // Accepts concrete expression type `T`.
  template <typename T, typename X>
  X operator()(const T& concrete, const X& expr);

  // Apply to `any_expression`.
  any_expression operator()(const any_expression& expr);

  // Take the vector of replacements.
  constexpr std::vector<std::tuple<scalar_expr, scalar_expr>>&& take_replacements() && noexcept {
    return std::move(replacements_);
  }

 private:
  const counter_visitor& counter_;

  // Function that accepts a count, and yields a variable to represent a replaced
  // expression.
  std::function<scalar_expr(std::size_t)> make_variable_;

  // Min # of times an expression needs to be seen to be replaced.
  std::size_t min_occurrences_;

  // Cache of transformed expression trees.
  expression_cache<> cache_;

  // Store a tuple of <variable, expression>
  std::vector<std::tuple<scalar_expr, scalar_expr>> replacements_;
};

// Eliminate sub-expressions in a scalar-valued expression.
std::tuple<scalar_expr, std::vector<std::tuple<scalar_expr, scalar_expr>>> eliminate_subexpressions(
    const scalar_expr& expr, std::function<scalar_expr(std::size_t)> make_variable,
    std::size_t min_occurences);

// Eliminate sub-expressions in a matrix-valued expression.
std::tuple<matrix_expr, std::vector<std::tuple<scalar_expr, scalar_expr>>> eliminate_subexpressions(
    const matrix_expr& expr, std::function<scalar_expr(std::size_t)> make_variable,
    std::size_t min_occurences);

}  // namespace wf
