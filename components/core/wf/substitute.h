// Copyright 2023 Gareth Cross
#pragma once
#include "wf/absl_imports.h"
#include "wf/expression.h"
#include "wf/expressions/compound_expression_element.h"
#include "wf/expressions/variable.h"
#include "wf/hashing.h"

namespace wf {

// Visitor for replacing variables in an expression tree.
class substitute_variables_visitor {
 public:
  substitute_variables_visitor() { cache_.reserve(50); }

  // Add a new substitution to the list we will apply.
  // This should be called before visiting any expressions.
  void add_substitution(const scalar_expr& target, scalar_expr replacement);

  // Add a new substitution to the list we will apply.
  // Version that accepts `variable` directly.
  void add_substitution(variable variable, scalar_expr replacement);

  // Add a new substitution to the list we will apply.
  // Version that accepts `compound_expression_element` directly.
  void add_substitution(compound_expression_element element, scalar_expr replacement);

  // Apply the substitute variable visitor. The cache is checked first.
  scalar_expr operator()(const scalar_expr& expression);
  matrix_expr operator()(const matrix_expr& expression);
  compound_expr operator()(const compound_expr& expression);
  boolean_expr operator()(const boolean_expr& expression);

  template <typename T, typename X>
  X operator()(const T& concrete, const X& abstract);

 private:
  std::unordered_map<variable, scalar_expr, hash_struct<variable>, is_identical_struct<variable>>
      variable_substitutions_;
  std::unordered_map<compound_expression_element, scalar_expr,
                     hash_struct<compound_expression_element>,
                     is_identical_struct<compound_expression_element>>
      element_substitutions_;
  std::unordered_map<scalar_expr, scalar_expr, hash_struct<scalar_expr>,
                     is_identical_struct<scalar_expr>>
      cache_{};
};

}  // namespace wf
