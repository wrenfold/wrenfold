// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/expression.h"
#include "wf/expression_cache.h"
#include "wf/expressions/compound_expression_element.h"
#include "wf/expressions/variable.h"
#include "wf/utility/hashing.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/types/span.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

using scalar_pair = std::tuple<const scalar_expr, const scalar_expr>;
using boolean_pair = std::tuple<const boolean_expr, const boolean_expr>;
using scalar_or_boolean_pair = std::variant<scalar_pair, boolean_pair>;

// Visitor for replacing variables in an expression tree.
class substitute_variables_visitor {
 public:
  substitute_variables_visitor() { cache_.reserve(50); }

  // Add a new substitution to the list we will apply.
  // This should be called before visiting any expressions.
  bool add_substitution(const scalar_expr& target, scalar_expr replacement);

  // Add a new substitution to the list we will apply.
  // Version that accepts `variable` directly.
  bool add_substitution(variable variable, scalar_expr replacement);

  // Add a new substitution to the list we will apply.
  // Version that accepts `compound_expression_element` directly.
  bool add_substitution(compound_expression_element element, scalar_expr replacement);

  // True if `target` is something we will replace.
  bool contains_target_variable(const scalar_expr& target) const;

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

// Replace all [target, replacement] pairs in the input expression.
scalar_expr substitute(const scalar_expr& input, absl::Span<const scalar_or_boolean_pair> pairs,
                       bool force_unordered_execution = false);
boolean_expr substitute(const boolean_expr& input, absl::Span<const scalar_or_boolean_pair> pairs,
                        bool force_unordered_execution = false);
matrix_expr substitute(const matrix_expr& input, absl::Span<const scalar_or_boolean_pair> pairs,
                       bool force_unordered_execution = false);

}  // namespace wf
