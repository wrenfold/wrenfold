// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <unordered_map>

#include "wf/any_expression.h"
#include "wf/expression.h"

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
  bool add_substitution(scalar_expr target, scalar_expr replacement);

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
  std::unordered_map<scalar_expr, scalar_expr, hash_struct<scalar_expr>,
                     is_identical_struct<scalar_expr>>
      substitutions_{};
  std::unordered_map<scalar_expr, scalar_expr, hash_struct<scalar_expr>,
                     is_identical_struct<scalar_expr>>
      cache_{};
};

// Replace all [target, replacement] pairs in the input expression.
any_expression substitute_any(any_expression input, absl::Span<const scalar_or_boolean_pair> pairs,
                              bool force_unordered_execution = false);

template <typename X, typename = enable_if_inherits_expression_base_t<X>>
X substitute(const X& input, absl::Span<const scalar_or_boolean_pair> pairs,
             bool force_unordered_execution = false) {
  return std::get<X>(substitute_any(input, pairs, force_unordered_execution));
}

}  // namespace wf
