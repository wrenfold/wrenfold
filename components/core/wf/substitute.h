// Copyright 2023 Gareth Cross
#pragma once
#include "wf/absl_imports.h"
#include "wf/expression.h"
#include "wf/expressions/variable.h"
#include "wf/hashing.h"

namespace math {

// Visitor for replacing variables in an expression tree.
class substitute_variables_visitor {
 public:
  substitute_variables_visitor() { cache_.reserve(50); }

  // Add a new substitution to the list we will apply.
  // This should be called before visiting any expressions.
  void add_substitution(const Expr& target, Expr replacement);

  // Add a new substitution to the list we will apply.
  // Version that accepts `Variable` directly.
  void add_substitution(variable variable, Expr replacement);

  // Apply the substitute variable visitor. The cache is checked first.
  Expr apply(const Expr& expression);

  template <typename T>
  Expr operator()(const T& concrete, const Expr& abstract);

 private:
  std::unordered_map<variable, Expr, hash_struct<variable>, is_identical_struct<variable>>
      substitutions_;
  std::unordered_map<Expr, Expr, hash_struct<Expr>, is_identical_struct<Expr>> cache_{};
};

}  // namespace math
