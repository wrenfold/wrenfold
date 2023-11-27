// Copyright 2023 Gareth Cross
#pragma once
#include "absl_imports.h"
#include "expression.h"
#include "expressions/variable.h"
#include "hashing.h"

namespace math {

// Visitor for replacing variables in an expression tree.
class SubstituteVariablesVisitor {
 public:
  SubstituteVariablesVisitor() { cache_.reserve(50); }

  // Add a new substitution to the list we will apply.
  // This should be called before visiting any expressions.
  void add_substitution(const Expr& target, Expr replacement);

  // Add a new substitution to the list we will apply.
  // Version that accepts `Variable` directly.
  void add_substitution(Variable variable, Expr replacement);

  // Apply the substitute variable visitor. The cache is checked first.
  Expr apply(const Expr& expression);

  template <typename T>
  Expr operator()(const T& concrete, const Expr& abstract);

 private:
  std::unordered_map<Variable, Expr, hash_struct<Variable>, is_identical_struct<Variable>>
      substitutions_;
  std::unordered_map<Expr, Expr, hash_struct<Expr>, is_identical_struct<Expr>> cache_{};
};

}  // namespace math
