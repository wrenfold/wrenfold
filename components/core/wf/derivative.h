// Copyright 2023 Gareth Cross
#pragma once
#include <unordered_map>

#include "wf/expression.h"
#include "wf/hashing.h"

namespace math {
class Variable;  //  Fwd declare.

// Visitor that takes the derivative of an input expression.
class derivative_visitor {
 public:
  // Construct w/ const reference to the variable to differentiate wrt to.
  // Must remain in scope for the duration of evaluation.
  explicit derivative_visitor(const Expr& argument);

  // Apply this visitor to the specified expression.
  Expr apply(const Expr& expression);

  Expr operator()(const Addition& add);
  Expr operator()(const CastBool&, const Expr& expr);
  Expr operator()(const Conditional& cond);
  Expr operator()(const Constant&) const;
  Expr operator()(const Derivative& derivative, const Expr& derivative_abstract) const;
  Expr operator()(const Multiplication& mul);
  Expr operator()(const Function& func);
  Expr operator()(const Infinity&) const;
  Expr operator()(const Integer&) const;
  Expr operator()(const Float&) const;
  Expr operator()(const Power& pow);
  Expr operator()(const Rational&) const;
  Expr operator()(const Relational&, const Expr& rel_expr) const;
  Expr operator()(const Undefined&) const;
  Expr operator()(const Variable& var) const;

 private:
  Expr cached_visit(const Expr& expr);

  // Argument we differentiate with respect to.
  const Expr& argument_;

  // Cache of f(x) --> df(x)/dx.
  std::unordered_map<Expr, Expr, hash_struct<Expr>, is_identical_struct<Expr>> cache_;
};

}  // namespace math