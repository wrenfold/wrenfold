// Copyright 2023 Gareth Cross
#pragma once
#include "wf/expression.h"
#include "wf/hashing.h"

namespace math {

// Convert expressions to `Float` values.
class EvaluateVisitor {
 public:
  template <typename T,
            typename = enable_if_does_not_contain_type_t<T, Integer, Rational, Constant>>
  Expr operator()(const T& input_typed, const Expr& input);

  Expr operator()(const Integer& x) const;
  Expr operator()(const Rational& x) const;
  Expr operator()(const Constant& x) const;

  // Visit and cache the result.
  Expr apply(const Expr& input);

 private:
  // Cached evaluated values:
  std::unordered_map<Expr, Expr, hash_struct<Expr>, is_identical_struct<Expr>> cache_;
};

}  // namespace math
