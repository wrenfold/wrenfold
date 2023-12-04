// Copyright 2023 Gareth Cross
#pragma once
#include <unordered_map>

#include "wf/expression.h"
#include "wf/hashing.h"

namespace math {

// Convert expressions to `float_constant` values.
class evaluate_visitor {
 public:
  template <typename T, typename = enable_if_does_not_contain_type_t<T, integer_constant,
                                                                     rational_constant, Constant>>
  Expr operator()(const T& input_typed, const Expr& input);

  Expr operator()(const integer_constant& x) const;
  Expr operator()(const rational_constant& x) const;
  Expr operator()(const Constant& x) const;

  // Visit and cache the result.
  Expr apply(const Expr& input);

 private:
  // Cached evaluated values:
  std::unordered_map<Expr, Expr, hash_struct<Expr>, is_identical_struct<Expr>> cache_;
};

}  // namespace math
