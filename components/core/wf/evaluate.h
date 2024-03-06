// Copyright 2023 Gareth Cross
#pragma once

#include <unordered_map>

#include "compound_expression.h"
#include "wf/expression.h"
#include "wf/hashing.h"

namespace wf {

// Convert expressions to `float_constant` values.
class evaluate_visitor {
 public:
  template <typename T, typename X,
            typename = enable_if_does_not_contain_type_t<T, integer_constant, rational_constant,
                                                         symbolic_constant>>
  X operator()(const T& input_typed, const X& input);

  scalar_expr operator()(const integer_constant& x) const;
  scalar_expr operator()(const rational_constant& x) const;
  scalar_expr operator()(const symbolic_constant& c) const;

  // Visit and cache the result.
  scalar_expr operator()(const scalar_expr& input);
  compound_expr operator()(const compound_expr& input);
  boolean_expr operator()(const boolean_expr& input);

 private:
  // Cached evaluated values:
  // TODO: Introduce a cache type that can accept any expression type.
  std::unordered_map<scalar_expr, scalar_expr, hash_struct<scalar_expr>,
                     is_identical_struct<scalar_expr>>
      cache_;
  std::unordered_map<compound_expr, compound_expr, hash_struct<compound_expr>,
                     is_identical_struct<compound_expr>>
      compound_cache_;
};

}  // namespace wf
