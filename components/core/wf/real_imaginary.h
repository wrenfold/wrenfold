// Copyright 2024 Gareth Cross
#pragma once
#include <tuple>
#include <unordered_map>

#include "wf/expression.h"

namespace wf {

// Visitor for splitting expressions into real and imaginary parts.
class real_imaginary_visitor {
 public:
  std::tuple<scalar_expr, scalar_expr> operator()(const scalar_expr& expr);

  std::tuple<scalar_expr, scalar_expr> operator()(const addition& add);
  std::tuple<scalar_expr, scalar_expr> operator()(const complex_infinity&) const;
  std::tuple<scalar_expr, scalar_expr> operator()(const compound_expression_element&,
                                                  const scalar_expr& arg) const;
  std::tuple<scalar_expr, scalar_expr> operator()(const conditional& cond);
  std::tuple<scalar_expr, scalar_expr> operator()(const derivative& diff,
                                                  const scalar_expr& diff_abstract) const;
  std::tuple<scalar_expr, scalar_expr> operator()(const float_constant&,
                                                  const scalar_expr& arg) const;
  std::tuple<scalar_expr, scalar_expr> operator()(const function& func,
                                                  const scalar_expr& func_abstract);
  std::tuple<scalar_expr, scalar_expr> operator()(const imaginary_unit&) const;
  std::tuple<scalar_expr, scalar_expr> operator()(const integer_constant&,
                                                  const scalar_expr& arg) const;
  std::tuple<scalar_expr, scalar_expr> operator()(const iverson_bracket&,
                                                  const scalar_expr& arg) const;
  std::tuple<scalar_expr, scalar_expr> operator()(const multiplication& mul);
  std::tuple<scalar_expr, scalar_expr> operator()(const power& pow);
  std::tuple<scalar_expr, scalar_expr> operator()(const rational_constant&,
                                                  const scalar_expr& arg) const;
  std::tuple<scalar_expr, scalar_expr> operator()(const symbolic_constant&,
                                                  const scalar_expr& arg) const;
  std::tuple<scalar_expr, scalar_expr> operator()(const undefined&) const;
  std::tuple<scalar_expr, scalar_expr> operator()(const variable& var,
                                                  const scalar_expr& arg) const;

 private:
  std::unordered_map<scalar_expr, std::tuple<scalar_expr, scalar_expr>, hash_struct<scalar_expr>,
                     is_identical_struct<scalar_expr>>
      cache_;
};

}  // namespace wf
