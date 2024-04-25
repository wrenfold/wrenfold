// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/compound_expression.h"
#include "wf/expression.h"
#include "wf/matrix_expression.h"

namespace wf {

// Simple plain-text formatter.
class plain_formatter {
 public:
  void operator()(const scalar_expr& x);
  void operator()(const matrix_expr& x);
  void operator()(const boolean_expr& x);

  void operator()(const addition& add);
  void operator()(const boolean_constant& b);
  void operator()(const compound_expression_element& el);
  void operator()(const external_function_invocation& invocation);
  void operator()(const custom_type_argument& arg);
  void operator()(const custom_type_construction& construct);
  void operator()(const conditional& conditional);
  void operator()(const symbolic_constant& constant);
  void operator()(const derivative& derivative);
  void operator()(const float_constant& num);
  void operator()(const complex_infinity&);
  void operator()(const imaginary_unit&);
  void operator()(const integer_constant& num);
  void operator()(const iverson_bracket& bracket);
  void operator()(const matrix& mat);
  void operator()(const multiplication& mul);
  void operator()(const power& pow);
  void operator()(const rational_constant& rational);
  void operator()(const relational& relational);
  void operator()(const function& func);
  void operator()(const undefined&);
  void operator()(const variable& var);

  // Get the output string (transferring ownership to the caller).
  std::string take_output() const { return std::move(output_); }

 private:
  // Wrap `expr` in braces if the precedence is <= the parent.
  void format_precedence(precedence parent, const scalar_expr& expr);

  // Format power with ** operator.
  void format_power(const scalar_expr& base, const scalar_expr& exponent);

  std::string output_{};
};

}  // namespace wf
