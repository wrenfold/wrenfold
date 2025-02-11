// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <vector>

#include "wf/compound_expression.h"
#include "wf/expression.h"
#include "wf/expressions/special_constants.h"
#include "wf/matrix_expression.h"

namespace wf {

// Recursively print expressions as a graphical utf-8 tree (mostly for diffing/debugging).
class tree_formatter_visitor {
 public:
  void operator()(const scalar_expr& x);
  void operator()(const matrix_expr& m);
  void operator()(const compound_expr& c);
  void operator()(const boolean_expr& b);

  void operator()(const addition& add);
  void operator()(const boolean_constant& b);
  void operator()(const complex_infinity&);
  void operator()(const compound_expression_element& el);
  void operator()(const conditional& conditional);
  void operator()(const custom_type_argument& arg);
  void operator()(const custom_type_construction& construct);
  void operator()(const derivative& diff);
  void operator()(const external_function_invocation& invocation);
  void operator()(const float_constant& f);
  void operator()(const function_argument_variable& fv);
  void operator()(const built_in_function_invocation& func);
  void operator()(const imaginary_unit&);
  void operator()(const integer_constant& i);
  void operator()(const iverson_bracket& bracket);
  void operator()(const matrix& mat);
  void operator()(const multiplication& op);
  void operator()(const power& pow);
  void operator()(const rational_constant& r);
  void operator()(const relational& relational);
  void operator()(const stop_derivative& nd);
  void operator()(const substitution& subs);
  void operator()(const symbolic_constant& constant);
  void operator()(const symbolic_function_invocation& invocation);
  void operator()(const undefined&);
  void operator()(const unevaluated& u);
  void operator()(const unique_variable& uv);
  void operator()(const variable& var);

  // Get the output string. Result is returned via move.
  std::string take_output();

 private:
  void apply_indentation();

  template <typename... Args>
  void format_append(std::string_view fmt_str, Args&&... args);

  template <typename T>
  void visit_left(const T& expr);

  template <typename T>
  void visit_right(const T& expr);

  // Visit every element of `container`. All but the last element are left-visited, and the
  // last element is right-visited.
  template <typename Container>
  void visit_all(const Container& container);

  // The indentation pattern at our current tree depth.
  // True indicates a left branch, false indicates a right branch.
  std::vector<unsigned char> indentations_;
  // The final output
  std::string output_;
};

}  // namespace wf
