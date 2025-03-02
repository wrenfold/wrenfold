// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/compound_expression.h"
#include "wf/expression.h"
#include "wf/expression_cache.h"
#include "wf/matrix_expression.h"

namespace wf {

// Simple plain-text formatter.
class plain_formatter {
 public:
  template <typename T,
            typename = std::enable_if_t<inherits_expression_base_v<T> || is_any_expression_v<T>>>
  std::string_view operator()(const T& x);

  static std::string convert(const scalar_expr& expr);
  static std::string convert(const boolean_expr& expr);
  static std::string convert(const matrix_expr& expr);
  static std::string convert(const compound_expr& expr);

  std::string operator()(const addition& add);
  std::string operator()(const boolean_constant& b) const;
  std::string operator()(const compound_expression_element& el);
  std::string operator()(const external_function_invocation& invocation);
  std::string operator()(const custom_type_argument& arg) const;
  std::string operator()(const custom_type_construction& construct);
  std::string operator()(const conditional& conditional);
  std::string operator()(const symbolic_constant& constant) const;
  std::string operator()(const derivative& derivative);
  std::string operator()(const float_constant& num) const;
  std::string operator()(const complex_infinity&) const;
  std::string operator()(const imaginary_unit&) const;
  std::string operator()(const integer_constant& num) const;
  std::string operator()(const iverson_bracket& bracket);
  std::string operator()(const matrix& mat);
  std::string operator()(const multiplication& mul);
  std::string operator()(const power& pow);
  std::string operator()(const rational_constant& rational) const;
  std::string operator()(const relational& relational);
  std::string operator()(const stop_derivative& nd);
  std::string operator()(const substitution& subs);
  std::string operator()(const symbolic_function_invocation& invocation);
  std::string operator()(const built_in_function_invocation& func);
  std::string operator()(const undefined&) const;
  std::string operator()(const unevaluated& u);
  std::string operator()(const variable& var) const;
  std::string operator()(const function_argument_variable& var) const;
  std::string operator()(const unique_variable& var) const;

 private:
  template <typename T>
  static std::string convert_impl(const T& expr);

  // Wrap `expr` in braces if the precedence is <= the parent.
  void format_precedence(std::string& output, precedence parent, const scalar_expr& expr);

  // Format power with ** operator.
  void format_power(std::string& output, const scalar_expr& base, const scalar_expr& exponent);

  // Cache stores a map from expression -> index into strings_
  template <typename Key>
  using map_type = std::unordered_map<Key, std::unique_ptr<std::string>, hash_struct<Key>,
                                      is_identical_struct<Key>>;
  expression_map_tuple<map_type> cache_{};
};

}  // namespace wf
