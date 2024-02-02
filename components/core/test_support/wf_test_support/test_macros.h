#pragma once
#include <gtest/gtest.h>

#include "wf/compound_expression.h"
#include "wf/expression.h"
#include "wf/fmt_imports.h"
#include "wf/matrix_expression.h"

namespace wf {

#define ASSERT_IDENTICAL(val1, val2) ASSERT_PRED_FORMAT2(identical_test_helper, val1, val2)
#define ASSERT_NOT_IDENTICAL(val1, val2) ASSERT_PRED_FORMAT2(not_identical_test_helper, val1, val2)

#define ASSERT_STR_EQ(val1, val2) ASSERT_PRED_FORMAT2(string_equal_test_helper, val1, val2)

template <typename A, typename B>
testing::AssertionResult format_failed_result(const std::string_view description,
                                              const std::string_view name_a,
                                              const std::string_view name_b, const A& a, B& b) {
  // If the formatted string has multiple lines, preface it w/ a line break:
  const std::string a_str = a.to_string();
  const std::string b_str = b.to_string();
  const std::string_view a_prefix = std::count(a_str.begin(), a_str.end(), '\n') > 0 ? "\n" : " ";
  const std::string_view b_prefix = std::count(b_str.begin(), b_str.end(), '\n') > 0 ? "\n" : " ";

  // clang-format off
  return testing::AssertionFailure() << fmt::format(
             "{} {} {}, where:\n{} ={}{}\nand {} ={}{}\n"
             "expression tree for `{}`:\n{}\n"
             "expression tree for `{}`:\n{}",
             name_a, description, name_b,
             name_a, a_prefix, a_str, name_b, b_prefix, b_str,
             name_a, a.to_expression_tree_string(),
             name_b, b.to_expression_tree_string());
  // clang-format on
}

// This indirection exists so that we can coerce numeric literals to expressions.
template <typename T, typename = void>
struct convert_assertion_argument;
template <typename T>
struct convert_assertion_argument<T, std::enable_if_t<std::is_convertible_v<T, Expr>>> {
  // This overload will get selected for numeric literals.
  Expr operator()(const T& arg) const { return static_cast<Expr>(arg); }
};
template <>
struct convert_assertion_argument<matrix_expr> {
  matrix_expr operator()(const matrix_expr& arg) const { return arg; }
};
template <>
struct convert_assertion_argument<compound_expr> {
  compound_expr operator()(const compound_expr& arg) const { return arg; }
};

// Test is_identical_to
template <typename A, typename B>
testing::AssertionResult identical_test_helper_2(const std::string_view name_a,
                                                 const std::string_view name_b, const A& a,
                                                 const B& b) {
  if (a.is_identical_to(b)) {
    return testing::AssertionSuccess();
  }
  return format_failed_result("is not identical to", name_a, name_b, a, b);
}

template <typename A, typename B>
testing::AssertionResult identical_test_helper(const std::string_view name_a,
                                               const std::string_view name_b, const A& a,
                                               const B& b) {
  return identical_test_helper_2(name_a, name_b, convert_assertion_argument<A>{}(a),
                                 convert_assertion_argument<B>{}(b));
}

// Test !is_identical_to
template <typename A, typename B>
testing::AssertionResult not_identical_test_helper_2(const std::string_view name_a,
                                                     const std::string_view name_b, const A& a,
                                                     const B& b) {
  if (!a.is_identical_to(b)) {
    return testing::AssertionSuccess();
  }
  return format_failed_result("is identical (and should not be) to", name_a, name_b, a, b);
}

template <typename A, typename B>
testing::AssertionResult not_identical_test_helper(const std::string_view name_a,
                                                   const std::string_view name_b, const A& a,
                                                   const B& b) {
  return not_identical_test_helper_2(name_a, name_b, convert_assertion_argument<A>{}(a),
                                     convert_assertion_argument<B>{}(b));
}

// Escape all newlines in a string so they can be printed to console literally.
inline std::string escape_newlines(const std::string_view input) {
  std::string output;
  output.reserve(input.size());
  for (char c : input) {
    if (c == '\n') {
      output += "\\n";
    } else {
      output += c;
    }
  }
  return output;
}

template <typename ExprType>
testing::AssertionResult string_equal_test_helper(const std::string_view,
                                                  const std::string_view name_b,
                                                  const std::string& a, const ExprType& b) {
  const std::string b_str = b.to_string();
  if (a == b_str) {
    return testing::AssertionSuccess();
  }
  return testing::AssertionFailure() << fmt::format(
             "String `{}` does not match ({}).ToString(), where:\n({}).ToString() = {}\n"
             "The expression tree for `{}` is:\n{}",
             escape_newlines(a), name_b, name_b, escape_newlines(b_str), name_b,
             b.to_expression_tree_string());
}

// ostream operator for `NumericSet`.
std::ostream& operator<<(std::ostream& s, number_set set) {
  s << string_from_number_set(set);
  return s;
}

}  // namespace wf
