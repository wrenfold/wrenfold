#pragma once
#include <gtest/gtest.h>
#include <complex>

#include "wf/compound_expression.h"
#include "wf/expression.h"
#include "wf/matrix_expression.h"
#include "wf/numerical_casts.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

// Compare expressions using `is_identical_to`.
#define ASSERT_IDENTICAL(val1, val2) ASSERT_PRED_FORMAT2(identical_test_helper, val1, val2)
#define ASSERT_NOT_IDENTICAL(val1, val2) ASSERT_PRED_FORMAT2(not_identical_test_helper, val1, val2)

// Convert `val2` to a string and compare to string literal `val1`.
#define ASSERT_STR_EQ(val1, val2) ASSERT_PRED_FORMAT2(string_equal_test_helper, val1, val2)

// Test std::complex values for near equality.
#define ASSERT_COMPLEX_NEAR(a, b, tol) ASSERT_PRED_FORMAT3(wf::expect_complex_near, a, b, tol)

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
struct convert_assertion_argument<T, std::enable_if_t<std::is_convertible_v<T, scalar_expr>>> {
  // This overload will get selected for numeric literals.
  scalar_expr operator()(const T& arg) const { return static_cast<scalar_expr>(arg); }
};
// Allow implicit conversion from std::complex<double> in unit tests.
template <>
struct convert_assertion_argument<std::complex<double>> {
  scalar_expr operator()(const std::complex<double>& arg) const {
    return scalar_expr::from_complex(arg.real(), arg.imag());
  }
};
template <typename T>
struct convert_assertion_argument<T, std::enable_if_t<std::is_convertible_v<T, boolean_expr>>> {
  boolean_expr operator()(const T& arg) const { return static_cast<boolean_expr>(arg); }
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
  for (const char c : input) {
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

// Compare complex numbers under the L1 norm.
inline testing::AssertionResult expect_complex_near(
    const std::string_view name_a, const std::string_view name_b, const std::string_view name_tol,
    const std::complex<double> a, const std::complex<double> b, const double tolerance) {
  if (const std::complex<double> delta = a - b;
      std::abs(delta.real()) > tolerance || std::abs(delta.imag()) > tolerance ||
      std::isnan(delta.real()) || std::isnan(delta.imag())) {
    return testing::AssertionFailure() << fmt::format(
               "std::complex comparison {a} == {b} failed because:\n"
               "{a} = ({a_real}, {a_imag}) and,\n{b} = ({b_real}, {b_imag})\n"
               "The diference between them is {a} - {b} = ({delta_real}, {delta_imag}).\n"
               "And {name_tol} evaluates to: {tol}.\n",
               fmt::arg("a", name_a), fmt::arg("b", name_b), fmt::arg("a_real", a.real()),
               fmt::arg("a_imag", a.imag()), fmt::arg("b_real", b.real()),
               fmt::arg("b_imag", b.imag()), fmt::arg("delta_real", delta.real()),
               fmt::arg("delta_imag", delta.imag()), fmt::arg("name_tol", name_tol),
               fmt::arg("tol", tolerance));
  }
  return testing::AssertionSuccess();
}
inline testing::AssertionResult expect_complex_near(const std::string_view name_a,
                                                    const std::string_view name_b,
                                                    const std::string_view name_tol,
                                                    const std::complex<double> a,
                                                    const scalar_expr& b, const double tolerance) {
  if (const std::optional<std::complex<double>> b_complex = complex_cast(b);
      b_complex.has_value()) {
    return expect_complex_near(name_a, name_b, name_tol, a, *b_complex, tolerance);
  } else {
    const std::string b_tree = b.to_expression_tree_string();
    return testing::AssertionFailure() << fmt::format(
               "std::complex comparison {a} == {b} failed because:\n"
               "{b} is of type {b_type}, which could not be coerced to std::complex.\n"
               "The expression tree of {b} is:\n{b_tree}\n",
               fmt::arg("a", name_a), fmt::arg("b", name_b), fmt::arg("b_type", b.type_name()),
               fmt::arg("b_tree", b_tree.c_str()));
  }
}

// ostream operator for `number_set`.
inline std::ostream& operator<<(std::ostream& s, const number_set set) {
  s << string_from_number_set(set);
  return s;
}

// ostream operator for `precedence`
inline std::ostream& operator<<(std::ostream& s, const precedence p) {
  s << string_from_precedence(p);
  return s;
}

}  // namespace wf
