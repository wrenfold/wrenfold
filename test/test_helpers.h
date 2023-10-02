#pragma once
#include <gtest/gtest.h>
#include "fmt_imports.h"

#include "expression.h"
#include "matrix_expression.h"

namespace math {

#define ASSERT_IDENTICAL(val1, val2) ASSERT_PRED_FORMAT2(IdenticalTestHelper, val1, val2)
#define ASSERT_NOT_IDENTICAL(val1, val2) ASSERT_PRED_FORMAT2(NotIdenticalTestHelper, val1, val2)

template <typename A, typename B>
testing::AssertionResult FormatFailedResult(const std::string_view description,
                                            const std::string_view name_a,
                                            const std::string_view name_b, const A& a, B& b) {
  // If the formatted string has multiple lines, preface it w/ a line break:
  const std::string a_str = a.ToString();
  const std::string b_str = b.ToString();
  const std::string_view a_prefix = std::count(a_str.begin(), a_str.end(), '\n') > 0 ? "\n" : " ";
  const std::string_view b_prefix = std::count(b_str.begin(), b_str.end(), '\n') > 0 ? "\n" : " ";

  // clang-format off
  return testing::AssertionFailure() << fmt::format(
             "{} {} {}, where:\n{} ={}{}\nand {} ={}{}\n"
             "expression tree for `{}`:\n{}\n"
             "expression tree for `{}`:\n{}",
             name_a, description, name_b,
             name_a, a_prefix, a_str, name_b, b_prefix, b_str,
             name_a, a.ToExpressionTreeString(),
             name_b, b.ToExpressionTreeString());
  // clang-format on
}

// This indirection exists so that we can coerce numeric literals to expressions.
template <typename T, typename = void>
struct ConvertAssertionArgument;
template <typename T>
struct ConvertAssertionArgument<T, std::enable_if_t<std::is_convertible_v<T, Expr>>> {
  // This overload will get selected for numeric literals.
  Expr operator()(const T& arg) const { return static_cast<Expr>(arg); }
};
template <>
struct ConvertAssertionArgument<MatrixExpr> {
  MatrixExpr operator()(const MatrixExpr& arg) const { return arg; }
};

// Test IsIdenticalTo
template <typename A, typename B>
testing::AssertionResult IdenticalTestHelper2(const std::string_view name_a,
                                              const std::string_view name_b, const A& a,
                                              const B& b) {
  if (a.IsIdenticalTo(b)) {
    return testing::AssertionSuccess();
  }
  return FormatFailedResult("is not identical to", name_a, name_b, a, b);
}

template <typename A, typename B>
testing::AssertionResult IdenticalTestHelper(const std::string_view name_a,
                                             const std::string_view name_b, const A& a,
                                             const B& b) {
  return IdenticalTestHelper2(name_a, name_b, ConvertAssertionArgument<A>{}(a),
                              ConvertAssertionArgument<B>{}(b));
}

// Test !IsIdenticalTo
template <typename A, typename B>
testing::AssertionResult NotIdenticalTestHelper2(const std::string_view name_a,
                                                 const std::string_view name_b, const A& a,
                                                 const B& b) {
  if (!a.IsIdenticalTo(b)) {
    return testing::AssertionSuccess();
  }
  return FormatFailedResult("is identical (and should not be) to", name_a, name_b, a, b);
}

template <typename A, typename B>
testing::AssertionResult NotIdenticalTestHelper(const std::string_view name_a,
                                                const std::string_view name_b, const A& a,
                                                const B& b) {
  return NotIdenticalTestHelper2(name_a, name_b, ConvertAssertionArgument<A>{}(a),
                                 ConvertAssertionArgument<B>{}(b));
}

}  // namespace math
