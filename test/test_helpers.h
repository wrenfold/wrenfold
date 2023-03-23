#pragma once
#include <gtest/gtest.h>

#include "expression.h"
#include "matrix_expression.h"

namespace math {

#define ASSERT_IDENTICAL(val1, val2) ASSERT_PRED_FORMAT2(IdenticalTestHelper, val1, val2)
#define ASSERT_NOT_IDENTICAL(val1, val2) ASSERT_PRED_FORMAT2(NotIdenticalTestHelper, val1, val2)

testing::AssertionResult FormatFailedResult(const std::string_view description,
                                            const std::string& name_a, const std::string& name_b,
                                            const Expr& a, const Expr& b);

// Test IsIdenticalTo
template <typename A, typename B>
testing::AssertionResult IdenticalTestHelper(const std::string& name_a, const std::string& name_b,
                                             const A& a, const B& b) {
  if (static_cast<Expr>(a).IsIdenticalTo(static_cast<Expr>(b))) {
    return testing::AssertionSuccess();
  }
  return FormatFailedResult("is not identical to", name_a, name_b, static_cast<Expr>(a),
                            static_cast<Expr>(b));
}

// Test !IsIdenticalTo
template <typename A, typename B>
testing::AssertionResult NotIdenticalTestHelper(const std::string& name_a,
                                                const std::string& name_b, const A& a, const B& b) {
  if (!static_cast<Expr>(a).IsIdenticalTo(static_cast<Expr>(b))) {
    return testing::AssertionSuccess();
  }
  return FormatFailedResult("is identical (and should not be) to", name_a, name_b,
                            static_cast<Expr>(a), static_cast<Expr>(b));
}

}  // namespace math
