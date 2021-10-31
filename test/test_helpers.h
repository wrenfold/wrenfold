#pragma once
#include <gtest/gtest.h>

#include "expr.h"

namespace math {

#define ASSERT_IDENTICAL(val1, val2) ASSERT_PRED_FORMAT2(IdenticalTestHelper, val1, val2)
#define ASSERT_NOT_IDENTICAL(val1, val2) ASSERT_PRED_FORMAT2(NotIdenticalTestHelper, val1, val2)

// Test IsIdenticalTo
testing::AssertionResult IdenticalTestHelper(const std::string& name_a, const std::string& name_b,
                                             const Expr& a, const Expr& b);

// Test !IsIdenticalTo
testing::AssertionResult NotIdenticalTestHelper(const std::string& name_a,
                                                const std::string& name_b, const Expr& a,
                                                const Expr& b);

}  // namespace math
