#pragma once
#include <gtest/gtest.h>

#include "expr.h"

namespace math {

#define ASSERT_IDENTICAL(val1, val2) ASSERT_PRED_FORMAT2(IdenticalTestHelper, val1, val2)
#define ASSERT_NOT_IDENTICAL(val1, val2) ASSERT_PRED_FORMAT2(NotIdenticalTestHelper, val1, val2)

// Define so we can test for equality w/ strings more easily.
inline bool operator==(const std::string& lhs, const Expr& rhs) { return lhs == rhs.ToString(); }
inline bool operator!=(const std::string& lhs, const Expr& rhs) { return lhs != rhs.ToString(); }

// Test IsIdenticalTo
testing::AssertionResult IdenticalTestHelper(const std::string& name_a, const std::string& name_b,
                                             const Expr& a, const Expr& b);

// Test !IsIdenticalTo
testing::AssertionResult NotIdenticalTestHelper(const std::string& name_a,
                                                const std::string& name_b, const Expr& a,
                                                const Expr& b);

}  // namespace math
