#include <gtest/gtest.h>

#include "constants.h"
#include "functions.h"

namespace math {

// Define so we can test for equality more easily.
bool operator==(const std::string& lhs, const Expr& rhs) {
  return lhs == rhs.ToNarrowString();
}

TEST(ScalarOperationsTest, TestAdditionAndSubtraction) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_EQ("x + y", x + y);
  ASSERT_EQ("x + y + w", x + y + w);
  ASSERT_EQ("x + 2", x + 2);
  ASSERT_EQ("w + 1.5 + y", w + 1.5 + y);
  ASSERT_EQ("x", x + 0);
  ASSERT_EQ("x", 0 + x);
  ASSERT_EQ("x + w", x + 0 + w);
  // should return this exact pointer:
  ASSERT_EQ(Constants::Zero.GetImpl(), (x - x).GetImpl());
  ASSERT_EQ("x + y - x", x + y - x);
  ASSERT_EQ("x - y", x - y);
  ASSERT_EQ("-y", 0 - y);
  ASSERT_EQ("y", y - 0);
}

TEST(ScalarOperationsTest, TestMultiplication) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  const Expr zero{0};
  ASSERT_EQ("x * y", x * y);
  ASSERT_EQ(Constants::Zero.GetImpl(), (x * 0).GetImpl());
  ASSERT_EQ(Constants::Zero.GetImpl(), (0 * z).GetImpl());
  ASSERT_EQ("0", 0 * x * y);
  ASSERT_EQ("0", 1 * zero);
  ASSERT_EQ("0", zero * zero);
  ASSERT_EQ("x", x * 1);
  ASSERT_EQ("x * z * y", x * z * y);
  ASSERT_EQ("x * y", 1 * x * y * 1);
  ASSERT_EQ("(x + y) * z", (x + y) * z);
  ASSERT_EQ("(x + y) * (z + x)", (x + y) * (z + x));
}

TEST(ScalarOperationsTest, TestNegation) {
  const Expr x{"x"};
  ASSERT_EQ("-x", -x);
  ASSERT_EQ("x", -(-x));
  ASSERT_EQ("0", -Constants::Zero);
  ASSERT_EQ(x.GetImpl(), (-(-x)).GetImpl());  //  No copy
}

TEST(ScalarOperationsTest, TestLog) {
  const Expr x{"x"};
  ASSERT_EQ("ln(x)", log(x));
  ASSERT_EQ(Constants::One.GetImpl(), log(Constants::Euler).GetImpl());
}

}  // namespace math
