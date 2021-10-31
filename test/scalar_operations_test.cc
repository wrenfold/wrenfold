#include "constants.h"
#include "functions.h"
#include "test_helpers.h"

namespace math {

// Define so we can test for equality more easily.
bool operator==(const std::string& lhs, const Expr& rhs) { return lhs == rhs.ToNarrowString(); }
bool operator!=(const std::string& lhs, const Expr& rhs) { return lhs != rhs.ToNarrowString(); }

TEST(ScalarOperationsTest, TestAdditionAndSubtraction) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_EQ("x + y", x + y);
  ASSERT_IDENTICAL(x + y, x + y);
  ASSERT_IDENTICAL(x + y, y + x);  //  commutative
  ASSERT_NOT_IDENTICAL(x + w, x + y);
  ASSERT_EQ("x + y + w", x + y + w);
  ASSERT_EQ("x + 2", x + 2);
  ASSERT_EQ("w + 1.5 + y", w + 1.5 + y);
  // adding zero is immediately removed:
  ASSERT_IDENTICAL(x, x + 0);
  ASSERT_IDENTICAL(x, 0 + x);
  ASSERT_EQ("x + w", x + 0 + w);
  // should return this exact pointer:
  ASSERT_EQ(Constants::Zero.GetImpl(), (x - x).GetImpl());
  ASSERT_NOT_IDENTICAL(x - y, y - x);
  ASSERT_EQ("x + y - x", x + y - x);
  ASSERT_EQ("x - y", x - y);
  ASSERT_IDENTICAL(-y, 0 - y);
  ASSERT_IDENTICAL(y, y - 0);
}

TEST(ScalarOperationsTest, TestMultiplication) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  const Expr zero{0};
  ASSERT_IDENTICAL(x * y, x * y);
  ASSERT_IDENTICAL(y * x, x * y);
  ASSERT_EQ("x * y", x * y);
  ASSERT_EQ(Constants::Zero.GetImpl(), (x * 0).GetImpl());
  ASSERT_EQ(Constants::Zero.GetImpl(), (0 * z).GetImpl());
  ASSERT_IDENTICAL(Constants::Zero, 0 * x * y);
  ASSERT_IDENTICAL(Constants::Zero, 1 * zero);
  ASSERT_IDENTICAL(Constants::Zero, zero * zero);
  ASSERT_EQ(x.GetImpl(), (x * 1).GetImpl());
  ASSERT_EQ("x * z * y", x * z * y);
  // multiplying by one is immediately removed:
  ASSERT_IDENTICAL(x * y, 1 * x * y * 1);
  ASSERT_EQ("(x + y) * z", (x + y) * z);
  ASSERT_EQ("(x + y) * (z + x)", (x + y) * (z + x));
}

TEST(ScalarOperationsTest, TestNegation) {
  const Expr x{"x"};
  ASSERT_EQ("-x", -x);
  ASSERT_IDENTICAL(-x, -x);
  ASSERT_EQ("x", -(-x));
  ASSERT_EQ("0", -Constants::Zero);
  ASSERT_EQ(x.GetImpl(), (-(-x)).GetImpl());  //  No copy should occur
}

TEST(ScalarOperationsTest, TestDivision) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_IDENTICAL(x / y, x / y);
  ASSERT_NOT_IDENTICAL(y / x, x / y);
  ASSERT_IDENTICAL(x / y / z, (x / y) / z);
  ASSERT_EQ("x / y", x / y);
  ASSERT_EQ("x / y / z", (x / y) / z);
  // should be simplified immediately
  ASSERT_EQ(Constants::One.GetImpl(), (x / x).GetImpl());
  ASSERT_EQ(Constants::Zero.GetImpl(), (0 / x).GetImpl());
  // c++ evaluates the multiplication first
  ASSERT_NE("y", y * x / x);
  ASSERT_EQ("y", y * (x / x));
  // division by one
  ASSERT_EQ("z", z / 1);
  ASSERT_EQ("z / y", z / y / 1);
}

TEST(ScalarOperationsTest, TestPower) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(x ^ y, x ^ y);
  ASSERT_IDENTICAL(x ^ y ^ 3, (x ^ y) ^ 3);
  ASSERT_NOT_IDENTICAL(y ^ x, x ^ y);
  ASSERT_EQ("x ^ y", x ^ y);
  ASSERT_EQ(Constants::One.GetImpl(), (x ^ 0).GetImpl());
  ASSERT_EQ(Constants::Zero.GetImpl(), (0 ^ y).GetImpl());
  ASSERT_EQ("2 ^ 3", Expr{2} ^ 3);
  ASSERT_EQ("y ^ (x + 1)", y ^ (x + 1));
  ASSERT_IDENTICAL(x ^ y, math::pow(x, y));
  ASSERT_THROW(Constants::Zero ^ 0, std::runtime_error);
}

TEST(ScalarOperationsTest, TestLog) {
  const Expr x{"x"};
  ASSERT_EQ("ln(x)", log(x));
  ASSERT_IDENTICAL(Constants::One, log(Constants::Euler));
}

}  // namespace math
