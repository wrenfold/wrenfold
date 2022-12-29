#include "binary_operations.h"
#include "constants.h"
#include "functions.h"
#include "operations_inline.h"
#include "test_helpers.h"

// Test basic composition of scalars and functions of scalars.
namespace math {

TEST(ScalarOperationsTest, TestAddition) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_TRUE((x + y).Is<Addition>());
  ASSERT_IDENTICAL((x + y).GetRaw<Addition>()->operator[](0), x);
  ASSERT_IDENTICAL((x + y).GetRaw<Addition>()->operator[](1), y);
  ASSERT_IDENTICAL(x + y, x + y);
  ASSERT_NOT_IDENTICAL(x + y, y + x);
  ASSERT_NOT_IDENTICAL(x + w, x + y);
  ASSERT_IDENTICAL(x, x + 0);
  ASSERT_IDENTICAL(x, 0 + x);
}

TEST(ScalarOperationsTest, TestSubtraction) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_TRUE((x - y).Is<Addition>());
  ASSERT_IDENTICAL((x - y).GetRaw<Addition>()->operator[](0), x);
  ASSERT_IDENTICAL((x - y).GetRaw<Addition>()->operator[](1), -y);
  ASSERT_IDENTICAL(y - x, y - x);
  ASSERT_IDENTICAL(-y, 0 - y);
  ASSERT_IDENTICAL(y, y - 0);
}

TEST(ScalarOperationsTest, TestMultiplication) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  const Expr zero{0};
  ASSERT_IDENTICAL(x * y, x * y);
  ASSERT_NOT_IDENTICAL(y * x, x * y);
  ASSERT_TRUE((x * y).Is<Multiplication>());
  ASSERT_IDENTICAL((x * y).GetRaw<Multiplication>()->operator[](0), x);
  ASSERT_IDENTICAL((x * y).GetRaw<Multiplication>()->operator[](1), y);
  ASSERT_IDENTICAL(Constants::Zero, x * 0);
  ASSERT_IDENTICAL(Constants::Zero, 0 * z);
  ASSERT_IDENTICAL(Constants::Zero, 0 * x * y);
  ASSERT_IDENTICAL(Constants::Zero, 1 * zero);
  ASSERT_IDENTICAL(Constants::Zero, zero * zero);
  ASSERT_IDENTICAL(x, x * 1);
}

TEST(ScalarOperationsTest, TestNegation) {
  const Expr x{"x"};
  ASSERT_IDENTICAL(-x, -x);
  ASSERT_IDENTICAL(-(-x), x);
  ASSERT_IDENTICAL(-(-(-x)), -x);
  ASSERT_EQ(x.GetImpl(), (-(-x)).GetImpl());  //  No copy should occur
}

TEST(ScalarOperationsTest, TestDivision) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_IDENTICAL(x / y, x / y);
  ASSERT_TRUE((x / y).Is<Division>());
  ASSERT_IDENTICAL((x / y).GetRaw<Division>()->Numerator(), x);
  ASSERT_IDENTICAL((x / y).GetRaw<Division>()->Denominator(), y);
  ASSERT_NOT_IDENTICAL(y / x, x / y);
  ASSERT_IDENTICAL(x / y / z, (x / y) / z);
  ASSERT_IDENTICAL(Constants::Zero, 0 / x);
  ASSERT_IDENTICAL(x, x / 1);
}

TEST(ScalarOperationsTest, TestPower) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(math::pow(x, y), math::pow(x, y));
  ASSERT_TRUE(math::pow(x, y).Is<Power>());
  ASSERT_IDENTICAL(math::pow(x, y).GetRaw<Power>()->Base(), x);
  ASSERT_IDENTICAL(math::pow(x, y).GetRaw<Power>()->Exponent(), y);
  ASSERT_IDENTICAL(math::pow(math::pow(x, y), 3), math::pow(math::pow(x, y), 3));
  ASSERT_NOT_IDENTICAL(math::pow(x, y), math::pow(y, x));
  ASSERT_IDENTICAL(Constants::One, math::pow(x, 0));
  ASSERT_IDENTICAL(Constants::Zero, math::pow(0, y));
  ASSERT_THROW(math::pow(Constants::Zero, 0), std::runtime_error);
}

TEST(ScalarOperationsTest, TestLog) {
  const Expr x{"x"};
  ASSERT_TRUE(log(x).Is<NaturalLog>());
  ASSERT_IDENTICAL(log(x), log(x));
  ASSERT_IDENTICAL(Constants::One, log(Constants::Euler));
}

}  // namespace math
