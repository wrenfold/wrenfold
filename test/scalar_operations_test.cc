#include "constants.h"
#include "expressions/addition.h"
#include "expressions/multiplication.h"
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
  ASSERT_IDENTICAL(x + y, x + y);
  ASSERT_NOT_IDENTICAL(x + y, y + x);
  ASSERT_NOT_IDENTICAL(x + w, x + y);
  ASSERT_IDENTICAL(x, x + 0_s);
  ASSERT_IDENTICAL(x, 0_s + x);
}

TEST(ScalarOperationsTest, TestSubtraction) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_TRUE((x - y).Is<Addition>());
  ASSERT_IDENTICAL((x - y).DynamicCast<Addition>()->operator[](0), x);
  ASSERT_IDENTICAL((x - y).DynamicCast<Addition>()->operator[](1), -y);
  ASSERT_IDENTICAL(y - x, y - x);
  ASSERT_IDENTICAL(-y, 0_s - y);
  ASSERT_IDENTICAL(y, y - 0_s);
}

TEST(ScalarOperationsTest, TestMultiplication) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_TRUE(TryCast<Multiplication>(x * y) != nullptr);
  ASSERT_IDENTICAL(x * y, x * y);

  // Canonicalization of order for a simple case:
  ASSERT_IDENTICAL(y * x, x * y);
  ASSERT_IDENTICAL(x * y * z, y * z * x);
  ASSERT_IDENTICAL(x * y * z, z * y * x);
  ASSERT_IDENTICAL(x * y * z, x * z * y);

  // Multiplication by zero:
  ASSERT_IDENTICAL(Constants::Zero, x * 0_s);
  ASSERT_IDENTICAL(Constants::Zero, 0_s * z);
  ASSERT_IDENTICAL(Constants::Zero, 0_s * x * y);

  // Collapsing of numeric terms:
  ASSERT_IDENTICAL(1_s, 1_s * 1_s * 1_s);
  ASSERT_IDENTICAL(x, 1_s * x);
  ASSERT_IDENTICAL(y, y * 2_s * (1_s / 2_s));
  ASSERT_IDENTICAL(y * z, y * 3_s * 2_s * (1_s / 6_s) * z);
  ASSERT_IDENTICAL(z * (10_s / 21_s), (5_s / 3_s) * z * (2_s / 7_s));

  // Collapsing with floats:
  ASSERT_IDENTICAL(x * 0.5_s, (x * 4.0_s) * 0.125_s);
  ASSERT_IDENTICAL(x * 1.25_s, x * 5_s * 0.125_s * 2.0_s);
  ASSERT_IDENTICAL(x * 0.0625_s, (x * 2.0_s) * (1_s / 32_s));

  // Collections of powers:
  ASSERT_IDENTICAL(pow(x, 1_s + 1_s), x * x);
}

TEST(ScalarOperationsTest, TestNegation) {
  const Expr x{"x"};
  ASSERT_IDENTICAL(-x, -x);
  ASSERT_TRUE(TryCast<Multiplication>(-x) != nullptr);
  ASSERT_IDENTICAL(-(-x), x);
  ASSERT_IDENTICAL(-(-(-x)), -x);
  ASSERT_EQ(x.GetImpl(), (-(-x)).GetImpl());  //  No copy should occur
}

TEST(ScalarOperationsTest, TestDivision) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_IDENTICAL(x / y, x / y);
  ASSERT_TRUE((x / y).Is<Multiplication>());
  //  ASSERT_IDENTICAL((x / y).DynamicCast<Multiplication>()->operator[](0), x);
  //  ASSERT_IDENTICAL((x / y).DynamicCast<Multiplication>()->operator[](1), pow(y, -1));
  ASSERT_NOT_IDENTICAL(y / x, x / y);
  ASSERT_IDENTICAL(x / y / z, (x / y) / z);
  ASSERT_IDENTICAL(Constants::Zero, 0_s / x);
  ASSERT_IDENTICAL(x, x / 1_s);
}

TEST(ScalarOperationsTest, TestPower) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_IDENTICAL(pow(x, y), pow(x, y));
  ASSERT_NOT_IDENTICAL(pow(x, y), pow(y, x));
  ASSERT_IDENTICAL(AsBaseAndExponent(pow(x, y)).first, x);
  ASSERT_IDENTICAL(AsBaseAndExponent(pow(x, y)).second, y);

  // Powers don't get combined automatically (for variable exponents):
  ASSERT_IDENTICAL(AsBaseAndExponent(pow(pow(x, y), z)).first, pow(x, y));
  ASSERT_IDENTICAL(AsBaseAndExponent(pow(pow(x, y), z)).second, z);

  // Powers of expressions to constants:
  ASSERT_IDENTICAL(Constants::One, pow(x * y, 0_s));
  ASSERT_IDENTICAL(Constants::Zero, pow(0_s, y + z));
  ASSERT_IDENTICAL(x + y, pow(x + y, 1_s));

  // Numeric simplification rules:
  ASSERT_IDENTICAL(Constants::One, pow(1_s, 1_s));
  ASSERT_IDENTICAL(8_s, pow(2_s, 3_s));
  ASSERT_IDENTICAL(-243_s, pow(-3_s, 5_s));
  ASSERT_IDENTICAL(Constants::Zero, pow(0_s, 10_s));
  ASSERT_IDENTICAL(Rational::Create(1, 8), pow(2_s, -3_s));
  ASSERT_IDENTICAL(25_s / 64_s, pow(5_s / 8_s, 2_s));
  ASSERT_IDENTICAL(343_s / 729_s, pow(9_s / 7_s, -3_s));
  ASSERT_IDENTICAL(1_s / 5_s, pow(5_s, -1_s));
  ASSERT_THROW(pow(0_s, -1_s), std::runtime_error);

  // Floats...
  ASSERT_IDENTICAL(Expr{std::pow(2.0, 4.5)}, pow(2_s, 4.5_s));
  ASSERT_IDENTICAL(Expr{std::pow(1.122, 6.0)}, pow(1.122_s, 6_s));
  ASSERT_IDENTICAL(Expr{std::pow(6.7, -0.5)}, pow(6.7_s, -0.5_s));

  // Rational powers of integers:
  ASSERT_IDENTICAL(pow(3_s, 3_s / 4_s), pow(3_s, 3_s / 4_s));
  ASSERT_IDENTICAL(3_s * pow(3_s, 1_s / 3_s), pow(3_s, 4_s / 3_s));
  ASSERT_IDENTICAL(3_s * pow(3_s, 1_s / 3_s), pow(9_s, 2_s / 3_s));
  ASSERT_IDENTICAL(pow(7_s, 7_s / 9_s) * pow(3_s, 7_s / 9_s) * pow(2_s, 7_s / 9_s),
                   pow(42_s, 7_s / 9_s));
  ASSERT_IDENTICAL(5929_s * pow(7_s, 1_s / 8_s) * pow(11_s, 1_s / 8_s), pow(77_s, 17_s / 8_s));

  // Negative rational powers:
  ASSERT_IDENTICAL(pow(2_s, -4_s / 5_s), pow(2_s, -4_s / 5_s));
  ASSERT_IDENTICAL((1_s / 2_s) * pow(2_s, -2_s / 5_s), pow(2_s, -7_s / 5_s));
  ASSERT_IDENTICAL((1_s / 5_s) * pow(5_s, -1_s / 11_s), pow(5_s, -12_s / 11_s));
  ASSERT_IDENTICAL((1_s / 8281_s) * pow(7_s, 2_s / 3_s) * pow(13_s, 2_s / 3_s),
                   pow(91_s, -4_s / 3_s));

  // Powers of powers, where the outer exponent is a numeric:
  ASSERT_IDENTICAL(pow(x, y * 2_s), pow(pow(x, y), 2_s));
  ASSERT_IDENTICAL(pow(x, y * 27_s), pow(pow(x, y * 3_s), 9_s));
  ASSERT_IDENTICAL(pow(x, y * 2_s / 14_s), pow(pow(x, y * 2_s / 7_s), 1_s / 2_s));
  ASSERT_IDENTICAL(pow(x, y * 0.25_s), pow(pow(x, y * 1_s / 2_s), 0.5_s));
  ASSERT_IDENTICAL(pow(x, y), pow(pow(x, y * 1_s / 4_s), 4_s));

  // TODO: This is wrong, fix it!
  ASSERT_IDENTICAL(x, pow(pow(x, 2_s), 1_s / 2_s));
  ASSERT_IDENTICAL(pow(x, -2_s), pow(pow(x, 14_s), -1_s / 7_s));
}

TEST(ScalarOperationsTest, TestLog) {
  const Expr x{"x"};
  ASSERT_TRUE(log(x).Is<NaturalLog>());
  ASSERT_IDENTICAL(log(x), log(x));
  ASSERT_IDENTICAL(Constants::One, log(Constants::Euler));
}

}  // namespace math
