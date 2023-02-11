#include "constants.h"
#include "expressions/addition.h"
#include "expressions/multiplication.h"
#include "expressions/power.h"
#include "functions.h"
#include "test_helpers.h"

// Test basic composition of scalars and functions of scalars.
namespace math {
using namespace math::custom_literals;

TEST(ScalarOperationsTest, TestAddition) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_TRUE(TryCast<Addition>(x + y) != nullptr);
  ASSERT_IDENTICAL(x + y, x + y);
  ASSERT_IDENTICAL(x + y, y + x);
  ASSERT_NOT_IDENTICAL(x + w, x + y);
  ASSERT_NOT_IDENTICAL(x - w, z + y);

  // Canonicalization of order:
  ASSERT_IDENTICAL(w + x + y, y + x + w);
  ASSERT_IDENTICAL(w + x + y, x + w + y);
  ASSERT_IDENTICAL(1_s + w, w + 1_s);
  ASSERT_IDENTICAL(5_s / 7_s + x, x + 5_s / 7_s);
  ASSERT_IDENTICAL(x * w + x * z, x * z + x * w);
  ASSERT_IDENTICAL(w * x + x * z, x * z + x * w);

  // Collapsing of numerics:
  ASSERT_IDENTICAL(x, x + 0_s);
  ASSERT_IDENTICAL(x, 0_s + x);
  ASSERT_IDENTICAL(w + 19_s / 7_s, w + 5_s / 7_s + 2_s);
  ASSERT_IDENTICAL(w + y + 1_s, y + 1_s / 2_s + w + 1_s / 2_s);
  ASSERT_IDENTICAL(2_s * z, 3_s + z - 2_s + z - 1_s);
  ASSERT_IDENTICAL(0_s, 3_s + -4_s + 1_s);
  ASSERT_IDENTICAL(0.5_s + x, x + 0.25_s + 1_s / 4_s);
  ASSERT_IDENTICAL(1.0_s, 3_s / 2_s + y - 0.5_s - y);

  // Collection of identical terms:
  ASSERT_IDENTICAL(2_s * x, x + x);
  ASSERT_IDENTICAL(-3_s * x, x - 4_s * x);
  ASSERT_IDENTICAL(3_s * pow(x, 2_s) - 4_s * y,
                   pow(x, 2_s) - 2_s * y + 2_s * pow(x, 2_s) - 2_s * y);
  ASSERT_IDENTICAL(-2_s * log(x) + 2_s * pow(y, 2_s), -log(x) + -log(x) + y * y + y * y);
  ASSERT_IDENTICAL(z * x, z * x / 3_s + 2_s * z * x / 3_s);
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
  ASSERT_IDENTICAL(pow(x, 2_s), x * x);
  ASSERT_IDENTICAL(pow(x, 3_s), x * x * x);
  ASSERT_IDENTICAL(pow(x, 2_s) * pow(y, 2_s), x * y * x * y);
  ASSERT_IDENTICAL(Constants::One, pow(x, 2_s) * pow(x, -2_s));
  ASSERT_IDENTICAL(x * pow(y, 2_s) * pow(log(z), 3_s), log(z) * y * x * log(z) * y * log(z));
  ASSERT_IDENTICAL(1_s / 33_s * x * pow(3_s, 2_s / 3_s) * pow(11_s, 2_s / 3_s),
                   pow(33_s, -2_s / 3_s) * pow(33_s, 1_s / 3_s) * x);

  // Including symbolics constants:
  ASSERT_IDENTICAL(pow(Constants::Pi, 3_s), Constants::Pi * Constants::Pi * Constants::Pi);
  ASSERT_IDENTICAL(pow(Constants::Euler, 2_s) * x, Constants::Euler * x * Constants::Euler);

  // Collections of powers of functions:
  ASSERT_IDENTICAL(pow(cos(x), 2_s), cos(x) * cos(x));
  ASSERT_IDENTICAL(pow(cos(x), 2_s) * pow(tan(y), 3_s / 5_s),
                   cos(x) * cos(x) * pow(tan(y), 1_s / 5_s) * pow(tan(y), 2_s / 5_s));
  ASSERT_IDENTICAL(pow(sin(x), 2_s) * pow(log(z * y), Constants::NegativeOne),
                   sin(x) * sin(x) / log(z * y));
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
  ASSERT_TRUE(TryCast<Multiplication>(x / y) != nullptr);
  ASSERT_NOT_IDENTICAL(y / x, x / y);
  ASSERT_IDENTICAL(x / y / z, (x / y) / z);
  ASSERT_IDENTICAL(Constants::Zero, 0_s / x);
  ASSERT_IDENTICAL(x, x / 1_s);
  ASSERT_IDENTICAL(1_s, x / x);
  ASSERT_IDENTICAL(x / y / z, x * pow(y, -1_s) * pow(z, -1_s));
  ASSERT_IDENTICAL(z / (y * x), z * pow(y, -1_s) * pow(x, -1_s));
  ASSERT_IDENTICAL(z / (y * z), 1_s / y);

  // Cancellation of powers:
  ASSERT_IDENTICAL(x, pow(x, 3_s) / pow(x, 2_s));
  ASSERT_IDENTICAL(Constants::One, pow(x, 3_s) / (x * x * x));
  ASSERT_IDENTICAL(x * y * pow(z, 1_s / 2_s),
                   pow(x, 5_s) * pow(y, 3_s) * pow(z, 3_s / 2_s) / (x * x * x * x * y * y * z));
}

TEST(ScalarOperationsTest, TestAsCoeffAndMultiplicand) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};

  ASSERT_IDENTICAL(Constants::Zero, AsCoefficientAndMultiplicand(0_s).first);
  ASSERT_IDENTICAL(Constants::One, AsCoefficientAndMultiplicand(0_s).second);

  ASSERT_IDENTICAL(2_s, AsCoefficientAndMultiplicand(2_s).first);
  ASSERT_IDENTICAL(Constants::One, AsCoefficientAndMultiplicand(2_s).second);

  ASSERT_IDENTICAL(Constants::One, AsCoefficientAndMultiplicand(x).first);
  ASSERT_IDENTICAL(x, AsCoefficientAndMultiplicand(x).second);

  ASSERT_IDENTICAL(Constants::One, AsCoefficientAndMultiplicand(x / y).first);
  ASSERT_IDENTICAL(x / y, AsCoefficientAndMultiplicand(x / y).second);

  // Special constants are symbols, and do not go in the multiplicand:
  ASSERT_IDENTICAL(3_s, AsCoefficientAndMultiplicand(3_s * Constants::Pi).first);
  ASSERT_IDENTICAL(Constants::Pi, AsCoefficientAndMultiplicand(3_s * Constants::Pi).second);

  ASSERT_IDENTICAL(5_s / 7_s, AsCoefficientAndMultiplicand(Constants::Pi * z * 5_s / 7_s).first);
  ASSERT_IDENTICAL(Constants::Pi * z, AsCoefficientAndMultiplicand(Constants::Pi * z).second);

  // Include some functions:
  ASSERT_IDENTICAL(1.22_s, AsCoefficientAndMultiplicand(1.22_s * sin(x) * cos(y)).first);
  ASSERT_IDENTICAL(cos(y) * sin(x), AsCoefficientAndMultiplicand(1.22_s * sin(x) * cos(y)).second);
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
  ASSERT_THROW(pow(0_s, -1_s), AssertionError);

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

  // Powers of multiplications:
  ASSERT_IDENTICAL(pow(x, 2_s) * pow(y, 2_s), pow(x * y, 2_s));
  ASSERT_IDENTICAL(pow(x, z) * pow(y, z), pow(x * y, z));

  // TODO: This is wrong, fix it!
  ASSERT_IDENTICAL(x, pow(pow(x, 2_s), 1_s / 2_s));
  ASSERT_IDENTICAL(pow(x, -2_s), pow(pow(x, 14_s), -1_s / 7_s));
}

TEST(ScalarOperationsTest, TestDistribute) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  const Expr p{"p"};
  const Expr q{"q"};
  const Expr v{"v"};
  ASSERT_IDENTICAL(x, x.Distribute());
  ASSERT_IDENTICAL(x + y, (x + y).Distribute());
  ASSERT_IDENTICAL(6_s + 3_s * x, (3_s * (x + 2_s)).Distribute());
  ASSERT_IDENTICAL(w * x + w * y, (w * (x + y)).Distribute());
  ASSERT_IDENTICAL(x * x * y + x * x * 5_s / 3_s, (x * x * (y + 5_s / 3_s)).Distribute());
  ASSERT_IDENTICAL(x * log(y) - x * w, ((log(y) - w) * x).Distribute());

  // Multiple distributions:
  ASSERT_IDENTICAL(w * y + w * z + x * y + x * z, ((w + x) * (y + z)).Distribute());
  ASSERT_IDENTICAL(w * w - 16_s, ((w - 4_s) * (w + 4_s)).Distribute());
  ASSERT_IDENTICAL((p * w * y + p * w * z - p * x * y - p * x * z) +
                       (q * w * y + q * w * z - q * x * y - q * x * z) +
                       (v * w * y + v * w * z - v * x * y - v * x * z),
                   ((w - x) * (p + q + v) * (y + z)).Distribute());
  // Recursive distributions:
  ASSERT_IDENTICAL(
      (2_s * p * q * w * x) - (2_s * p * q * w * y) + (p * v * w * x) - (p * v * w * y),
      (w * (x - y) * (p * (v + q * 2_s))).Distribute());
}

}  // namespace math
