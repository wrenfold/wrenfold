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
  ASSERT_TRUE((x + y).Is<Addition>());
  ASSERT_IDENTICAL(x + y, x + y);
  ASSERT_IDENTICAL(x + y, y + x);
  ASSERT_NOT_IDENTICAL(x + w, x + y);
  ASSERT_NOT_IDENTICAL(x - w, z + y);
  ASSERT_EQ("Addition", (x + y).TypeName());

  // Canonicalization of order:
  ASSERT_IDENTICAL(w + x + y, y + x + w);
  ASSERT_IDENTICAL(w + x + y, x + w + y);
  ASSERT_IDENTICAL(1 + w, w + 1);
  ASSERT_IDENTICAL(5_s / 7 + x, x + 5 / 7_s);
  ASSERT_IDENTICAL(x * w + x * z, x * z + x * w);
  ASSERT_IDENTICAL(w * x + x * z, x * z + x * w);

  // Collapsing of numerics:
  ASSERT_IDENTICAL(x, x + 0);
  ASSERT_IDENTICAL(x, 0 + x);
  ASSERT_IDENTICAL(w + 19 / 7_s, w + 5 / 7_s + 2);
  ASSERT_IDENTICAL(w + y + 1, y + 1 / 2_s + w + 1 / 2_s);
  ASSERT_IDENTICAL(2 * z, 3 + z - 2 + z - 1);
  ASSERT_IDENTICAL(0_s, 3_s + -4_s + 1_s);
  ASSERT_IDENTICAL(0.5_s + x, x + 0.25_s + 1_s / 4_s);
  ASSERT_IDENTICAL(1.0_s, 3_s / 2 + y - 0.5_s - y);

  // Collection of identical terms:
  ASSERT_IDENTICAL(2 * x, x + x);
  ASSERT_IDENTICAL(-3 * x, x - 4_s * x);
  ASSERT_IDENTICAL(3 * pow(x, 2) - 4 * y, pow(x, 2) - 2 * y + 2 * pow(x, 2) - 2 * y);
  ASSERT_IDENTICAL(-2 * log(x) + 2 * pow(y, 2), -log(x) + -log(x) + y * y + y * y);
  ASSERT_IDENTICAL(z * x, z * x / 3_s + 2 * z * x / 3_s);
}

TEST(ScalarOperationsTest, TestMultiplication) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_TRUE((x * y).Is<Multiplication>());
  ASSERT_IDENTICAL(x * y, x * y);
  ASSERT_EQ("Multiplication", (x * y).TypeName());

  // Canonicalization of order for a simple case:
  ASSERT_IDENTICAL(y * x, x * y);
  ASSERT_IDENTICAL(x * y * z, y * z * x);
  ASSERT_IDENTICAL(x * y * z, z * y * x);
  ASSERT_IDENTICAL(x * y * z, x * z * y);

  // Multiplication by zero:
  ASSERT_IDENTICAL(Constants::Zero, x * 0);
  ASSERT_IDENTICAL(Constants::Zero, 0 * z);
  ASSERT_IDENTICAL(Constants::Zero, 0 * x * y);

  // Collapsing of numeric terms:
  ASSERT_IDENTICAL(1_s, 1_s * 1 * 1);
  ASSERT_IDENTICAL(x, 1 * x);
  ASSERT_IDENTICAL(y, y * 2 * (1 / 2_s));
  ASSERT_IDENTICAL(y * z, (y * 3) * 2 * (1 / 6_s) * z);
  ASSERT_IDENTICAL(z * (10_s / 21), (5 / 3_s) * z * (2 / 7_s));

  // Collapsing with floats:
  ASSERT_IDENTICAL(x * 0.5_s, (x * 4.0_s) * 0.125_s);
  ASSERT_IDENTICAL(x * 1.25_s, x * 5_s * 0.125_s * 2.0_s);
  ASSERT_IDENTICAL(x * 0.0625_s, (x * 2.0_s) * (1_s / 32_s));

  // Automatic distribution of numeric constants into additions:
  ASSERT_IDENTICAL(3 * x + 3 * y, 3 * (x + y));
  ASSERT_IDENTICAL(-5_s / 6 * z - 5_s / 6 * Constants::Pi, -5_s / 6 * (z + Constants::Pi));
  ASSERT_IDENTICAL(x / 3 - y, (x - 3 * y) / 3);
  ASSERT_IDENTICAL(3.2 * x - 6.4 * y, (x - 2 * y) * 3.2);

  // Collections of powers:
  ASSERT_IDENTICAL(pow(x, 2), x * x);
  ASSERT_IDENTICAL(pow(x, 3), x * x * x);
  ASSERT_IDENTICAL(pow(x, 2) * pow(y, 2), x * y * x * y);
  ASSERT_IDENTICAL(1, pow(x, 2) * pow(x, -2));
  ASSERT_IDENTICAL(x * pow(y, 2) * pow(log(z), 3), log(z) * y * x * log(z) * y * log(z));
  ASSERT_IDENTICAL(x * pow(3, 2 / 3_s) * pow(11, 2 / 3_s) / 33,
                   pow(33, -2 / 3_s) * pow(33, 1 / 3_s) * x);
  ASSERT_IDENTICAL(x, sqrt(x) * sqrt(x));
  ASSERT_IDENTICAL(pow(x, 3 / 2_s), sqrt(x) * sqrt(x) * sqrt(x));

  // Normalization of powers of integers:
  ASSERT_IDENTICAL(2 * pow(2, 1 / 7_s), pow(2, 3 / 7_s) * pow(2, 5 / 7_s));
  ASSERT_IDENTICAL(pow(5, 10 / 11_s) / 25, pow(5, -5 / 11_s) * pow(5, -7 / 11_s));

  // Including symbolics constants:
  ASSERT_IDENTICAL(pow(Constants::Pi, 3), Constants::Pi * Constants::Pi * Constants::Pi);
  ASSERT_IDENTICAL(pow(Constants::Euler, 2) * x, Constants::Euler * x * Constants::Euler);

  // Collections of powers of functions:
  ASSERT_IDENTICAL(pow(cos(x), 2), cos(x) * cos(x));
  ASSERT_IDENTICAL(pow(cos(x), 2) * pow(tan(y), 3 / 5_s),
                   cos(x) * cos(x) * pow(tan(y), 1 / 5_s) * pow(tan(y), 2 / 5_s));
  ASSERT_IDENTICAL(pow(sin(x), 2) * pow(log(z * y), Constants::NegativeOne),
                   sin(x) * sin(x) / log(z * y));
}

TEST(ScalarOperationsTest, TestNegation) {
  const Expr x{"x"};
  ASSERT_IDENTICAL(-x, -x);
  ASSERT_TRUE((-x).Is<Multiplication>());
  ASSERT_IDENTICAL(-(-x), x);
  ASSERT_IDENTICAL(-(-(-x)), -x);
}

TEST(ScalarOperationsTest, TestDivision) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_IDENTICAL(x / y, x / y);
  ASSERT_TRUE((x / y).Is<Multiplication>());
  ASSERT_NOT_IDENTICAL(y / x, x / y);
  ASSERT_IDENTICAL(x / y / z, (x / y) / z);
  ASSERT_IDENTICAL(Constants::Zero, 0 / x);
  ASSERT_IDENTICAL(x, x / 1);
  ASSERT_IDENTICAL(1_s, x / x);
  ASSERT_IDENTICAL(x / y / z, x * pow(y, -1) * pow(z, -1_s));
  ASSERT_IDENTICAL(z / (y * x), z * pow(y, -1) * pow(x, -1_s));
  ASSERT_IDENTICAL(z / (y * z), 1_s / y);

  // Cancellation of powers:
  ASSERT_TRUE(pow(x, 3).Is<Power>());
  ASSERT_TRUE(pow(x, 2).Is<Power>());
  ASSERT_TRUE(CastPtr<Power>(pow(x, 3))->Base().IsIdenticalTo(CastPtr<Power>(pow(x, 2))->Base()));

  ASSERT_IDENTICAL(x, pow(x, 3) / pow(x, 2));
  ASSERT_IDENTICAL(Constants::One, pow(x, 3) / (x * x * x));
  ASSERT_IDENTICAL(x * y * pow(z, 1_s / 2_s),
                   pow(x, 5) * pow(y, 3) * pow(z, 3_s / 2) / (x * x * x * x * y * y * z));
}

TEST(ScalarOperationsTest, TestAsCoeffAndMultiplicand) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};

  ASSERT_IDENTICAL(Constants::Zero, AsCoefficientAndMultiplicand(0).first);
  ASSERT_IDENTICAL(Constants::One, AsCoefficientAndMultiplicand(0).second);

  ASSERT_IDENTICAL(2_s, AsCoefficientAndMultiplicand(2).first);
  ASSERT_IDENTICAL(Constants::One, AsCoefficientAndMultiplicand(2).second);

  ASSERT_IDENTICAL(Constants::One, AsCoefficientAndMultiplicand(x).first);
  ASSERT_IDENTICAL(x, AsCoefficientAndMultiplicand(x).second);

  ASSERT_IDENTICAL(Constants::One, AsCoefficientAndMultiplicand(x / y).first);
  ASSERT_IDENTICAL(x / y, AsCoefficientAndMultiplicand(x / y).second);

  // Special constants are symbols, and do not go in the multiplicand:
  ASSERT_IDENTICAL(3_s, AsCoefficientAndMultiplicand(3 * Constants::Pi).first);
  ASSERT_IDENTICAL(Constants::Pi, AsCoefficientAndMultiplicand(3 * Constants::Pi).second);

  ASSERT_IDENTICAL(5_s / 7, AsCoefficientAndMultiplicand(Constants::Pi * z * 5_s / 7).first);
  ASSERT_IDENTICAL(Constants::Pi * z, AsCoefficientAndMultiplicand(Constants::Pi * z).second);

  // Include some functions:
  ASSERT_IDENTICAL(1.22_s, AsCoefficientAndMultiplicand(1.22 * sin(x) * cos(y)).first);
  ASSERT_IDENTICAL(cos(y) * sin(x), AsCoefficientAndMultiplicand(1.22 * sin(x) * cos(y)).second);
}

TEST(ScalarOperationsTest, TestPower) {
  auto [x, y, z] = Symbols("x", "y", "z");
  ASSERT_IDENTICAL(pow(x, y), pow(x, y));
  ASSERT_NOT_IDENTICAL(pow(x, y), pow(y, x));
  ASSERT_IDENTICAL(AsBaseAndExponent(pow(x, y)).first, x);
  ASSERT_IDENTICAL(AsBaseAndExponent(pow(x, y)).second, y);
  ASSERT_EQ("Power", pow(x, y).TypeName());

  // Powers don't get combined automatically (for variable exponents):
  ASSERT_IDENTICAL(AsBaseAndExponent(pow(pow(x, y), z)).first, pow(x, y));
  ASSERT_IDENTICAL(AsBaseAndExponent(pow(pow(x, y), z)).second, z);

  // Powers of expressions to constants:
  ASSERT_IDENTICAL(Constants::One, pow(x * y, 0));
  ASSERT_IDENTICAL(Constants::Zero, pow(0, y + z));
  ASSERT_IDENTICAL(x + y, pow(x + y, 1));

  // Numeric simplification rules:
  ASSERT_IDENTICAL(Constants::One, pow(1, 1));
  ASSERT_IDENTICAL(8, pow(2, 3));
  ASSERT_IDENTICAL(-243_s, pow(-3, 5));
  ASSERT_IDENTICAL(Constants::Zero, pow(0, 10));
  ASSERT_IDENTICAL(Rational::Create(1, 8), pow(2, -3));
  ASSERT_IDENTICAL(25 / 64_s, pow(5 / 8_s, 2));
  ASSERT_IDENTICAL(343 / 729_s, pow(9 / 7_s, -3));
  ASSERT_IDENTICAL(1 / 5_s, pow(5, -1));
  ASSERT_THROW(pow(0, -1), AssertionError);

  // Floats...
  ASSERT_IDENTICAL(Expr{std::pow(2.0, 4.5)}, pow(2, 4.5));
  ASSERT_IDENTICAL(Expr{std::pow(1.122, 6.0)}, pow(1.122, 6));
  ASSERT_IDENTICAL(Expr{std::pow(6.7, -0.5)}, pow(6.7, -0.5));

  // Rational powers of integers:
  ASSERT_IDENTICAL(1, pow(1, 7_s / 9));
  ASSERT_IDENTICAL(1, pow(1, -3_s / 22));
  ASSERT_IDENTICAL(pow(3, 3 / 4_s), pow(3, 3 / 4_s));
  ASSERT_IDENTICAL(3 * pow(3, 1 / 3_s), pow(3, 4 / 3_s));
  ASSERT_IDENTICAL(3 * pow(3, 1 / 3_s), pow(9, 2 / 3_s));
  ASSERT_IDENTICAL(pow(7, 7 / 9_s) * pow(3, 7 / 9_s) * pow(2, 7 / 9_s), pow(42, 7 / 9_s));
  ASSERT_IDENTICAL(5929 * pow(7, 1 / 8_s) * pow(11, 1 / 8_s), pow(77, 17 / 8_s));

  // Negative rational powers:
  ASSERT_IDENTICAL(pow(2, -4 / 5_s), pow(2, -4 / 5_s));
  ASSERT_IDENTICAL((1 / 2_s) * pow(2, -2 / 5_s), pow(2, -7 / 5_s));
  ASSERT_IDENTICAL((1 / 5_s) * pow(5, -1 / 11_s), pow(5, -12 / 11_s));
  ASSERT_IDENTICAL((1 / 8281_s) * pow(7, 2 / 3_s) * pow(13, 2 / 3_s), pow(91, -4 / 3_s));

  // Powers of powers. Cases that simplify:
  ASSERT_IDENTICAL(pow(x, y * 2), pow(pow(x, y), 2));
  ASSERT_IDENTICAL(pow(x, y * 27), pow(pow(x, y * 3), 9));
  ASSERT_IDENTICAL(pow(x, 3.0), pow(pow(x, 1.5), 2.0));
  ASSERT_IDENTICAL(pow(x, 2 / 5_s), pow(pow(x, 1 / 5_s), 2));
  ASSERT_IDENTICAL(pow(x, 6 / 55_s), pow(pow(x, 2 / 11_s), 3_s / 5));
  ASSERT_IDENTICAL(pow(x, 3 / 4_s * z), pow(pow(x, 3 / 4_s), z));
  ASSERT_IDENTICAL(pow(x, 0.25), pow(pow(x, 1 / 2_s), 0.5));
  ASSERT_IDENTICAL(pow(x, 3.0), pow(pow(x, 1.0), 3));
  ASSERT_IDENTICAL(pow(x, -1 / 2_s), 1 / sqrt(x));

  // Should not simplify:
  ASSERT_NOT_IDENTICAL(x, pow(pow(x, 2), 1 / 2_s));
  ASSERT_NOT_IDENTICAL(x, pow(pow(x, 4), 1 / 4_s));
  ASSERT_NOT_IDENTICAL(pow(x, y / 14_s), pow(pow(x, y * 2 / 7_s), 1 / 2_s));
  ASSERT_NOT_IDENTICAL(pow(x, y / 4), pow(pow(x, y / 2), 1 / 2_s));
  ASSERT_NOT_IDENTICAL(pow(x, -2), pow(pow(x, 14), -1 / 7_s));
  ASSERT_NOT_IDENTICAL(pow(x, y / 4), pow(pow(x, y), 1 / 4_s));
  ASSERT_NOT_IDENTICAL(pow(x, 2 * z), pow(pow(x, 2), z));
  ASSERT_NOT_IDENTICAL(pow(x, -1 / 2_s), sqrt(1 / x));
  ASSERT_NOT_IDENTICAL(pow(x, 1 / 2_s) * pow(z, -1 / 2_s), sqrt(x / z));

  // Powers of multiplications:
  ASSERT_IDENTICAL(pow(x, 2) * pow(y, 2), pow(x * y, 2));
  ASSERT_IDENTICAL(pow(x, z) * pow(y, z), pow(x * y, z));

  // TODO: This should produce `i` = sqrt(-1)
  ASSERT_IDENTICAL(sqrt(-1) * sqrt(2) * sqrt(x), pow(-2 * x, 1_s / 2));
  ASSERT_IDENTICAL(sqrt(-1) * 3 * sqrt(x), pow(-9 * x, 1_s / 2));
}

// Test creating relational expressions
TEST(ScalarOperationsTest, TestRelationals) {
  const auto [x, y, z] = Symbols("x", "y", "z");
  ASSERT_TRUE((x > y).Is<Relational>());
  ASSERT_TRUE((x == y).Is<Relational>());
  ASSERT_IDENTICAL(x > y, x > y);
  ASSERT_IDENTICAL(x > y, y < x);
  ASSERT_IDENTICAL(z <= x, x >= z);
  ASSERT_IDENTICAL(x == y, y == x);
  ASSERT_NOT_IDENTICAL(x < y, x <= y);

  // Simplification of numerical cases:
  ASSERT_IDENTICAL(Constants::True, 3 > 2_s);
  ASSERT_IDENTICAL(Constants::True, 3 >= 2_s);
  ASSERT_IDENTICAL(Constants::False, 3 == 2_s);
  ASSERT_IDENTICAL(Constants::True, 4 == 4_s);
  ASSERT_IDENTICAL(Constants::False, -4 > 0_s);
  ASSERT_IDENTICAL(Constants::False, -4 >= 0_s);
  ASSERT_IDENTICAL(Constants::True, 0 < 1_s);
  ASSERT_IDENTICAL(Constants::True, 2 <= 2_s);

  ASSERT_IDENTICAL(Constants::True, 3 / 2_s > 1 / 5_s);
  ASSERT_IDENTICAL(Constants::True, 7 / 9_s > 3 / 6_s);
  ASSERT_IDENTICAL(Constants::True, 45 / 11_s > 4);
  ASSERT_IDENTICAL(Constants::True, 9 > 116 / 13_s);
  ASSERT_IDENTICAL(Constants::False, 9 < 116 / 13_s);
  ASSERT_IDENTICAL(Constants::False, 9 <= 116 / 13_s);
  ASSERT_IDENTICAL(Constants::True, 9 < 128 / 13_s);
  ASSERT_IDENTICAL(Constants::False, 9 > 128 / 13_s);
  ASSERT_IDENTICAL(Constants::False, 6 / 7_s == 2 / 3_s);
  ASSERT_IDENTICAL(Constants::True, 6 / 7_s == 12 / 14_s);

  // Float comparisons:
  ASSERT_IDENTICAL(Constants::True, 1.2_s == 1.2);
  ASSERT_IDENTICAL(Constants::True, 0.5_s < 0.6);
  ASSERT_IDENTICAL(Constants::False, 2.45_s < 1.999);
  ASSERT_IDENTICAL(Constants::True, 7.7_s >= 7.7);
  ASSERT_IDENTICAL(Constants::False, 0.6667_s >= 10.0);

  // Float to integer comparison (mostly tested in integer_utils):
  ASSERT_IDENTICAL(Constants::True, 18.2_s > 10);
  ASSERT_IDENTICAL(Constants::True, 10.0_s == 10);
  ASSERT_IDENTICAL(Constants::True, 109.2_s < 110);
  ASSERT_IDENTICAL(Constants::True, 22.0_s <= 22);

  // Constant to constant comparison:
  ASSERT_IDENTICAL(Constants::True, Constants::Pi > Constants::Euler);
  ASSERT_IDENTICAL(Constants::True, Constants::Euler == Constants::Euler);
  ASSERT_IDENTICAL(Constants::False, Constants::Euler >= Constants::Pi);
  ASSERT_IDENTICAL(Constants::False, Constants::True > Constants::Pi);
  ASSERT_IDENTICAL(Constants::True, Constants::True == Constants::True);
  ASSERT_IDENTICAL(Constants::False, Constants::True == Constants::False);
  ASSERT_IDENTICAL(Constants::True, Constants::False < Constants::True);

  // Constant to integer comparison is simplified
  ASSERT_IDENTICAL(Constants::True, Constants::True > 0);
  ASSERT_IDENTICAL(Constants::False, Constants::True == 0);
  ASSERT_IDENTICAL(Constants::False, Constants::False < 0);
  ASSERT_IDENTICAL(Constants::True, Constants::False == 0);
  ASSERT_IDENTICAL(Constants::True, Constants::Pi > 3);
  ASSERT_IDENTICAL(Constants::True, Constants::Pi >= 3);
  ASSERT_IDENTICAL(Constants::False, Constants::Pi > 4);
  ASSERT_IDENTICAL(Constants::False, Constants::Pi == 3);
  ASSERT_IDENTICAL(Constants::True, Constants::Pi < 4);
  ASSERT_IDENTICAL(Constants::True, Constants::Euler > 2);
  ASSERT_IDENTICAL(Constants::False, Constants::Euler > 3);
  ASSERT_IDENTICAL(Constants::True, Constants::Euler < 3);
}

TEST(ScalarOperationsTest, TestConditional) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};

  ASSERT_TRUE(where(x > 0, y, z).Is<Conditional>());
  ASSERT_IDENTICAL(x, where(Constants::True, x, z));
  ASSERT_IDENTICAL(z, where(Constants::False, x, z));
  ASSERT_IDENTICAL(where(x < 0, cos(x), log(z)),
                   where(x < 0, where(x < 0, cos(x), sin(x)), log(z)));

  // both branches are identical anyway:
  ASSERT_IDENTICAL(abs(x) + z * 5, where(x - y < z, abs(x) + z * 5, abs(x) + z * 5));
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
  ASSERT_IDENTICAL(6 + 3 * x, (3 * (x + 2)).Distribute());
  ASSERT_IDENTICAL(w * x + w * y, (w * (x + y)).Distribute());
  ASSERT_IDENTICAL(x * x * y + x * x * 5 / 3_s, (x * x * (y + 5 / 3_s)).Distribute());
  ASSERT_IDENTICAL(x * log(y) - x * w, ((log(y) - w) * x).Distribute());

  // Multiple distributions:
  ASSERT_IDENTICAL(w * y + w * z + x * y + x * z, ((w + x) * (y + z)).Distribute());
  ASSERT_IDENTICAL(w * w - 16, ((w - 4) * (w + 4)).Distribute());
  ASSERT_IDENTICAL((p * w * y + p * w * z - p * x * y - p * x * z) +
                       (q * w * y + q * w * z - q * x * y - q * x * z) +
                       (v * w * y + v * w * z - v * x * y - v * x * z),
                   ((w - x) * (p + q + v) * (y + z)).Distribute());
  // Recursive distributions:
  ASSERT_IDENTICAL((2 * p * q * w * x) - (2_s * p * q * w * y) + (p * v * w * x) - (p * v * w * y),
                   (w * (x - y) * (p * (v + q * 2_s))).Distribute());

  // Distribute through relational:
  ASSERT_IDENTICAL(-8 + 2 * x + pow(x, 2) < q * x + q * y - v * x - v * y,
                   (((x + y) * (q - v)) > (x - 2) * (x + 4)).Distribute());
  ASSERT_IDENTICAL(x * y + 2 * y == sin(p), ((x + 2) * y == sin(p)).Distribute());
}

TEST(ScalarOperationsTest, TestCollect) {
  auto [x, y, z, w] = Symbols("x", "y", "z", "w");

  // No relevant terms:
  ASSERT_IDENTICAL(5, Collect(5, x));
  ASSERT_IDENTICAL(Constants::Pi + y, Collect(Constants::Pi + y, x));

  // Single term that does not need collection:
  ASSERT_IDENTICAL(x * 3, Collect(x * 3, x));
  ASSERT_IDENTICAL(pow(x, y), Collect(pow(x, y), x));

  // Collecting polynomial terms:
  ASSERT_IDENTICAL(x * (y + 2), Collect(x * y + 2 * x, x));
  ASSERT_IDENTICAL(x * (y + z) + pow(x, 2) * (5 + z),
                   Collect(x * y + x * z + pow(x, 2) * 5 + pow(x, 2) * z, x));

  // Recursive collection, w/ cancelling terms:
  // clang-format off
  auto f1 = pow(x, z) * Constants::Pi +
                 pow(x, z) * sin(y) +
                 log(x * 5 + x * y) +
                 pow(x, 2) * z +
                 pow(x, 2) * -z;
  // clang-format on
  ASSERT_IDENTICAL(pow(x, z) * (Constants::Pi + sin(y)) + log(x * (5 + y)), Collect(f1, x));

  // Many recursions for one term:
  // clang-format off
  auto f2 =
    pow(x, 2) * log(pow(x, y) * sin(z) +
    cos(y) * pow(x, y) -
    x * 5 +
    x * abs(pow(x, 3) * z + pow(x, 3) * -3 + y)) -
    pow(x, 2) * z + x * 3 +
    sin(abs(z) - x) * x +
    Constants::Pi;
  // clang-format on
  ASSERT_IDENTICAL(pow(x, 2) * (-z + log(pow(x, y) * (sin(z) + cos(y)) +
                                         x * (-5 + abs(x * x * x * (z - 3) + y)))) +
                       x * (3 + sin(abs(z) - x)) + Constants::Pi,
                   Collect(f2, x));

  // power with non-trivial exponent:
  // clang-format off
  auto f3 = pow(x, y) * pow(x, 2) * 5 +
                 pow(x, 3) * Constants::Pi +
                 pow(x, 2) * sin(y) +
                 pow(x, y + 2) * -log(z) +
                 22 +
                 pow(x, 3) * z;
  // clang-format on
  ASSERT_IDENTICAL(
      22 + pow(x, y + 2) * (5 - log(z)) + pow(x, 3) * (Constants::Pi + z) + pow(x, 2) * sin(y),
      Collect(f3, x));

  // collected base is a function:
  // clang-format off
  auto f4 = cos(x) * cos(x) * 5 +
                 cos(x) * cos(x) * (y - 17) +
                 cos(x) * sin(cos(x) * 4 -
                 cos(x) * log(z)) -
                 cos(x) * tan(z);
  // clang-format on
  ASSERT_IDENTICAL(pow(cos(x), 2) * (y - 12) + cos(x) * (sin(cos(x) * (4 - log(z))) - tan(z)),
                   Collect(f4, cos(x)));
}

TEST(ScalarOperationsTest, TestCollectMany) {
  auto [x, y, z, w] = Symbols("x", "y", "z", "w");

  // Collecting multiple variables:
  // clang-format off
  auto f1 = x * y +
                 x * -5 -
                 x * y * log(z);
  // clang-format on
  ASSERT_IDENTICAL(x * (-5 + y * (1 - log(z))), CollectMany(f1, {x, y}));
  ASSERT_IDENTICAL(y * x * (1 - log(z)) - x * 5, CollectMany(f1, {y, x}));

  // clang-format off
  auto f2 = x * x * y * y * y +
            x * x * 2 * y * y * y -
            x * x * y * y * sin(z) -
            x * x * y * y +
            x * x + x * x * 4;
  // clang-format on
  ASSERT_IDENTICAL(x * x * (y * y * y * 3 + y * y * (-sin(z) - 1) + 5), CollectMany(f2, {x, y}));
  ASSERT_IDENTICAL(y * y * y * x * x * 3 + y * y * x * x * (-sin(z) - 1) + pow(x, 2) * 5,
                   CollectMany(f2, {y, x}));

  // Collecting many functions:
  // clang-format off
  auto f3 = log(y) * z * -cos(x) * cos(x) +
                 cos(x) * cos(x) * 16 -
                 log(y) / tan(z) * cos(x) * cos(x) +
                 log(y) * (z * z) +
                 log(y) * -3;
  // clang-format on
  ASSERT_IDENTICAL(cos(x) * cos(x) * (16 + log(y) * (-z - 1 / tan(z))) + log(y) * (z * z - 3),
                   CollectMany(f3, {cos(x), log(y)}));
  ASSERT_IDENTICAL(log(y) * (pow(cos(x), 2) * (-z - 1 / tan(z)) + z * z - 3) + pow(cos(x), 2) * 16,
                   CollectMany(f3, {log(y), cos(x)}));

  // Three layers of nesting
  // clang-format off
  auto f4 =
    pow(x, 2) * (pow(y, 2) * (pow(z, 2) - 4 * z + 8) - 2 * y + 10) +
    x * (pow(y, 3) * z * (8 - sin(y)) + pow(y, 2) * z * log(x)) +
    pow(y, 2) * (pow(z, 2) * (5 + Constants::Euler) - z * 8 + 1) + y * (pow(z, 5) - 22);
  // clang-format on
  ASSERT_IDENTICAL(f4, CollectMany(f4.Distribute(), {x, y, z}));

  // Generate a polynomial in four variables:
  // Use std::function, so that we can recurse this lambda.
  std::function<Expr(absl::Span<const Expr>)> make_poly;
  make_poly = [&make_poly](absl::Span<const Expr> vars) -> Expr {
    if (vars.size() == 1) {
      return pow(vars[0], 2) + vars[0] + 1;
    } else {
      return pow(vars[0], 2) * make_poly(vars.subspan(1)) + vars[0] * make_poly(vars.subspan(1)) +
             1;
    }
  };

  auto f5 = make_poly({x, y, z, w});
  ASSERT_IDENTICAL(f5, CollectMany(f5.Distribute(), {x, y, z, w}));
}

}  // namespace math
