#include "constants.h"
#include "error_types.h"
#include "expressions/derivative_expression.h"
#include "functions.h"
#include "matrix_functions.h"
#include "test_helpers.h"

// Test `diff` operation.
namespace math {
using namespace math::custom_literals;

TEST(DerivativesTest, TestConstants) {
  const Expr x{"x"};
  ASSERT_IDENTICAL(Constants::Zero, (5_s).Diff(x));
  ASSERT_IDENTICAL(Constants::Zero, (22.5_s).Diff(x, 4));
  ASSERT_IDENTICAL(Constants::Zero, Constants::Pi.Diff(x));
  ASSERT_IDENTICAL(Constants::Zero, Constants::Euler.Diff(x));
  ASSERT_IDENTICAL(Constants::Zero, Constants::Infinity.Diff(x));
  ASSERT_THROW(x.Diff(5), TypeError);
  ASSERT_THROW(x.Diff(Constants::Pi), TypeError);
}

TEST(DerivativesTest, TestAdditionAndSubtraction) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(Constants::Zero, (x + y).Diff(w));
  ASSERT_IDENTICAL(Constants::Zero, (x - y).Diff(w, 2));
  ASSERT_IDENTICAL(Constants::One, (x + y).Diff(y));
  ASSERT_IDENTICAL(-1, (x - y).Diff(y));
  ASSERT_IDENTICAL(-5 / 7_s, ((x - y * 5) / 7_s).Diff(y));
  ASSERT_IDENTICAL(Constants::Zero, (x - y).Diff(y, 2));
  ASSERT_IDENTICAL(2, (x + y + x).Diff(x));
}

TEST(DerivativesTest, TestMultiplication) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(0, (x * y).Diff(w));
  ASSERT_IDENTICAL(y, (x * y).Diff(x));
  ASSERT_IDENTICAL(x, (x * y).Diff(y));
  ASSERT_IDENTICAL(3, (x * 3).Diff(x));
  ASSERT_IDENTICAL(0, (x * y).Diff(x, 2));
  ASSERT_IDENTICAL(2 * x, (x * x).Diff(x));
  ASSERT_IDENTICAL(2, (x * x).Diff(x, 2));
  ASSERT_IDENTICAL(3 * pow(x, 2), (x * x * x).Diff(x));
  ASSERT_IDENTICAL(2 * x * y, (x * y * x).Diff(x));
  ASSERT_IDENTICAL(2 * y, (x * y * x).Diff(x, 2));
  ASSERT_IDENTICAL(5 * pow(y, 4), pow(y, 5).Diff(y));
  ASSERT_IDENTICAL(20 * pow(y, 3), pow(y, 5).Diff(y, 2));
  ASSERT_IDENTICAL(1 / y, (x / y).Diff(x));
  ASSERT_IDENTICAL(-y / (x * x), (y / x).Diff(x));
  ASSERT_IDENTICAL(2 * y / pow(x, 3), (y / x).Diff(x, 2));
  ASSERT_IDENTICAL(x * y + sin(w) * y - 3 * log(x) * pow(w, 2),
                   (w * x * y - cos(w) * y - log(x) * pow(w, 3)).Diff(w));
}

TEST(DerivativesTest, TestPower) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(Constants::Zero, pow(x, y).Diff(w));

  ASSERT_IDENTICAL(y * pow(x, y - 1), pow(x, y).Diff(x));
  ASSERT_IDENTICAL(y * (y - 1) * pow(x, y - 2), pow(x, y).Diff(x, 2));
  ASSERT_IDENTICAL(y * (y - 1) * (y - 2) * pow(x, y - 3), pow(x, y).Diff(x, 3));

  ASSERT_IDENTICAL(pow(y, w) * log(y), pow(y, w).Diff(w));
  ASSERT_IDENTICAL(pow(y, w) * pow(log(y), 2), pow(y, w).Diff(w, 2));
  ASSERT_IDENTICAL(pow(y, w) * pow(log(y), 3), pow(y, w).Diff(w, 3));
  ASSERT_IDENTICAL(pow(x, x) * log(x) + pow(x, x), pow(x, x).Diff(x));

  const Expr coeff = (5 / 7_s) * pow(2, (5 / 7_s) * x) * pow(x, (5 / 7_s) * x);
  ASSERT_IDENTICAL((coeff * (1 + log(2) + log(x))).Distribute(),
                   pow(x * 2, x * 5 / 7_s).Diff(x).Distribute());
}

TEST(DerivativesTest, TestLog) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(1 / x, log(x).Diff(x));
  ASSERT_IDENTICAL(0, log(x).Diff(y));
  ASSERT_IDENTICAL(1 / y, log(x * y).Diff(y));
  ASSERT_IDENTICAL(1 / (2 * x), log(sqrt(x)).Diff(x));
}

TEST(DerivativesTest, TestTrig) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(cos(x), sin(x).Diff(x));
  ASSERT_IDENTICAL(-sin(x), cos(x).Diff(x));
  ASSERT_IDENTICAL(-cos(y) * cos(cos(sin(y))) * sin(sin(y)), sin(cos(sin(y))).Diff(y));
  ASSERT_IDENTICAL(y * cos(x * y), sin(x * y).Diff(x));
  ASSERT_IDENTICAL(-2 * sin(2 * x + y), cos(2 * x + y).Diff(x));
  ASSERT_IDENTICAL(1 / pow(cos(x), 2), tan(x).Diff(x));
  ASSERT_IDENTICAL(-y / (x * x) / (cos(y / x) * cos(y / x)), tan(y / x).Diff(x));
}

TEST(DerivativesTest, TestInverseTrig) {
  const Expr x{"x"};
  const Expr y{"y"};

  ASSERT_IDENTICAL(Constants::Zero, acos(5).Diff(x));
  ASSERT_IDENTICAL(-1 / sqrt(1 - x * x), acos(x).Diff(x));
  ASSERT_IDENTICAL(-y / sqrt(1 - x * x * y * y), acos(x * y).Diff(x));

  ASSERT_IDENTICAL(Constants::Zero, asin(y).Diff(x));
  ASSERT_IDENTICAL(1 / sqrt(1 - x * x), asin(x).Diff(x));
  ASSERT_IDENTICAL(sqrt(x) / sqrt(1 - y * y * x), asin(y * sqrt(x)).Diff(y));

  ASSERT_IDENTICAL(Constants::Zero, atan(Constants::Euler).Diff(y));
  ASSERT_IDENTICAL(1_s / (x * x + 1), atan(x).Diff(x));
  ASSERT_IDENTICAL(3_s * (x * x) / (pow(x, 6) + 1), atan(pow(x, 3)).Diff(x));
}

TEST(DerivativesTest, TestAtan2) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_IDENTICAL(0, atan2(y, x).Diff(z));
  ASSERT_IDENTICAL(x / (x * x + y * y), atan2(y, x).Diff(y));
  ASSERT_IDENTICAL(-y / (x * x + y * y), atan2(y, x).Diff(x));
  ASSERT_IDENTICAL(-y / (pow(log(x), 2) + y * y) * (1 / x), atan2(y, log(x)).Diff(x));
  ASSERT_IDENTICAL(5 * x * -sin(y) / (pow(5 * x, 2) + pow(cos(y), 2)),
                   atan2(cos(y), 5 * x).Diff(y));
  ASSERT_IDENTICAL(0, atan2(x, x).Diff(x));
}

TEST(DerivativesTest, TestAbs) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(0, abs(x).Diff(y));
  ASSERT_IDENTICAL(y / abs(y), abs(y).Diff(y));
  ASSERT_IDENTICAL(cos(x) * sin(x) / abs(sin(x)), abs(sin(x)).Diff(x));
}

TEST(DerivativesTest, TestSignum) {
  const auto [x, y] = Symbols("x", "y");
  ASSERT_IDENTICAL(0, signum(x).Diff(y));
  ASSERT_IDENTICAL(Derivative::create(signum(x), x, 1), signum(x).Diff(x));
  ASSERT_IDENTICAL(Derivative::create(signum(x), x, 2), signum(x).Diff(x, 2));
}

TEST(DerivativesTest, TestMaxMin) {
  const auto [x, y, z] = Symbols("x", "y", "z");

  ASSERT_IDENTICAL(0, max(x, y).Diff(z));
  ASSERT_IDENTICAL(0, min(2 * x, y - 3).Diff(z));

  ASSERT_IDENTICAL(where(x < y, 0, 1), max(x, y).Diff(x));
  ASSERT_IDENTICAL(where(y < x, 0, 1), min(x, y).Diff(x));

  ASSERT_IDENTICAL(where(cos(x) < sin(x), cos(x), -sin(x)), max(cos(x), sin(x)).Diff(x));
}

TEST(DerivativesTest, TestMatrix) {
  // Matrix derivative should do element-wise differentiation.
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_IDENTICAL(Vector(0, 0, 0), Vector(x, y, 1).Diff(z));
  ASSERT_IDENTICAL(Vector(0, 1, 0), Vector(x, y, z).Diff(y));
  ASSERT_IDENTICAL(Vector(cos(y), 2 * x, -z / pow(x, 2)),
                   Vector(x * cos(y), y + x * x, z / x).Diff(x));
}

TEST(DerivativesTest, TestRelational) {
  // Cannot diff a relational:
  const auto [x, y] = Symbols("x", "y");
  ASSERT_THROW((x < y).Diff(x), TypeError);
  ASSERT_THROW((y == x).Diff(x), TypeError);
}

TEST(DerivativesTest, TestDerivativeExpression) {
  const auto [x, y, z] = Symbols("x", "y", "z");

  // Create an abstract derivative expression and differentiate it.
  // Normally you wouldn't do this with a function like abs(), but it serves for a test here.
  auto f = Derivative::create(abs(x + y), x, 1);
  ASSERT_IDENTICAL(Derivative::create(abs(x + y), x, 2), f.Diff(x));
  ASSERT_IDENTICAL(Derivative::create(f, y, 1), f.Diff(y));
  ASSERT_IDENTICAL(0, f.Diff(z));
}

}  // namespace math
