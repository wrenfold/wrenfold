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
  ASSERT_IDENTICAL(Constants::Zero, (5_s).diff(x));
  ASSERT_IDENTICAL(Constants::Zero, (22.5_s).diff(x, 4));
  ASSERT_IDENTICAL(Constants::Zero, Constants::Pi.diff(x));
  ASSERT_IDENTICAL(Constants::Zero, Constants::Euler.diff(x));
  ASSERT_IDENTICAL(Constants::Zero, Constants::ComplexInfinity.diff(x));
  ASSERT_THROW(x.diff(5), TypeError);
  ASSERT_THROW(x.diff(Constants::Pi), TypeError);
}

TEST(DerivativesTest, TestAdditionAndSubtraction) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(Constants::Zero, (x + y).diff(w));
  ASSERT_IDENTICAL(Constants::Zero, (x - y).diff(w, 2));
  ASSERT_IDENTICAL(Constants::One, (x + y).diff(y));
  ASSERT_IDENTICAL(-1, (x - y).diff(y));
  ASSERT_IDENTICAL(-5 / 7_s, ((x - y * 5) / 7_s).diff(y));
  ASSERT_IDENTICAL(Constants::Zero, (x - y).diff(y, 2));
  ASSERT_IDENTICAL(2, (x + y + x).diff(x));
}

TEST(DerivativesTest, TestMultiplication) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(0, (x * y).diff(w));
  ASSERT_IDENTICAL(y, (x * y).diff(x));
  ASSERT_IDENTICAL(x, (x * y).diff(y));
  ASSERT_IDENTICAL(3, (x * 3).diff(x));
  ASSERT_IDENTICAL(0, (x * y).diff(x, 2));
  ASSERT_IDENTICAL(2 * x, (x * x).diff(x));
  ASSERT_IDENTICAL(2, (x * x).diff(x, 2));
  ASSERT_IDENTICAL(3 * pow(x, 2), (x * x * x).diff(x));
  ASSERT_IDENTICAL(2 * x * y, (x * y * x).diff(x));
  ASSERT_IDENTICAL(2 * y, (x * y * x).diff(x, 2));
  ASSERT_IDENTICAL(5 * pow(y, 4), pow(y, 5).diff(y));
  ASSERT_IDENTICAL(20 * pow(y, 3), pow(y, 5).diff(y, 2));
  ASSERT_IDENTICAL(1 / y, (x / y).diff(x));
  ASSERT_IDENTICAL(-y / (x * x), (y / x).diff(x));
  ASSERT_IDENTICAL(2 * y / pow(x, 3), (y / x).diff(x, 2));
  ASSERT_IDENTICAL(x * y + sin(w) * y - 3 * log(x) * pow(w, 2),
                   (w * x * y - cos(w) * y - log(x) * pow(w, 3)).diff(w));
}

TEST(DerivativesTest, TestPower) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(Constants::Zero, pow(x, y).diff(w));

  ASSERT_IDENTICAL(y * pow(x, y - 1), pow(x, y).diff(x));
  ASSERT_IDENTICAL(y * (y - 1) * pow(x, y - 2), pow(x, y).diff(x, 2));
  ASSERT_IDENTICAL(y * (y - 1) * (y - 2) * pow(x, y - 3), pow(x, y).diff(x, 3));

  ASSERT_IDENTICAL(pow(y, w) * log(y), pow(y, w).diff(w));
  ASSERT_IDENTICAL(pow(y, w) * pow(log(y), 2), pow(y, w).diff(w, 2));
  ASSERT_IDENTICAL(pow(y, w) * pow(log(y), 3), pow(y, w).diff(w, 3));
  ASSERT_IDENTICAL(pow(x, x) * log(x) + pow(x, x), pow(x, x).diff(x));

  const Expr coeff = (5 / 7_s) * pow(2, (5 / 7_s) * x) * pow(x, (5 / 7_s) * x);
  ASSERT_IDENTICAL((coeff * (1 + log(2) + log(x))).distribute(),
                   pow(x * 2, x * 5 / 7_s).diff(x).distribute());
}

TEST(DerivativesTest, TestLog) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(1 / x, log(x).diff(x));
  ASSERT_IDENTICAL(0, log(x).diff(y));
  ASSERT_IDENTICAL(1 / y, log(x * y).diff(y));
  ASSERT_IDENTICAL(1 / (2 * x), log(sqrt(x)).diff(x));
}

TEST(DerivativesTest, TestTrig) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(cos(x), sin(x).diff(x));
  ASSERT_IDENTICAL(-sin(x), cos(x).diff(x));
  ASSERT_IDENTICAL(-cos(y) * cos(cos(sin(y))) * sin(sin(y)), sin(cos(sin(y))).diff(y));
  ASSERT_IDENTICAL(y * cos(x * y), sin(x * y).diff(x));
  ASSERT_IDENTICAL(-2 * sin(2 * x + y), cos(2 * x + y).diff(x));
  ASSERT_IDENTICAL(1 / pow(cos(x), 2), tan(x).diff(x));
  ASSERT_IDENTICAL(-y / (x * x) / (cos(y / x) * cos(y / x)), tan(y / x).diff(x));
}

TEST(DerivativesTest, TestInverseTrig) {
  const Expr x{"x"};
  const Expr y{"y"};

  ASSERT_IDENTICAL(Constants::Zero, acos(5).diff(x));
  ASSERT_IDENTICAL(-1 / sqrt(1 - x * x), acos(x).diff(x));
  ASSERT_IDENTICAL(-y / sqrt(1 - x * x * y * y), acos(x * y).diff(x));

  ASSERT_IDENTICAL(Constants::Zero, asin(y).diff(x));
  ASSERT_IDENTICAL(1 / sqrt(1 - x * x), asin(x).diff(x));
  ASSERT_IDENTICAL(sqrt(x) / sqrt(1 - y * y * x), asin(y * sqrt(x)).diff(y));

  ASSERT_IDENTICAL(Constants::Zero, atan(Constants::Euler).diff(y));
  ASSERT_IDENTICAL(1_s / (x * x + 1), atan(x).diff(x));
  ASSERT_IDENTICAL(3_s * (x * x) / (pow(x, 6) + 1), atan(pow(x, 3)).diff(x));
}

TEST(DerivativesTest, TestAtan2) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_IDENTICAL(0, atan2(y, x).diff(z));
  ASSERT_IDENTICAL(x / (x * x + y * y), atan2(y, x).diff(y));
  ASSERT_IDENTICAL(-y / (x * x + y * y), atan2(y, x).diff(x));
  ASSERT_IDENTICAL(-y / (pow(log(x), 2) + y * y) * (1 / x), atan2(y, log(x)).diff(x));
  ASSERT_IDENTICAL(5 * x * -sin(y) / (pow(5 * x, 2) + pow(cos(y), 2)),
                   atan2(cos(y), 5 * x).diff(y));
  ASSERT_IDENTICAL(0, atan2(x, x).diff(x));
}

TEST(DerivativesTest, TestAbs) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(0, abs(x).diff(y));
  ASSERT_IDENTICAL(y / abs(y), abs(y).diff(y));
  ASSERT_IDENTICAL(cos(x) * sin(x) / abs(sin(x)), abs(sin(x)).diff(x));
}

TEST(DerivativesTest, TestSignum) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(0, signum(x).diff(y));
  ASSERT_IDENTICAL(Derivative::create(signum(2 * x), x, 1), signum(x * 2).diff(x));
  ASSERT_IDENTICAL(Derivative::create(signum(x), x, 2), signum(x).diff(x, 2));
}

TEST(DerivativesTest, TestMaxMin) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  ASSERT_IDENTICAL(0, max(x, y).diff(z));
  ASSERT_IDENTICAL(0, min(2 * x, y - 3).diff(z));

  ASSERT_IDENTICAL(where(x < y, 0, 1), max(x, y).diff(x));
  ASSERT_IDENTICAL(where(y < x, 0, 1), min(x, y).diff(x));

  ASSERT_IDENTICAL(where(cos(x) < sin(x), cos(x), -sin(x)), max(cos(x), sin(x)).diff(x));
}

TEST(DerivativesTest, TestMatrix) {
  // Matrix derivative should do element-wise differentiation.
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_IDENTICAL(make_vector(0, 0, 0), make_vector(x, y, 1).diff(z));
  ASSERT_IDENTICAL(make_vector(0, 1, 0), make_vector(x, y, z).diff(y));
  ASSERT_IDENTICAL(make_vector(cos(y), 2 * x, -z / pow(x, 2)),
                   make_vector(x * cos(y), y + x * x, z / x).diff(x));
}

TEST(DerivativesTest, TestRelational) {
  // Cannot diff a relational:
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(Derivative::create(x < y, x, 1), (x < y).diff(x));
  ASSERT_IDENTICAL(Derivative::create(x == y, x, 1), (x == y).diff(x));
}

TEST(DerivativesTest, TestCastBool) {
  const auto [x, y] = make_symbols("x", "y");

  // Check that this produces a `Derivative` expression:
  ASSERT_IDENTICAL(Derivative::create(cast_int_from_bool(x < y), x, 1),
                   cast_int_from_bool(x < y).diff(x));
  ASSERT_IDENTICAL(Derivative::create(Derivative::create(cast_int_from_bool(x < y), x, 1), y, 1),
                   cast_int_from_bool(x < y).diff(x).diff(y));
}

TEST(DerivativesTest, TestDerivativeExpression) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  // Create an abstract derivative expression and differentiate it.
  // Normally you wouldn't do this with a function like abs(), but it serves for a test here.
  auto f = Derivative::create(abs(x + y), x, 1);
  ASSERT_IDENTICAL(Derivative::create(abs(x + y), x, 2), f.diff(x));
  ASSERT_IDENTICAL(Derivative::create(f, y, 1), f.diff(y));
  ASSERT_IDENTICAL(0, f.diff(z));
}

}  // namespace math
