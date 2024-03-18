// Copyright 2024 Gareth Cross
#include "wf/constants.h"
#include "wf/error_types.h"
#include "wf/expressions/derivative_expression.h"
#include "wf/functions.h"
#include "wf/matrix_functions.h"

#include "wf_test_support/test_macros.h"

// Test `diff` operation.
namespace wf {
using namespace wf::custom_literals;

TEST(DerivativesTest, TestConstants) {
  const scalar_expr x{"x"};
  ASSERT_IDENTICAL(constants::zero, (5_s).diff(x));
  ASSERT_IDENTICAL(constants::zero, (22.5_s).diff(x, 4));
  ASSERT_IDENTICAL(constants::zero, constants::pi.diff(x));
  ASSERT_IDENTICAL(constants::zero, constants::euler.diff(x));
  ASSERT_IDENTICAL(constants::zero, constants::complex_infinity.diff(x));
  ASSERT_IDENTICAL(constants::zero, constants::imaginary_unit.diff(x));
  ASSERT_THROW(x.diff(5), type_error);
  ASSERT_THROW(x.diff(constants::pi), type_error);
  ASSERT_THROW(x.diff(constants::imaginary_unit), type_error);
}

TEST(DerivativesTest, TestAdditionAndSubtraction) {
  const scalar_expr w{"w"};
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  ASSERT_IDENTICAL(constants::zero, (x + y).diff(w));
  ASSERT_IDENTICAL(constants::zero, (x - y).diff(w, 2));
  ASSERT_IDENTICAL(constants::one, (x + y).diff(y));
  ASSERT_IDENTICAL(-1, (x - y).diff(y));
  ASSERT_IDENTICAL(-5 / 7_s, ((x - y * 5) / 7_s).diff(y));
  ASSERT_IDENTICAL(constants::zero, (x - y).diff(y, 2));
  ASSERT_IDENTICAL(2, (x + y + x).diff(x));
}

TEST(DerivativesTest, TestMultiplication) {
  const scalar_expr w{"w"};
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
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
  const scalar_expr w{"w"};
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  ASSERT_IDENTICAL(constants::zero, pow(x, y).diff(w));

  ASSERT_IDENTICAL(y * pow(x, y - 1), pow(x, y).diff(x));
  ASSERT_IDENTICAL(y * (y - 1) * pow(x, y - 2), pow(x, y).diff(x, 2));
  ASSERT_IDENTICAL(y * (y - 1) * (y - 2) * pow(x, y - 3), pow(x, y).diff(x, 3));

  ASSERT_IDENTICAL(pow(y, w) * log(y), pow(y, w).diff(w));
  ASSERT_IDENTICAL(pow(y, w) * pow(log(y), 2), pow(y, w).diff(w, 2));
  ASSERT_IDENTICAL(pow(y, w) * pow(log(y), 3), pow(y, w).diff(w, 3));
  ASSERT_IDENTICAL(pow(x, x) * log(x) + pow(x, x), pow(x, x).diff(x));
  ASSERT_IDENTICAL(
      sin(x) * pow(cos(x), sin(x) - 1) * -sin(x) + pow(cos(x), sin(x)) * log(cos(x)) * cos(x),
      pow(cos(x), sin(x)).diff(x));
}

TEST(DerivativesTest, TestLog) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  ASSERT_IDENTICAL(1 / x, log(x).diff(x));
  ASSERT_IDENTICAL(0, log(x).diff(y));
  ASSERT_IDENTICAL(1 / y, log(x * y).diff(y));
  ASSERT_IDENTICAL(1 / (2 * x), log(sqrt(x)).diff(x));
}

TEST(DerivativesTest, TestTrig) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  ASSERT_IDENTICAL(cos(x), sin(x).diff(x));
  ASSERT_IDENTICAL(-sin(x), cos(x).diff(x));
  ASSERT_IDENTICAL(-cos(y) * cos(cos(sin(y))) * sin(sin(y)), sin(cos(sin(y))).diff(y));
  ASSERT_IDENTICAL(y * cos(x * y), sin(x * y).diff(x));
  ASSERT_IDENTICAL(-2 * sin(2 * x + y), cos(2 * x + y).diff(x));
  ASSERT_IDENTICAL(1 / pow(cos(x), 2), tan(x).diff(x));
  ASSERT_IDENTICAL(-y / (x * x) / (cos(y / x) * cos(y / x)), tan(y / x).diff(x));
}

TEST(DerivativesTest, TestInverseTrig) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};

  ASSERT_IDENTICAL(constants::zero, acos(5).diff(x));
  ASSERT_IDENTICAL(-1 / sqrt(1 - x * x), acos(x).diff(x));
  ASSERT_IDENTICAL(-y / sqrt(1 - x * x * y * y), acos(x * y).diff(x));

  ASSERT_IDENTICAL(constants::zero, asin(y).diff(x));
  ASSERT_IDENTICAL(1 / sqrt(1 - x * x), asin(x).diff(x));
  ASSERT_IDENTICAL(sqrt(x) / sqrt(1 - y * y * x), asin(y * sqrt(x)).diff(y));

  ASSERT_IDENTICAL(constants::zero, atan(constants::euler).diff(y));
  ASSERT_IDENTICAL(1_s / (x * x + 1), atan(x).diff(x));
  ASSERT_IDENTICAL(3_s * (x * x) / (pow(x, 6) + 1), atan(pow(x, 3)).diff(x));
}

TEST(DerivativesTest, TestHyperbolicTrig) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(0, cosh(x).diff(y));
  ASSERT_IDENTICAL(sinh(x), cosh(x).diff(x));
  ASSERT_IDENTICAL(sinh(x * y) * y, cosh(x * y).diff(x));

  ASSERT_IDENTICAL(0, sinh(x).diff(y));
  ASSERT_IDENTICAL(cosh(x), sinh(x).diff(x));
  ASSERT_IDENTICAL(cosh(x * y) * y, sinh(x * y).diff(x));

  ASSERT_IDENTICAL(0, tanh(x).diff(y));
  ASSERT_IDENTICAL(1 - tanh(x) * tanh(x), tanh(x).diff(x));
  ASSERT_IDENTICAL((1 - tanh(x * y) * tanh(x * y)) * y, tanh(x * y).diff(x));
}

TEST(DerivativesTest, TestInverseHyperbolicTrig) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(0, acosh(x).diff(y));
  ASSERT_IDENTICAL(1 / sqrt(x - 1) * 1 / sqrt(x + 1), acosh(x).diff(x));
  ASSERT_IDENTICAL(y / sqrt(y * x - 1) * 1 / sqrt(y * x + 1), acosh(x * y).diff(x));

  ASSERT_IDENTICAL(0, asinh(x).diff(y));
  ASSERT_IDENTICAL(1 / sqrt(x * x + 1), asinh(x).diff(x));
  ASSERT_IDENTICAL(y / sqrt(x * x * y * y + 1), asinh(x * y).diff(x));

  ASSERT_IDENTICAL(0, atanh(x).diff(y));
  ASSERT_IDENTICAL(1 / (1 - x * x), atanh(x).diff(x));
  ASSERT_IDENTICAL(y / (1 - x * x * y * y), atanh(x * y).diff(x));
}

TEST(DerivativesTest, TestAtan2) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  const scalar_expr z{"z"};
  ASSERT_IDENTICAL(0, atan2(y, x).diff(z));
  ASSERT_IDENTICAL(x / (x * x + y * y), atan2(y, x).diff(y));
  ASSERT_IDENTICAL(-y / (x * x + y * y), atan2(y, x).diff(x));
  ASSERT_IDENTICAL(-y / (pow(log(x), 2) + y * y) * (1 / x), atan2(y, log(x)).diff(x));
  ASSERT_IDENTICAL(5 * x * -sin(y) / (pow(5 * x, 2) + pow(cos(y), 2)),
                   atan2(cos(y), 5 * x).diff(y));
  ASSERT_IDENTICAL(0, atan2(x, x).diff(x));
}

TEST(DerivativesTest, TestAbs) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  ASSERT_IDENTICAL(0, abs(x).diff(y));
  ASSERT_IDENTICAL(y / abs(y), abs(y).diff(y));
  ASSERT_IDENTICAL(cos(x) * sin(x) / abs(sin(x)), abs(sin(x)).diff(x));
}

TEST(DerivativesTest, TestSignum) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(0, signum(x).diff(y));

  ASSERT_IDENTICAL(0, signum(x).diff(x, 1));
  ASSERT_IDENTICAL(0, signum(x * x - y).diff(x, 1));

  ASSERT_IDENTICAL(derivative::create(signum(2 * x), x, 1),
                   signum(x * 2).diff(x, 1, non_differentiable_behavior::abstract));
  ASSERT_IDENTICAL(derivative::create(signum(x), x, 2),
                   signum(x).diff(x, 2, non_differentiable_behavior::abstract));
}

TEST(DerivativesTest, TestFloor) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(0, floor(x).diff(y));
  ASSERT_IDENTICAL(0, floor(x).diff(x));
  ASSERT_IDENTICAL(derivative::create(floor(x + y), x, 2),
                   floor(x + y).diff(x, 2, non_differentiable_behavior::abstract));
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
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  const scalar_expr z{"z"};
  ASSERT_IDENTICAL(make_vector(0, 0, 0), make_vector(x, y, 1).diff(z));
  ASSERT_IDENTICAL(make_vector(0, 1, 0), make_vector(x, y, z).diff(y));
  ASSERT_IDENTICAL(make_vector(cos(y), 2 * x, -z / pow(x, 2)),
                   make_vector(x * cos(y), y + x * x, z / x).diff(x));
}

TEST(DerivativesTest, TestRelational) {
  // Cannot diff a relational:
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(0, iverson(x < y).diff(x));
  ASSERT_IDENTICAL(0, iverson(x == y).diff(x));
  ASSERT_IDENTICAL(derivative::create(iverson(x < y), x, 1),
                   iverson(x < y).diff(x, 1, non_differentiable_behavior::abstract));
  ASSERT_IDENTICAL(derivative::create(iverson(x == y), x, 1),
                   iverson(x == y).diff(x, 1, non_differentiable_behavior::abstract));
}

TEST(DerivativesTest, TestIversonBracket) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(0, iverson(x < y).diff(x));
  ASSERT_IDENTICAL(0, iverson(x < y).diff(x).diff(y));

  // Check that this produces a `derivative` expression:
  ASSERT_IDENTICAL(derivative::create(iverson(x < y), x, 1),
                   iverson(x < y).diff(x, 1, non_differentiable_behavior::abstract));
  ASSERT_IDENTICAL(derivative::create(derivative::create(iverson(x < y), x, 1), y, 1),
                   iverson(x < y)
                       .diff(x, 1, non_differentiable_behavior::abstract)
                       .diff(y, 1, non_differentiable_behavior::abstract));
}

TEST(DerivativesTest, TestDerivativeExpression) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  // Create an abstract derivative expression and differentiate it.
  // Normally you wouldn't do this with a function like abs(), but it serves for a test here.
  auto f = derivative::create(abs(x + y), x, 1);
  ASSERT_IDENTICAL(derivative::create(abs(x + y), x, 2), f.diff(x));
  ASSERT_IDENTICAL(derivative::create(f, y, 1), f.diff(y));
  ASSERT_IDENTICAL(0, f.diff(z));
}

}  // namespace wf
