// Copyright 2023 Gareth Cross
#include "constants.h"
#include "expression.h"
#include "functions.h"
#include "operations.h"

#include "geometry/quaternion.h"
#include "test_helpers.h"

// Test `limits` operation.
namespace math {
using namespace custom_literals;

// Test some trivial limits.
TEST(LimitsTest, TestSimpleLimits1) {
  auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(22, limit(22, y).value());
  ASSERT_IDENTICAL(0, limit(x, x).value());
  ASSERT_IDENTICAL(-y, limit(x - y, x).value());
  ASSERT_IDENTICAL(6, limit((x - 3) * (x - 2), x).value());
  ASSERT_IDENTICAL(1, limit(cos(x), x).value());
  ASSERT_IDENTICAL(-1, limit(sin(x - Constants::Pi / 2), x).value());
}

TEST(LimitsTest, TestIndeterminateMultiplicativeForms1) {
  auto [x, y, z] = make_symbols("x", "y", "z");

  // (x^3 - x*y^2) / (x - x*y)
  auto f0 = (pow(x, 3) - x * y * y) / (x - x * y);
  ASSERT_IDENTICAL(Constants::Undefined, f0.subs(x, 0));
  ASSERT_IDENTICAL(-pow(y, 2) / (1 - y), limit(f0, x).value());

  auto f1 = sin(x) / x;
  ASSERT_IDENTICAL(Constants::Undefined, f1.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f1, x).value());

  auto f2 = sin(x) / (2 * x);
  ASSERT_IDENTICAL(Constants::Undefined, f2.subs(x, 0));
  ASSERT_IDENTICAL(1_s / 2, limit(f2, x).value());

  auto f3 = sin(x * y) / (x * z);
  ASSERT_IDENTICAL(Constants::Undefined, f3.subs(x, 0));
  ASSERT_IDENTICAL(y / z, limit(f3, x).value());
  ASSERT_IDENTICAL(pow(y / z, 2), limit(pow(f3, 2), x).value());
  ASSERT_IDENTICAL(pow(y / z, 3), limit(pow(f3, 3), x).value());
  ASSERT_IDENTICAL(z / y, limit(pow(f3, -1), x).value());

  auto f4 = (1 - cos(x)) / pow(x, 2);
  ASSERT_IDENTICAL(Constants::Undefined, f4.subs(x, 0));
  ASSERT_IDENTICAL(1_s / 2, limit(f4, x).value());

  auto f5 = x * log(x);
  ASSERT_IDENTICAL(Constants::Undefined, f5.subs(x, 0));
  ASSERT_IDENTICAL(0, limit(f5, x).value());

  auto f6 = sin(sqrt(x)) / sqrt(x);
  ASSERT_IDENTICAL(Constants::Undefined, f6.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f6, x).value());

  auto f7 = pow(x, 2) * pow(log(x), 2);
  ASSERT_IDENTICAL(0, limit(f7, x).value());

  auto f8 = sin(z * x) / tan(x * y);
  ASSERT_IDENTICAL(Constants::Undefined, f8.subs(x, 0));
  ASSERT_IDENTICAL(z / y, limit(f8, x).value());

  auto f9 = tan(3 * x) / tan(5 * x);
  ASSERT_IDENTICAL(Constants::Undefined, f9.subs(x, 0));
  ASSERT_IDENTICAL(3_s / 5, limit(f9, x).value());

  auto f10 = pow(sin(x), 2) / (x * cos(x));
  ASSERT_IDENTICAL(Constants::Undefined, f10.subs(x, 0));
  ASSERT_IDENTICAL(0, limit(f10, x).value());

  auto f11 = sin(x) * log(x);
  ASSERT_IDENTICAL(Constants::Undefined, f11.subs(x, 0));
  ASSERT_IDENTICAL(0, limit(f11, x).value());
  ASSERT_IDENTICAL(0, limit(pow(f11, 2), x).value());

  auto f12 = x * z / sqrt(x * x * z + y * y * z);
  ASSERT_IDENTICAL(0, limit(f12, z).value());
}

TEST(LimitsTest, TestIndeterminateMultiplicativeForms2) {
  auto [x, y, z] = make_symbols("x", "y", "z");

  auto f1 = tan(7 * x) / log(1 + 2 * x) + 3 * cos(x);
  ASSERT_IDENTICAL(Constants::Undefined, f1.subs(x, 0));
  ASSERT_IDENTICAL(7_s / 2 + 3, limit(f1, x).value());

  auto f2 = (sin(x) - x) / pow(x, 2);
  ASSERT_IDENTICAL(Constants::Undefined, f2.subs(x, 0));
  ASSERT_IDENTICAL(0, limit(f2, x).value());

  // exists only from the right
  auto f3 = log(3 * x * x + 1) / (2 * x * x);
  ASSERT_IDENTICAL(Constants::Undefined, f3.subs(x, 0));
  ASSERT_IDENTICAL(3_s / 2, limit(f3, x).value());

  auto f6 = log(cos(x) / 2 + 1_s / 2) / sin(x);
  ASSERT_IDENTICAL(Constants::Undefined, f6.subs(x, 0));
  ASSERT_IDENTICAL(0, limit(f6, x).value());

  auto f7 = x / acos(1 - x * 5);
  ASSERT_IDENTICAL(Constants::Undefined, f7.subs(x, 0));
  ASSERT_IDENTICAL(0, limit(f7, x).value());

  auto f8 = log(x) * tan(x);
  ASSERT_IDENTICAL(Constants::Undefined, f8.subs(x, 0));
  ASSERT_IDENTICAL(0, limit(f8, x).value());

  auto f9 = pow(x / y, 2) * pow(Constants::Euler, x) / pow(tan(z * x), 2);
  ASSERT_IDENTICAL(Constants::Undefined, f9.subs(x, 0));
  ASSERT_IDENTICAL(pow(z * y, -2), limit(f9, x).value());

  auto f10 = pow(x * log(x * x * z), 2);
  ASSERT_IDENTICAL(Constants::Undefined, f10.subs(x, 0));
  ASSERT_IDENTICAL(0, limit(f10, x).value());
}

// Test limits of the form 0^0
TEST(LimitsTest, TestIndeterminatePowerForms1) {
  auto [x, y, z] = make_symbols("x", "y", "z");

  auto f1 = pow(x, x);
  ASSERT_IDENTICAL(Constants::Undefined, f1.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f1, x).value());

  auto f2 = pow(y * x, x / z);
  ASSERT_IDENTICAL(Constants::Undefined, f2.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f2, x).value());

  auto f3 = pow(sin(x * y), x);
  ASSERT_IDENTICAL(Constants::Undefined, f3.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f3, x).value());

  auto f4 = pow(x, sin(x)) * 3;
  ASSERT_IDENTICAL(Constants::Undefined, f4.subs(x, 0));
  ASSERT_IDENTICAL(3, limit(f4, x).value());

  auto f5 = pow(x * x, x);
  ASSERT_IDENTICAL(Constants::Undefined, f5.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f5, x).value());

  auto f6 = pow(x * x, x * x);
  ASSERT_IDENTICAL(Constants::Undefined, f6.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f6, x).value());

  auto f7 = pow(cos(x + Constants::Pi / 2), log(1 + x));
  ASSERT_IDENTICAL(Constants::Undefined, f7.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f7, x).value());
}

// Test limits of the form ∞^0
TEST(LimitsTest, TestIndeterminatePowerForms2) {
  auto [x, y, z] = make_symbols("x", "y", "z");

  // Contains both 0^0 (sin(x)/x) and ∞^0 (1/x)^x
  auto f1 = pow(sin(x) / x, x);
  ASSERT_IDENTICAL(Constants::Undefined, f1.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f1, x).value());

  auto f2 = pow(log(x * y), z * x);
  ASSERT_IDENTICAL(Constants::Undefined, f2.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f2, x).value());

  auto f3 = pow(1 / x, x);
  ASSERT_IDENTICAL(Constants::Undefined, f3.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f3, x).value());

  auto f4 = pow(1 / sin(x), 4 * x * x);
  ASSERT_IDENTICAL(Constants::Undefined, f4.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f4, x).value());

  auto f5 = pow(1 / sqrt(x), -4 * x) + 2 * cos(x);
  ASSERT_IDENTICAL(Constants::Undefined, f5.subs(x, 0));
  ASSERT_IDENTICAL(3, limit(f5, x).value());

  auto f6 = pow(1 + 1 / x, x);
  ASSERT_IDENTICAL(Constants::Undefined, f6.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f6, x).value());
}

// Test limits of the form 1^∞
TEST(LimitsTest, TestIndeterminatePowerForms3) {
  auto [x, y, z] = make_symbols("x", "y", "z");

  auto f1 = pow(cos(x * y), 1 / x);
  ASSERT_IDENTICAL(Constants::Undefined, f1.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f1, x).value());

  auto f2 = pow(1 - x, log(x / z));
  ASSERT_IDENTICAL(Constants::Undefined, f2.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f2, x).value());

  auto f3 = pow(log(Constants::Euler - x), 1 / tan(x));
  ASSERT_IDENTICAL(Constants::Undefined, f3.subs(x, 0));
  ASSERT_IDENTICAL(pow(Constants::Euler, -1 / Constants::Euler), limit(f3, x).value());

  auto f4 = pow(x * x + 1, 1 / abs(x));
  ASSERT_IDENTICAL(Constants::Undefined, f4.subs(x, 0));
  ASSERT_IDENTICAL(1, limit(f4, x).value());
}

// Test limits of the form ∞ - ∞
TEST(LimitsTest, TestIndeterminateSubtractiveForms1) {
  auto [x, y, z] = make_symbols("x", "y", "z");

  auto f1 = 1 / x - 1 / log(x + 1);
  ASSERT_IDENTICAL(Constants::Undefined, f1.subs(x, 0));
  ASSERT_IDENTICAL(-1_s / 2, limit(f1, x).value());

  auto f2 = 1 / x - 1 / sin(x);
  ASSERT_IDENTICAL(Constants::Undefined, f2.subs(x, 0));
  ASSERT_IDENTICAL(0, limit(f2, x).value());
}

TEST(LimitsTest, TestInvalidForms) {
  auto [x, y, z] = make_symbols("x", "y", "z");

  // This would recurse indefinitely:
  const auto e = Constants::Euler;
  auto f1 = (pow(e, 1 / x) + pow(e, -1 / x)) / (pow(e, 1 / x) - pow(e, -1 / x));
  ASSERT_FALSE(limit(f1, x).has_value());

  // TODO: for x->0+ we _could_ evaluate this as 1?
  auto f2 = abs(x) / x;
  ASSERT_FALSE(limit(f2, x).has_value());

  // We could evaluate this if we specialized logic for tan(x)
  auto f3 = tan(x + Constants::Pi / 2) / tan(Constants::Pi / 2 - x);
  ASSERT_FALSE(limit(f3, x).has_value());

  // We don't support affine-extension of real numbers outside of limits, so these do not exist:
  ASSERT_FALSE(limit(1 / x, x).has_value());
  ASSERT_FALSE(limit(log(x), x).has_value());
}

// Test the limit function called on matrices.
TEST(LimitsTest, TestMatrixLimits) {
  auto [x, y, z, t] = make_symbols("x", "y", "z", "t");

  auto f1 = make_matrix(2, 2, sin(y * x) / (x * 3), x * log(x * y), x + 2,
                        3 * (x - 1) * sin(y * x) / (x * z));
  ASSERT_IDENTICAL(make_matrix(2, 2, y / 3, 0, 2, -3 * y / z), limit(f1, x).value());

  // Create rotation matrix and take limit as angles goes to zero:
  auto R = Quaternion::from_rotation_vector(x * t, y * t, z * t).to_rotation_matrix();
  ASSERT_IDENTICAL(make_identity(3), limit(R, t).value());

  // If one term in the matrix is invalid, the whole thing should be none:
  auto f2 = make_matrix(2, 2, 1 / x, x * x, x + 1, z);
  ASSERT_FALSE(limit(f2, x).has_value());
}

}  // namespace math
