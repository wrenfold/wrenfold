// Copyright 2023 Gareth Cross
#include "constants.h"
#include "expression.h"
#include "expressions/addition.h"
#include "functions.h"
#include "geometry/quaternion.h"
#include "operations.h"

#include "test_helpers.h"

// Test `limits` operation.
namespace math {
using namespace custom_literals;

// Test some trivial limits.
TEST(LimitsTest, TestSimpleLimits1) {
  const Expr x{"x", NumberSet::RealNonNegative};
  const Expr y{"y"};
  ASSERT_IDENTICAL(22, limit(22, x).value());
  ASSERT_IDENTICAL(0, limit(x, x).value());
  ASSERT_IDENTICAL(-y, limit(x - y, x).value());
  ASSERT_IDENTICAL(6, limit((x - 3) * (x - 2), x).value());
  ASSERT_IDENTICAL(1, limit(cos(x), x).value());
  ASSERT_IDENTICAL(-1, limit(sin(x - Constants::Pi / 2), x).value());

  // Invalid argument domain:
  ASSERT_THROW(limit(23 * y, y), DomainError);
}

TEST(LimitsTest, TestIndeterminateMultiplicativeForms1) {
  const Expr x{"x", NumberSet::RealNonNegative};
  const auto [y, z] = make_symbols("y", "z");

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
}

TEST(LimitsTest, TestIndeterminateMultiplicativeForms2) {
  const Expr x{"x", NumberSet::RealNonNegative};
  const auto [y, z] = make_symbols("y", "z");

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
  const Expr x{"x", NumberSet::RealNonNegative};
  const auto [y, z] = make_symbols("y", "z");

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
  const Expr x{"x", NumberSet::RealNonNegative};
  const auto [y, z] = make_symbols("y", "z");

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
  const Expr x{"x", NumberSet::RealNonNegative};
  const auto [y, z] = make_symbols("y", "z");

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
  const Expr x{"x", NumberSet::RealNonNegative};

  auto f1 = 1 / x - 1 / log(x + 1);
  ASSERT_IDENTICAL(Constants::Undefined, f1.subs(x, 0));
  ASSERT_IDENTICAL(-1_s / 2, limit(f1, x).value());

  auto f2 = 1 / x - 1 / sin(x);
  ASSERT_IDENTICAL(Constants::Undefined, f2.subs(x, 0));
  ASSERT_IDENTICAL(0, limit(f2, x).value());
}

TEST(LimitsTest, TestInvalidForms) {
  const Expr x{"x", NumberSet::RealNonNegative};

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

  // We don't support conditionals yet:
  ASSERT_FALSE(limit(where(x > 0, sin(x), x * log(x)), x).has_value());
}

// Test the limit function called on matrices.
TEST(LimitsTest, TestMatrixLimits) {
  const Expr x{"x", NumberSet::RealNonNegative};
  auto [y, z] = make_symbols("y", "z");
  auto f1 = make_matrix(2, 2, sin(y * x) / (x * 3), x * log(x * y), x + 2,
                        3 * (x - 1) * sin(y * x) / (x * z));
  ASSERT_IDENTICAL(make_matrix(2, 2, y / 3, 0, 2, -3 * y / z), limit(f1, x).value());
}

// Test on computation of quaternion derivative:
TEST(LimitsTest, TestMatrixLimitsQuaternion) {
  const auto [x0, y0, z0] = make_symbols("x0", "y0", "z0");
  const auto [x, y, z] = make_symbols("x", "y", "z");
  const Expr t{"t", NumberSet::RealNonNegative};

  const Quaternion Q = Quaternion::from_rotation_vector(x0, y0, z0, false);

  const Quaternion Q_subbed = Q.subs(x0, x * t).subs(y0, y * t).subs(z0, z * t);
  ASSERT_IDENTICAL(make_vector(1, 0, 0, 0), limit(Q_subbed.to_vector_wxyz(), t).value());

  // Take the derivative wrt the rotation vector params:
  const MatrixExpr Q_diff_subbed =
      Q.to_vector_wxyz().jacobian({x0, y0, z0}).subs(x0, x * t).subs(y0, y * t).subs(z0, z * t);

  const MatrixExpr Q_diff_subbed_expected =
      make_matrix(4, 3, 0, 0, 0, 1 / 2_s, 0, 0, 0, 1 / 2_s, 0, 0, 0, 1 / 2_s);
  ASSERT_IDENTICAL(Q_diff_subbed_expected, limit(Q_diff_subbed, t).value());
}

// Test on computation of matrix derivative.
TEST(LimitsTest, TestMatrixLimitsRotation) {
  const auto [x0, y0, z0] = make_symbols("x0", "y0", "z0");
  const auto [x, y, z] = make_symbols("x", "y", "z");
  const Expr t{"t", NumberSet::RealNonNegative};

  const Quaternion Q = Quaternion::from_rotation_vector(x0, y0, z0, false);
  const MatrixExpr R = Q.to_rotation_matrix();

  const MatrixExpr R_subbed = R.subs(x0, x * t).subs(y0, y * t).subs(z0, z * t);
  ASSERT_IDENTICAL(make_identity(3), limit(R_subbed, t).value());

  // Do the derivative of the rotation elements wrt the rotation vector:
  const MatrixExpr R_diff =
      vectorize_matrix(R).jacobian({x0, y0, z0}).subs(x0, x * t).subs(y0, y * t).subs(z0, z * t);

  // clang-format off
  // Section 10.3.1.1 of "A tutorial on SE(3) transformation parameterizations
  //   and on-manifold optimization", J.L. Blanco
  const MatrixExpr R_diff_expected = make_matrix(9, 3,
                                                 0,  0,  0,
                                                 0,  0,  1,
                                                 0, -1,  0,
                                                 0,  0, -1,
                                                 0,  0,  0,
                                                 1,  0,  0,
                                                 0,  1,  0,
                                                -1,  0,  0,
                                                 0,  0,  0);
  // clang-format on
  ASSERT_IDENTICAL(R_diff_expected, limit(R_diff, t).value());
}

}  // namespace math
