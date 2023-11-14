// Copyright 2023 Gareth Cross
#include "eigen_test_helpers.h"
#include "numeric_testing.h"
#include "numerical_jacobian.h"
#include "test_helpers.h"
#include "type_annotations.h"

#include "quat_interpolation_expressions.h"

#define MATH_SPAN_EIGEN_SUPPORT
#include "span_eigen.h"

#include "generated.h"

namespace math {

using Eigen::AngleAxisd;
using Eigen::Matrix3d;
using Eigen::Quaterniond;
using Eigen::Vector3d;

Vector3d local_coordinates(const Quaterniond& a, const Quaterniond& b) {
  const AngleAxisd w_ab{a.inverse() * b};
  return w_ab.angle() * w_ab.axis();
}

Quaterniond retract(const Quaterniond& q, const Vector3d& w) {
  return q * Quaterniond{AngleAxisd{w.norm(), w.normalized()}};
}

Quaterniond quat_interp(const Quaterniond& a, const Quaterniond& b, const double alpha) {
  return retract(a, local_coordinates(a, b) * alpha);
}

// Test the quaternion interpolation result numerically.
TEST(QuaternionInterpolationTest, TestQuatInterpolation) {
  auto evaluator = create_evaluator(&math::quaternion_interpolation);

  const Quaterniond q0 = Quaterniond{AngleAxisd(M_PI / 3, Vector3d::UnitX())} *
                         Quaterniond{AngleAxisd(M_PI / 6, Vector3d::UnitZ())};
  const Quaterniond q1{AngleAxisd(-M_PI / 4, Vector3d::UnitY())};

  Quaterniond q_eval{};
  Matrix3d D0_eval, D1_eval;
  evaluator(q0.coeffs(), q1.coeffs(), 0.25, q_eval.coeffs(), D0_eval, D1_eval);

  Quaterniond q_gen{};
  Matrix3d D0_gen{}, D1_gen{};
  gen::quaternion_interpolation(q0, q1, 0.25, q_gen, D0_gen, D1_gen);

  EXPECT_EIGEN_NEAR(q_eval.coeffs(), q_gen.coeffs(), 2.0e-16);
  EXPECT_EIGEN_NEAR(D0_eval, D0_gen, 2.0e-16);
  EXPECT_EIGEN_NEAR(D1_eval, D1_gen, 2.0e-16);

  // compare to interpolation with eigen
  EXPECT_EIGEN_NEAR(quat_interp(q0, q1, 0.25).coeffs(), q_gen.coeffs(), 2.0e-16);

  // compute derivatives numerically
  const Matrix3d D0_num = numerical_jacobian(Vector3d::Zero().eval(), [&](const Vector3d& w) {
    return local_coordinates(quat_interp(q0, q1, 0.25), quat_interp(retract(q0, w), q1, 0.25));
  });
  const Matrix3d D1_num = numerical_jacobian(Vector3d::Zero().eval(), [&](const Vector3d& w) {
    return local_coordinates(quat_interp(q0, q1, 0.25), quat_interp(q0, retract(q1, w), 0.25));
  });

  EXPECT_EIGEN_NEAR(D0_num, D0_gen, 1.0e-12);
  EXPECT_EIGEN_NEAR(D1_num, D1_gen, 1.0e-12);
}

}  // namespace math
