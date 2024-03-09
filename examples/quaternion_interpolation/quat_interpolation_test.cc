// Copyright 2023 Gareth Cross
#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numeric_testing.h"
#include "wf_test_support/numerical_jacobian.h"
#include "wf_test_support/test_macros.h"

#include "quat_interpolation_expressions.h"

#define MATH_SPAN_EIGEN_SUPPORT
#include "wf_runtime/span_eigen.h"

#include "generated.h"

namespace wf {

using Eigen::AngleAxisd;
using Eigen::Matrix3d;
using Eigen::Quaterniond;
using Eigen::Vector3d;

template <typename Scalar>
Eigen::Quaternion<Scalar> quat_interp(const Eigen::Quaternion<Scalar>& a,
                                      const Eigen::Quaternion<Scalar>& b, const Scalar alpha) {
  return manifold<Quaterniond>::retract(
      a, (manifold<Quaterniond>::local_coordinates(a, b) * alpha).eval());
}

auto quat_interpolation(ta::static_matrix<4, 1> q0_vec, ta::static_matrix<4, 1> q1_vec,
                        scalar_expr alpha) {
  return quaternion_interpolation(q0_vec, q1_vec, alpha, 1.0e-16);
}

// Test the quaternion interpolation result numerically.
TEST(QuaternionInterpolationTest, TestQuatInterpolation) {
  auto evaluator = create_evaluator(&quat_interpolation);

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
  EXPECT_EIGEN_NEAR(D0_eval, D0_gen, 5.0e-16);
  EXPECT_EIGEN_NEAR(D1_eval, D1_gen, 5.0e-16);

  // compare to interpolation with eigen
  EXPECT_EIGEN_NEAR(quat_interp(q0, q1, 0.25).coeffs(), q_gen.coeffs(), 2.0e-16);

  // compute derivatives numerically
  const Matrix3d D0_num = numerical_jacobian(Vector3d::Zero(), [&](const Vector3d& w) {
    return manifold<Quaterniond>::local_coordinates(
        quat_interp(q0, q1, 0.25), quat_interp(manifold<Quaterniond>::retract(q0, w), q1, 0.25));
  });
  const Matrix3d D1_num = numerical_jacobian(Vector3d::Zero(), [&](const Vector3d& w) {
    return manifold<Quaterniond>::local_coordinates(
        quat_interp(q0, q1, 0.25), quat_interp(q0, manifold<Quaterniond>::retract(q1, w), 0.25));
  });

  EXPECT_EIGEN_NEAR(D0_num, D0_gen, 1.0e-12);
  EXPECT_EIGEN_NEAR(D1_num, D1_gen, 1.0e-12);
}

}  // namespace wf
