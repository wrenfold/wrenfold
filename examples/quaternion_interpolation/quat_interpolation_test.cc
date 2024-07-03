// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numerical_jacobian.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/ostream.h>
WF_END_THIRD_PARTY_INCLUDES

#define WF_SPAN_EIGEN_SUPPORT
#include "wrenfold/span.h"

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

// Forward declare, defined below.
const std::vector<Eigen::Quaterniond>& get_test_quats();

// Test the quaternion interpolation result numerically.
TEST(QuaternionInterpolationTest, TestQuatInterpolation) {
  for (const auto& q0 : get_test_quats()) {
    for (const auto& q1 : get_test_quats()) {
      for (const double alpha : {0.0, 0.001, 0.5, 0.999, 1.0}) {
        Quaterniond q_gen{};
        Matrix3d D0_gen{}, D1_gen{};
        gen::quaternion_interpolation(q0, q1, alpha, q_gen, D0_gen, D1_gen);

        // compare to interpolation with eigen
        ASSERT_EIGEN_NEAR(quat_interp(q0, q1, alpha).coeffs(), q_gen.coeffs(), 2.0e-15)
            << fmt::format("q0 = [{}], q1 = [{}], alpha = {}", fmt::streamed(q0), fmt::streamed(q1),
                           alpha);

        // compute derivatives numerically
        const Matrix3d D0_num = numerical_jacobian(
            Vector3d::Zero(),
            [&](const Vector3d& w) {
              return manifold<Quaterniond>::local_coordinates(
                  quat_interp(q0, q1, alpha),
                  quat_interp(manifold<Quaterniond>::retract(q0, w), q1, alpha));
            },
            1.0e-5);
        const Matrix3d D1_num = numerical_jacobian(
            Vector3d::Zero(),
            [&](const Vector3d& w) {
              return manifold<Quaterniond>::local_coordinates(
                  quat_interp(q0, q1, alpha),
                  quat_interp(q0, manifold<Quaterniond>::retract(q1, w), alpha));
            },
            1.0e-5);

        ASSERT_EIGEN_NEAR(D0_num, D0_gen, 1.0e-6) << fmt::format(
            "q0 = [{}], q1 = [{}], alpha = {}", fmt::streamed(q0), fmt::streamed(q1), alpha);
        ASSERT_EIGEN_NEAR(D1_num, D1_gen, 1.0e-6) << fmt::format(
            "q0 = [{}], q1 = [{}], alpha = {}", fmt::streamed(q0), fmt::streamed(q1), alpha);
      }
    }
  }
}

// Test the version with no conditionals.
TEST(QuaternionInterpolationTest, TestQuatInterpolationNoConditional) {
  for (const auto& q0 : get_test_quats()) {
    for (const auto& q1 : get_test_quats()) {
      if ((q0.coeffs() - q1.coeffs()).cwiseAbs().array().maxCoeff() < 1.0e-16) {
        // No conditional version cannot handle this case.
        continue;
      }
      for (const double alpha : {0.001, 0.5, 0.999}) {
        Quaterniond q_gen{};
        Matrix3d D0_gen{}, D1_gen{};
        gen::quaternion_interpolation_no_conditional(q0, q1, alpha, q_gen, D0_gen, D1_gen);

        // compare to interpolation with eigen
        ASSERT_EIGEN_NEAR(quat_interp(q0, q1, alpha).coeffs(), q_gen.coeffs(), 1.0e-15);

        // compute derivatives numerically
        const Matrix3d D0_num = numerical_jacobian(
            Vector3d::Zero(),
            [&](const Vector3d& w) {
              return manifold<Quaterniond>::local_coordinates(
                  quat_interp(q0, q1, alpha),
                  quat_interp(manifold<Quaterniond>::retract(q0, w), q1, alpha));
            },
            1.0e-5);
        const Matrix3d D1_num = numerical_jacobian(
            Vector3d::Zero(),
            [&](const Vector3d& w) {
              return manifold<Quaterniond>::local_coordinates(
                  quat_interp(q0, q1, alpha),
                  quat_interp(q0, manifold<Quaterniond>::retract(q1, w), alpha));
            },
            1.0e-5);

        ASSERT_EIGEN_NEAR(D0_num, D0_gen, 1.0e-6);
        ASSERT_EIGEN_NEAR(D1_num, D1_gen, 1.0e-6);
      }
    }
  }
}

// Approximately uniformly distributed quaternions to test with:
const std::vector<Eigen::Quaterniond>& get_test_quats() {
  static const std::vector<Eigen::Quaterniond> qs = {
      {-0.57298648, -0.08750189, 0.06677656, 0.81213965},
      {-0.2370452, 0.14964947, -0.05382689, -0.95839307},
      {-0.05667896, 0.9098583, -0.40514895, 0.06927982},
      {0.36729586, -0.83911827, 0.04949433, 0.39815147},
      {0.80939134, 0.49074925, -0.18582131, 0.26366886},
      {0.07408736, -0.52878892, -0.75088736, -0.38866633},
      {-0.96280537, -0.11736205, 0.11123707, 0.21646773},
      {-0.30721295, -0.56297515, 0.75847701, -0.115723},
      {0.10692899, -0.23943148, -0.36935472, -0.89152445},
      {-0.05170143, -0.99148252, -0.09745459, -0.06922411},
      {0.02992758, 0.16170645, -0.98260312, -0.08629296},
      {0.13617515, -0.04442757, -0.40290028, -0.90396564},
      {0.57572434, -0.0157669, -0.31692404, -0.75355958},
      {0.19109932, 0.50931141, 0.53628868, 0.64535059},
      {0.89106998, -0.41739639, 0.17515614, 0.0330888},
      {-0.45677614, 0.14769249, -0.0698937, 0.87444689},
      {0.24841122, -0.31902749, -0.73918904, -0.53862129},
      {-0.21849917, 0.83212554, 0.42977397, 0.27407942},
      {-0.49591105, 0.75591709, -0.26506932, -0.33526084},
      {-0.54581159, 0.12241662, -0.58730306, -0.58496068}};
  return qs;
}

}  // namespace wf
