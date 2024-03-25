#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numerical_jacobian.h"
#include "wf_test_support/test_macros.h"

#define MATH_SPAN_EIGEN_SUPPORT
#include "wf_runtime/span_eigen.h"

#include "generated.h"

namespace wf {

using Eigen::AngleAxisd;
using Eigen::Matrix3d;
using Eigen::Quaterniond;
using Eigen::Vector3d;

TEST(PyRotationErrorTest, TestRotationError) {
  const Quaterniond q0(AngleAxisd{-0.5322, Vector3d{-0.2, 0.5, 0.3}.normalized()});
  const Quaterniond q1(AngleAxisd{1.8, Vector3d{0.8, -0.3, 0.4}.normalized()});

  Vector3d error{};
  Matrix3d D0, D1;
  gen::rotation_error(q0, q1, 2.0, error, D0, D1);
  EXPECT_EIGEN_NEAR(error, manifold<Quaterniond>::local_coordinates(q0, q1) * 2.0, 1.0e-15);

  gen::rotation_error(q0, q0, 2.0, error, nullptr, nullptr);
  EXPECT_EIGEN_NEAR(Vector3d::Zero(), error, 1.0e-16);

  gen::rotation_error(q1, q1, 2.0, error, nullptr, nullptr);
  EXPECT_EIGEN_NEAR(Vector3d::Zero(), error, 1.0e-16);

  const auto D0_num = numerical_jacobian(Vector3d::Zero(), [&](const Vector3d& w) {
    gen::rotation_error(manifold<Quaterniond>::retract(q0, w), q1, 2.0, error, nullptr, nullptr);
    return error;
  });
  EXPECT_EIGEN_NEAR(D0_num, D0, 1.0e-12);

  const auto D1_num = numerical_jacobian(Vector3d::Zero(), [&](const Vector3d& w) {
    gen::rotation_error(q0, manifold<Quaterniond>::retract(q1, w), 2.0, error, nullptr, nullptr);
    return error;
  });
  EXPECT_EIGEN_NEAR(D1_num, D1, 1.0e-12);
}

}  // namespace wf
