#include <cmath>
#include <limits>

#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numerical_jacobian.h"

#include "wrenfold/span.h"

#include "generated.h"

namespace wf {
namespace {

using Eigen::Matrix2d;
using Eigen::Vector2d;
using Eigen::Vector3d;

// Ceres SphereManifold coordinates use a Householder reflection whose pivot is z.
Vector3d sphere_retract(const Vector3d& x, const Vector2d& delta) {
  const double angle = delta.norm();
  if (angle == 0.0) {
    return x;
  }

  Eigen::Matrix3d householder = Eigen::Matrix3d::Identity();
  const double sigma = x.head<2>().squaredNorm();
  if (sigma <= std::numeric_limits<double>::epsilon()) {
    if (x.z() < 0.0) {
      householder(2, 2) = -1.0;
    }
  } else {
    const double pivot = x.z() <= 0.0 ? x.z() - 1.0 : -sigma / (x.z() + 1.0);
    const Vector3d v(x.x() / pivot, x.y() / pivot, 1.0);
    householder -= 2.0 / v.squaredNorm() * (v * v.transpose());
  }
  const Vector3d tangent(delta.x() * std::sin(angle) / angle, delta.y() * std::sin(angle) / angle,
                         std::cos(angle));
  return householder * tangent;
}

TEST(Unit3ErrorTest, DefaultSignature) {
  const Vector3d v0 = Vector3d(0.3, -0.4, 0.8).normalized();
  const Vector2d displacement(0.25, -0.15);
  const Vector3d v1 = sphere_retract(v0, displacement);
  constexpr double weight = 2.0;

  Vector2d error;
  Matrix2d D0, D1;
  gen::unit3_error(v0, v1, weight, error, D0, D1);
  EXPECT_EIGEN_NEAR(error, weight * displacement, 1.0e-14);

  gen::unit3_error(v0, v0, weight, error, nullptr, nullptr);
  EXPECT_EIGEN_NEAR(error, Vector2d::Zero(), 1.0e-15);

  const auto D0_num = numerical_jacobian(
      Vector2d::Zero(),
      [&](const Vector2d& delta) {
        gen::unit3_error(sphere_retract(v0, delta), v1, weight, error, nullptr, nullptr);
        return error;
      },
      1.0e-3);
  EXPECT_EIGEN_NEAR(D0_num, D0, 1.0e-11);

  const auto D1_num = numerical_jacobian(
      Vector2d::Zero(),
      [&](const Vector2d& delta) {
        gen::unit3_error(v0, sphere_retract(v1, delta), weight, error, nullptr, nullptr);
        return error;
      },
      1.0e-3);
  EXPECT_EIGEN_NEAR(D1_num, D1, 1.0e-11);
}

TEST(Unit3ErrorTest, EigenSignature) {
  const Vector3d v0 = Vector3d(-0.5, 0.2, 0.7).normalized();
  const Vector2d displacement(-0.2, 0.3);
  const Vector3d v1 = sphere_retract(v0, displacement);
  constexpr double weight = 1.5;

  Vector2d error;
  Matrix2d D0, D1;
  gen::unit3_error_eigen(v0, v1, weight, error, &D0, &D1);
  EXPECT_EIGEN_NEAR(error, weight * displacement, 1.0e-14);

  gen::unit3_error_eigen<double>(v1, v1, weight, error, nullptr, nullptr);
  EXPECT_EIGEN_NEAR(error, Vector2d::Zero(), 1.0e-15);

  const auto D0_num = numerical_jacobian(
      Vector2d::Zero(),
      [&](const Vector2d& delta) {
        gen::unit3_error_eigen<double>(sphere_retract(v0, delta), v1, weight, error, nullptr,
                                       nullptr);
        return error;
      },
      1.0e-3);
  EXPECT_EIGEN_NEAR(D0_num, D0, 1.0e-11);

  const auto D1_num = numerical_jacobian(
      Vector2d::Zero(),
      [&](const Vector2d& delta) {
        gen::unit3_error_eigen<double>(v0, sphere_retract(v1, delta), weight, error, nullptr,
                                       nullptr);
        return error;
      },
      1.0e-3);
  EXPECT_EIGEN_NEAR(D1_num, D1, 1.0e-11);
}

TEST(Unit3ErrorTest, ZeroResidualJacobians) {
  // These points have a smooth tangent frame and exactly zero local coordinates at equality.
  for (const Vector3d& x : {Vector3d(1, 0, 0), Vector3d(-1, 0, 0), Vector3d(0, 1, 0),
                            Vector3d(0.6, -0.8, 0)}) {
    SCOPED_TRACE(testing::Message() << "x = " << x.transpose());
    constexpr double weight = 2.0;
    Vector2d error;
    Matrix2d D0, D1;
    gen::unit3_error(x, x, weight, error, D0, D1);
    EXPECT_EIGEN_NEAR(error, Vector2d::Zero(), 1.0e-15);
    EXPECT_EIGEN_NEAR(D0, (-weight * Matrix2d::Identity()).eval(), 1.0e-12);
    EXPECT_EIGEN_NEAR(D1, (weight * Matrix2d::Identity()).eval(), 1.0e-12);

    const auto D0_num = numerical_jacobian(
        Vector2d::Zero(),
        [&](const Vector2d& delta) {
          gen::unit3_error(sphere_retract(x, delta), x, weight, error, nullptr, nullptr);
          return error;
        },
        1.0e-3);
    const auto D1_num = numerical_jacobian(
        Vector2d::Zero(),
        [&](const Vector2d& delta) {
          gen::unit3_error(x, sphere_retract(x, delta), weight, error, nullptr, nullptr);
          return error;
        },
        1.0e-3);
    EXPECT_EIGEN_NEAR(D0, D0_num, 1.0e-11);
    EXPECT_EIGEN_NEAR(D1, D1_num, 1.0e-11);

    gen::unit3_error_eigen<double>(x, x, weight, error, &D0, &D1);
    EXPECT_EIGEN_NEAR(error, Vector2d::Zero(), 1.0e-15);
    EXPECT_EIGEN_NEAR(D0, D0_num, 1.0e-11);
    EXPECT_EIGEN_NEAR(D1, D1_num, 1.0e-11);
  }
}

TEST(Unit3ErrorTest, RuntimeAntipode) {
  const Vector3d north = Vector3d::UnitZ();
  const Vector3d south = -north;
  const Vector2d expected(0.0, 2.0 * std::acos(-1.0));
  Vector2d error;

  gen::unit3_error(north, south, 2.0, error, nullptr, nullptr);
  EXPECT_EIGEN_NEAR(error, expected, 1.0e-15);

  gen::unit3_error_eigen<double>(north, south, 2.0, error, nullptr, nullptr);
  EXPECT_EIGEN_NEAR(error, expected, 1.0e-15);
}

}  // namespace
}  // namespace wf
