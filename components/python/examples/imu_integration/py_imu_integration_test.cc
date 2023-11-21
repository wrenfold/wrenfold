#include "eigen_test_helpers.h"
#include "numerical_jacobian.h"
#include "test_helpers.h"

#define MATH_SPAN_EIGEN_SUPPORT
#include "span_eigen.h"

#include "generated.h"

namespace math {

using Eigen::AngleAxisd;
using Eigen::Matrix3d;
using Eigen::Quaterniond;
using Eigen::Vector3d;

struct PreintegratedMeasurements {
  Quaterniond i_R_j{Quaterniond::Identity()};
  Vector3d i_p_j{Vector3d::Zero()};
  Vector3d i_v_j{Vector3d::Zero()};
};

template <>
struct manifold<PreintegratedMeasurements> {
  static constexpr int dimension = 9;
  using scalar_type = double;

  static Eigen::Vector<double, dimension> local_coordinates(
      const PreintegratedMeasurements& x, const PreintegratedMeasurements& y) noexcept {
    return (Eigen::Vector<double, dimension>()
                << manifold<Quaterniond>::local_coordinates(x.i_R_j, y.i_R_j),
            manifold<Vector3d>::local_coordinates(x.i_p_j, y.i_p_j),
            manifold<Vector3d>::local_coordinates(x.i_v_j, y.i_v_j))
        .finished();
  }

  static PreintegratedMeasurements retract(const PreintegratedMeasurements& x,
                                           const Eigen::Vector<double, dimension>& dx) noexcept {
    return {manifold<Quaterniond>::retract(x.i_R_j, dx.head<3>()),
            manifold<Vector3d>::retract(x.i_p_j, dx.middleRows<3>(3)),
            manifold<Vector3d>::retract(x.i_v_j, dx.tail<3>())};
  }
};

TEST(PyImuIntegrationTest, TestImuIntegration) {}

TEST(PyImuIntegrationTest, TestIntegrationJacobians) {
  const Quaterniond i_R_j(AngleAxisd{-0.23, Vector3d{-0.5, 0.8, 0.2}.normalized()});
  const Vector3d i_p_j{2.4, -3.1, 0.4};
  const Vector3d i_v_j{5.6, -8.1, 4.2};
  const Vector3d gyro_bias{-0.02, 0.07, 0.03};
  const Vector3d accel_bias{0.24, 0.1, -0.3};
  const Vector3d angular_velocity{0.66, -0.3, 0.9};
  const Vector3d linear_acceleration{0.8, 2.2, -7.0};
  const double dt = 0.2;

  Quaterniond i_R_k;
  Vector3d i_p_k;
  Vector3d i_v_k;
  Eigen::Matrix<double, 9, 9> k_D_j;
  Eigen::Matrix<double, 9, 6> k_D_measurements;
  Eigen::Matrix<double, 9, 6> k_D_bias;

  gen::integrate_imu(i_R_j, i_p_j, i_v_j, gyro_bias, accel_bias, angular_velocity,
                     linear_acceleration, dt, i_R_k, i_p_k, i_v_k, k_D_j, k_D_measurements,
                     k_D_bias);

  // Check Jacobians:
  const auto k_D_j_numerical = numerical_jacobian(
      PreintegratedMeasurements{i_R_j, i_p_j, i_v_j}, [&](const PreintegratedMeasurements& pim) {
        PreintegratedMeasurements pim_out{};
        gen::integrate_imu(pim.i_R_j, pim.i_p_j, pim.i_v_j, gyro_bias, accel_bias, angular_velocity,
                           linear_acceleration, dt, pim_out.i_R_j, pim_out.i_p_j, pim_out.i_v_j,
                           nullptr, nullptr, nullptr);
        return pim_out;
      });
  EXPECT_EIGEN_NEAR(k_D_j_numerical, k_D_j, 1.0e-12);
}

TEST(PyImuIntegrationTest, TestErrorJacobians) {}

}  // namespace math
