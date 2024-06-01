#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numerical_jacobian.h"
#include "wf_test_support/test_macros.h"

#define WF_SPAN_EIGEN_SUPPORT
#include "wrenfold/span_eigen.h"

#include "generated.h"

namespace wf {

using Eigen::AngleAxisd;
using Eigen::Matrix3d;
using Eigen::Quaterniond;
using Eigen::Vector;
using Eigen::Vector3d;

// 9DOF product of rotation x translation x velocity.
struct NavigationState {
  Quaterniond rotation{Quaterniond::Identity()};
  Vector3d translation{Vector3d::Zero()};
  Vector3d velocity{Vector3d::Zero()};
};

// To simplify computing derivatives numerically, we define `NavigationState` as a manifold.
// The rotational retraction is on the right tangent space.
template <>
struct manifold<NavigationState> {
  static constexpr int dimension = 9;
  using scalar_type = double;

  static Eigen::Vector<double, dimension> local_coordinates(const NavigationState& x,
                                                            const NavigationState& y) noexcept {
    return (Eigen::Vector<double, dimension>()
                << manifold<Quaterniond>::local_coordinates(x.rotation, y.rotation),
            manifold<Vector3d>::local_coordinates(x.translation, y.translation),
            manifold<Vector3d>::local_coordinates(x.velocity, y.velocity))
        .finished();
  }

  static NavigationState retract(const NavigationState& x,
                                 const Eigen::Vector<double, dimension>& dx) noexcept {
    return {manifold<Quaterniond>::retract(x.rotation, dx.head<3>()),
            manifold<Vector3d>::retract(x.translation, dx.middleRows<3>(3)),
            manifold<Vector3d>::retract(x.velocity, dx.tail<3>())};
  }
};

// Store the ground-truth stat and IMU data.
struct TestSample {
  double time{};
  NavigationState navigation_state{};
  Vector3d angular_velocity{};
  Vector3d linear_acceleration{};
};

template <typename Function>
std::vector<TestSample> generate_samples(const double duration, const double dt,
                                         const Vector3d& gyro_bias,
                                         const Vector3d& accelerometer_bias, Function&& func) {
  WF_ASSERT_GT(dt, 0);
  WF_ASSERT_GT(duration, dt);

  std::vector<TestSample> samples;
  samples.push_back(func(0.0));

  for (double t = dt; t <= duration; t += dt) {
    const double t0 = samples.back().time;
    const Vector<double, 6> measurements =
        integrate_boole(
            [&](double t) -> Vector<double, 6> {
              const TestSample sample = func(t);
              return (Vector<double, 6>() << sample.angular_velocity, sample.linear_acceleration)
                  .finished();
            },
            t0, t) /
        (t - t0);

    TestSample sample = func(t);
    sample.angular_velocity = measurements.head<3>() + gyro_bias;
    sample.linear_acceleration = measurements.tail<3>() + accelerometer_bias;
    samples.push_back(sample);
  }
  return samples;
}

// Integrate some synthetic IMU data to validate that we defined the integration correctly.
TEST(PyImuIntegrationTest, TestImuIntegration) {
  // Generate about 3 seconds of test data:
  const Vector3d gyro_bias{0.03, -0.02, 0.015};
  const Vector3d accel_bias{0.1, -0.22, 0.05};
  const std::vector<TestSample> samples =
      generate_samples(3.0, 0.001, gyro_bias, accel_bias, [](double t) {
        TestSample sample;
        gen::integration_test_sequence(
            t, sample.navigation_state.rotation, sample.navigation_state.translation,
            sample.navigation_state.velocity, sample.angular_velocity, sample.linear_acceleration);
        sample.time = t;
        return sample;
      });
  ASSERT_FALSE(samples.empty());

  auto it = samples.begin();
  const NavigationState& initial_state = it->navigation_state;

  double t = it->time;
  NavigationState pim{};
  for (++it; it != samples.end(); ++it) {
    const TestSample& gt_state = *it;

    // Update the preintegrated measurements:
    NavigationState new_pim{};
    gen::integrate_imu(pim.rotation, pim.translation, pim.velocity, gyro_bias, accel_bias,
                       gt_state.angular_velocity, gt_state.linear_acceleration, gt_state.time - t,
                       new_pim.rotation, new_pim.translation, new_pim.velocity, nullptr, nullptr,
                       nullptr);
    pim = new_pim;
    t = gt_state.time;

    // Compute the errors:
    Vector<double, 9> error{};
    gen::unweighted_imu_preintegration_error(
        initial_state.rotation, initial_state.translation, initial_state.velocity,
        gt_state.navigation_state.rotation, gt_state.navigation_state.translation,
        gt_state.navigation_state.velocity, pim.rotation, pim.translation, pim.velocity,
        gt_state.time, Vector3d{0.0, 0.0, -9.80665}, error, nullptr, nullptr, nullptr, nullptr,
        nullptr, nullptr, nullptr);

    const auto zero = Vector<double, 9>::Zero();
    ASSERT_EIGEN_NEAR(zero, error, 1.0e-6) << fmt::format("time = {} seconds", gt_state.time);
  }
}

// Test jacobians of `integrate_imu` numerically.
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
  const auto k_D_j_numerical =
      numerical_jacobian(NavigationState{i_R_j, i_p_j, i_v_j}, [&](const NavigationState& pim) {
        NavigationState pim_out{};
        gen::integrate_imu(pim.rotation, pim.translation, pim.velocity, gyro_bias, accel_bias,
                           angular_velocity, linear_acceleration, dt, pim_out.rotation,
                           pim_out.translation, pim_out.velocity, nullptr, nullptr, nullptr);
        return pim_out;
      });
  EXPECT_EIGEN_NEAR(k_D_j_numerical, k_D_j, 1.0e-12);

  const auto k_D_measurements_numerical = numerical_jacobian(
      (Vector<double, 6>() << angular_velocity, linear_acceleration).finished(),
      [&](const Vector<double, 6>& meas) {
        NavigationState pim_out{};
        gen::integrate_imu(i_R_j, i_p_j, i_v_j, gyro_bias, accel_bias, meas.head<3>(),
                           meas.tail<3>(), dt, pim_out.rotation, pim_out.translation,
                           pim_out.velocity, nullptr, nullptr, nullptr);
        return pim_out;
      });
  EXPECT_EIGEN_NEAR(k_D_measurements_numerical, k_D_measurements, 1.0e-12);
  EXPECT_EIGEN_NEAR(-k_D_measurements_numerical, k_D_bias, 1.0e-12);
}

// Test jacobians of `unweighted_imu_preintegration_error` numerically.
TEST(PyImuIntegrationTest, TestErrorJacobians) {
  const NavigationState initial_state{
      manifold<Quaterniond>::retract(Quaterniond::Identity(), Vector3d(-0.5, 0.22, 0.91)),
      Vector3d{10.2, -1.1, 3.1}, Vector3d{-2.2, 3.01, -5.0}};

  const NavigationState final_state{
      manifold<Quaterniond>::retract(Quaterniond::Identity(), Vector3d(1.2, -0.5, 0.2)),
      Vector3d{-4.0, 2.2, -1.0}, Vector3d{0.0, -0.155, 2.2}};

  const NavigationState pim_state{
      manifold<Quaterniond>::retract(Quaterniond::Identity(), Vector3d(0.044, 0.1, -0.23)),
      Vector3d{0.4, -0.2, 0.01}, Vector3d{1.0, -0.5, 0.22}};

  const double dt = 0.3;
  const Vector3d g_world{0.2, -0.3, 9.70};

  Vector<double, 9> error{};
  Eigen::Matrix<double, 9, 9> error_D_initial, error_D_final, error_D_pim;
  gen::unweighted_imu_preintegration_error(
      initial_state.rotation, initial_state.translation, initial_state.velocity,
      final_state.rotation, final_state.translation, final_state.velocity, pim_state.rotation,
      pim_state.translation, pim_state.velocity, dt, g_world, error, error_D_initial.leftCols<3>(),
      error_D_initial.middleCols<3>(3), error_D_initial.rightCols<3>(), error_D_final.leftCols<3>(),
      error_D_final.middleCols<3>(3), error_D_final.rightCols<3>(), error_D_pim);

  const auto error_D_initial_numerical =
      numerical_jacobian(initial_state, [&](const NavigationState& initial_state) {
        gen::unweighted_imu_preintegration_error(
            initial_state.rotation, initial_state.translation, initial_state.velocity,
            final_state.rotation, final_state.translation, final_state.velocity, pim_state.rotation,
            pim_state.translation, pim_state.velocity, dt, g_world, error, nullptr, nullptr,
            nullptr, nullptr, nullptr, nullptr, nullptr);
        return error;
      });
  EXPECT_EIGEN_NEAR(error_D_initial_numerical, error_D_initial, 1.0e-12);

  const auto error_D_final_numerical =
      numerical_jacobian(final_state, [&](const NavigationState& final_state) {
        gen::unweighted_imu_preintegration_error(
            initial_state.rotation, initial_state.translation, initial_state.velocity,
            final_state.rotation, final_state.translation, final_state.velocity, pim_state.rotation,
            pim_state.translation, pim_state.velocity, dt, g_world, error, nullptr, nullptr,
            nullptr, nullptr, nullptr, nullptr, nullptr);
        return error;
      });
  EXPECT_EIGEN_NEAR(error_D_final_numerical, error_D_final, 1.0e-12);

  const auto error_D_pim_numerical =
      numerical_jacobian(pim_state, [&](const NavigationState& pim_state) {
        gen::unweighted_imu_preintegration_error(
            initial_state.rotation, initial_state.translation, initial_state.velocity,
            final_state.rotation, final_state.translation, final_state.velocity, pim_state.rotation,
            pim_state.translation, pim_state.velocity, dt, g_world, error, nullptr, nullptr,
            nullptr, nullptr, nullptr, nullptr, nullptr);
        return error;
      });
  EXPECT_EIGEN_NEAR(error_D_pim_numerical, error_D_pim, 1.0e-12);
}

}  // namespace wf
