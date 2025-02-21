// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numerical_jacobian.h"

#define WF_SPAN_EIGEN_SUPPORT
#include "wrenfold/span.h"

#include "generated.h"

namespace wf {

// Test derivatives numerically.
TEST(RosenbrockTest, TestDerivatives) {
  // Test parameters:
  const double a = 2.0;
  const double b = 100.0;
  const Eigen::Vector2d xy = {0.23, -0.76};

  // Evaluate derivatives analytically:
  Eigen::Vector2d h;
  Eigen::Matrix2d h_D_xy_analytical;
  const double f = gen::rosenbrock(a, b, xy, h, &h_D_xy_analytical);
  EXPECT_NEAR(f, h.dot(h), 1.0e-12);

  // Evaluate first derivatives numerically:
  const auto h_D_xy_numerical = numerical_jacobian(xy, [a, b](const Eigen::Vector2d& xy) {
    Eigen::Vector2d h;
    gen::rosenbrock<double>(a, b, xy, h, nullptr);
    return h;
  });
  EXPECT_EIGEN_NEAR(h_D_xy_numerical, h_D_xy_analytical, 1.0e-12);

  // Check minima produces zero:
  EXPECT_EQ(0.0, gen::rosenbrock<double>(a, b, Eigen::Vector2d(a, a * a), h, nullptr));
}

// Toy optimization problem where we find the minima of the Rosenbrock function.
TEST(RosenbrockTest, TestOptimization) {
  constexpr double a = 1.2;
  constexpr double b = 100.0;
  constexpr int max_iterations = 5;

  // Our initial guess:
  Eigen::Vector2d xy{-0.78, 3.12};

  for (int iter = 0; iter < max_iterations; ++iter) {
    Eigen::Matrix2d h_D_xy;
    Eigen::Vector2d h;
    const double f = gen::rosenbrock(a, b, xy, h, &h_D_xy);
    fmt::print("iter = {}, f = {}, xy = [{}, {}]\n", iter, f, xy.x(), xy.y());

    // We have set this problem up such that f(x, y) = h(x, y)^T * h(x, y)
    // We can apply non-linear least squares on our residual function h(x, y).
    const auto llt = (h_D_xy.transpose() * h_D_xy).selfadjointView<Eigen::Lower>().llt();
    ASSERT_TRUE(Eigen::ComputationInfo::Success == llt.info());

    const Eigen::Vector2d step = -llt.solve(h_D_xy.transpose() * h);
    xy += step;
  }

  ASSERT_EIGEN_NEAR(Eigen::Vector2d(a, a * a), xy, 1.0e-12);
}

}  // namespace wf
