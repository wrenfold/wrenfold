#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numerical_jacobian.h"
#include "wf_test_support/test_macros.h"

#define MATH_SPAN_EIGEN_SUPPORT
#include "wf_runtime/span_eigen.h"

#include "generated.h"

namespace wf {

std::size_t eval_bspline_coefficients_order4(const double x, const std::size_t num_knots,
                                             Eigen::Vector4d& output_coefficients) {
  //  WF_ASSERT(output_coefficients);
  WF_ASSERT_GREATER(num_knots, 4);
  WF_ASSERT_GREATER_OR_EQ(x, 0.0);
  WF_ASSERT_LESS_OR_EQ(x, 1.0);

  // Find the index of the knot that is <= x.
  // Knots are equally spaced over [0, 1], and `num_knots` includes both non-repeated endpoints.
  // For example, if num_knots = 9: [0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1]
  // If you sampled at `x = 0.9` --> floor(x * (n - 1)) --> 7
  const std::size_t num_intervals = num_knots - 1;

  // min() here because the last interval is inclusive on the right side.
  const double x_interval = std::floor(x * static_cast<double>(num_intervals));
  const std::size_t i = std::min(static_cast<std::size_t>(x_interval), num_intervals - 1);

  // Compute the start of the interval that `x` falls in:
  const double knot_spacing = 1.0 / static_cast<double>(num_intervals);
  const double interval_start = static_cast<double>(i) * knot_spacing;

  // Shift and scale into [0, 1].
  const double x_unit = (x - interval_start) / knot_spacing;

  // TODO: Generate flipped versions of the polynomials so we don't call std::reverse()?
  if (i == 0) {
    gen::bspline_order4_interval_0(x_unit, output_coefficients, nullptr, nullptr);
  } else if (i == num_intervals - 1) {
    gen::bspline_order4_interval_0(1.0 - x_unit, output_coefficients, nullptr, nullptr);
    std::reverse(output_coefficients.begin(), output_coefficients.end());
  } else if (i == 1) {
    gen::bspline_order4_interval_1(x_unit, output_coefficients, nullptr, nullptr);
  } else if (i == num_intervals - 2) {
    gen::bspline_order4_interval_1(1.0 - x_unit, output_coefficients, nullptr, nullptr);
    std::reverse(output_coefficients.begin(), output_coefficients.end());
  } else if (i == 2) {
    gen::bspline_order4_interval_2(x_unit, output_coefficients, nullptr, nullptr);
  } else if (i == num_intervals - 3) {
    gen::bspline_order4_interval_2(1.0 - x_unit, output_coefficients, nullptr, nullptr);
    std::reverse(output_coefficients.begin(), output_coefficients.end());
  } else {
    gen::bspline_order4_interval_3(x_unit, output_coefficients, nullptr, nullptr);
  }
  return i;
}

std::size_t eval_bspline_coefficients_order7(const double x, const std::size_t num_knots,
                                             Eigen::Matrix<double, 7, 1>& output_coefficients) {
  WF_ASSERT_GREATER(num_knots, 7);
  WF_ASSERT_GREATER_OR_EQ(x, 0.0);
  WF_ASSERT_LESS_OR_EQ(x, 1.0);

  // Find the index of the knot that is <= x.
  // Knots are equally spaced over [0, 1], and `num_knots` includes both non-repeated endpoints.
  // For example, if num_knots = 9: [0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1]
  // If you sampled at `x = 0.9` --> floor(x * (n - 1)) --> 7
  const std::size_t num_intervals = num_knots - 1;

  // min() here because the last interval is inclusive on the right side.
  const double x_interval = std::floor(x * static_cast<double>(num_intervals));
  const std::size_t i = std::min(static_cast<std::size_t>(x_interval), num_intervals - 1);

  // Compute the start of the interval that `x` falls in:
  const double knot_spacing = 1.0 / static_cast<double>(num_intervals);
  const double interval_start = static_cast<double>(i) * knot_spacing;

  // Shift and scale into [0, 1].
  const double x_unit = (x - interval_start) / knot_spacing;

  // TODO: Generate flipped versions of the polynomials so we don't call std::reverse()?
  if (i == 0) {
    gen::bspline_order7_interval_0(x_unit, output_coefficients, nullptr, nullptr);
  } else if (i == num_intervals - 1) {
    gen::bspline_order7_interval_0(1.0 - x_unit, output_coefficients, nullptr, nullptr);
    std::reverse(output_coefficients.begin(), output_coefficients.end());
  } else if (i == 1) {
    gen::bspline_order7_interval_1(x_unit, output_coefficients, nullptr, nullptr);
  } else if (i == num_intervals - 2) {
    gen::bspline_order7_interval_1(1.0 - x_unit, output_coefficients, nullptr, nullptr);
    std::reverse(output_coefficients.begin(), output_coefficients.end());
  } else if (i == 2) {
    gen::bspline_order7_interval_2(x_unit, output_coefficients, nullptr, nullptr);
  } else if (i == num_intervals - 3) {
    gen::bspline_order7_interval_2(1.0 - x_unit, output_coefficients, nullptr, nullptr);
    std::reverse(output_coefficients.begin(), output_coefficients.end());
  } else if (i == 3) {
    gen::bspline_order7_interval_3(x_unit, output_coefficients, nullptr, nullptr);
  } else if (i == num_intervals - 4) {
    gen::bspline_order7_interval_3(1.0 - x_unit, output_coefficients, nullptr, nullptr);
    std::reverse(output_coefficients.begin(), output_coefficients.end());
  } else if (i == 4) {
    gen::bspline_order7_interval_4(x_unit, output_coefficients, nullptr, nullptr);
  } else if (i == num_intervals - 5) {
    gen::bspline_order7_interval_4(1.0 - x_unit, output_coefficients, nullptr, nullptr);
    std::reverse(output_coefficients.begin(), output_coefficients.end());
  } else if (i == 5) {
    gen::bspline_order7_interval_5(x_unit, output_coefficients, nullptr, nullptr);
  } else if (i == num_intervals - 6) {
    gen::bspline_order7_interval_5(1.0 - x_unit, output_coefficients, nullptr, nullptr);
    std::reverse(output_coefficients.begin(), output_coefficients.end());
  } else {
    gen::bspline_order7_interval_6(x_unit, output_coefficients, nullptr, nullptr);
  }
  return i;
}

TEST(PyBSplineTest, TestBSplineCoeffsOrder4) {
  const std::size_t num_knots = 11;
  const std::size_t num_samples = 1000;

  // Ensure we have continuity at the knots:
  Eigen::Vector4d b_prev, b_next;
  gen::bspline_order4_interval_0(1.0, b_prev, nullptr, nullptr);
  gen::bspline_order4_interval_1(0.0, b_next, nullptr, nullptr);
  ASSERT_EIGEN_NEAR(b_prev.tail(3).eval(), b_next.head(3).eval(), 1.0e-12);

  gen::bspline_order4_interval_1(1.0, b_prev, nullptr, nullptr);
  gen::bspline_order4_interval_2(0.0, b_next, nullptr, nullptr);
  ASSERT_EIGEN_NEAR(b_prev.tail(3).eval(), b_next.head(3).eval(), 1.0e-12);

  gen::bspline_order4_interval_2(1.0, b_prev, nullptr, nullptr);
  gen::bspline_order4_interval_3(0.0, b_next, nullptr, nullptr);
  ASSERT_EIGEN_NEAR(b_prev.tail(3).eval(), b_next.head(3).eval(), 1.0e-12);

  for (std::size_t i = 0; i < num_samples; ++i) {
    const double x = static_cast<double>(i) / static_cast<double>(num_samples - 1);

    Eigen::Vector4d evaluated{};
    eval_bspline_coefficients_order4(x, num_knots, evaluated);

    // Sum of b-spline coefficients must one:
    ASSERT_NEAR(1.0, evaluated.sum(), 1.0e-12)
        << fmt::format("evaluated: {}", evaluated.transpose());
  }
}

TEST(PyBSplineTest, TestBSplineCoeffsOrder7) {
  const std::size_t num_knots = 11;
  const std::size_t num_samples = 1000;

  for (std::size_t i = 0; i < num_samples; ++i) {
    const double x = static_cast<double>(i) / static_cast<double>(num_samples - 1);

    Eigen::Matrix<double, 7, 1> evaluated{};
    eval_bspline_coefficients_order7(x, num_knots, evaluated);

    // Sum of b-spline coefficients must one:
    ASSERT_NEAR(1.0, evaluated.sum(), 1.0e-10)
        << fmt::format("evaluated: {}", evaluated.transpose());
  }
}

}  // namespace wf
