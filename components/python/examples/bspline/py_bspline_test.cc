#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numerical_jacobian.h"
#include "wf_test_support/test_macros.h"

#define MATH_SPAN_EIGEN_SUPPORT
#include "wf_runtime/span_eigen.h"

#include "generated.h"

namespace wf {

// Find the index of the first basis function that influences points at position `x`.
// Points sampled at `x` will be a function of the bases [i, i + order).
auto compute_basis_index(const double x, const std::size_t num_knots) {
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
  const double scale_factor = 1.0 / knot_spacing;
  const double x_unit = (x - interval_start) / knot_spacing;

  return std::make_tuple(i, x_unit, scale_factor);
}

std::size_t eval_bspline_coefficients_order4(const double x, const std::size_t num_knots,
                                             Eigen::Matrix<double, 4, 3>& output_coefficients) {
  WF_ASSERT_GREATER(num_knots, 4);

  const std::size_t num_intervals = num_knots - 1;
  const auto [i, x_unit, scale_factor] = compute_basis_index(x, num_knots);

  // TODO: Generate flipped versions of the polynomials so we don't call reverseInPlace()?
  if (i == 0) {
    gen::bspline_order4_interval_0(x_unit, scale_factor, output_coefficients);
  } else if (i == num_intervals - 1) {
    gen::bspline_order4_interval_0(1.0 - x_unit, -scale_factor, output_coefficients);
    output_coefficients.colwise().reverseInPlace();
  } else if (i == 1) {
    gen::bspline_order4_interval_1(x_unit, scale_factor, output_coefficients);
  } else if (i == num_intervals - 2) {
    gen::bspline_order4_interval_1(1.0 - x_unit, -scale_factor, output_coefficients);
    output_coefficients.colwise().reverseInPlace();
  } else if (i == 2) {
    gen::bspline_order4_interval_2(x_unit, scale_factor, output_coefficients);
  } else if (i == num_intervals - 3) {
    gen::bspline_order4_interval_2(1.0 - x_unit, -scale_factor, output_coefficients);
    output_coefficients.colwise().reverseInPlace();
  } else {
    gen::bspline_order4_interval_3(x_unit, scale_factor, output_coefficients);
  }
  return i;
}

std::size_t eval_bspline_coefficients_order7(const double x, const std::size_t num_knots,
                                             Eigen::Matrix<double, 7, 6>& output_coefficients) {
  WF_ASSERT_GREATER(num_knots, 7);

  const std::size_t num_intervals = num_knots - 1;
  const auto [i, x_unit, scale_factor] = compute_basis_index(x, num_knots);

  // TODO: Generate flipped versions of the polynomials so we don't call reverseInPlace()?
  if (i == 0) {
    gen::bspline_order7_interval_0(x_unit, scale_factor, output_coefficients);
  } else if (i == num_intervals - 1) {
    gen::bspline_order7_interval_0(1.0 - x_unit, -scale_factor, output_coefficients);
    output_coefficients.colwise().reverseInPlace();
  } else if (i == 1) {
    gen::bspline_order7_interval_1(x_unit, scale_factor, output_coefficients);
  } else if (i == num_intervals - 2) {
    gen::bspline_order7_interval_1(1.0 - x_unit, -scale_factor, output_coefficients);
    output_coefficients.colwise().reverseInPlace();
  } else if (i == 2) {
    gen::bspline_order7_interval_2(x_unit, scale_factor, output_coefficients);
  } else if (i == num_intervals - 3) {
    gen::bspline_order7_interval_2(1.0 - x_unit, -scale_factor, output_coefficients);
    output_coefficients.colwise().reverseInPlace();
  } else if (i == 3) {
    gen::bspline_order7_interval_3(x_unit, scale_factor, output_coefficients);
  } else if (i == num_intervals - 4) {
    gen::bspline_order7_interval_3(1.0 - x_unit, -scale_factor, output_coefficients);
    output_coefficients.colwise().reverseInPlace();
  } else if (i == 4) {
    gen::bspline_order7_interval_4(x_unit, scale_factor, output_coefficients);
  } else if (i == num_intervals - 5) {
    gen::bspline_order7_interval_4(1.0 - x_unit, -scale_factor, output_coefficients);
    output_coefficients.colwise().reverseInPlace();
  } else if (i == 5) {
    gen::bspline_order7_interval_5(x_unit, scale_factor, output_coefficients);
  } else if (i == num_intervals - 6) {
    gen::bspline_order7_interval_5(1.0 - x_unit, -scale_factor, output_coefficients);
    output_coefficients.colwise().reverseInPlace();
  } else {
    gen::bspline_order7_interval_6(x_unit, scale_factor, output_coefficients);
  }
  return i;
}

TEST(PyBSplineTest, TestBSplineCoeffsOrder4) {
  constexpr std::size_t num_knots = 11;
  constexpr double interval_scale = 1.0 / static_cast<double>(num_knots - 1);

  // Ensure we have continuity at the knots:
  Eigen::Matrix<double, 4, 3> b_prev, b_next;
  gen::bspline_order4_interval_0(1.0, interval_scale, b_prev);
  gen::bspline_order4_interval_1(0.0, interval_scale, b_next);
  ASSERT_EIGEN_NEAR(b_prev.bottomRows(3).eval(), b_next.topRows(3).eval(), 1.0e-12);

  gen::bspline_order4_interval_1(1.0, interval_scale, b_prev);
  gen::bspline_order4_interval_2(0.0, interval_scale, b_next);
  ASSERT_EIGEN_NEAR(b_prev.bottomRows(3).eval(), b_next.topRows(3).eval(), 1.0e-12);

  gen::bspline_order4_interval_2(1.0, interval_scale, b_prev);
  gen::bspline_order4_interval_3(0.0, interval_scale, b_next);
  ASSERT_EIGEN_NEAR(b_prev.bottomRows(3).eval(), b_next.topRows(3).eval(), 1.0e-12);

  constexpr std::size_t num_samples = 1000;
  for (std::size_t i = 0; i < num_samples; ++i) {
    const double x = static_cast<double>(i) / static_cast<double>(num_samples - 1);

    Eigen::Matrix<double, 4, 3> evaluated{};
    eval_bspline_coefficients_order4(x, num_knots, evaluated);

    // Sum of b-spline coefficients must one:
    ASSERT_NEAR(1.0, evaluated.col(0).sum(), 1.0e-12)
        << fmt::format("evaluated: {}", evaluated.transpose());
  }

  // Check derivative numerically:
  for (std::size_t interval = 0; interval < num_knots - 1; ++interval) {
    const double x = (static_cast<double>(interval) + 0.5) * interval_scale;

    Eigen::Matrix<double, 4, 3> evaluated{};
    eval_bspline_coefficients_order4(x, num_knots, evaluated);

    for (int d = 1; d < evaluated.cols(); ++d) {
      const Eigen::Vector4d D_numerical = numerical_derivative(0.001, [&](const double dx) {
        Eigen::Matrix<double, 4, 3> out{};
        eval_bspline_coefficients_order4(x + dx, num_knots, out);
        return out.col(d - 1).eval();
      });
      ASSERT_EIGEN_NEAR(D_numerical, evaluated.col(d).eval(), 1.0e-9)
          << fmt::format("d = {}, x = {}", d, x);
    }
  }
}

TEST(PyBSplineTest, TestBSplineCoeffsOrder7) {
  constexpr std::size_t num_knots = 11;
  constexpr double interval_scale = 1.0 / static_cast<double>(num_knots - 1);

  Eigen::Matrix<double, 7, 6> b_prev, b_next;
  gen::bspline_order7_interval_0(1.0, interval_scale, b_prev);
  gen::bspline_order7_interval_1(0.0, interval_scale, b_next);
  ASSERT_EIGEN_NEAR(b_prev.bottomRows(6).eval(), b_next.topRows(6).eval(), 1.0e-12);

  gen::bspline_order7_interval_1(1.0, interval_scale, b_prev);
  gen::bspline_order7_interval_2(0.0, interval_scale, b_next);
  ASSERT_EIGEN_NEAR(b_prev.bottomRows(6).eval(), b_next.topRows(6).eval(), 1.0e-12);

  gen::bspline_order7_interval_2(1.0, interval_scale, b_prev);
  gen::bspline_order7_interval_3(0.0, interval_scale, b_next);
  ASSERT_EIGEN_NEAR(b_prev.bottomRows(6).eval(), b_next.topRows(6).eval(), 1.0e-12);

  gen::bspline_order7_interval_3(1.0, interval_scale, b_prev);
  gen::bspline_order7_interval_4(0.0, interval_scale, b_next);
  ASSERT_EIGEN_NEAR(b_prev.bottomRows(6).eval(), b_next.topRows(6).eval(), 1.0e-12);

  gen::bspline_order7_interval_4(1.0, interval_scale, b_prev);
  gen::bspline_order7_interval_5(0.0, interval_scale, b_next);
  ASSERT_EIGEN_NEAR(b_prev.bottomRows(6).eval(), b_next.topRows(6).eval(), 1.0e-10);

  gen::bspline_order7_interval_5(1.0, interval_scale, b_prev);
  gen::bspline_order7_interval_6(0.0, interval_scale, b_next);
  ASSERT_EIGEN_NEAR(b_prev.bottomRows(6).eval(), b_next.topRows(6).eval(), 1.0e-10);

  constexpr std::size_t num_samples = 1000;
  for (std::size_t i = 0; i < num_samples; ++i) {
    const double x = static_cast<double>(i) / static_cast<double>(num_samples - 1);

    Eigen::Matrix<double, 7, 6> evaluated{};
    eval_bspline_coefficients_order7(x, num_knots, evaluated);

    // Sum of b-spline coefficients must one:
    ASSERT_NEAR(1.0, evaluated.col(0).sum(), 1.0e-10)
        << fmt::format("evaluated: {}", evaluated.transpose());
  }

  for (std::size_t interval = 0; interval < num_knots - 1; ++interval) {
    const double x = (static_cast<double>(interval) + 0.5) * interval_scale;

    Eigen::Matrix<double, 7, 6> evaluated{};
    eval_bspline_coefficients_order7(x, num_knots, evaluated);

    for (int d = 1; d < evaluated.cols(); ++d) {
      const Eigen::Matrix<double, 7, 1> D_numerical =
          numerical_derivative(0.001, [&](const double dx) {
            Eigen::Matrix<double, 7, 6> out{};
            eval_bspline_coefficients_order7(x + dx, num_knots, out);
            return out.col(d - 1).eval();
          });

      // High tolerance here, the higher order derivatives have very large values.
      ASSERT_EIGEN_NEAR(D_numerical, evaluated.col(d).eval(), 1.0e-6)
          << fmt::format("d = {}, x = {}", d, x);
    }
  }
}

}  // namespace wf
