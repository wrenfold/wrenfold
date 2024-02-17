#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numerical_jacobian.h"
#include "wf_test_support/test_macros.h"

#define MATH_SPAN_EIGEN_SUPPORT
#include "wf_runtime/span_eigen.h"

#include "bspline_numerical.h"

#include "generated.h"

namespace wf {

struct basis_index {
  // Index into the knots of the _first_ relevant control point for the sampled interval.
  // For an order `k` spline, you would need points [i, i + 1, ..., i + k - 1]
  std::size_t index;
  // Normalized value of `x`.
  double x_normalized;
  // The scale factor used to convert `x` into `x_normalized`, which we use later to scale
  // derivatives correctly.
  double scale_factor;
};

// Find the index of the first basis function that influences points at position `x`.
// Points sampled at `x` will be a function of the bases [i, i + order).
basis_index compute_basis_index(const double x, const std::size_t order,
                                const std::size_t num_knots) {
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
  const double scale_factor = static_cast<double>(num_intervals) / static_cast<double>(order - 1);
  const double x_normalized = (x - interval_start) * scale_factor;

  return basis_index{i, x_normalized, scale_factor};
}

// Supporting method for `eval_bspline_coefficients`. We use it to generate an if-else
// sequence that checks the spline index:
//  if (i == 0) { ... }
//  else if (i == N - 1) { ... }
//  else if (i == 1) {... }
//  else if (i == N - 2) { ... }, etc...
template <typename F, std::size_t... Indices>
constexpr bool compile_time_for(F&& func, std::index_sequence<Indices...>) {
  return (func(std::integral_constant<std::size_t, Indices>{}) || ...);
}
template <std::size_t Num, typename F>
constexpr bool compile_time_for(F&& func) {
  return compile_time_for(std::forward<F>(func), std::make_index_sequence<Num>());
}

// Compute b-spline coefficients at position `x`.
// For an order K spline, we compute a [K x (K-1)] matrix where the rows are the spline coefficients
// over the interval that contains `x`. Row `i` refers to the i'th control point. Column `j` refers
// to the j'th derivative. We return the index of the first control point required to evaluate the
// spline at `x`.
template <typename... Polynomials>
std::size_t eval_bspline_coefficients(
    const double x, const std::size_t num_knots,
    Eigen::Matrix<double, sizeof...(Polynomials), sizeof...(Polynomials) - 1>& output_coefficients,
    Polynomials&&... polynomials) {
  constexpr std::size_t spline_order = sizeof...(polynomials);
  WF_ASSERT_GREATER(num_knots, spline_order, "Number of knots must exceed spline order");

  const std::size_t num_intervals = num_knots - 1;
  const basis_index bi = compute_basis_index(x, spline_order, num_knots);

  // Put the polynomial evaluators into a tuple so we can access them with a compile time index
  // in the loop below.
  // const auto callables = std::make_tuple(std::forward<Polynomials>(polynomials)...);

  using function_ptr_type = std::decay_t<type_list_front_t<type_list<Polynomials...>>>;
  const std::array<function_ptr_type, spline_order> callables{polynomials...};

  for (std::size_t i = 0; i < spline_order; ++i) {
    const std::size_t shifted_i = i + bi.index;
    if (shifted_i < spline_order) {

    }
  }

  if (bi.index < spline_order - 1) {
  } else if (bi.index > num_knots - spline_order - 2) {
  }

  // We iterate over integers [0, spline_order - 1). Each time the lambda is called, it is invoked
  // with an integral constant denoting the loop iteration. If the loop iteration matches our
  // sought-after index `i`, we get the appropriate function from `lambdas` and call it.
  const bool endpoint_interval =
      compile_time_for<spline_order - 1>([&](const auto integral_constant) {
        if constexpr (integral_constant() == spline_order) {
        } else {
        }
        if (constexpr std::size_t idx = integral_constant(); bi.index == idx) {
          std::get<idx>(callables)(bi.x_normalized, bi.scale_factor, output_coefficients);
          return true;
        } else if (bi.index == num_intervals - 1 - idx) {
          // We take an interval from the start of the spline, and flip it horizontally.
          std::get<idx>(callables)(1.0 - bi.x_normalized, -bi.scale_factor, output_coefficients);
          // Flip the order of rows (reversing the order of control points coefficients).
          output_coefficients.colwise().reverseInPlace();
          return true;
        }
        return false;
      });

  // If no iteration above found it, then this must be one of the repeated central intervals.
  // All of these are repitions of each other, so we can call `middle_interval` for all of them.
  if (!endpoint_interval) {
    middle_interval(bi.x_normalized, bi.scale_factor, output_coefficients);
  }
  return bi.index;
}

// Cumulative version of `eval_bspline_coefficients`.
// Unlike the former, this method only outputs a [(K-1) x (K-1)] matrix. This is because the
// b-spline coefficient on the first relevant control point is always 1 (and the derivatives are
// always zero). So we omit the first row in the output.
template <typename MiddleInterval, typename... EndpointIntervals>
std::size_t eval_bspline_cumulative_coefficients(
    const double x, const std::size_t num_knots,
    Eigen::Matrix<double, sizeof...(EndpointIntervals), sizeof...(EndpointIntervals)>&
        output_coefficients,
    MiddleInterval&& middle_interval, EndpointIntervals&&... intervals) {
  constexpr std::size_t spline_order = sizeof...(intervals) + 1;
  WF_ASSERT_GREATER(num_knots, spline_order, "Number of knots must exceed spline order");

  const std::size_t num_intervals = num_knots - 1;
  const basis_index bi = compute_basis_index(x, num_knots);
  const auto callables = std::make_tuple(std::forward<EndpointIntervals>(intervals)...);

  const bool endpoint_interval =
      compile_time_for<spline_order - 1>([&](const auto integral_constant) {
        if (constexpr std::size_t idx = integral_constant(); bi.index == idx) {
          std::get<idx>(callables)(bi.x_normalized, bi.scale_factor, output_coefficients);
          return true;
        } else if (bi.index == num_intervals - 1 - idx) {
          // We take an interval from the start of the spline, and flip it horizontally.
          std::get<idx>(callables)(1.0 - bi.x_normalized, -bi.scale_factor, output_coefficients);
          output_coefficients.colwise().reverseInPlace();

          // We also need to invert the positional part of the spline:
          output_coefficients.col(0).array() = 1.0 - output_coefficients.col(0).array();
          output_coefficients.template rightCols<sizeof...(EndpointIntervals) - 1>() *= -1.0;
          return true;
        }
        return false;
      });

  // If no iteration above found it, then this must be one of the repeated central intervals.
  // All of these are repitions of each other, so we can call `middle_interval` for all of them.
  if (!endpoint_interval) {
    middle_interval(bi.x_normalized, bi.scale_factor, output_coefficients);
  }
  return bi.index;
}

TEST(BSplineTest, TestBSplineCoeffsOrder4) {
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

  const auto eval4 = [&](auto&&... args) {
    return eval_bspline_coefficients(
        std::forward<decltype(args)>(args)...,
        gen::bspline_order4_interval_3<double, Eigen::Matrix<double, 4, 3>&>,
        gen::bspline_order4_interval_0<double, Eigen::Matrix<double, 4, 3>&>,
        gen::bspline_order4_interval_1<double, Eigen::Matrix<double, 4, 3>&>,
        gen::bspline_order4_interval_2<double, Eigen::Matrix<double, 4, 3>&>);
  };

  constexpr std::size_t num_samples = 1000;
  for (std::size_t i = 0; i < num_samples; ++i) {
    const double x = static_cast<double>(i) / static_cast<double>(num_samples - 1);

    Eigen::Matrix<double, 4, 3> evaluated{};
    const std::size_t b_index_0 = eval4(x, num_knots, evaluated);

    for (int j = 0; j < 4; ++j) {
      ASSERT_NEAR(bspline_numerical(4, num_knots)(x, b_index_0 + j), evaluated(j, 0), 1.0e-12);
    }

    // Sum of b-spline coefficients must one:
    ASSERT_NEAR(1.0, evaluated.col(0).sum(), 1.0e-12)
        << fmt::format("evaluated: {}", evaluated.transpose());
  }

  // Check derivative numerically:
  for (std::size_t interval = 0; interval < num_knots - 1; ++interval) {
    const double x = (static_cast<double>(interval) + 0.5) * interval_scale;

    Eigen::Matrix<double, 4, 3> evaluated{};
    eval4(x, num_knots, evaluated);

    for (int d = 1; d < evaluated.cols(); ++d) {
      const Eigen::Vector4d D_numerical = numerical_derivative(0.001, [&](const double dx) {
        Eigen::Matrix<double, 4, 3> out{};
        eval4(x + dx, num_knots, out);
        return out.col(d - 1).eval();
      });
      ASSERT_EIGEN_NEAR(D_numerical, evaluated.col(d).eval(), 1.0e-9)
          << fmt::format("d = {}, x = {}", d, x);
    }
  }

  // Check the cumulative version:
  const auto eval4_cumulative = [&](auto&&... args) {
    return eval_bspline_cumulative_coefficients(
        std::forward<decltype(args)>(args)...,
        gen::bspline_cumulative_order4_interval_3<double, Eigen::Matrix<double, 3, 3>&>,
        gen::bspline_cumulative_order4_interval_0<double, Eigen::Matrix<double, 3, 3>&>,
        gen::bspline_cumulative_order4_interval_1<double, Eigen::Matrix<double, 3, 3>&>,
        gen::bspline_cumulative_order4_interval_2<double, Eigen::Matrix<double, 3, 3>&>);
  };

  for (std::size_t i = 0; i < num_samples; ++i) {
    const double x = static_cast<double>(i) / static_cast<double>(num_samples - 1);

    Eigen::Matrix<double, 3, 3> evaluated{};
    const std::size_t b_index_0 = eval4_cumulative(x, num_knots, evaluated);

    // Compare the position term to the numerical implementation:
    // Start at j = 1 because the first coefficient is always 1.
    for (int j = 1; j < 4; ++j) {
      ASSERT_NEAR(bspline_numerical(4, num_knots).cumulative(x, b_index_0 + j), evaluated(j - 1, 0),
                  1.0e-12);
    }
  }

  // Check derivative:
  for (std::size_t interval = 0; interval < num_knots - 1; ++interval) {
    const double x = (static_cast<double>(interval) + 0.5) * interval_scale;

    Eigen::Matrix<double, 3, 3> evaluated{};
    eval4_cumulative(x, num_knots, evaluated);

    for (int d = 1; d < evaluated.cols(); ++d) {
      const Eigen::Vector3d D_numerical = numerical_derivative(0.001, [&](const double dx) {
        Eigen::Matrix<double, 3, 3> out{};
        eval4_cumulative(x + dx, num_knots, out);
        return out.col(d - 1).eval();
      });
      ASSERT_EIGEN_NEAR(D_numerical, evaluated.col(d).eval(), 1.0e-9)
          << fmt::format("d = {}, x = {}", d, x);
    }
  }
}

TEST(BSplineTest, TestBSplineCoeffsOrder7) {
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

  const auto eval7 = [&](auto&&... args) {
    return eval_bspline_coefficients(
        std::forward<decltype(args)>(args)...,
        gen::bspline_order7_interval_6<double, Eigen::Matrix<double, 7, 6>&>,
        gen::bspline_order7_interval_0<double, Eigen::Matrix<double, 7, 6>&>,
        gen::bspline_order7_interval_1<double, Eigen::Matrix<double, 7, 6>&>,
        gen::bspline_order7_interval_2<double, Eigen::Matrix<double, 7, 6>&>,
        gen::bspline_order7_interval_3<double, Eigen::Matrix<double, 7, 6>&>,
        gen::bspline_order7_interval_4<double, Eigen::Matrix<double, 7, 6>&>,
        gen::bspline_order7_interval_5<double, Eigen::Matrix<double, 7, 6>&>);
  };

  constexpr std::size_t num_samples = 1000;
  for (std::size_t i = 0; i < num_samples; ++i) {
    const double x = static_cast<double>(i) / static_cast<double>(num_samples - 1);

    Eigen::Matrix<double, 7, 6> evaluated{};
    const std::size_t b_index_0 = eval7(x, num_knots, evaluated);

    for (std::size_t j = 0; j < 7; ++j) {
      ASSERT_NEAR(bspline_numerical(7, num_knots)(x, b_index_0 + j), evaluated(j, 0), 1.0e-10);
    }

    // Sum of b-spline coefficients must one:
    ASSERT_NEAR(1.0, evaluated.col(0).sum(), 1.0e-10)
        << fmt::format("evaluated: {}", evaluated.transpose());
  }

  for (std::size_t interval = 0; interval < num_knots - 1; ++interval) {
    const double x = (static_cast<double>(interval) + 0.5) * interval_scale;

    Eigen::Matrix<double, 7, 6> evaluated{};
    eval7(x, num_knots, evaluated);

    for (int d = 1; d < evaluated.cols(); ++d) {
      const Eigen::Matrix<double, 7, 1> D_numerical =
          numerical_derivative(0.001, [&](const double dx) {
            Eigen::Matrix<double, 7, 6> out{};
            eval7(x + dx, num_knots, out);
            return out.col(d - 1).eval();
          });

      // High tolerance here, the higher order derivatives have very large values.
      ASSERT_EIGEN_NEAR(D_numerical, evaluated.col(d).eval(), 1.0e-6)
          << fmt::format("d = {}, x = {}", d, x);
    }
  }

  const auto eval7_cumulative = [&](auto&&... args) {
    return eval_bspline_cumulative_coefficients(
        std::forward<decltype(args)>(args)...,
        gen::bspline_cumulative_order7_interval_6<double, Eigen::Matrix<double, 6, 6>&>,
        gen::bspline_cumulative_order7_interval_0<double, Eigen::Matrix<double, 6, 6>&>,
        gen::bspline_cumulative_order7_interval_1<double, Eigen::Matrix<double, 6, 6>&>,
        gen::bspline_cumulative_order7_interval_2<double, Eigen::Matrix<double, 6, 6>&>,
        gen::bspline_cumulative_order7_interval_3<double, Eigen::Matrix<double, 6, 6>&>,
        gen::bspline_cumulative_order7_interval_4<double, Eigen::Matrix<double, 6, 6>&>,
        gen::bspline_cumulative_order7_interval_5<double, Eigen::Matrix<double, 6, 6>&>);
  };

  for (std::size_t i = 0; i < num_samples; ++i) {
    const double x = static_cast<double>(i) / static_cast<double>(num_samples - 1);

    Eigen::Matrix<double, 6, 6> evaluated{};
    const std::size_t b_index_0 = eval7_cumulative(x, num_knots, evaluated);

    // Compare the position term to the numerical implementation:
    // Start at j = 1 because the first coefficient is always 1.
    for (int j = 1; j < 7; ++j) {
      ASSERT_NEAR(bspline_numerical(7, num_knots).cumulative(x, b_index_0 + j), evaluated(j - 1, 0),
                  1.0e-11);
    }
  }

  // Check derivative:
  for (std::size_t interval = 0; interval < num_knots - 1; ++interval) {
    const double x = (static_cast<double>(interval) + 0.5) * interval_scale;

    Eigen::Matrix<double, 6, 6> evaluated{};
    eval7_cumulative(x, num_knots, evaluated);

    for (int d = 1; d < evaluated.cols(); ++d) {
      const auto D_numerical = numerical_derivative(0.001, [&](const double dx) {
        Eigen::Matrix<double, 6, 6> out{};
        eval7_cumulative(x + dx, num_knots, out);
        return out.col(d - 1).eval();
      });
      ASSERT_EIGEN_NEAR(D_numerical, evaluated.col(d).eval(), 1.0e-6)
          << fmt::format("d = {}, x = {}", d, x);
    }
  }
}

}  // namespace wf
