// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/utility/visit_switch.h"

#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numerical_jacobian.h"
#include "wf_test_support/test_macros.h"

#define WF_SPAN_EIGEN_SUPPORT
#include "wrenfold/span_eigen.h"

#include "bspline_numerical.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4100)  //  Disable unused parameter.
#elif defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-parameter"
#endif
#include "generated.h"
#ifdef _MSC_VER
#pragma warning(pop)
#elif defined(__clang__)
#pragma clang diagnostic pop
#endif

namespace wf {

// Map argument x from [0, 1] to the index of the corresponding interval between two knots.
std::size_t compute_basis_index(const double x, const std::size_t num_knots) {
  WF_ASSERT_GREATER_OR_EQ(x, 0.0);
  WF_ASSERT_LESS_OR_EQ(x, 1.0);

  // Find the index of the knot that is <= x.
  // Knots are equally spaced over [0, 1], and `num_knots` includes both non-repeated endpoints.
  // For example, if num_knots = 9: [0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1]
  // If you sampled at `x = 0.9` --> floor(x * (n - 1)) --> 7
  const std::size_t num_intervals = num_knots - 1;

  // min() here because the last interval is inclusive on the right side.
  const double x_interval = std::floor(x * static_cast<double>(num_intervals));
  const std::size_t interval = std::min(static_cast<std::size_t>(x_interval), num_intervals - 1);
  return interval;
}

// Given an index `interval` (between [0, num_intervals]), convert it the index into the basis
// functions of an order `order` bspline. Because we support a variable number of knots, we tile
// the central b-spline basis function over the # of knots specified at runtime. This function maps
// from the "tiled" indices to the indices of the minimum set of basis polynomials.
constexpr std::size_t determine_poly_index(const std::size_t interval,
                                           const std::size_t num_intervals,
                                           const std::size_t order) noexcept {
  if (interval < order - 1) {
    return interval;
  } else if (interval < num_intervals) {
    return order - 1;
  } else {
    return order - (num_intervals - interval);
  }
}

// A macro that takes a function, and creates a lambda that forwards any number of arguments.
// This is so we can pass generic methods to `eval_bspline_coefficients` without turning them into
// concrete function pointers.
#define MAKE_LAMBDA(func) [](auto&&... args) { return func(std::forward<decltype(args)>(args)...); }

// Compute b-spline coefficients at position `x`.
// For an order K spline, we compute a [K x (K-1)] matrix where the rows are the spline coefficients
// over the interval that contains `x`. Row `i` refers to the i'th control point. Column `j` refers
// to the j'th derivative. We return the index of the first control point required to evaluate the
// spline at `x`.
template <std::size_t Order, typename... Polynomials>
std::size_t eval_bspline_coefficients(const double x, const std::size_t num_knots,
                                      Eigen::Matrix<double, Order, Order - 1>& output_coefficients,
                                      Polynomials&&... polynomials) {
  WF_ASSERT_GREATER(num_knots, Order, "Number of knots must exceed spline order");
  const std::size_t interval = compute_basis_index(x, num_knots);

  const std::size_t num_intervals = num_knots - 1;
  const double knot_spacing = 1.0 / static_cast<double>(num_intervals);
  constexpr double base_knot_spacing = 1.0 / static_cast<double>(Order);
  const double scale_factor = base_knot_spacing / knot_spacing;

  const auto polynomial_tuple = std::make_tuple(std::forward<Polynomials>(polynomials)...);

  for (std::size_t i = 0; i < Order; ++i) {
    const std::size_t shifted_i = i + interval;
    const std::size_t polynomial_index = determine_poly_index(shifted_i, num_intervals, Order);

    // Scale and shift `x` to be in the appropriate domain for basis function `polynomial_index`.
    const double source_origin =
        static_cast<double>(std::max<int>(0, static_cast<int>(shifted_i) - Order + 1)) *
        knot_spacing;
    const double dest_origin =
        static_cast<double>(std::max<int>(0, static_cast<int>(polynomial_index) - Order + 1)) *
        base_knot_spacing;
    const double scaled_x = (x - source_origin) * scale_factor + dest_origin;

    // Invoke the appropriate basis polynomial.
    // TODO: No idea if this is a performant pattern or not. It allows me to avoid converting
    //  `polynomials` into an array of function pointers (I don't know how to decay them to a
    //  single consistent type). Instead we create `sizeof...(Polynomials)` switch cases. This means
    //  we can leave the types of `polynomials` ambiguous until the callsite. For the purpose of
    //  this example it is probably fine. In actual practice, I would probably prefer to avoid the
    //  complexity induced by supporting splines of variable order.
    detail::visit_switch<sizeof...(Polynomials)>(polynomial_index, [&](const auto n) {
      std::get<n()>(polynomial_tuple)(scaled_x, scale_factor, output_coefficients.row(i));
    });
  }
  return interval;
}

TEST(BSplineTest, TestBSplineCoeffsOrder4) {
  const auto eval4 = [&](auto&&... args) {
    return eval_bspline_coefficients<4>(
        std::forward<decltype(args)>(args)..., MAKE_LAMBDA(gen::bspline_order4_poly_0),
        MAKE_LAMBDA(gen::bspline_order4_poly_1), MAKE_LAMBDA(gen::bspline_order4_poly_2),
        MAKE_LAMBDA(gen::bspline_order4_poly_3), MAKE_LAMBDA(gen::bspline_order4_poly_4),
        MAKE_LAMBDA(gen::bspline_order4_poly_5), MAKE_LAMBDA(gen::bspline_order4_poly_6));
  };

  constexpr std::size_t num_knots = 11;
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
    const double x = (static_cast<double>(interval) + 0.5) / static_cast<double>(num_knots - 1);

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
    return eval_bspline_coefficients<4>(std::forward<decltype(args)>(args)...,
                                        MAKE_LAMBDA(gen::bspline_cumulative_order4_poly_0),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order4_poly_1),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order4_poly_2),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order4_poly_3),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order4_poly_4),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order4_poly_5),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order4_poly_6));
  };

  for (std::size_t i = 0; i < num_samples; ++i) {
    const double x = static_cast<double>(i) / static_cast<double>(num_samples - 1);

    Eigen::Matrix<double, 4, 3> evaluated{};
    const std::size_t b_index_0 = eval4_cumulative(x, num_knots, evaluated);

    for (int j = 0; j < 4; ++j) {
      ASSERT_NEAR(bspline_numerical(4, num_knots).cumulative(x, b_index_0 + j), evaluated(j, 0),
                  1.0e-12);
    }
  }

  // Check derivative:
  for (std::size_t interval = 0; interval < num_knots - 1; ++interval) {
    const double x = (static_cast<double>(interval) + 0.5) / static_cast<double>(num_knots - 1);

    Eigen::Matrix<double, 4, 3> evaluated{};
    eval4_cumulative(x, num_knots, evaluated);

    for (int d = 1; d < evaluated.cols(); ++d) {
      const Eigen::Vector4d D_numerical = numerical_derivative(0.001, [&](const double dx) {
        Eigen::Matrix<double, 4, 3> out{};
        eval4_cumulative(x + dx, num_knots, out);
        return out.col(d - 1).eval();
      });
      ASSERT_EIGEN_NEAR(D_numerical, evaluated.col(d).eval(), 1.0e-9)
          << fmt::format("d = {}, x = {}", d, x);
    }
  }
}

TEST(BSplineTest, TestBSplineCoeffsOrder6) {
  // TODO: Having to pass the flipped basis polynomials for the right side of the interval is a bit
  //  cumbersome and makes this invocation look a little ridiculous. We could improve this by simply
  //  mirroring the polynomials from the left side of the interval.
  const auto eval6 = [&](auto&&... args) {
    return eval_bspline_coefficients<6>(
        std::forward<decltype(args)>(args)..., MAKE_LAMBDA(gen::bspline_order6_poly_0),
        MAKE_LAMBDA(gen::bspline_order6_poly_1), MAKE_LAMBDA(gen::bspline_order6_poly_2),
        MAKE_LAMBDA(gen::bspline_order6_poly_3), MAKE_LAMBDA(gen::bspline_order6_poly_4),
        MAKE_LAMBDA(gen::bspline_order6_poly_5), MAKE_LAMBDA(gen::bspline_order6_poly_6),
        MAKE_LAMBDA(gen::bspline_order6_poly_7), MAKE_LAMBDA(gen::bspline_order6_poly_8),
        MAKE_LAMBDA(gen::bspline_order6_poly_9), MAKE_LAMBDA(gen::bspline_order6_poly_10));
  };

  constexpr std::size_t num_samples = 1000;
  constexpr std::size_t num_knots = 12;
  for (std::size_t i = 0; i < num_samples; ++i) {
    const double x = static_cast<double>(i) / static_cast<double>(num_samples - 1);

    Eigen::Matrix<double, 6, 5> evaluated{};
    const std::size_t b_index_0 = eval6(x, num_knots, evaluated);

    for (std::size_t j = 0; j < 6; ++j) {
      ASSERT_NEAR(bspline_numerical(6, num_knots)(x, b_index_0 + j), evaluated(j, 0), 1.0e-9);
    }

    // Sum of b-spline coefficients must one:
    ASSERT_NEAR(1.0, evaluated.col(0).sum(), 1.0e-9)
        << fmt::format("evaluated: {}", evaluated.transpose());
  }

  for (std::size_t interval = 0; interval < num_knots - 1; ++interval) {
    const double x = (static_cast<double>(interval) + 0.5) / static_cast<double>(num_knots - 1);

    Eigen::Matrix<double, 6, 5> evaluated{};
    eval6(x, num_knots, evaluated);

    for (int d = 1; d < evaluated.cols(); ++d) {
      const Eigen::Matrix<double, 6, 1> D_numerical =
          numerical_derivative(0.001, [&](const double dx) {
            Eigen::Matrix<double, 6, 5> out{};
            eval6(x + dx, num_knots, out);
            return out.col(d - 1).eval();
          });

      // High tolerance here, the higher order derivatives have very large values.
      ASSERT_EIGEN_NEAR(D_numerical, evaluated.col(d).eval(), 2.0e-6)
          << fmt::format("d = {}, x = {}", d, x);
    }
  }

  const auto eval6_cumulative = [&](auto&&... args) {
    return eval_bspline_coefficients<6>(std::forward<decltype(args)>(args)...,
                                        MAKE_LAMBDA(gen::bspline_cumulative_order6_poly_0),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order6_poly_1),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order6_poly_2),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order6_poly_3),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order6_poly_4),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order6_poly_5),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order6_poly_6),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order6_poly_7),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order6_poly_8),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order6_poly_9),
                                        MAKE_LAMBDA(gen::bspline_cumulative_order6_poly_10));
  };

  for (std::size_t i = 0; i < num_samples; ++i) {
    const double x = static_cast<double>(i) / static_cast<double>(num_samples - 1);

    Eigen::Matrix<double, 6, 5> evaluated{};
    const std::size_t b_index_0 = eval6_cumulative(x, num_knots, evaluated);

    // Compare the position term to the numerical implementation:
    for (int j = 0; j < 6; ++j) {
      ASSERT_NEAR(bspline_numerical(6, num_knots).cumulative(x, b_index_0 + j), evaluated(j, 0),
                  1.0e-9);
    }
  }

  // Check derivative:
  for (std::size_t interval = 0; interval < num_knots - 1; ++interval) {
    const double x = (static_cast<double>(interval) + 0.5) / static_cast<double>(num_knots - 1);

    Eigen::Matrix<double, 6, 5> evaluated{};
    eval6_cumulative(x, num_knots, evaluated);

    for (int d = 1; d < evaluated.cols(); ++d) {
      const auto D_numerical = numerical_derivative(0.001, [&](const double dx) {
        Eigen::Matrix<double, 6, 5> out{};
        eval6_cumulative(x + dx, num_knots, out);
        return out.col(d - 1).eval();
      });
      ASSERT_EIGEN_NEAR(D_numerical, evaluated.col(d).eval(), 1.0e-6)
          << fmt::format("d = {}, x = {}", d, x);
    }
  }
}

}  // namespace wf
