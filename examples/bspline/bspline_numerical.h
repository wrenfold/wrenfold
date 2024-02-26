// Copyright 2023 Gareth Cross
#pragma once
#include <cstddef>

namespace wf {

// Numerical implementation of the b-spline basis functions.
// Used to test the code-generated equivalent.
class bspline_numerical {
 public:
  constexpr bspline_numerical(std::size_t order, std::size_t num_knots) noexcept
      : order_(order), num_knots_(num_knots) {}

  // Number of knots with repetitions.
  constexpr std::size_t num_extended_knots() const noexcept {
    return num_knots_ + 2 * (order_ - 1);
  }

  // The spacing between knots.
  constexpr double knot_spacing() const noexcept {
    return 1.0 / static_cast<double>(num_knots_ - 1);
  }

  // Position of the i'th knot on the x-axis. Includes repeated knots.
  constexpr double knot_value(std::size_t i) const noexcept {
    if (i < order_ - 1) {
      return 0.0;
    } else if (i > num_extended_knots() - order_ - 1) {
      return 1.0;
    }
    return ((i + 1) - order_) * knot_spacing();
  }

  // Linear interpolation function.
  constexpr double weight(double x, std::size_t i, std::size_t k) const noexcept {
    if (knot_value(i) == knot_value(i + k)) {
      return 0.0;
    }
    return (x - knot_value(i)) / (knot_value(i + k) - knot_value(i));
  }

  // Evaluate the Cox De Boor recurrence relation.
  constexpr double cox_de_boor(double x, std::size_t i, std::size_t k) const noexcept {
    if (k == 0) {
      const std::size_t last_non_repeated_knot = num_knots_ + (order_ - 1) - 1;
      const bool upper_bound =
          x < knot_value(i + 1) || (i + 1 == last_non_repeated_knot && x == knot_value(i + 1));
      if (knot_value(i) <= x && upper_bound) {
        return 1.0;
      } else {
        return 0.0;
      }
    }
    return weight(x, i, k) * cox_de_boor(x, i, k - 1) +
           (1.0 - weight(x, i + 1, k)) * cox_de_boor(x, i + 1, k - 1);
  }

  // Evaluate the i'th basis function at position `x`.
  constexpr double operator()(double x, std::size_t i) const noexcept {
    return cox_de_boor(x, i, order_ - 1);
  }

  // Evaluate a cumulative version of the b-spline at position `x`.
  constexpr double cumulative(double x, std::size_t i) const noexcept {
    double sum = 0.0;
    for (std::size_t s = i; s < num_extended_knots() - order_; ++s) {
      sum += operator()(x, s);
    }
    return sum;
  }

 private:
  std::size_t order_;
  std::size_t num_knots_;
};

static_assert(1.0 == bspline_numerical(3, 4)(0.0, 0));
static_assert(0.0 == bspline_numerical(3, 4)(1 / 3.0, 0));

static_assert(0.0 == bspline_numerical(3, 4)(0.0, 1));
static_assert(0.0 == bspline_numerical(3, 4)(2.0 / 3.0, 1));

static_assert(0.0 == bspline_numerical(3, 4)(0.0, 2));
static_assert(0.0 == bspline_numerical(3, 4)(1.0, 2));

static_assert(0.0 == bspline_numerical(3, 4)(0.0, 4));
static_assert(1.0 == bspline_numerical(3, 4)(1.0, 4));

}  // namespace wf
