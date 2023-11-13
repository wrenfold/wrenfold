// Copyright 2023 Gareth Cross
#pragma once
#include <Eigen/Core>

#define MATH_SPAN_EIGEN_SUPPORT
#include "span_eigen.h"

// Methods for testing derivatives by comparing to the numerical approximation.
namespace math {

/**
 * Numerically compute first derivative of f(x) via central difference. Uses the third-order
 * approximation, which has error in O(h^6).
 *
 * The function `func` is presumed to be centered on the linearization point `x`, such that only
 * the step increment `dx` (a scalar) is passed as an argument.
 *
 * References:
 * http://www.rsmas.miami.edu/personal/miskandarani/Courses/MSC321/lectfiniteDifference.pdf
 * https://en.wikipedia.org/wiki/Finite_difference_coefficient
 */
template <typename Scalar, typename Function>
auto numerical_derivative(const Scalar dx, Function func) -> decltype(func(dx)) {
  using ResultType = decltype(func(dx));
  const Scalar dx2 = dx * 2;
  const Scalar dx3 = dx * 3;
  const ResultType c1 = func(dx) - func(-dx);
  const ResultType c2 = func(dx2) - func(-dx2);
  const ResultType c3 = func(dx3) - func(-dx3);
  return (c1 * 45 - c2 * 9 + c3) / (60 * dx);
}

namespace detail {
constexpr auto derived_type(...) -> void;
template <typename Derived>
constexpr auto derived_type(const Eigen::MatrixBase<Derived>&) -> Derived;

template <typename T, typename = void>
struct eigen_matrix_base_derived_type;
template <typename T>
using eigen_matrix_base_derived_type_t = typename eigen_matrix_base_derived_type<T>::type;

template <typename T>
struct eigen_matrix_base_derived_type<T, detail::enable_if_inherits_matrix_base_t<T>> {
  using type = decltype(derived_type(std::declval<const T>()));
};

template <typename T, typename = void>
struct eigen_matrix_base_traits;

template <typename T>
struct eigen_matrix_base_traits<T, detail::enable_if_inherits_matrix_base_t<T>> {
  using derived_type = eigen_matrix_base_derived_type_t<T>;
  static constexpr int rows = Eigen::MatrixBase<derived_type>::RowsAtCompileTime;
  static constexpr int cols = Eigen::MatrixBase<derived_type>::ColsAtCompileTime;
  using scalar_type = typename Eigen::MatrixBase<derived_type>::Scalar;
};

}  // namespace detail

/**
 * Numerically compute the jacobian of vector function `y = f(x)` via the central-difference. `func`
 * accepts type `XExpr` and returns type `YExpr`, both of which may be manifolds. This method uses
 * the Manifold<> trait to determine how make the manifold locally euclidean.
 */
template <typename Derived, typename Function>
auto numerical_jacobian(const Eigen::MatrixBase<Derived>& x, Function func, const double h = 0.01) {
  using YType = std::decay_t<decltype(func(x))>;
  using XTraits = detail::eigen_matrix_base_traits<Eigen::MatrixBase<Derived>>;
  using YTraits = detail::eigen_matrix_base_traits<YType>;
  static_assert(XTraits::cols == 1 && YTraits::cols == 1, "X and Y must be column vectors");

  // Compute the output expression at the linearization point.
  const Eigen::Matrix<typename YTraits::scalar_type, YTraits::rows, 1> y_0 = func(x);

  // Possibly allocate for the result, since dimensions may be dynamic.
  Eigen::Matrix<typename YTraits::scalar_type, YTraits::rows, XTraits::rows> J;
  if constexpr (YTraits::rows == Eigen::Dynamic || XTraits::rows == Eigen::Dynamic) {
    J.resize(y_0.rows(), x.rows());
  }

  // Pre-allocate `delta` once and re-use it.
  Eigen::Matrix<typename XTraits::scalar_type, XTraits::rows, 1> delta;
  if constexpr (XTraits::rows == Eigen::Dynamic) {
    delta.resize(x.rows());
  }

  for (int j = 0; j < x.rows(); ++j) {
    // Take derivative wrt dimension `j` of X
    J.col(j) = numerical_derivative(
        static_cast<typename XTraits::scalar_type>(h),
        [&](auto dx) -> Eigen::Matrix<typename YTraits::scalar_type, YTraits::rows, 1> {
          delta.setZero();
          delta[j] = dx;
          const auto y = func(x + delta);
          return y - y_0;
        });
  }
  return J;
}

}  // namespace math
