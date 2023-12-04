// Copyright 2023 Gareth Cross
#pragma once
#include "Eigen/Core"

#define MATH_SPAN_EIGEN_SUPPORT
#include "wf_runtime/span_eigen.h"

// Methods for testing derivatives by comparing to the numerical approximation.
namespace wf {

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
  using result_type = decltype(func(dx));
  const Scalar dx2 = dx * 2;
  const Scalar dx3 = dx * 3;
  const result_type c1 = func(dx) - func(-dx);
  const result_type c2 = func(dx2) - func(-dx2);
  const result_type c3 = func(dx3) - func(-dx3);
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
struct eigen_matrix_base_derived_type<T, enable_if_inherits_matrix_base_t<T>> {
  using type = decltype(derived_type(std::declval<const T>()));
};

template <typename T, typename = void>
struct eigen_matrix_base_traits;

template <typename T>
struct eigen_matrix_base_traits<T, enable_if_inherits_matrix_base_t<T>> {
  using derived_type = eigen_matrix_base_derived_type_t<T>;
  static constexpr int rows = Eigen::MatrixBase<derived_type>::RowsAtCompileTime;
  static constexpr int cols = Eigen::MatrixBase<derived_type>::ColsAtCompileTime;
  using scalar_type = typename Eigen::MatrixBase<derived_type>::Scalar;
};

template <typename T, typename = void>
struct evaluated_type {
  using type = T;
};

// Specialization for Eigen so we can determine the underlying type with actual storage.
template <typename T>
struct evaluated_type<T, enable_if_inherits_matrix_base_t<T>> {
  using type = std::decay_t<decltype(std::declval<T>().eval())>;
};

}  // namespace detail

template <typename T, typename = void>
struct manifold;

template <typename T>
struct manifold<T, enable_if_inherits_matrix_base_t<T>> {
  static constexpr int dimension = detail::eigen_matrix_base_traits<T>::rows;
  using scalar_type = typename detail::eigen_matrix_base_traits<T>::scalar_type;

  static_assert(Eigen::Dynamic != dimension, "Dimension must be known at compile time.");
  static_assert(1 == detail::eigen_matrix_base_traits<T>::cols, "Must be a column vector.");

  static Eigen::Vector<scalar_type, dimension> local_coordinates(const T& x, const T& y) noexcept {
    return y - x;
  }

  template <typename Derived>
  static T retract(const T& x, const Eigen::MatrixBase<Derived>& dx) noexcept {
    const Eigen::Vector<scalar_type, dimension> dx_eval = dx.eval();
    return x + dx_eval;
  }
};

template <typename T>
struct manifold<T, enable_if_inherits_quaternion_base_t<T>> {
  static constexpr int dimension = 3;
  using scalar_type = typename T::Scalar;

  static Eigen::Vector<scalar_type, dimension> local_coordinates(const T& x, const T& y) noexcept {
    const Eigen::AngleAxis<scalar_type> delta{x.inverse() * y};
    return delta.angle() * delta.axis();
  }

  template <typename Derived>
  static T retract(const T& x, const Eigen::MatrixBase<Derived>& dx) noexcept {
    const Eigen::Vector<scalar_type, dimension> dx_eval = dx.eval();
    return x * Eigen::Quaternion<scalar_type>{
                   Eigen::AngleAxis<scalar_type>{dx_eval.norm(), dx_eval.normalized()}};
  }
};

/**
 * Numerically compute the jacobian of vector function `y = f(x)` via the central-difference. `func`
 * accepts type `XExpr` and returns type `YExpr`, both of which may be manifolds. This method uses
 * the Manifold<> trait to determine how make the manifold locally euclidean.
 */
template <typename XType, typename Function>
auto numerical_jacobian(const XType& x, Function func, const double h = 0.01) {
  using XEvalType = typename detail::evaluated_type<XType>::type;
  using XTraits = manifold<XEvalType>;
  using YTraits = manifold<std::invoke_result_t<Function, XType>>;

  // Compute the output expression at the linearization point.
  const auto y_0 = func(x);

  // Storage for jacobian + tangent space delta:
  Eigen::Matrix<typename YTraits::scalar_type, YTraits::dimension, XTraits::dimension> J;
  Eigen::Matrix<typename XTraits::scalar_type, XTraits::dimension, 1> delta;

  for (int j = 0; j < XTraits::dimension; ++j) {
    // Take derivative wrt dimension `j` of X
    J.col(j) = numerical_derivative(
        static_cast<typename XTraits::scalar_type>(h),
        [&](auto dx) -> Eigen::Matrix<typename YTraits::scalar_type, YTraits::dimension, 1> {
          delta.setZero();
          delta[j] = dx;
          const auto y = func(XTraits::retract(x, delta));
          return YTraits::local_coordinates(y_0, y);
        });
  }
  return J;
}

// Perform numerical integration via Boole's rule.
// Some care must be taken that `func` does not return Eigen expressions with invalid lifetimes.
template <typename Function>
auto integrate_boole(Function&& func, const double lower, const double upper)
    -> decltype(func(lower)) {
  const double h = (upper - lower) / 4.0;
  const auto f0 = func(lower);
  const auto f1 = func(lower + h);
  const auto f2 = func(lower + h * 2);
  const auto f3 = func(lower + h * 3);
  const auto f4 = func(upper);
  const auto sum = f0 * 7.0 + f1 * 32.0 + f2 * 12.0 + f3 * 32.0 + f4 * 7.0;
  const double normalization = 2 * h / 45.0;
  return sum * normalization;
}

}  // namespace wf
