// Copyright 2023 Gareth Cross
#pragma once
#include <gtest/gtest.h>
#include <Eigen/Core>
#include <Eigen/Geometry>

#include "expressions/numeric_expressions.h"
#include "fmt_imports.h"
#include "matrix_expression.h"

#define EXPECT_EIGEN_NEAR(a, b, tol) EXPECT_PRED_FORMAT3(math::expect_eigen_near, a, b, tol)
#define ASSERT_EIGEN_NEAR(a, b, tol) ASSERT_PRED_FORMAT3(math::expect_eigen_near, a, b, tol)

// Allow formatting of Eigen matrices.
template <typename T>
struct fmt::formatter<T, std::enable_if_t<std::is_base_of_v<Eigen::MatrixBase<T>, T>, char>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& m, FormatContext& ctx) const -> decltype(ctx.out()) {
    const Eigen::IOFormat heavy(Eigen::FullPrecision, 0, ", ", ",\n", "[", "]", "[", "]");
    std::stringstream ss;
    ss << m.format(heavy);
    return fmt::format_to(ctx.out(), "{}", ss.str());
  }
};

namespace math {

// Compare two eigen matrices. Use EXPECT_EIGEN_NEAR()
template <typename Ta, typename Tb>
testing::AssertionResult expect_eigen_near(const std::string_view name_a,
                                           const std::string_view name_b,
                                           const std::string_view name_tol, const Ta& a,
                                           const Tb& b, double tolerance) {
  if (a.rows() != b.rows() || a.cols() != b.cols()) {
    return testing::AssertionFailure()
           << fmt::format("Dimensions of {} [{}, {}] and {} [{}, {}] do not match.", name_a,
                          a.rows(), a.cols(), name_b, b.rows(), b.cols());
  }
  for (int i = 0; i < a.rows(); ++i) {
    for (int j = 0; j < a.cols(); ++j) {
      const double delta = a(i, j) - b(i, j);
      if (std::abs(delta) > tolerance || std::isnan(delta)) {
        return testing::AssertionFailure() << fmt::format(
                   "Matrix equality {} == {} failed because:\n"
                   "{}({}, {}) - {}({}, {}) = {} > {}\n"
                   "Where {} evaluates to:\n{}\nAnd {} evaluates to:\n{}\n"
                   "And {} evaluates to: {}\n",
                   name_a, name_b, name_a, i, j, name_b, i, j, delta, tolerance, name_a, a, name_b,
                   b, name_tol, tolerance);
      }
    }
  }
  return testing::AssertionSuccess();
}

// Construct an eigen matrix from a matrix expr by evaluating and converting to floats.
inline Eigen::MatrixXd eigen_matrix_from_matrix_expr(const MatrixExpr& m) {
  MatrixExpr m_eval = m.eval();
  Eigen::MatrixXd result{m_eval.rows(), m_eval.cols()};
  for (index_t i = 0; i < result.rows(); ++i) {
    for (index_t j = 0; j < result.cols(); ++j) {
      if (const Float* as_flt = cast_ptr<Float>(m_eval(i, j)); as_flt != nullptr) {
        result(i, j) = as_flt->get_value();
      } else if (const Integer* as_int = cast_ptr<Integer>(m_eval(i, j)); as_int != nullptr) {
        result(i, j) = static_cast<Float>(*as_int).get_value();
      } else if (const Rational* as_rational = cast_ptr<Rational>(m_eval(i, j));
                 as_rational != nullptr) {
        result(i, j) = static_cast<Float>(*as_rational).get_value();
      } else {
        throw TypeError("Cannot coerce value to float: {}", m_eval(i, j));
      }
    }
  }
  return result;
}

// Convert Quaternion to Vector4, ordered [w,x,y,z] (scalar first).
inline Eigen::Vector4d eigen_wxyz_vec_from_quaternion(const Eigen::Quaterniond& q) {
  return (Eigen::Vector4d() << q.w(), q.x(), q.y(), q.z()).finished();
}

// Local coordinates implemented with Eigen.
// Computes log(a^-1 * b)
template <typename Scalar>
Eigen::Vector<Scalar, 3> local_coordinates(const Eigen::Quaternion<Scalar>& a,
                                           const Eigen::Quaternion<Scalar>& b) {
  const Eigen::AngleAxis<Scalar> w_ab{a.inverse() * b};
  return w_ab.angle() * w_ab.axis();
}

// Retract implement with Eigen quaternions.
// Right multiply the tangent perturbation `w`: q * exp(w)
template <typename Scalar>
Eigen::Quaternion<Scalar> retract(const Eigen::Quaternion<Scalar>& q,
                                  const Eigen::Vector<Scalar, 3>& w) {
  return q * Eigen::Quaternion<Scalar>{Eigen::AngleAxis<Scalar>{w.norm(), w.normalized()}};
}

}  // namespace math
