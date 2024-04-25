// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <gtest/gtest.h>
#include <Eigen/Core>
#include <Eigen/Geometry>

#include "wf/expressions/numeric_expressions.h"
#include "wf/matrix_expression.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

#define EXPECT_EIGEN_NEAR(a, b, tol) EXPECT_PRED_FORMAT3(wf::expect_eigen_near, a, b, tol)
#define ASSERT_EIGEN_NEAR(a, b, tol) ASSERT_PRED_FORMAT3(wf::expect_eigen_near, a, b, tol)

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

namespace wf {

// Compare two eigen matrices. Use EXPECT_EIGEN_NEAR()
template <typename Ta, typename Tb>
testing::AssertionResult expect_eigen_near(const std::string_view name_a,
                                           const std::string_view name_b,
                                           const std::string_view name_tol, const Ta& a,
                                           const Tb& b, const double tolerance) {
  if (a.rows() != b.rows() || a.cols() != b.cols()) {
    return testing::AssertionFailure()
           << fmt::format("Dimensions of {} [{}, {}] and {} [{}, {}] do not match.", name_a,
                          a.rows(), a.cols(), name_b, b.rows(), b.cols());
  }
  for (int i = 0; i < a.rows(); ++i) {
    for (int j = 0; j < a.cols(); ++j) {
      if (const double delta = a(i, j) - b(i, j);
          std::abs(delta) > tolerance || std::isnan(delta)) {
        return testing::AssertionFailure() << fmt::format(
                   "Matrix equality {a} == {b} failed because:\n"
                   "({a})({i}, {j}) - ({b})({i}, {j}) = {delta} > {tol}\n"
                   "Where {a} evaluates to:\n{a_eval}\nAnd {b} evaluates to:\n{b_eval}\n"
                   "And {name_tol} evaluates to: {tol}\n",
                   fmt::arg("a", name_a), fmt::arg("b", name_b), fmt::arg("i", i), fmt::arg("j", j),
                   fmt::arg("a_eval", a), fmt::arg("b_eval", b), fmt::arg("name_tol", name_tol),
                   fmt::arg("tol", tolerance), fmt::arg("delta", delta));
      }
    }
  }
  return testing::AssertionSuccess();
}

// Construct an eigen matrix from a matrix expr by evaluating and converting to floats.
inline Eigen::MatrixXd eigen_matrix_from_matrix_expr(const matrix_expr& m) {
  matrix_expr m_eval = m.eval();
  Eigen::MatrixXd result{m_eval.rows(), m_eval.cols()};
  for (index_t i = 0; i < result.rows(); ++i) {
    for (index_t j = 0; j < result.cols(); ++j) {
      if (const float_constant* as_flt = get_if<const float_constant>(m_eval(i, j));
          as_flt != nullptr) {
        result(i, j) = as_flt->value();
      } else if (const integer_constant* as_int = get_if<const integer_constant>(m_eval(i, j));
                 as_int != nullptr) {
        result(i, j) = static_cast<float_constant>(*as_int).value();
      } else if (const rational_constant* as_rational =
                     get_if<const rational_constant>(m_eval(i, j));
                 as_rational != nullptr) {
        result(i, j) = static_cast<float_constant>(*as_rational).value();
      } else {
        throw type_error("Cannot coerce value to float: {}", m_eval(i, j));
      }
    }
  }
  return result;
}

// Convert Quaternion to Vector4, ordered [w,x,y,z] (scalar first).
inline Eigen::Vector4d eigen_wxyz_vec_from_quaternion(const Eigen::Quaterniond& q) {
  return (Eigen::Vector4d() << q.w(), q.x(), q.y(), q.z()).finished();
}

}  // namespace wf
