// Copyright 2023 Gareth Cross
#pragma once
#include <fmt/format.h>
#include <gtest/gtest.h>
#include <Eigen/Core>

#define EXPECT_EIGEN_NEAR(a, b, tol) EXPECT_PRED_FORMAT3(math::ExpectEigenNear, a, b, tol)
#define ASSERT_EIGEN_NEAR(a, b, tol) ASSERT_PRED_FORMAT3(math::ExpectEigenNear, a, b, tol)

// Allow formatting of Eigen matrices.
template <typename T>
struct fmt::formatter<T, std::enable_if_t<std::is_base_of_v<Eigen::MatrixBase<T>, T>, char>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& m, FormatContext& ctx) const -> decltype(ctx.out()) {
    const Eigen::IOFormat heavy(Eigen::FullPrecision, 0, ", ", ";\n", "[", "]", "[", "]");
    std::stringstream ss;
    ss << m.format(heavy);
    return fmt::format_to(ctx.out(), "{}", ss.str());
  }
};

namespace math {

// Compare two eigen matrices. Use EXPECT_EIGEN_NEAR()
template <typename Ta, typename Tb>
testing::AssertionResult ExpectEigenNear(const std::string& name_a, const std::string& name_b,
                                         const std::string& name_tol, const Ta& a, const Tb& b,
                                         double tolerance) {
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
inline Eigen::MatrixXd EigenMatrixFromMatrixExpr(const MatrixExpr& m) {
  MatrixExpr m_eval = m.Eval();
  Eigen::MatrixXd result{m_eval.NumRows(), m_eval.NumCols()};
  for (index_t i = 0; i < result.rows(); ++i) {
    for (index_t j = 0; j < result.cols(); ++j) {
      if (const Float* as_flt = CastPtr<Float>(m_eval(i, j)); as_flt != nullptr) {
        result(i, j) = as_flt->GetValue();
      } else if (const Integer* as_int = CastPtr<Integer>(m_eval(i, j)); as_int != nullptr) {
        result(i, j) = static_cast<Float>(*as_int).GetValue();
      } else if (const Rational* as_rational = CastPtr<Rational>(m_eval(i, j));
                 as_rational != nullptr) {
        result(i, j) = static_cast<Float>(*as_rational).GetValue();
      } else {
        throw TypeError("Cannot coerce value to float: {}", m_eval(i, j));
      }
    }
  }
  return result;
}

}  // namespace math
