// Copyright 2023 Gareth Cross
#pragma once
#include <fmt/format.h>
#include <gtest/gtest.h>
#include <Eigen/Core>

#define EXPECT_EIGEN_NEAR(a, b, tol) EXPECT_PRED_FORMAT3(math::ExpectEigenNear, a, b, tol)
#define ASSERT_EIGEN_NEAR(a, b, tol) ASSERT_PRED_FORMAT3(math::ExpectEigenNear, a, b, tol)

namespace math {

// Compare two eigen matrices. Use EXPECT_EIGEN_NEAR()
template <typename Ta, typename Tb>
testing::AssertionResult ExpectEigenNear(const std::string& name_a, const std::string& name_b,
                                         const std::string& name_tol,
                                         const Eigen::MatrixBase<Ta>& a,
                                         const Eigen::MatrixBase<Tb>& b, double tolerance) {
  if (a.rows() != b.rows() || a.cols() != b.cols()) {
    return testing::AssertionFailure()
           << "Dimensions of " << name_a << " and " << name_b << " do not match.";
  }
  for (int i = 0; i < a.rows(); ++i) {
    for (int j = 0; j < a.cols(); ++j) {
      const double delta = a(i, j) - b(i, j);
      if (std::abs(delta) > tolerance || std::isnan(delta)) {
        const std::string index_str = fmt::format("({}, {})", i, j);
        return testing::AssertionFailure()
               << "Matrix equality " << name_a << " == " << name_b << " failed because:\n"
               << name_a << index_str << " - " << name_b << index_str << " = " << delta << " > "
               << name_tol << "\nWhere " << name_a << " evaluates to:\n"
               << a << "\n and " << name_b << " evaluates to:\n"
               << b << "\n and " << name_tol << " evaluates to: " << tolerance << "\n";
      }
    }
  }
  return testing::AssertionSuccess();
}

}  // namespace math
