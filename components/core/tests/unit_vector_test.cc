// wrenfold symbolic code generator.
// Copyright (c) 2026 wrenfold contributors
// For license information refer to accompanying LICENSE file.
#include "wf/geometry/unit_vector.h"

#include <cmath>

#include <gtest/gtest.h>
#include <Eigen/Core>

#include "wf/matrix_functions.h"
#include "wf_test_support/eigen_test_macros.h"

#ifdef WF_TEST_CERES
#include <ceres/sphere_manifold.h>
#endif

namespace wf {

TEST(UnitVectorTest, ConstructionAndDimensions) {
  const unit_vector x{make_vector(1, 2, 3)};
  EXPECT_EQ(3, x.dimension());
  EXPECT_TRUE(x.is_identical_to(unit_vector{make_vector(1, 2, 3)}));
  const unit_vector symbols = unit_vector::from_name_prefix("x", 3);
  EXPECT_EQ(3, symbols.jacobian(symbols.to_vector()).rows());
  EXPECT_EQ(3, symbols.jacobian(symbols.to_vector()).cols());
  EXPECT_THROW(unit_vector{make_vector(1)}, dimension_error);
  EXPECT_THROW(unit_vector{make_matrix(2, 2, 1, 0, 0, 1)}, dimension_error);
  EXPECT_THROW(x.retract(make_vector(1, 2, 3)), dimension_error);
  EXPECT_THROW(x.local_coordinates(unit_vector{make_vector(1, 0)}), dimension_error);
  EXPECT_EQ(4, unit_vector::from_name_prefix("x", 4).dimension());
  ASSERT_EIGEN_NEAR(
      eigen_matrix_from_matrix_expr(unit_vector{make_vector(3, 4)}.normalized().to_vector()),
      Eigen::Vector2d(0.6, 0.8), 1.0e-15);
}

TEST(UnitVectorTest, SymbolicAntipode) {
  const unit_vector north{make_vector(0, 0, 1)};
  const unit_vector south{make_vector(0, 0, -1)};
  ASSERT_EIGEN_NEAR(eigen_matrix_from_matrix_expr(north.local_coordinates(south)),
                    Eigen::Vector2d(0.0, std::acos(-1.0)), 1.0e-15);
}

#ifdef WF_TEST_CERES

static matrix_expr symbolic_vector(const Eigen::VectorXd& x) {
  std::vector<scalar_expr> values;
  values.reserve(x.size());
  for (const double value : x) {
    values.emplace_back(value);
  }
  return matrix_expr::create(x.size(), 1, std::move(values));
}

template <int N>
static void compare_with_ceres(Eigen::Matrix<double, N, 1> x,
                               const Eigen::Matrix<double, N - 1, 1>& delta) {
  ceres::SphereManifold<N> sphere;
  const unit_vector symbolic_x{symbolic_vector(x)};
  Eigen::Matrix<double, N, N - 1, Eigen::RowMajor> plus_jacobian;
  Eigen::Matrix<double, N - 1, N, Eigen::RowMajor> minus_jacobian;
  ASSERT_TRUE(sphere.PlusJacobian(x.data(), plus_jacobian.data()));
  ASSERT_TRUE(sphere.MinusJacobian(x.data(), minus_jacobian.data()));
  ASSERT_EIGEN_NEAR(eigen_matrix_from_matrix_expr(symbolic_x.retract_derivative()), plus_jacobian,
                    1.0e-12);
  ASSERT_EIGEN_NEAR(eigen_matrix_from_matrix_expr(symbolic_x.local_coordinates_derivative()),
                    minus_jacobian, 1.0e-12);

  Eigen::Matrix<double, N, 1> y;
  ASSERT_TRUE(sphere.Plus(x.data(), delta.data(), y.data()));
  ASSERT_EIGEN_NEAR(
      eigen_matrix_from_matrix_expr(symbolic_x.retract(symbolic_vector(delta)).to_vector()), y,
      1.0e-12);
  Eigen::Matrix<double, N - 1, 1> recovered;
  ASSERT_TRUE(sphere.Minus(y.data(), x.data(), recovered.data()));
  ASSERT_EIGEN_NEAR(
      eigen_matrix_from_matrix_expr(symbolic_x.local_coordinates(unit_vector{symbolic_vector(y)})),
      recovered, 1.0e-12);
  ASSERT_EIGEN_NEAR(recovered, delta, 1.0e-12);

  Eigen::Matrix<double, N - 1, 1> zero = Eigen::Matrix<double, N - 1, 1>::Zero();
  ASSERT_EIGEN_NEAR(
      eigen_matrix_from_matrix_expr(symbolic_x.retract(symbolic_vector(zero)).to_vector()), x,
      1.0e-12);
}

TEST(UnitVectorTest, MatchesCeresSphereManifold) {
  const Eigen::Vector2d delta2(0.08, -0.17);
  compare_with_ceres<3>(Eigen::Vector3d(0, 0, 1), delta2);
  compare_with_ceres<3>(Eigen::Vector3d(0, 0, -1), delta2);
  compare_with_ceres<3>(Eigen::Vector3d(0.3, -0.4, 0.8660254037844386), delta2);
  compare_with_ceres<3>(Eigen::Vector3d(1.0e-9, 0, 1).normalized(), delta2);
  compare_with_ceres<3>(Eigen::Vector3d(0.3, -0.4, 0.8660254037844386) * 2, delta2);
  compare_with_ceres<4>(Eigen::Vector4d(0.2, -0.3, 0.4, 0.8426149773176358),
                        Eigen::Vector3d(0.04, 0.12, -0.09));
}

TEST(UnitVectorTest, AntipodalConventionMatchesCeres) {
  ceres::SphereManifold<3> sphere;
  const Eigen::Vector3d x(0.3, -0.4, 0.8660254037844386);
  const Eigen::Vector3d opposite = -x;
  Eigen::Vector2d expected;
  ASSERT_TRUE(sphere.Minus(opposite.data(), x.data(), expected.data()));
  ASSERT_EIGEN_NEAR(eigen_matrix_from_matrix_expr(unit_vector{symbolic_vector(x)}.local_coordinates(
                        unit_vector{symbolic_vector(opposite)})),
                    expected, 1.0e-12);
}

TEST(UnitVectorTest, AmbientCostJacobianMatchesCeresTangentJacobian) {
  ceres::SphereManifold<3> sphere;
  const Eigen::Vector3d x(0.3, -0.4, 0.8660254037844386);
  const unit_vector symbolic_x{symbolic_vector(x)};
  const matrix_expr ambient_jacobian = make_matrix(2, 3, 1, 2, 0, 0, 0, 1);
  Eigen::Matrix<double, 3, 2, Eigen::RowMajor> ceres_plus_jacobian;
  ASSERT_TRUE(sphere.PlusJacobian(x.data(), ceres_plus_jacobian.data()));
  Eigen::Matrix<double, 2, 3> ambient_numeric;
  ambient_numeric << 1, 2, 0, 0, 0, 1;
  ASSERT_EIGEN_NEAR(
      eigen_matrix_from_matrix_expr(ambient_jacobian * symbolic_x.retract_derivative()),
      (ambient_numeric * ceres_plus_jacobian).eval(), 1.0e-12);
}
#endif

}  // namespace wf
