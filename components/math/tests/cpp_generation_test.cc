// Copyright 2023 Gareth Cross
#include "eigen_test_helpers.h"
#include "numeric_testing.h"
#include "test_helpers.h"
#include "type_annotations.h"

#include "test_expressions.h"  //  Symbolic test functions.

#define MATH_SPAN_EIGEN_SUPPORT
#include "span_eigen.h"

#include "generated.h"

namespace math {

TEST(CppGenerationTest, TestSimpleMultiplyAdd) {
  auto evaluator = create_evaluator(&simple_multiply_add);
  ASSERT_NEAR(evaluator(1.5, -2.2, 0.11), gen::simple_multiply_add(1.5, -2.2, 0.11), 1.0e-15);
  ASSERT_NEAR(evaluator(5.112, -0.01, -4.2), gen::simple_multiply_add(5.112, -0.01, -4.2), 1.0e-15);
}

TEST(CppGenerationTest, TestVectorRotation2D) {
  auto evaluator = create_evaluator(&vector_rotation_2d);

  for (double angle = -2.0 * M_PI; angle < 2.0 * M_PI; angle += 0.2) {
    Eigen::Vector2d D_angle_eval;

    Eigen::Vector2d v_rot_gen, D_angle_gen;
    gen::vector_rotation_2d(angle, Eigen::Vector2d{-6.5, 7.2}, v_rot_gen, D_angle_gen);
    EXPECT_EIGEN_NEAR(evaluator(angle, {-6.5, 7.2}, D_angle_eval), v_rot_gen, 1.0e-15);
    EXPECT_EIGEN_NEAR(D_angle_eval, D_angle_gen, 1.0e-15);

    // should still work without the optional arg
    gen::vector_rotation_2d(angle, Eigen::Vector2d{-5.5, 12.0}, v_rot_gen, nullptr);
    EXPECT_EIGEN_NEAR(evaluator(angle, {-5.5, 12.0}, D_angle_eval), v_rot_gen, 1.0e-15);

    // Pass a map to the data:
    const std::array<double, 2> input_v = {7.123, -4.001};
    const Eigen::Map<const Eigen::Vector2d> input_v_map(input_v.data());

    gen::vector_rotation_2d(angle, input_v_map, v_rot_gen, D_angle_gen);
    EXPECT_EIGEN_NEAR(evaluator(angle, input_v_map, D_angle_eval), v_rot_gen, 1.0e-15);
    EXPECT_EIGEN_NEAR(D_angle_eval, D_angle_gen, 1.0e-15);

    // pass a map for the output:
    gen::vector_rotation_2d(angle, Eigen::Vector2d{2.0, 3.0}, v_rot_gen,
                            Eigen::Map<Eigen::Vector2d>(D_angle_gen.data()));
    EXPECT_EIGEN_NEAR(evaluator(angle, {2.0, 3.0}, D_angle_eval), v_rot_gen, 1.0e-15);
    EXPECT_EIGEN_NEAR(D_angle_eval, D_angle_gen, 1.0e-15);
  }
}

TEST(CppGenerationTest, TestVectorNorm3D) {
  auto evaluator = create_evaluator(&vector_norm_3d);

  Eigen::RowVector3d D_vec_eval, D_vec_gen;
  EXPECT_NEAR(evaluator({0.5, -0.23, 0.9}, D_vec_eval),
              gen::vector_norm_3d<double>(Eigen::Vector3d{0.5, -0.23, 0.9}, D_vec_gen), 1.0e-15);
  EXPECT_EIGEN_NEAR(D_vec_eval, D_vec_gen, 1.0e-15);

  EXPECT_NEAR(evaluator({0.0, 5.2, -0.0001}, D_vec_eval),
              gen::vector_norm_3d<double>(Eigen::Vector3d{0.0, 5.2, -0.0001}, D_vec_gen), 1.0e-15);
  EXPECT_EIGEN_NEAR(D_vec_eval, D_vec_gen, 1.0e-15);
}

TEST(CppGenerationTest, TestHeaviside) {
  constexpr auto inf = std::numeric_limits<double>::infinity();
  EXPECT_EQ(0.0, gen::heaviside(-1.0));
  EXPECT_EQ(0.0, gen::heaviside(std::nextafter(0.0, -inf)));
  EXPECT_EQ(1.0, gen::heaviside(0.0));
  EXPECT_EQ(1.0, gen::heaviside(1.0));
  EXPECT_EQ(1.0, gen::heaviside(std::nextafter(0.0, inf)));
  EXPECT_EQ(1.0, gen::heaviside(1.));
}

TEST(CppGenerationTest, TestExclusiveOr) {
  constexpr auto inf = std::numeric_limits<double>::infinity();
  EXPECT_EQ(0.0, gen::exclusive_or(0.5, 1.0));
  EXPECT_EQ(0.0, gen::exclusive_or(-2.3, -0.5));
  EXPECT_EQ(1.0, gen::exclusive_or(-1.0, 1.0));
  EXPECT_EQ(1.0, gen::exclusive_or(1.0, -1.0));
  EXPECT_EQ(1.0, gen::exclusive_or(std::nextafter(0.0, -inf), std::nextafter(0.0, inf)));
}

// We use this test to check that abs() is generated correctly.
TEST(CppGenerationTest, TestHandwrittenSignum) {
  auto evaluator = create_evaluator(&handwritten_signum);

  EXPECT_EQ(evaluator(0.0), gen::handwritten_signum(0.0));
  EXPECT_EQ(0.0, gen::handwritten_signum(0.0));
  EXPECT_EQ(0.0, gen::handwritten_signum(-0.0));

  EXPECT_EQ(evaluator(1.0e-16), gen::handwritten_signum(1.0e-16));
  EXPECT_EQ(1.0, gen::handwritten_signum(1.0e-16));

  EXPECT_EQ(evaluator(-1.0e-16), gen::handwritten_signum(-1.0e-16));
  EXPECT_EQ(-1.0, gen::handwritten_signum(-1.0e-16));

  EXPECT_EQ(1.0, gen::handwritten_signum(2.3));
  EXPECT_EQ(-1.0, gen::handwritten_signum(-800.0));
}

// We use this to test the signum implementation.
TEST(CppGenerationTest, TestHandwrittenAbs) {
  EXPECT_EQ(0.0, gen::handwritten_abs(0.0));
  EXPECT_EQ(0.0, gen::handwritten_abs(-0.0));
  EXPECT_EQ(1.0, gen::handwritten_abs(1.0));
  EXPECT_EQ(22.0, gen::handwritten_abs(22.0));
  EXPECT_EQ(3.0, gen::handwritten_abs(-3.0));
  EXPECT_EQ(1e9, gen::handwritten_abs(-1.0e9));
}

TEST(CppGenerationTest, TestAtan2WithDerivatives) {
  auto evaluator = create_evaluator(&atan2_with_derivatives);

  double D_y_num, D_x_num;
  double D_y_gen, D_x_gen;

  // clang-format off
  const std::vector<std::tuple<double, double, double>> test_cases = {
      {0.0, 1.0, 0.0},
      {1.0, 0.0, M_PI / 2},
      {1.0, 1.0, M_PI / 4},
      {1.0, -1.0, 3 * M_PI / 4},
      {0.0, -1.0, M_PI},
      {-1.0, -1.0, -3 * M_PI / 4},
      {-1.0, 0.0, -M_PI / 2},
      {-1.0, 1.0, -M_PI / 4}
  };
  // clang-format on

  for (auto [y, x, solution] : test_cases) {
    EXPECT_EQ(evaluator(y, x, D_y_num, D_x_num),
              gen::atan2_with_derivatives(y, x, D_y_gen, D_x_gen));
    EXPECT_EQ(solution, gen::atan2_with_derivatives(y, x, D_y_gen, D_x_gen));
    EXPECT_NEAR(D_y_num, D_y_gen, 1.0e-15);
    EXPECT_NEAR(D_x_num, D_x_gen, 1.0e-15);
  }
}

}  // namespace math