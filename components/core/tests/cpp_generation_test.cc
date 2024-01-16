// Copyright 2023 Gareth Cross
#include "wf/type_annotations.h"

#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numeric_testing.h"
#include "wf_test_support/test_macros.h"

#include "test_expressions.h"  //  Symbolic test functions.

#define MATH_SPAN_EIGEN_SUPPORT
#include "wf_runtime/span_eigen.h"

// Declare custom numeric types before importing the generated code:
namespace wf::numeric {
struct Point2d {
  double x;
  double y;

  Eigen::Vector2d to_vector() const { return {x, y}; }
};

struct Circle {
  Point2d center;
  double radius;

  Eigen::Vector3d to_vector() const { return {center.x, center.y, radius}; }
};
}  // namespace wf::numeric

namespace test {
template <typename Scalar>
Scalar external_function_1(Scalar a, Scalar b) {
  return a * b;
}
}  // namespace test

#include "generated.h"

namespace wf {

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
TEST(CppGenerationTest, TestSignumAndAbs) {
  auto evaluator = create_evaluator(&signum_and_abs);

  double abs_num, abs_gen;
  EXPECT_EQ(evaluator(0.0, abs_num), gen::signum_and_abs(0.0, abs_gen));
  EXPECT_EQ(abs_num, abs_gen);

  EXPECT_EQ(0.0, gen::signum_and_abs(0.0, abs_gen));
  EXPECT_EQ(0.0, abs_gen);
  EXPECT_EQ(0.0, gen::signum_and_abs(-0.0, abs_gen));
  EXPECT_EQ(0.0, abs_gen);

  EXPECT_EQ(evaluator(1.0e-16, abs_num), gen::signum_and_abs(1.0e-16, abs_gen));
  EXPECT_EQ(abs_num, abs_gen);

  EXPECT_EQ(1.0, gen::signum_and_abs(1.0e-16, abs_gen));
  EXPECT_EQ(1.0e-16, abs_gen);

  EXPECT_EQ(evaluator(-1.0e-16, abs_num), gen::signum_and_abs(-1.0e-16, abs_gen));
  EXPECT_EQ(abs_num, abs_gen);
  EXPECT_EQ(-1.0, gen::signum_and_abs(-1.0e-16, abs_gen));
  EXPECT_EQ(1.0e-16, abs_gen);

  EXPECT_EQ(1.0, gen::signum_and_abs(2.3, abs_gen));
  EXPECT_EQ(2.3, abs_gen);

  EXPECT_EQ(-1.0, gen::signum_and_abs(-800.0, abs_gen));
  EXPECT_EQ(800.0, abs_gen);
}

TEST(CppGenerationTest, TestNestedConditionals1) {
  auto evaluator = create_evaluator(&nested_conditionals_1);
  // Exercise all the different control paths.
  // x > 0, y > 0, |x| > |y|
  EXPECT_EQ(evaluator(0.5, 0.2), gen::nested_conditionals_1(0.5, 0.2));
  // x > 0, y > 0, |x| <= |y|
  EXPECT_EQ(evaluator(0.1, 0.3), gen::nested_conditionals_1(0.1, 0.3));
  // x > 0, y <= 0, |x| > |y|
  EXPECT_EQ(evaluator(2.4, -0.11), gen::nested_conditionals_1(2.4, -0.11));
  // x > 0, y <= 0, |x| <= |y|
  EXPECT_EQ(evaluator(1.3, -3.0), gen::nested_conditionals_1(1.3, -3.0));
  // x <= 0, y > 0, |x| > |y|
  EXPECT_EQ(evaluator(-0.8, 0.66), gen::nested_conditionals_1(-0.8, 0.66));
  // x <= 0, y <= 0, |x| <= |y|
  EXPECT_EQ(evaluator(-0.123, -0.5), gen::nested_conditionals_1(-0.123, -0.5));
}

TEST(CppGenerationTest, TestNestedConditionals2) {
  auto evaluator = create_evaluator(&nested_conditionals_2);
  // x > 0, y > 0, |x| > |y|
  EXPECT_EQ(evaluator(0.73, 0.02), gen::nested_conditionals_2(0.73, 0.02));
  // x > 0, y > 0, |x| <= |y|
  EXPECT_EQ(evaluator(1.32, 1.32), gen::nested_conditionals_2(1.32, 1.32));
  // x > 0, y <= 0, |x| > |y|
  EXPECT_EQ(evaluator(7.2, -7.0), gen::nested_conditionals_2(7.2, -7.0));
  // x > 0, y <= 0, |x| <= |y|
  EXPECT_EQ(evaluator(5.6, -6.3), gen::nested_conditionals_2(5.6, -6.3));
  // x <= 0, y > 0, |x| > |y|
  EXPECT_EQ(evaluator(-1.0, 0.95), gen::nested_conditionals_2(-1.0, 0.95));
  // x <= 0, y <= 0, |x| <= |y|
  EXPECT_EQ(evaluator(-1.7, -2.0), gen::nested_conditionals_2(-1.7, -2.0));
}

TEST(CppGenerationTest, TestAtan2WithDerivatives) {
  auto evaluator = create_evaluator(&atan2_with_derivatives);

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
    double D_y_num, D_x_num;
    double D_y_gen, D_x_gen;

    EXPECT_EQ(evaluator(y, x, D_y_num, D_x_num),
              gen::atan2_with_derivatives(y, x, D_y_gen, D_x_gen));
    EXPECT_EQ(solution, gen::atan2_with_derivatives(y, x, D_y_gen, D_x_gen));
    EXPECT_NEAR(D_y_num, D_y_gen, 1.0e-15);
    EXPECT_NEAR(D_x_num, D_x_gen, 1.0e-15);
  }
}

TEST(CppGenerationTest, TestCreateRotationMatrix) {
  auto evaluator = create_evaluator(&create_rotation_matrix);

  Eigen::Matrix3d R_num, R_gen;
  Eigen::Matrix<double, 9, 3> D_num, D_gen;

  // Evaluate at zero
  evaluator({0.0, 0.0, 0.0}, R_num, D_num);
  gen::create_rotation_matrix<double>(Eigen::Vector3d::Zero().eval(), R_gen, D_gen);
  EXPECT_EQ(R_num, R_gen);
  EXPECT_EQ(D_num, D_gen);

  const Eigen::Vector3d w1{-0.23, 0.52, 0.2};
  evaluator(w1, R_num, D_num);
  gen::create_rotation_matrix<double>(w1, R_gen, D_gen);
  EXPECT_EIGEN_NEAR(R_num, R_gen, 1.0e-15);
  EXPECT_EIGEN_NEAR(D_num, D_gen, 1.0e-15);

  const Eigen::Vector3d w2{-0.0022, 0.0003, -0.00015};
  evaluator(w2, R_num, D_num);
  gen::create_rotation_matrix<double>(w2, R_gen, D_gen);
  EXPECT_EIGEN_NEAR(R_num, R_gen, 1.0e-15);
  EXPECT_EIGEN_NEAR(D_num, D_gen, 1.0e-15);

  const Eigen::Vector3d w3{5.0, -4.0, 2.1};
  evaluator(w3, R_num, D_num);
  gen::create_rotation_matrix<double>(w3, R_gen, D_gen);
  EXPECT_EIGEN_NEAR(R_num, R_gen, 1.0e-15);
  EXPECT_EIGEN_NEAR(D_num, D_gen, 1.0e-15);
}

// Test that we can generate and call a function that has only optional outputs.
TEST(CppGenerationTest, TestNoRequiredOutputs) {
  auto evaluator = create_evaluator(&no_required_outputs);

  constexpr double x = 2.187;
  double out1_num, out2_num;
  evaluator(x, out1_num, out2_num);

  double out1_gen, out2_gen;
  constexpr double* null = nullptr;  //  TODO: Use spans, not pointers.
  gen::no_required_outputs(x, &out1_gen, null);
  gen::no_required_outputs(x, null, &out2_gen);

  EXPECT_NEAR(out1_num, out1_gen, 1.0e-15);
  EXPECT_NEAR(out2_num, out2_gen, 1.0e-15);
}

// Convert to/from symbolic::Point2d --> numeric::Point2d.
template <>
struct custom_type_native_converter<symbolic::Point2d> {
  using native_type = numeric::Point2d;

  numeric::Point2d operator()(const symbolic::Point2d& p) const {
    return numeric::Point2d{cast_checked<float_constant>(p.x).get_value(),
                            cast_checked<float_constant>(p.y).get_value()};
  }

  symbolic::Point2d operator()(const numeric::Point2d& p) const {
    return symbolic::Point2d{p.x, p.y};
  }
};

// Check that we can accept and return a custom type.
TEST(CppGenerationTest, TestCustomType1) {
  auto evaluator = create_evaluator(&custom_type_1);

  EXPECT_EIGEN_NEAR(evaluator({0.0, 0.0}).to_vector(),
                    gen::custom_type_1<double>({0.0, 0.0}).to_vector(), 1.0e-15);
  EXPECT_EIGEN_NEAR(evaluator({-0.5, 2.1}).to_vector(),
                    gen::custom_type_1<double>({-0.5, 2.1}).to_vector(), 1.0e-15);
  EXPECT_EIGEN_NEAR(evaluator({10.8, -7.88}).to_vector(),
                    gen::custom_type_1<double>({10.8, -7.88}).to_vector(), 1.0e-15);
}

TEST(CppGenerationTest, TestCustomType2) {
  auto evaluator = create_evaluator(&custom_type_2);

  numeric::Point2d p_num, p_gen;
  Eigen::Matrix2d D_num, D_gen;
  evaluator(-0.5, 1.717, p_num, D_num);
  gen::custom_type_2(-0.5, 1.717, &p_gen, D_gen);
  EXPECT_EIGEN_NEAR(p_num.to_vector(), p_gen.to_vector(), 1.0e-15);
  EXPECT_EIGEN_NEAR(D_num, D_gen, 1.0e-15);

  evaluator(0.04, 5.0, p_num, D_num);
  gen::custom_type_2(0.04, 5.0, &p_gen, D_gen);
  EXPECT_EIGEN_NEAR(p_num.to_vector(), p_gen.to_vector(), 1.0e-15);
  EXPECT_EIGEN_NEAR(D_num, D_gen, 1.0e-15);

  evaluator(1.232, 0.02, p_num, D_num);
  gen::custom_type_2(1.232, 0.02, &p_gen, D_gen);
  EXPECT_EIGEN_NEAR(p_num.to_vector(), p_gen.to_vector(), 1.0e-15);
  EXPECT_EIGEN_NEAR(D_num, D_gen, 1.0e-15);
}

// Convert to/from symbolic::Circle --> numeric::Circle.
template <>
struct custom_type_native_converter<symbolic::Circle> {
  using native_type = numeric::Circle;

  numeric::Circle operator()(const symbolic::Circle& p) const {
    return numeric::Circle{numeric::Point2d{cast_checked<float_constant>(p.center.x).get_value(),
                                            cast_checked<float_constant>(p.center.y).get_value()},
                           cast_checked<float_constant>(p.radius).get_value()};
  }

  symbolic::Circle operator()(const numeric::Circle& p) const {
    return symbolic::Circle{symbolic::Point2d{p.center.x, p.center.y}, p.radius};
  }
};

TEST(CppGenerationTest, TestNestedCustomType1) {
  auto evaluator = create_evaluator(&nested_custom_type_1);

  constexpr numeric::Circle c1{numeric::Point2d{-0.5, 0.8}, 3.51};
  ASSERT_EIGEN_NEAR(evaluator(c1, {-0.25, 1.2}).to_vector(),
                    gen::nested_custom_type_1<double>(c1, {-0.25, 1.2}).to_vector(), 1.0e-15);
  ASSERT_EIGEN_NEAR(evaluator(c1, {-9.2, -12.0}).to_vector(),
                    gen::nested_custom_type_1<double>(c1, {-9.2, -12.0}).to_vector(), 1.0e-15);

  constexpr numeric::Circle c2{numeric::Point2d{10.0, 0.5}, 2.2};
  ASSERT_EIGEN_NEAR(evaluator(c2, {10.1, 0.0}).to_vector(),
                    gen::nested_custom_type_1<double>(c2, {10.1, 0.0}).to_vector(), 1.0e-15);
  ASSERT_EIGEN_NEAR(evaluator(c2, {-13.0, 4.0}).to_vector(),
                    gen::nested_custom_type_1<double>(c2, {-13.0, 4.0}).to_vector(), 1.0e-15);
}

}  // namespace wf
