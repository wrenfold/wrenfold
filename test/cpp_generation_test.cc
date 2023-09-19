// Copyright 2023 Gareth Cross
#include "eigen_test_helpers.h"
#include "numeric_testing.h"
#include "test_helpers.h"
#include "type_annotations.h"

#include "test_expressions.h"  //  Symbolic test functions.

// #define MATH_SPAN_RUNTIME_ASSERT(x) ASSERT(x)
#define MATH_SPAN_EIGEN_SUPPORT
#include "span_eigen.h"

#include "generated.h"

template <typename T>
struct fmt::formatter<T, std::enable_if_t<std::is_base_of_v<Eigen::DenseBase<T>, T>, char>> {
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

TEST(CppGenerationTest, TestSimpleMultiplyAdd) {
  auto evaluator = CreateEvaluator(&SimpleMultiplyAdd);
  ASSERT_NEAR(evaluator(1.5, -2.2, 0.11), gen::simple_multiply_add(1.5, -2.2, 0.11), 1.0e-15);
  ASSERT_NEAR(evaluator(5.112, -0.01, -4.2), gen::simple_multiply_add(5.112, -0.01, -4.2), 1.0e-15);
}

TEST(CppGenerationTest, TestVectorRotation2D) {
  auto evaluator = CreateEvaluator(&VectorRotation2D);

  Eigen::Vector2d D_angle_eval;
  Eigen::Vector2d v_rot_eval = evaluator(1.12, {-6.5, 7.2}, D_angle_eval);

  Eigen::Vector2d v_rot_gen, D_angle_gen;
  gen::vector_rotation_2d(1.12, Eigen::Vector2d{-6.5, 7.2}, v_rot_gen, D_angle_gen);
  EXPECT_EIGEN_NEAR(v_rot_eval, v_rot_gen, 1.0e-15);
  EXPECT_EIGEN_NEAR(D_angle_eval, D_angle_gen, 1.0e-15);

  // should still work if the optional arg is omitted
  v_rot_eval = evaluator(-0.7, {-5.5, 12.0}, D_angle_eval);
  gen::vector_rotation_2d(-0.7, Eigen::Vector2d{-5.5, 12.0}, v_rot_gen, nullptr);
  EXPECT_EIGEN_NEAR(v_rot_eval, v_rot_gen, 1.0e-15);

  // Pass a map to the data:
  const std::array<double, 2> input_v = {7.123, -4.001};
  const Eigen::Map<const Eigen::Vector2d> input_v_map(input_v.data());
  v_rot_eval = evaluator(22.0, input_v_map, D_angle_eval);
  gen::vector_rotation_2d(22.0, input_v_map, v_rot_gen, D_angle_gen);

  EXPECT_EIGEN_NEAR(v_rot_eval, v_rot_gen, 1.0e-15);
  EXPECT_EIGEN_NEAR(D_angle_eval, D_angle_gen, 1.0e-15);

  // pass a map for the output:
  v_rot_eval = evaluator(0.3, {2.0, 3.0}, D_angle_eval);
  gen::vector_rotation_2d(0.3, Eigen::Vector2d{2.0, 3.0}, v_rot_gen,
                          Eigen::Map<Eigen::Vector2d>(D_angle_gen.data()));
  EXPECT_EIGEN_NEAR(v_rot_eval, v_rot_gen, 1.0e-15);
  EXPECT_EIGEN_NEAR(D_angle_eval, D_angle_gen, 1.0e-15);
}

TEST(CppGenerationTest, TestVectorNorm3D) {
  auto evaluator = CreateEvaluator(&VectorNorm3D);

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
  EXPECT_EQ(0.0, gen::heaviside(0.0));
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

}  // namespace math
