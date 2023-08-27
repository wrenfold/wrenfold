// Copyright 2023 Gareth Cross
#include "eigen_test_helpers.h"
#include "numeric_testing.h"
#include "test_helpers.h"
#include "type_annotations.h"

#include "test_expressions.h"  //  Symbolic test functions.

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

  Eigen::Vector2d D_angle_eval, D_angle_gen;
  EXPECT_EIGEN_NEAR(evaluator(1.12, {-6.5, 7.2}, D_angle_eval),
                    gen::vector_rotation_2d(1.12, {-6.5, 7.2}, D_angle_gen), 1.0e-15);
  EXPECT_EIGEN_NEAR(D_angle_eval, D_angle_gen, 1.0e-15);

  // should still work if the optional arg is omitted
  EXPECT_EIGEN_NEAR(evaluator(-0.7, {-5.5, 12.0}, D_angle_eval),
                    gen::vector_rotation_2d(-0.7, {-5.5, 12.0}, std::nullopt), 1.0e-15);

  EXPECT_EIGEN_NEAR(evaluator(22.0, {7.123, -4.001}, D_angle_eval),
                    gen::vector_rotation_2d(22.0, {7.123, -4.001}, D_angle_gen), 1.0e-15);
  EXPECT_EIGEN_NEAR(D_angle_eval, D_angle_gen, 1.0e-15);

  EXPECT_EIGEN_NEAR(evaluator(0.0, {2.0, 3.0}, D_angle_eval),
                    gen::vector_rotation_2d(0.0, {2.0, 3.0}, D_angle_gen), 1.0e-15);
  EXPECT_EIGEN_NEAR(D_angle_eval, D_angle_gen, 1.0e-15);
}

TEST(CppGenerationTest, TestVectorNorm3D) {
  auto evaluator = CreateEvaluator(&VectorNorm3D);

  Eigen::RowVector3d D_vec_eval, D_vec_gen;
  EXPECT_NEAR(evaluator({0.5, -0.23, 0.9}, D_vec_eval),
              gen::vector_norm_3d({0.5, -0.23, 0.9}, D_vec_gen), 1.0e-15);
  EXPECT_EIGEN_NEAR(D_vec_eval, D_vec_gen, 1.0e-15);

  EXPECT_NEAR(evaluator({0.0, 5.2, -0.0001}, D_vec_eval),
              gen::vector_norm_3d({0.0, 5.2, -0.0001}, D_vec_gen), 1.0e-15);
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

}  // namespace math
