// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/type_annotations.h"

#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numeric_testing.h"
#include "wf_test_support/test_macros.h"

#include "test_expressions.h"  //  Symbolic test functions.

#define WF_SPAN_EIGEN_SUPPORT
#include "wrenfold/span.h"

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

// An integer that can be implicitly cast to std::int64_t, and nothing else.
// We use this to make sure our generated code casts correctly when interfacing with
// `MixedNumerics`.
class type_safe_int64_t {
 public:
  template <typename T, typename = std::enable_if_t<std::is_integral_v<T>>>
  constexpr type_safe_int64_t(T x) noexcept : x_(static_cast<std::int64_t>(x)) {}  //  NOLINT

  template <typename T, typename = enable_if_same_t<T, std::int64_t>>
  constexpr operator T() const noexcept {  //  NOLINT
    return x_;
  }

  // Allow explicit cast to double.
  explicit constexpr operator double() const noexcept { return static_cast<double>(x_); }

 private:
  std::int64_t x_;
};

struct MixedNumerics {
  double value;
  type_safe_int64_t mode;
};
}  // namespace wf::numeric

// These functions don't perform amy meaningful task, they just exist so we can call them from
// generated code to test external function support.
namespace test {

template <typename Scalar>
Scalar external_function_1(Scalar a, Scalar b) {
  return a * b;
}

template <typename Scalar>
Scalar external_function_2(const Eigen::Matrix<Scalar, 2, 3>& m) {
  return m.template topRows<1>().dot(m.template bottomRows<1>());
}

template <typename Scalar>
Eigen::Matrix<Scalar, 2, 2> external_function_3(const Eigen::Matrix<Scalar, 2, 1>& a,
                                                const Eigen::Matrix<Scalar, 2, 1>& b) {
  return a * b.transpose();
}

wf::numeric::Point2d external_function_4(const wf::numeric::Point2d p) {
  return {p.x * p.y, p.y - 2.0};
}

double external_function_5(const wf::numeric::Circle& a, const wf::numeric::Circle& b) {
  const double distance =
      std::sqrt(std::pow(a.center.x - b.center.x, 2.0) + std::pow(a.center.y - b.center.y, 2.0));
  return distance < a.radius + b.radius ? 1.0 : 0.0;
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
    Eigen::Vector2d v_rot_num, D_angle_num;
    Eigen::Vector2d v_rot_gen, D_angle_gen;
    evaluator(angle, Eigen::Vector2d(-6.5, 7.2), v_rot_num, D_angle_num);
    gen::vector_rotation_2d(angle, Eigen::Vector2d{-6.5, 7.2}, v_rot_gen, D_angle_gen);

    EXPECT_EIGEN_NEAR(v_rot_num, v_rot_gen, 1.0e-15);
    EXPECT_EIGEN_NEAR(D_angle_num, D_angle_gen, 1.0e-15);

    // should still work without the optional arg
    evaluator(angle, {-5.5, 12.0}, v_rot_num, D_angle_num);
    gen::vector_rotation_2d(angle, Eigen::Vector2d{-5.5, 12.0}, v_rot_gen, nullptr);
    EXPECT_EIGEN_NEAR(v_rot_num, v_rot_gen, 1.0e-15);

    // Pass a map to the data:
    constexpr std::array<double, 2> input_v = {7.123, -4.001};
    const Eigen::Map<const Eigen::Vector2d> input_v_map(input_v.data());

    evaluator(angle, input_v_map, v_rot_num, D_angle_num);
    gen::vector_rotation_2d(angle, input_v_map, v_rot_gen, D_angle_gen);
    EXPECT_EIGEN_NEAR(v_rot_num, v_rot_gen, 1.0e-15);
    EXPECT_EIGEN_NEAR(D_angle_num, D_angle_gen, 1.0e-15);

    // pass a map for the output:
    evaluator(angle, {2.0, 3.0}, v_rot_num, D_angle_num);
    gen::vector_rotation_2d(angle, Eigen::Vector2d{2.0, 3.0}, v_rot_gen,
                            Eigen::Map<Eigen::Vector2d>(D_angle_gen.data()));
    EXPECT_EIGEN_NEAR(v_rot_num, v_rot_gen, 1.0e-15);
    EXPECT_EIGEN_NEAR(D_angle_num, D_angle_gen, 1.0e-15);
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

TEST(CppGenerationTest, TestSignum) {
  auto evaluator = create_evaluator(&signum_test);

  EXPECT_EQ(evaluator(0.0), gen::signum_test(0.0));
  EXPECT_EQ(0.0, gen::signum_test(0.0));
  EXPECT_EQ(0.0, gen::signum_test(-0.0));

  EXPECT_EQ(evaluator(1.0e-16), gen::signum_test(1.0e-16));
  EXPECT_EQ(1.0, gen::signum_test(1.0e-16));

  EXPECT_EQ(evaluator(-1.0e-16), gen::signum_test(-1.0e-16));
  EXPECT_EQ(-1.0, gen::signum_test(-1.0e-16));

  EXPECT_EQ(1.0, gen::signum_test(2.3));
  EXPECT_EQ(-1.0, gen::signum_test(-800.0));
}

TEST(CppGenerationTest, TestAbs) {
  auto evaluator = create_evaluator(&abs_test);

  EXPECT_EQ(evaluator(0.0), gen::abs_test(0.0));
  EXPECT_EQ(0.0, gen::abs_test(0.0));

  EXPECT_EQ(evaluator(1.0e-16), gen::abs_test(1.0e-16));
  EXPECT_EQ(1.0e-16, gen::abs_test(1.0e-16));

  EXPECT_EQ(evaluator(-1.0e-16), gen::abs_test(-1.0e-16));
  EXPECT_EQ(1.0e-16, gen::abs_test(-1.0e-16));

  EXPECT_EQ(2.3, gen::abs_test(2.3));
  EXPECT_EQ(800.0, gen::abs_test(-800.0));
}

TEST(CppGenerationTest, TestFloor) {
  EXPECT_EQ(0.0, gen::floor_test(0.0));
  EXPECT_EQ(0.0, gen::floor_test(0.123));
  EXPECT_EQ(1.0, gen::floor_test(1.9999999));
  EXPECT_EQ(3.0, gen::floor_test(3.14));
  EXPECT_EQ(-1.0, gen::floor_test(-0.01));
  EXPECT_EQ(-2.0, gen::floor_test(-1.25));
}

// Test that the generated functions call the STL equivalents.
TEST(CppGenerationTest, TestHyperbolicTrigFunctions) {
  EXPECT_EQ(std::cosh(1.6), gen::cosh_test(1.6));
  EXPECT_EQ(std::sinh(-1.3), gen::sinh_test(-1.3));
  EXPECT_EQ(std::tanh(0.22), gen::tanh_test(0.22));
  EXPECT_EQ(std::acosh(2.1), gen::acosh_test(2.1));
  EXPECT_EQ(std::asinh(-5.2), gen::asinh_test(-5.2));
  EXPECT_EQ(std::atanh(0.42), gen::atanh_test(0.42));
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

TEST(CppGenerationTest, TestQuaternionFromMatrix) {
  const double sqrt2inv = 1.0 / std::sqrt(2.0);
  const std::vector<Eigen::Quaterniond> qs = {
      {1.0, 0.0, 0.0, 0.0},
      {0.0, 1.0, 0.0, 0.0},
      {0.0, 0.0, 1.0, 0.0},
      {0.0, 0.0, 0.0, 1.0},
      {0.5, 0.5, 0.5, 0.5},
      {0.5, -0.5, 0.5, 0.5},
      {0.5, 0.5, -0.5, 0.5},
      {0.5, 0.5, 0.5, -0.5},
      {sqrt2inv, sqrt2inv, 0.0, 0.0},
      {-sqrt2inv, sqrt2inv, 0.0, 0.0},
      {sqrt2inv, -sqrt2inv, 0.0, 0.0},
      {0.0, sqrt2inv, sqrt2inv, 0.0},
      {0.0, -sqrt2inv, sqrt2inv, 0.0},
      {0.0, sqrt2inv, -sqrt2inv, 0.0},
      {0.0, 0.0, sqrt2inv, sqrt2inv},
      {0.0, 0.0, -sqrt2inv, sqrt2inv},
      {0.0, 0.0, sqrt2inv, -sqrt2inv},
      {sqrt2inv, 0.0, sqrt2inv, 0.0},
      {-sqrt2inv, 0.0, sqrt2inv, 0.0},
      {sqrt2inv, 0.0, -sqrt2inv, 0.0},
      {0.0, sqrt2inv, 0.0, sqrt2inv},
      {0.0, -sqrt2inv, 0.0, sqrt2inv},
      {0.0, sqrt2inv, 0.0, -sqrt2inv},
      {0.733810013024147, -0.6552231475913741, 0.10236796590444823, 0.14739840976937107},
  };
  for (const Eigen::Quaterniond& q : qs) {
    Eigen::Quaterniond q_out{};
    gen::quaternion_from_matrix<double>(q.toRotationMatrix(), q_out);
    EXPECT_EIGEN_NEAR(Eigen::Matrix3d::Identity(), (q_out.inverse() * q).toRotationMatrix(),
                      1.0e-14)
        << "q_out = " << q_out << "\nq = " << q;
  }
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
    return numeric::Point2d{get<const float_constant>(p.x).value(),
                            get<const float_constant>(p.y).value()};
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

TEST(CppGenerationTest, TestCustomType3) {
  numeric::Point2d p{};
  gen::custom_type_3<double>(p);
  EXPECT_EQ(M_PI * 0.5, p.x);
  EXPECT_EQ(M_E * 3.0, p.y);
}

// Convert to/from symbolic::Circle --> numeric::Circle.
template <>
struct custom_type_native_converter<symbolic::Circle> {
  using native_type = numeric::Circle;

  numeric::Circle operator()(const symbolic::Circle& p) const {
    return numeric::Circle{numeric::Point2d{get<const float_constant>(p.center.x).value(),
                                            get<const float_constant>(p.center.y).value()},
                           get<const float_constant>(p.radius).value()};
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

TEST(CppGenerationTest, TestExternalFunctionCall1) {
  // We can't create an evaluator for expressions with external functions, so just check against
  // numerical values.
  ASSERT_NEAR(-126.0, gen::external_function_call_1(3.0, -2.0), 1.0e-15);
  ASSERT_NEAR(-1000.0, gen::external_function_call_1(10.0, 0.0), 1.0e-15);
  ASSERT_NEAR(-0.5625, gen::external_function_call_1(-0.25, 0.5), 1.0e-15);
  ASSERT_NEAR(0.0, gen::external_function_call_1(0.0, 1.3), 1.0e-15);
  ASSERT_NEAR(0.0, gen::external_function_call_1(1.1, 5.0), 1.0e-15);
}

TEST(CppGenerationTest, TestExternalFunctionCall2) {
  ASSERT_NEAR(
      3.0,
      gen::external_function_call_2<double>(Eigen::Vector2d{1.0, -0.5}, Eigen::Vector2d{2.0, 3.0}),
      1.0e-15);
  ASSERT_NEAR(
      55.56,
      gen::external_function_call_2<double>(Eigen::Vector2d{10.0, 2.0}, Eigen::Vector2d{-2.0, 3.2}),
      1.0e-15);
  ASSERT_NEAR(
      5.6,
      gen::external_function_call_2<double>(Eigen::Vector2d{0.0, 1.2}, Eigen::Vector2d{-2.3, -1.0}),
      1.0e-15);
}

TEST(CppGenerationTest, TestExternalFunctionCall3) {
  ASSERT_EIGEN_NEAR(Eigen::Matrix2d::Zero(),
                    gen::external_function_call_3<double>(0.0, Eigen::Vector2d{-1.0, 1.0}),
                    1.0e-15);
  ASSERT_EIGEN_NEAR((Eigen::Matrix2d() << 2.0, 3.0, 2.0, 3.0).finished(),
                    gen::external_function_call_3<double>(1.0, Eigen::Vector2d{2.0, 3.0}), 1.0e-15);
  ASSERT_EIGEN_NEAR((Eigen::Matrix2d() << -1.5, 7.5, -4.5, 22.5).finished(),
                    gen::external_function_call_3<double>(3.0, Eigen::Vector2d{-0.5, 2.5}),
                    1.0e-15);
}

TEST(CppGenerationTest, TestExternalFunctionCall4) {
  ASSERT_NEAR(-2.0, gen::external_function_call_4(0.0, 0.0), 1.0e-15);
  ASSERT_NEAR(32.0, gen::external_function_call_4(10.0, 2.0), 1.0e-15);
  ASSERT_NEAR(-416.0, gen::external_function_call_4(-3.0, 13.0), 1.0e-15);
  ASSERT_NEAR(-12.5, gen::external_function_call_4(0.0, 2.5), 1.0e-15);
}

TEST(CppGenerationTest, TestExternalFunctionCall5) {
  ASSERT_EQ(1.0, gen::external_function_call_5({{0.0, 0.0}, 2.0}, 0.0, 0.0));
  ASSERT_EQ(1.0, gen::external_function_call_5({{-1.0, 2.0}, 4.0}, 0.5, -0.25));
  ASSERT_EQ(1.0, gen::external_function_call_5({{2.5, 0.5}, 0.1}, 2.75, 0.0));
  ASSERT_EQ(-1.0, gen::external_function_call_5({{3.0, 0.0}, 2.0}, -1.0, 0.0));
  ASSERT_EQ(-1.0, gen::external_function_call_5({{-5.0, -1.0}, 6.0}, 4.0, 2.0));
}

TEST(CppGenerationTest, TestExternalFunctionCall6) {
  ASSERT_EIGEN_NEAR(Eigen::Vector2d(3.0, -5.0),
                    gen::external_function_call_6(1.0, -0.5).to_vector(), 1.0e-15);
  ASSERT_EIGEN_NEAR(Eigen::Vector2d(146.25, 2.5),
                    gen::external_function_call_6(5.0, 3.25).to_vector(), 1.0e-15);
  ASSERT_EIGEN_NEAR(Eigen::Vector2d(0.0, -4.0), gen::external_function_call_6(0.0, 2.0).to_vector(),
                    1.0e-15);
  ASSERT_EIGEN_NEAR(Eigen::Vector2d(623.4375, -14.5),
                    gen::external_function_call_6(-3.5, 4.75).to_vector(), 1.0e-15);
}

TEST(CppGenerationTest, TestIntegerArgument1) {
  using return_type = decltype(gen::integer_argument_1(1, 2.0));
  static_assert(std::is_same_v<return_type, double>);
  ASSERT_EQ(2.0, gen::integer_argument_1(numeric::type_safe_int64_t(1), 2.0));
  ASSERT_EQ(67.2, gen::integer_argument_1(numeric::type_safe_int64_t(16), 4.2));
}

TEST(CppGenerationTest, TestIntegerOutputValues1) {
  std::int64_t baz;
  const auto f = gen::integer_output_values_1(numeric::type_safe_int64_t(4), 2.2, baz);
  static_assert(std::is_same_v<decltype(f), const std::int64_t>);
  ASSERT_EQ(static_cast<std::int64_t>(4 * 2.2), f);
  ASSERT_EQ(static_cast<std::int64_t>(static_cast<double>(4) - 2.2), baz);
}

// Test that generated code correctly casts integers explicitly.
TEST(CppGenerationTest, TestIntegerStructMember1) {
  ASSERT_EQ(0, gen::integer_struct_member_1(numeric::MixedNumerics{0.5, 0}, 2.0));
  ASSERT_EQ(std::sin(0.3) * -2, gen::integer_struct_member_1(numeric::MixedNumerics{0.3, -2}, 2.0));
  ASSERT_EQ(std::sin(-0.25) * 9,
            gen::integer_struct_member_1(numeric::MixedNumerics{-0.25, 9}, 0.0));
  // mode == 1
  ASSERT_EQ(std::cos(0.3), gen::integer_struct_member_1(numeric::MixedNumerics{0.3, 1}, 1.0));
  ASSERT_EQ(std::cos(0.3 * 1.8), gen::integer_struct_member_1(numeric::MixedNumerics{0.3, 1}, 1.8));
}

// Test that we can assign an integer struct member in an output value.
TEST(CppGenerationTest, TestIntegerStructMember2) {
  const auto a = gen::integer_struct_member_2(6.1, 0.5);
  ASSERT_EQ(static_cast<std::int64_t>(6.1 * 0.5), static_cast<std::int64_t>(a.mode));
  ASSERT_EQ(6.1 - 0.5, a.value);

  const auto b = gen::integer_struct_member_2(-1.3, 5.0);
  ASSERT_EQ(static_cast<std::int64_t>(-1.3 * 5.0), static_cast<std::int64_t>(b.mode));
  ASSERT_EQ(-1.3 - 5.0, b.value);
}

}  // namespace wf
