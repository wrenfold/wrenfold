// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <Eigen/Geometry>

#include "wf/collect.h"
#include "wf/expressions/multiplication.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/functions.h"
#include "wf/geometry/quaternion.h"
#include "wf/matrix_functions.h"
#include "wf/substitute.h"

#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numerical_jacobian.h"
#include "wf_test_support/test_macros.h"

namespace wf {
using namespace wf::custom_literals;

TEST(QuaternionTest, TestConstructor) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  quaternion q{w, x, y, z};
  ASSERT_IDENTICAL(w, q.w());
  ASSERT_IDENTICAL(x, q.x());
  ASSERT_IDENTICAL(y, q.y());
  ASSERT_IDENTICAL(z, q.z());

  // Default construction:
  quaternion q_identity{};
  ASSERT_IDENTICAL(0, q_identity.x());
  ASSERT_IDENTICAL(0, q_identity.y());
  ASSERT_IDENTICAL(0, q_identity.z());
  ASSERT_IDENTICAL(1, q_identity.w());

  ASSERT_TRUE(quaternion::from_vector_wxyz(make_vector(w, x, y, z)).is_identical_to(q));
  ASSERT_TRUE(quaternion::from_vector_xyzw(make_vector(x, y, z, w)).is_identical_to(q));
  ASSERT_IDENTICAL(make_vector(x, y, z, w),
                   quaternion::from_vector_xyzw(make_vector(x, y, z, w)).to_vector_xyzw());

  ASSERT_THROW(quaternion::from_vector_wxyz(make_vector(1, 2, 3, 4, 5)), wf::dimension_error);
  ASSERT_THROW(quaternion::from_vector_xyzw(make_vector(1, 2, y)), wf::dimension_error);

  quaternion q_named = quaternion::from_name_prefix("q");
  ASSERT_IDENTICAL(scalar_expr{"q_x"}, q_named.x());
  ASSERT_IDENTICAL(scalar_expr{"q_y"}, q_named.y());
  ASSERT_IDENTICAL(scalar_expr{"q_z"}, q_named.z());
  ASSERT_IDENTICAL(scalar_expr{"q_w"}, q_named.w());
}

TEST(QuaternionTest, TestIsIdenticalTo) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  const quaternion q1{w, x, y, z};
  ASSERT_TRUE(q1.is_identical_to(q1));
  ASSERT_TRUE(q1.is_identical_to({q1.w(), q1.x(), q1.y(), q1.z()}));
  ASSERT_FALSE(q1.is_identical_to({1, 0, 0, 0}));
  ASSERT_FALSE(q1.is_identical_to({q1.w(), 2, q1.y(), q1.z()}));
  ASSERT_FALSE(q1.is_identical_to({q1.w(), q1.x(), 2, q1.z()}));
  ASSERT_FALSE(q1.is_identical_to({q1.w(), q1.x(), q1.y(), 2}));
}

TEST(QuaternionTest, TestNorm) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  const quaternion q{w, x, y, z};
  ASSERT_IDENTICAL(w * w + x * x + y * y + z * z, q.squared_norm());
  ASSERT_IDENTICAL(sqrt(w * w + x * x + y * y + z * z), q.norm());
  ASSERT_IDENTICAL(1, quaternion().norm());
  ASSERT_IDENTICAL(pow(506_s / 49, 1_s / 2), quaternion(3, -4_s / 7, 0, 1).norm());
  ASSERT_IDENTICAL(2 * sqrt(pow(x, 2)), quaternion(x, x, x, x).norm());
}

TEST(QuaternionTest, TestNormalize) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  const quaternion q{w, x, y, z};

  const auto q_normalized = q.normalized();
  ASSERT_IDENTICAL(q.w() / q.norm(), q_normalized.w());
  ASSERT_IDENTICAL(q.x() / q.norm(), q_normalized.x());
  ASSERT_IDENTICAL(q.y() / q.norm(), q_normalized.y());
  ASSERT_IDENTICAL(q.z() / q.norm(), q_normalized.z());

  // We can demonstrate that the norm is one:
  ASSERT_IDENTICAL(1, q.normalized().norm().collect(q.squared_norm()));
}

TEST(QuaternionTest, TestConjugate) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  const quaternion q{w, x, y, z};

  const quaternion q_conj = q.conjugate();
  ASSERT_IDENTICAL(q_conj.w(), q.w());
  ASSERT_IDENTICAL(-q_conj.x(), q.x());
  ASSERT_IDENTICAL(-q_conj.y(), q.y());
  ASSERT_IDENTICAL(-q_conj.z(), q.z());

  ASSERT_IDENTICAL(quaternion().to_vector_wxyz(), quaternion().conjugate().to_vector_wxyz());
}

TEST(QuaternionTest, TestMultiply) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  auto [a, b, c, d] = make_symbols("a", "b", "c", "d");

  const quaternion q1{w, x, y, z};
  const quaternion q2{a, b, c, d};

  // Compare to matrix multiplication:
  // clang-format off
  const matrix_expr q1_mat =
      make_matrix(4, 4,
                    w, -x, -y, -z,
                    x,  w, -z,  y,
                    y,  z,  w, -x,
                    z, -y,  x,  w);
  // clang-format on
  ASSERT_IDENTICAL(q1_mat * q2.to_vector_wxyz(), (q1 * q2).to_vector_wxyz());

  // clang-format off
  const matrix_expr q2_mat =
      make_matrix(4, 4,
                   a, -b, -c, -d,
                   b,  a, -d,  c,
                   c,  d,  a, -b,
                   d, -c,  b,  a);
  // clang-format on
  ASSERT_IDENTICAL(q2_mat * q1.to_vector_wxyz(), (q2 * q1).to_vector_wxyz());

  // Make sure we are consistent w/ Eigen multiplication order:
  const Eigen::Quaternion<double> q1_num{
      0.65328148,
      -0.65328148,
      0.27059805,
      -0.27059805,
  };
  const Eigen::Quaternion<double> q2_num{0.8923991, 0.23911762, 0.09904576, 0.36964381};

  // TODO: Add a mechanism for subbing multiple things at once:
  const quaternion result = (q1 * q2)
                                .subs(q1.w(), q1_num.w())
                                .subs(q1.x(), q1_num.x())
                                .subs(q1.y(), q1_num.y())
                                .subs(q1.z(), q1_num.z())
                                .subs(q2.w(), q2_num.w())
                                .subs(q2.x(), q2_num.x())
                                .subs(q2.y(), q2_num.y())
                                .subs(q2.z(), q2_num.z());
  // Won't match exactly due to floating point order:
  ASSERT_NEAR((q1_num * q2_num).w(), get<const float_constant>(result.w()).value(), 1.0e-15);
  ASSERT_NEAR((q1_num * q2_num).x(), get<const float_constant>(result.x()).value(), 1.0e-15);
  ASSERT_NEAR((q1_num * q2_num).y(), get<const float_constant>(result.y()).value(), 1.0e-15);
  ASSERT_NEAR((q1_num * q2_num).z(), get<const float_constant>(result.z()).value(), 1.0e-15);
}

TEST(QuaternionTest, TestInverse) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  const quaternion q{w, x, y, z};

  const scalar_expr q_norm_2 = q.squared_norm();
  const quaternion q_q_inv = q * q.inverse();

  // Workaround for limitations in collect/subs:
  const scalar_expr negative_q_norm_2{multiplication{-1, q_norm_2}};
  ASSERT_IDENTICAL(1, q_q_inv.subs(-q_norm_2, negative_q_norm_2).w().collect(q_norm_2));
  ASSERT_IDENTICAL(0, q_q_inv.subs(-q_norm_2, negative_q_norm_2).x());
  ASSERT_IDENTICAL(0, q_q_inv.subs(-q_norm_2, negative_q_norm_2).y());
  ASSERT_IDENTICAL(0, q_q_inv.subs(-q_norm_2, negative_q_norm_2).z());
}

TEST(QuaternionTest, TestToRotationMatrix) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  const quaternion q{w, x, y, z};

  ASSERT_IDENTICAL(make_identity(3), quaternion().to_rotation_matrix());
  ASSERT_IDENTICAL(make_identity(3), quaternion(-1, 0, 0, 0).to_rotation_matrix());

  // Check the matrix against rotating the vector w/ quaternion:
  auto [a, b, c] = make_symbols("a", "b", "c");
  auto v = make_vector(a, b, c);

  const quaternion q_v_q_conj = q * quaternion(0, a, b, c) * q.conjugate();
  const matrix_expr R_v_expected = make_vector(q_v_q_conj.x(), q_v_q_conj.y(), q_v_q_conj.z());
  const matrix_expr R_v = q.to_rotation_matrix() * v;

  // If we did everything correctly, there should be no difference between these.
  // We need to provide a small hint, and enforce that |q|^2 = 1
  ASSERT_IDENTICAL(0,
                   collect((R_v_expected[0] - R_v[0]).distribute(), a).subs(q.squared_norm(), 1));
  ASSERT_IDENTICAL(0,
                   collect((R_v_expected[1] - R_v[1]).distribute(), b).subs(q.squared_norm(), 1));
  ASSERT_IDENTICAL(0,
                   collect((R_v_expected[2] - R_v[2]).distribute(), c).subs(q.squared_norm(), 1));

  // Conjugate and transpose should match:
  ASSERT_IDENTICAL(q.conjugate().to_rotation_matrix(), q.to_rotation_matrix().transposed());
}

TEST(QuaternionTest, TestFromAxisAngle) {
  auto [angle, vx, vy, vz] = make_symbols("theta", "vx", "vy", "vz");
  const quaternion q = quaternion::from_angle_axis(angle, vx, vy, vz);
  ASSERT_IDENTICAL(q.w(), cos(angle / 2));
  ASSERT_IDENTICAL(q.x(), vx * sin(angle / 2));
  ASSERT_IDENTICAL(q.y(), vy * sin(angle / 2));
  ASSERT_IDENTICAL(q.z(), vz * sin(angle / 2));
  ASSERT_TRUE(q.is_identical_to(quaternion::from_angle_axis(angle, make_vector(vx, vy, vz))));

  // Show that the norm is one.
  // TODO: Have the trig simplification cos^2(x) + sin^2(x) = 1 be automatic.
  const scalar_expr half_angle = angle / 2;
  const scalar_expr q_norm_2 = collect(q.squared_norm(), sin(half_angle));
  ASSERT_IDENTICAL(1, q_norm_2.subs(vx * vx + vy * vy + vz * vz, 1)
                          .subs(pow(cos(half_angle), 2) + pow(sin(half_angle), 2), 1));

  // V should be unmodified by the rotation matrix
  const matrix_expr v_rot = q.to_rotation_matrix() * make_vector(vx, vy, vz);
  ASSERT_IDENTICAL(make_vector(vx, vy, vz), v_rot.distribute());

  // Compare to Rodrigues formula:
  // We need a couple of trig identities to do this:
  //  cos(theta/2)*sin(theta/2) --> sin(theta) / 2
  //  sin(theta/2)**2 --> (1 - cos(theta)) / 2
  const matrix_expr R = q.to_rotation_matrix()
                            .subs(cos(half_angle) * sin(half_angle), sin(angle) / 2)
                            .subs(sin(half_angle) * sin(half_angle), (1 - cos(angle)) / 2)
                            .distribute();

  // clang-format off
  const matrix_expr K = make_matrix(3, 3,
                                     0, -vz, vy,
                                     vz, 0, -vx,
                                    -vy, vx, 0);
  // clang-format on
  const matrix_expr R_rodrigues = make_identity(3) + K * sin(angle) + (1 - cos(angle)) * K * K;
  ASSERT_IDENTICAL(R_rodrigues.distribute(), R);

  // check that this throws if we pass invalid arguments:
  ASSERT_THROW(quaternion::from_angle_axis(0, make_vector(vx, vy)), dimension_error);
}

// Sample points uniformly on the sphere (approximately) using fibonacci sphere.
Eigen::Vector3d fib_sphere(const int i, const int n) {
  const double ratio = (1 + std::sqrt(5.0)) / 2;
  const double theta = 2 * M_PI * static_cast<double>(i) / ratio;
  const double phi = std::acos(1.0 - 2.0 * (static_cast<double>(i) + 0.5) / static_cast<double>(n));
  return {std::cos(theta) * std::sin(phi), std::sin(theta) * std::sin(phi), std::cos(phi)};
}

auto generate_angle_axis_test_pairs(const int num_vectors, const int num_angles) {
  using Eigen::Vector3d;
  std::vector<std::tuple<double, Vector3d>> test_pairs;
  for (int i = 0; i < num_vectors; ++i) {
    const Vector3d axis = fib_sphere(i, num_vectors);
    for (int j = 0; j < num_angles; ++j) {
      const double angle_num =
          (static_cast<double>(j) + 0.5) / static_cast<double>(num_angles) * 2 * M_PI - M_PI;
      test_pairs.emplace_back(angle_num, axis);
    }
  }
  return test_pairs;
}

// Generate a bunch of angle-axis test pairs:
auto get_angle_axis_test_pairs() {
  using Eigen::Vector3d;

  // clang-format off
  std::vector<std::tuple<double, Vector3d>> test_pairs = {
      // some simple rotations:
      std::make_tuple(M_PI / 6, Vector3d::UnitX()),
      std::make_tuple(M_PI / 4, Vector3d::UnitX()),
      std::make_tuple(M_PI / 2, Vector3d::UnitX()),
      std::make_tuple(3 * M_PI / 4, Vector3d::UnitX()),
      std::make_tuple(M_PI / 6, Vector3d::UnitY()),
      std::make_tuple(M_PI / 4, Vector3d::UnitY()),
      std::make_tuple(M_PI / 2, Vector3d::UnitY()),
      std::make_tuple(3 * M_PI / 4, Vector3d::UnitY()),
      std::make_tuple(M_PI / 6, Vector3d::UnitZ()),
      std::make_tuple(M_PI / 4, Vector3d::UnitZ()),
      std::make_tuple(M_PI / 2, Vector3d::UnitZ()),
      std::make_tuple(3 * M_PI / 4, Vector3d::UnitZ()),
      // zero rotation:
      std::make_tuple(0.0, Vector3d::UnitX()),
      // 180 degrees:
      std::make_tuple(M_PI, Vector3d::UnitX()),
  };
  // clang-format on

  // Add a bunch of pseudo-random rotations for good measure.
  // TODO: Turn these numbers up when building in release mode.
  constexpr int num_vectors = 25;
  constexpr int num_angles = 10;
  auto pseudo_random_pairs = generate_angle_axis_test_pairs(num_vectors, num_angles);
  test_pairs.insert(test_pairs.end(), pseudo_random_pairs.begin(), pseudo_random_pairs.end());

  // Insert some small angle versions:
  for (const auto& [angle, axis] : pseudo_random_pairs) {
    test_pairs.emplace_back(angle * 1.0e-18, axis);
  }
  return test_pairs;
}

TEST(QuaternionTest, FromRotationVector) {
  auto [vx, vy, vz] = make_symbols("vx", "vy", "vz");
  const scalar_expr angle = sqrt(vx * vx + vy * vy + vz * vz);
  const scalar_expr half_angle = angle / 2;
  const quaternion q_no_conditional = quaternion::from_rotation_vector(vx, vy, vz, std::nullopt);
  ASSERT_IDENTICAL(q_no_conditional.w(), cos(angle / 2));
  ASSERT_IDENTICAL(q_no_conditional.x(), vx * sin(angle / 2) / angle);
  ASSERT_IDENTICAL(q_no_conditional.y(), vy * sin(angle / 2) / angle);
  ASSERT_IDENTICAL(q_no_conditional.z(), vz * sin(angle / 2) / angle);

  const quaternion q = quaternion::from_rotation_vector(vx, vy, vz, 1.0e-16);

  // Check that angle of norm 0 works and produces identity:
  const quaternion q_ident = q.subs(vx, 0).subs(vy, 0).subs(vz, 0);
  ASSERT_IDENTICAL(1, q_ident.w());
  ASSERT_IDENTICAL(0, q_ident.x());
  ASSERT_IDENTICAL(0, q_ident.y());
  ASSERT_IDENTICAL(0, q_ident.z());

  // Do some numerical tests.
  // Due to conditional logic we can't compare to rodrigues yet.
  for (auto [angle_num, axis_num] : get_angle_axis_test_pairs()) {
    const Eigen::Quaterniond q_num{Eigen::AngleAxisd{angle_num, axis_num}};
    EXPECT_EIGEN_NEAR(q_num.toRotationMatrix(),
                      eigen_matrix_from_matrix_expr(q.subs(vx, axis_num.x() * angle_num)
                                                        .subs(vy, axis_num.y() * angle_num)
                                                        .subs(vz, axis_num.z() * angle_num)
                                                        .to_rotation_matrix()),
                      1.0e-15);
  }

  // Test a couple of very small values:
  EXPECT_EIGEN_NEAR(
      eigen_matrix_from_matrix_expr(
          q.subs(vx, 1.0e-19).subs(vy, -1.2e-22).subs(vz, 3.0e-18).to_vector_wxyz()),
      eigen_matrix_from_matrix_expr(
          q_no_conditional.subs(vx, 1.0e-19).subs(vy, -1.2e-22).subs(vz, 3.0e-18).to_vector_wxyz()),
      1.0e-32);

  EXPECT_EIGEN_NEAR(eigen_matrix_from_matrix_expr(
                        q.subs(vx, -2.3e-17).subs(vy, 4.1e-18).subs(vz, -3.3e-17).to_vector_wxyz()),
                    eigen_matrix_from_matrix_expr(q_no_conditional.subs(vx, -2.3e-17)
                                                      .subs(vy, 4.1e-18)
                                                      .subs(vz, -3.3e-17)
                                                      .to_vector_wxyz()),
                    1.0e-32);

  ASSERT_THROW(quaternion::from_rotation_vector(make_vector(-3, vx), std::nullopt),
               dimension_error);
  ASSERT_THROW(quaternion::from_rotation_vector(make_identity(3), std::nullopt), dimension_error);
}

TEST(QuaternionTest, TestAngleConversions) {
  auto [angle] = make_symbols("theta");
  const scalar_expr half_angle = angle / 2;
  // clang-format off
  const matrix_expr R_x =
      make_matrix(3, 3,
                   1, 0, 0,
                   0, cos(angle), -sin(angle),
                   0, sin(angle), cos(angle));
  // clang-format on
  ASSERT_IDENTICAL(R_x, quaternion::from_x_angle(angle)
                            .to_rotation_matrix()
                            .subs(sin(half_angle) * sin(half_angle), (1 - cos(angle)) / 2)
                            .subs(cos(half_angle) * sin(half_angle), sin(angle) / 2)
                            .distribute());

  // clang-format off
  const matrix_expr R_y =
      make_matrix(3, 3,
                   cos(angle), 0, sin(angle),
                   0, 1, 0,
                   -sin(angle), 0, cos(angle));
  // clang-format on
  ASSERT_IDENTICAL(R_y, quaternion::from_y_angle(angle)
                            .to_rotation_matrix()
                            .subs(sin(half_angle) * sin(half_angle), (1 - cos(angle)) / 2)
                            .subs(cos(half_angle) * sin(half_angle), sin(angle) / 2)
                            .distribute());

  // clang-format off
  const matrix_expr R_z =
      make_matrix(3, 3,
                   cos(angle), -sin(angle), 0,
                   sin(angle), cos(angle), 0,
                   0, 0, 1);
  // clang-format on
  ASSERT_IDENTICAL(R_z, quaternion::from_z_angle(angle)
                            .to_rotation_matrix()
                            .subs(sin(half_angle) * sin(half_angle), (1 - cos(angle)) / 2)
                            .subs(cos(half_angle) * sin(half_angle), sin(angle) / 2)
                            .distribute());
}

// Because we don't yet have assumptions, the easiest way to test this for now is numerical:
TEST(QuaternionTest, TestToAxisAngle) {
  using Eigen::Vector3d;

  auto [angle, x, y, z] = make_symbols("angle", "x", "y", "z");
  const quaternion q = quaternion::from_angle_axis(angle, x, y, z);

  {
    auto [angle_recovered, axis_recovered] = q.to_angle_axis(0);
    ASSERT_IDENTICAL(0, angle_recovered.subs(angle, 0));
    ASSERT_IDENTICAL(0, angle_recovered.subs(x, 0).subs(y, 0).subs(z, 0).subs(angle, 0));
    ASSERT_IDENTICAL(make_vector(1, 0, 0), axis_recovered.subs(angle, 0));
    ASSERT_IDENTICAL(make_vector(1, 0, 0), axis_recovered.subs(x, 0).subs(y, 0).subs(z, 0));
  }

  for (auto [angle_num, axis_num] : get_angle_axis_test_pairs()) {
    if (angle_num < 0.0) {
      // Our solution will always be in [0, pi]
      angle_num *= -1.0;
      axis_num *= -1.0;
    }
    auto [angle_recovered, axis_recovered] =
        std::abs(angle_num) > 1.0e-3 ? q.to_angle_axis(std::nullopt) : q.to_angle_axis(0);

    EXPECT_NEAR(angle_num,
                get<const float_constant>(angle_recovered.subs(angle, angle_num)
                                              .subs(x, axis_num.x())
                                              .subs(y, axis_num.y())
                                              .subs(z, axis_num.z())
                                              .eval())
                    .value(),
                1.0e-15)
        << fmt::format("While testing axis = {}, angle = {}", axis_num.transpose(), angle_num);

    const Eigen::Vector3d axis_recovered_num =
        eigen_matrix_from_matrix_expr(axis_recovered.subs(angle, angle_num)
                                          .subs(x, axis_num.x())
                                          .subs(y, axis_num.y())
                                          .subs(z, axis_num.z()));
    EXPECT_EIGEN_NEAR(axis_num, axis_recovered_num, 1.0e-15)
        << fmt::format("While testing axis = {}, angle = {}", axis_num.transpose(), angle_num);
  }

  // Test analytically zero case:
  const auto [s0, v0] = quaternion().to_angle_axis(std::nullopt);
  ASSERT_IDENTICAL(0, s0);
  ASSERT_IDENTICAL(make_vector(1, 0, 0), v0);
}

TEST(QuaternionTest, TestToRotationVector) {
  const scalar_expr x{"x", number_set::real};
  const scalar_expr y{"y", number_set::real};
  const scalar_expr z{"z", number_set::real};
  const quaternion Q = quaternion::from_rotation_vector(x, y, z, std::nullopt);
  const matrix_expr w = Q.to_rotation_vector(std::nullopt);

  // We sub this in for the angle (the norm of [x, y, z]).
  const scalar_expr a{"a", number_set::real_non_negative};

  // Sin and cosine of the half-angle:
  const scalar_expr s{"s", number_set::real};
  const scalar_expr c{"c", number_set::real};

  // Simplify the round-trip conversion.
  const scalar_expr vector_norm = sqrt(x * x + y * y + z * z);
  const matrix_expr w_simplified = w.subs(vector_norm, a)
                                       .subs(sin(a / 2), s)
                                       .subs(cos(a / 2), c)
                                       .collect({s, a})
                                       .subs(vector_norm, a)
                                       .subs(pow(pow(s, 2), 1_s / 2), abs(s))
                                       .subs(s / abs(s), signum(s));

  // We can't simplify this completely w/o some trig-simp functionality. We can get close though:
  // Since atan2(|s|, c) --> |a/2|, we have 2 * |a/2| * signum(a) / a, which is 1.
  ASSERT_IDENTICAL(make_matrix(3, 1, x, y, z) *
                       (2 * atan2(abs(s), abs(c)) * where(c < 0, -1, 1) * signum(s) / a),
                   w_simplified);

  // Check numerically as well:
  const auto w_num = quaternion::from_rotation_vector(x, y, z, 0).to_rotation_vector(1.0e-16);
  for (auto [angle_num, axis_num] : get_angle_axis_test_pairs()) {
    const std::array<scalar_or_boolean_pair, 3> pairs = {
        std::make_tuple(x, angle_num * axis_num.x()), std::make_tuple(y, angle_num * axis_num.y()),
        std::make_tuple(z, angle_num * axis_num.z())};
    const Eigen::Vector3d axis_recovered_num =
        eigen_matrix_from_matrix_expr(substitute(w_num, pairs).eval());
    EXPECT_EIGEN_NEAR(axis_num * angle_num, axis_recovered_num, 1.0e-15 * std::abs(angle_num))
        << fmt::format("While testing axis = {}, angle = {}", axis_num.transpose(), angle_num);
  }

  // Check with negative sign on `q.w`.
  // We construct the quaternion, and then negate it and convert back to rotation vector.
  const auto negated_q_w_num =
      (-quaternion::from_rotation_vector(x, y, z, 0)).to_rotation_vector(1.0e-16);
  for (auto [angle_num, axis_num] : get_angle_axis_test_pairs()) {
    const std::array<scalar_or_boolean_pair, 3> pairs = {
        std::make_tuple(x, angle_num * axis_num.x()), std::make_tuple(y, angle_num * axis_num.y()),
        std::make_tuple(z, angle_num * axis_num.z())};
    const Eigen::Vector3d axis_recovered_num =
        eigen_matrix_from_matrix_expr(substitute(negated_q_w_num, pairs).eval());
    EXPECT_EIGEN_NEAR(axis_num * angle_num, axis_recovered_num, 1.0e-15 * std::abs(angle_num))
        << fmt::format("While testing axis = {}, angle = {}", axis_num.transpose(), angle_num);
  }

  // Check the alternative formulation:
  const auto w_num_acos =
      quaternion::from_rotation_vector(x, y, z, 0).to_rotation_vector(1.0e-16, false);

  for (auto [angle_num, axis_num] : get_angle_axis_test_pairs()) {
    const std::array<scalar_or_boolean_pair, 3> pairs = {
        std::make_tuple(x, angle_num * axis_num.x()), std::make_tuple(y, angle_num * axis_num.y()),
        std::make_tuple(z, angle_num * axis_num.z())};
    const Eigen::Vector3d axis_recovered_num =
        eigen_matrix_from_matrix_expr(substitute(w_num_acos, pairs).eval());
    EXPECT_EIGEN_NEAR(axis_num * angle_num, axis_recovered_num, 1.0e-14 * std::abs(angle_num))
        << fmt::format("While testing axis = {}, angle = {}", axis_num.transpose(), angle_num);
  }

  // Check alternative formulation with negated w:
  const auto negated_q_w_num_acos =
      (-quaternion::from_rotation_vector(x, y, z, 0)).to_rotation_vector(1.0e-16, false);
  for (auto [angle_num, axis_num] : get_angle_axis_test_pairs()) {
    const std::array<scalar_or_boolean_pair, 3> pairs = {
        std::make_tuple(x, angle_num * axis_num.x()), std::make_tuple(y, angle_num * axis_num.y()),
        std::make_tuple(z, angle_num * axis_num.z())};
    const Eigen::Vector3d axis_recovered_num =
        eigen_matrix_from_matrix_expr(substitute(negated_q_w_num_acos, pairs).eval());
    EXPECT_EIGEN_NEAR(axis_num * angle_num, axis_recovered_num, 1.0e-14 * std::abs(angle_num))
        << fmt::format("While testing axis = {}, angle = {}", axis_num.transpose(), angle_num);
  }
}

TEST(QuaternionTest, TestFromRotationMatrix) {
  const quaternion q_ident = quaternion::from_rotation_matrix(make_identity(3));
  ASSERT_IDENTICAL(1, q_ident.w());
  ASSERT_IDENTICAL(0, q_ident.x());
  ASSERT_IDENTICAL(0, q_ident.y());
  ASSERT_IDENTICAL(0, q_ident.z());

  // clang-format off
  const quaternion q_pi_over_2_x =
      quaternion::from_rotation_matrix(make_matrix(3, 3,
        1, 0, 0,
        0, 0, -1,
        0, 1, 0));
  // clang-format on
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_x.w());
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_x.x());
  ASSERT_IDENTICAL(0, q_pi_over_2_x.y());
  ASSERT_IDENTICAL(0, q_pi_over_2_x.z());

  const quaternion q_pi_x =
      quaternion::from_rotation_matrix(make_matrix(3, 3, 1, 0, 0, 0, -1, 0, 0, 0, -1));
  ASSERT_IDENTICAL(0, q_pi_x.w());
  ASSERT_IDENTICAL(1, q_pi_x.x());
  ASSERT_IDENTICAL(0, q_pi_x.y());
  ASSERT_IDENTICAL(0, q_pi_x.z());

  // clang-format off
  const quaternion q_pi_over_2_y =
      quaternion::from_rotation_matrix(make_matrix(3, 3,
         0, 0, 1,
         0, 1, 0,
        -1, 0, 0));
  // clang-format on
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_y.w());
  ASSERT_IDENTICAL(0, q_pi_over_2_y.x());
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_y.y());
  ASSERT_IDENTICAL(0, q_pi_over_2_y.z());

  const quaternion q_pi_y =
      quaternion::from_rotation_matrix(make_matrix(3, 3, -1, 0, 0, 0, 1, 0, 0, 0, -1));
  ASSERT_IDENTICAL(0, q_pi_y.w());
  ASSERT_IDENTICAL(0, q_pi_y.x());
  ASSERT_IDENTICAL(1, q_pi_y.y());
  ASSERT_IDENTICAL(0, q_pi_y.z());

  // clang-format off
  const quaternion q_pi_over_2_z =
      quaternion::from_rotation_matrix(make_matrix(3, 3,
         0, -1, 0,
         1,  0, 0,
         0,  0, 1));
  // clang-format on
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_z.w());
  ASSERT_IDENTICAL(0, q_pi_over_2_z.x());
  ASSERT_IDENTICAL(0, q_pi_over_2_z.y());
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_z.z());

  const quaternion q_pi_z =
      quaternion::from_rotation_matrix(make_matrix(3, 3, -1, 0, 0, 0, -1, 0, 0, 0, 1));
  ASSERT_IDENTICAL(0, q_pi_z.w());
  ASSERT_IDENTICAL(0, q_pi_z.x());
  ASSERT_IDENTICAL(0, q_pi_z.y());
  ASSERT_IDENTICAL(1, q_pi_z.z());

  const quaternion q_1{0, -1 / sqrt(2_s), 1 / sqrt(2_s), 0};
  const quaternion q_1_out = quaternion::from_rotation_matrix(q_1.to_rotation_matrix());

  // Sign is flipped, but the underlying rotation is the same.
  ASSERT_IDENTICAL(q_1.w(), -q_1_out.w());
  ASSERT_IDENTICAL(q_1.x(), -q_1_out.x());
  ASSERT_IDENTICAL(q_1.y(), -q_1_out.y());
  ASSERT_IDENTICAL(q_1.z(), -q_1_out.z());
  ASSERT_IDENTICAL(q_1.to_rotation_matrix(), q_1_out.to_rotation_matrix());

  // round-trip test a bunch of edge cases:
  // clang-format off
  const std::vector<quaternion> qs = {
      {0, 1 / sqrt(2_s), 1 / sqrt(2_s), 0},
      {0, -1 / sqrt(2_s), 1 / sqrt(2_s), 0},
      {0, 1 / sqrt(2_s), -1 / sqrt(2_s), 0},
      {0, 0, 1 / sqrt(2_s), 1 / sqrt(2_s)},
      {0, 0, -1 / sqrt(2_s), 1 / sqrt(2_s)},
      {0, 0, 1 / sqrt(2_s), -1 / sqrt(2_s)},
      {1 / sqrt(2_s), 1 / sqrt(2_s), 0, 0},
      {-1 / sqrt(2_s), 1 / sqrt(2_s), 0, 0},
      {1 / sqrt(2_s), -1 / sqrt(2_s), 0, 0},
  };
  // clang-format on
  for (const quaternion& q : qs) {
    ASSERT_IDENTICAL(q.to_rotation_matrix(),
                     quaternion::from_rotation_matrix(q.to_rotation_matrix()).to_rotation_matrix());
  }

  // TODO: With a bit more work on collect(), I could analytically demonstrate the mapping:
  //  S(4) -> SO(3) -> S(4)
  //  For now I'll just test this numerically.

  auto cast_to_float = [](const scalar_expr& expr) -> double {
    if (expr.is_type<float_constant>()) {
      return get<const float_constant>(expr).value();
    }
    return static_cast<double>(get<const integer_constant>(expr).value());
  };

  constexpr int num_vectors = 75;
  constexpr int num_angles = 20;
  for (auto [angle_num, axis_num] : generate_angle_axis_test_pairs(num_vectors, num_angles)) {
    const Eigen::Quaterniond q_num{Eigen::AngleAxisd(angle_num, axis_num)};

    const matrix_expr R =
        quaternion{q_num.w(), q_num.x(), q_num.y(), q_num.z()}.to_rotation_matrix();
    const quaternion q = quaternion::from_rotation_matrix(R);

    // Compute the sign flip so that signs match:
    Eigen::Index max_index{};
    q_num.coeffs().cwiseAbs().maxCoeff(&max_index);
    const double sign = q_num.coeffs()[max_index] /
                        cast_to_float(q.to_vector_xyzw()[static_cast<wf::index_t>(max_index)]);

    ASSERT_NEAR(q_num.w(), sign * cast_to_float(q.w()), 1.0e-15)
        << fmt::format("q_num = [{}, {}, {}, {}]\nq = {}\nR:\n{}", q_num.w(), q_num.x(), q_num.y(),
                       q_num.z(), q.to_vector_wxyz().transposed(), R);
    ASSERT_NEAR(q_num.x(), sign * cast_to_float(q.x()), 1.0e-15)
        << fmt::format("q_num = [{}, {}, {}, {}]\nq = {}\nR:\n{}", q_num.w(), q_num.x(), q_num.y(),
                       q_num.z(), q.to_vector_wxyz().transposed(), R);
    ASSERT_NEAR(q_num.y(), sign * cast_to_float(q.y()), 1.0e-15)
        << fmt::format("q_num = [{}, {}, {}, {}]\nq = {}\nR:\n{}", q_num.w(), q_num.x(), q_num.y(),
                       q_num.z(), q.to_vector_wxyz().transposed(), R);
    ASSERT_NEAR(q_num.z(), sign * cast_to_float(q.z()), 1.0e-15)
        << fmt::format("q_num = [{}, {}, {}, {}]\nq = {}\nR:\n{}", q_num.w(), q_num.x(), q_num.y(),
                       q_num.z(), q.to_vector_wxyz().transposed(), R);
  }

  // Invalid dimensions:
  ASSERT_THROW(quaternion::from_rotation_matrix(make_zeros(2, 3)), wf::dimension_error);
}

// Test calling `jacobian` directly on a quaternion.
TEST(QuaternionTest, TestJacobian) {
  const auto [x, y, z, angle] = make_symbols("x", "y", "z", "angle");
  const auto Q = quaternion::from_angle_axis(angle, x, y, z).inverse();

  // swap the order around:
  const auto vars = make_vector(y, x, angle);
  const matrix_expr J = Q.jacobian(vars);
  ASSERT_EQ(4, J.rows());
  ASSERT_EQ(3, J.cols());

  for (index_t i = 0; i < J.rows(); ++i) {
    for (index_t j = 0; j < J.cols(); ++j) {
      ASSERT_IDENTICAL(
          Q.wxyz()[static_cast<std::size_t>(i)].diff(vars[static_cast<std::size_t>(j)]), J(i, j));
    }
  }
}

TEST(QuaternionTest, TestRightRetractDerivative) {
  const auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  const auto J = quaternion{w, x, y, z}.right_retract_derivative();

  for (const auto& [angle, axis] : get_angle_axis_test_pairs()) {
    const Eigen::Quaterniond q_num{Eigen::AngleAxisd(angle, axis)};
    const Eigen::Matrix<double, 4, 3> J_numerical = numerical_jacobian(
        Eigen::Vector3d::Zero(),
        [&](const Eigen::Vector3d& dw) -> Eigen::Vector4d {
          const Eigen::AngleAxisd aa{dw.norm(), dw.normalized()};
          return eigen_wxyz_vec_from_quaternion(q_num * static_cast<Eigen::Quaterniond>(aa));
        },
        0.001);

    const std::array<scalar_or_boolean_pair, 4> pairs = {
        std::make_pair(w, q_num.w()), std::make_pair(x, q_num.x()), std::make_pair(y, q_num.y()),
        std::make_pair(z, q_num.z())};

    const matrix_expr J_analytical = substitute(J, pairs);
    EXPECT_EIGEN_NEAR(J_numerical, eigen_matrix_from_matrix_expr(J_analytical), 1.0e-12);
  }
}

TEST(QuaternionTest, TestRightLocalCoordinatesDerivative) {
  const auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  const auto J = quaternion{w, x, y, z}.right_local_coordinates_derivative();

  // Compare analytical derivative to numerical derivative, evaluated at a few different rotations.
  for (const auto& [angle, axis] : get_angle_axis_test_pairs()) {
    const Eigen::Quaterniond q_num{Eigen::AngleAxisd(angle, axis)};
    const Eigen::Matrix<double, 3, 4> J_numerical = numerical_jacobian(
        eigen_wxyz_vec_from_quaternion(q_num),
        [&](const Eigen::Vector4d& q_perturbed) -> Eigen::Vector3d {
          const Eigen::AngleAxisd aa{
              q_num.conjugate() *
              Eigen::Quaterniond(q_perturbed[0], q_perturbed[1], q_perturbed[2], q_perturbed[3])};
          return (aa.angle() * aa.axis()).eval();
        },
        0.001);

    const std::array<scalar_or_boolean_pair, 4> pairs = {
        std::make_pair(w, q_num.w()), std::make_pair(x, q_num.x()), std::make_pair(y, q_num.y()),
        std::make_pair(z, q_num.z())};

    const matrix_expr J_analytical = substitute(J, pairs);
    EXPECT_EIGEN_NEAR(J_numerical, eigen_matrix_from_matrix_expr(J_analytical), 1.0e-12);
  }
}

TEST(QuaternionTest, TestJacobianOfSO3) {
  const auto [theta, x, y, z] = make_symbols("theta", "x", "y", "z");
  const auto J_expr = left_jacobian_of_so3(make_vector(theta * x, theta * y, theta * z), 1.0e-16);
  const auto J_expr_no_eps =
      left_jacobian_of_so3(make_vector(theta * x, theta * y, theta * z), std::nullopt);

  // Compare jacobian of SO(3) expression to numerical integration.
  for (const auto& pair : get_angle_axis_test_pairs()) {
    const auto angle = std::get<0>(pair);
    const auto axis = std::get<1>(pair);

    const matrix_expr J_sub =
        substitute(J_expr, {std::make_tuple(theta, angle), std::make_tuple(x, axis[0]),
                            std::make_tuple(y, axis[1]), std::make_tuple(z, axis[2])});

    // Evaluate numerical integration over multiple steps to get a tighter tolerance:
    const auto func = [&](double alpha) {
      return Eigen::Quaterniond(Eigen::AngleAxisd(angle * alpha, axis)).toRotationMatrix();
    };

    Eigen::Matrix3d J_numerical_integral = Eigen::Matrix3d::Zero();
    constexpr std::size_t num_steps = 32;
    for (std::size_t index = 1; index <= num_steps; ++index) {
      const double lower = static_cast<double>(index - 1) / static_cast<double>(num_steps);
      const double upper = static_cast<double>(index) / static_cast<double>(num_steps);
      J_numerical_integral += integrate_boole(func, lower, upper);
    }
    EXPECT_EIGEN_NEAR(J_numerical_integral, eigen_matrix_from_matrix_expr(J_sub), 1.0e-12);

    // Compare to numerical (left) jacobian:
    const Eigen::Quaterniond q(Eigen::AngleAxisd(angle, axis));
    const Eigen::Matrix3d J_numerical =
        numerical_jacobian(Eigen::Vector3d::Zero(), [&](const auto& dw) {
          // evaluate exp(w + dw)
          const auto q_perturb = manifold<Eigen::Quaterniond>::retract(
              Eigen::Quaterniond::Identity(), (axis * angle) + dw);
          // evaluate log(exp(w + dw) * q^T)
          const Eigen::AngleAxisd aa{q_perturb * q.conjugate()};
          return (aa.angle() * aa.axis()).eval();
        });
    EXPECT_EIGEN_NEAR(J_numerical, eigen_matrix_from_matrix_expr(J_sub), 1.0e-12);

    if (angle != 0.0) {
      const matrix_expr J_sub_no_eps =
          substitute(J_expr_no_eps, {std::make_tuple(theta, angle), std::make_tuple(x, axis[0]),
                                     std::make_tuple(y, axis[1]), std::make_tuple(z, axis[2])});
      EXPECT_EIGEN_NEAR(J_numerical, eigen_matrix_from_matrix_expr(J_sub), 1.0e-12);
    }
  }
}

TEST(QuaternionTest, TestInverseJacobianOfSO3) {
  const scalar_expr theta{"theta", number_set::real};
  const scalar_expr x{"x", number_set::real};
  const scalar_expr y{"y", number_set::real};
  const scalar_expr z{"z", number_set::real};

  const auto J_expr = left_jacobian_of_so3(make_vector(theta * x, theta * y, theta * z), 1.0e-16);
  const auto J_inv_expr =
      inverse_left_jacobian_of_so3(make_vector(theta * x, theta * y, theta * z), 1.0e-16);
  const auto J_inv_expr_no_eps =
      inverse_left_jacobian_of_so3(make_vector(theta * x, theta * y, theta * z), std::nullopt);

  // Compare J * J^-1 numerically:
  for (const auto& pair : get_angle_axis_test_pairs()) {
    const auto angle = std::get<0>(pair);
    const auto axis = std::get<1>(pair);

    const matrix_expr J_sub =
        substitute(J_expr, {std::make_tuple(theta, angle), std::make_tuple(x, axis[0]),
                            std::make_tuple(y, axis[1]), std::make_tuple(z, axis[2])});
    const matrix_expr J_inv_sub =
        substitute(J_inv_expr, {std::make_tuple(theta, angle), std::make_tuple(x, axis[0]),
                                std::make_tuple(y, axis[1]), std::make_tuple(z, axis[2])});
    EXPECT_EIGEN_NEAR(Eigen::Matrix3d::Identity(), eigen_matrix_from_matrix_expr(J_sub * J_inv_sub),
                      1.0e-15);

    // Compare to numerical jacobian. The numerical derivative doesn't work near pi.
    if (std::abs(angle - M_PI) > 1.0e-6) {
      const Eigen::Quaterniond q(Eigen::AngleAxisd(angle, axis));
      const Eigen::Matrix3d J_inv_numerical =
          numerical_jacobian(Eigen::Vector3d::Zero(), [&](const auto& dx) {
            // evaluate exp(dx) * q
            const auto q_perturb_left =
                manifold<Eigen::Quaterniond>::retract(Eigen::Quaterniond::Identity(), dx) * q;
            // Compute log(exp(dx) * q) - w
            // The -w part will not affect the numerical derivative, so drop it.
            const Eigen::AngleAxisd aa{q_perturb_left};
            return (aa.angle() * aa.axis()).eval();
          });
      EXPECT_EIGEN_NEAR(J_inv_numerical, eigen_matrix_from_matrix_expr(J_inv_sub), 1.0e-12)
          << fmt::format("angle = {}, axis = {}", angle, axis.transpose());

      if (angle != 0.0) {
        const matrix_expr J_inv_sub_no_eps = substitute(
            J_inv_expr_no_eps, {std::make_tuple(theta, angle), std::make_tuple(x, axis[0]),
                                std::make_tuple(y, axis[1]), std::make_tuple(z, axis[2])});
        EXPECT_EIGEN_NEAR(J_inv_numerical, eigen_matrix_from_matrix_expr(J_inv_sub_no_eps), 1.0e-12)
            << fmt::format("angle = {}, axis = {}", angle, axis.transpose());
      }
    }
  }
}

}  // namespace wf
