#include <Eigen/Geometry>

#include "expression_impl.h"
#include "expressions/numeric_expressions.h"
#include "geometry/quaternion.h"

#include "eigen_test_helpers.h"
#include "test_helpers.h"

namespace math {
using namespace math::custom_literals;
using namespace math::matrix_operator_overloads;

TEST(QuaternionTest, TestConstructor) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  Quaternion q{w, x, y, z};
  ASSERT_IDENTICAL(w, q.w());
  ASSERT_IDENTICAL(x, q.x());
  ASSERT_IDENTICAL(y, q.y());
  ASSERT_IDENTICAL(z, q.z());

  // Default construction:
  Quaternion q_identity{};
  ASSERT_IDENTICAL(0, q_identity.x());
  ASSERT_IDENTICAL(0, q_identity.y());
  ASSERT_IDENTICAL(0, q_identity.z());
  ASSERT_IDENTICAL(1, q_identity.w());
}

TEST(QuaternionTest, TestIsIdenticalTo) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  const Quaternion q1{w, x, y, z};
  ASSERT_TRUE(q1.is_identical_to(q1));
  ASSERT_TRUE(q1.is_identical_to({q1.w(), q1.x(), q1.y(), q1.z()}));
  ASSERT_FALSE(q1.is_identical_to({1, 0, 0, 0}));
  ASSERT_FALSE(q1.is_identical_to({q1.w(), 2, q1.y(), q1.z()}));
  ASSERT_FALSE(q1.is_identical_to({q1.w(), q1.x(), 2, q1.z()}));
  ASSERT_FALSE(q1.is_identical_to({q1.w(), q1.x(), q1.y(), 2}));
}

TEST(QuaternionTest, TestNorm) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  const Quaternion q{w, x, y, z};
  ASSERT_IDENTICAL(w * w + x * x + y * y + z * z, q.squared_norm());
  ASSERT_IDENTICAL(sqrt(w * w + x * x + y * y + z * z), q.norm());
  ASSERT_IDENTICAL(1, Quaternion().norm());
  ASSERT_IDENTICAL(pow(506_s / 49, 1_s / 2), Quaternion(3, -4_s / 7, 0, 1).norm());
  ASSERT_IDENTICAL(2 * sqrt(pow(x, 2)), Quaternion(x, x, x, x).norm());
}

TEST(QuaternionTest, TestNormalize) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  const Quaternion q{w, x, y, z};

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
  const Quaternion q{w, x, y, z};

  const Quaternion q_conj = q.conjugate();
  ASSERT_IDENTICAL(q_conj.w(), q.w());
  ASSERT_IDENTICAL(-q_conj.x(), q.x());
  ASSERT_IDENTICAL(-q_conj.y(), q.y());
  ASSERT_IDENTICAL(-q_conj.z(), q.z());

  ASSERT_IDENTICAL(Quaternion().to_vector_wxyz(), Quaternion().conjugate().to_vector_wxyz());
}

TEST(QuaternionTest, TestMultiply) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  auto [a, b, c, d] = make_symbols("a", "b", "c", "d");

  const Quaternion q1{w, x, y, z};
  const Quaternion q2{a, b, c, d};

  // Compare to matrix multiplication:
  // clang-format off
  const MatrixExpr q1_mat =
      make_matrix(4, 4,
                    w, -x, -y, -z,
                    x,  w, -z,  y,
                    y,  z,  w, -x,
                    z, -y,  x,  w);
  // clang-format on
  ASSERT_IDENTICAL(q1_mat * q2.to_vector_wxyz(), (q1 * q2).to_vector_wxyz());

  // clang-format off
  const MatrixExpr q2_mat =
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
  const Quaternion result = (q1 * q2)
                                .subs(q1.w(), q1_num.w())
                                .subs(q1.x(), q1_num.x())
                                .subs(q1.y(), q1_num.y())
                                .subs(q1.z(), q1_num.z())
                                .subs(q2.w(), q2_num.w())
                                .subs(q2.x(), q2_num.x())
                                .subs(q2.y(), q2_num.y())
                                .subs(q2.z(), q2_num.z());
  // Won't match exactly due to floating point order:
  ASSERT_NEAR((q1_num * q2_num).w(), cast_checked<Float>(result.w()).get_value(), 1.0e-15);
  ASSERT_NEAR((q1_num * q2_num).x(), cast_checked<Float>(result.x()).get_value(), 1.0e-15);
  ASSERT_NEAR((q1_num * q2_num).y(), cast_checked<Float>(result.y()).get_value(), 1.0e-15);
  ASSERT_NEAR((q1_num * q2_num).z(), cast_checked<Float>(result.z()).get_value(), 1.0e-15);
}

TEST(QuaternionTest, TestInverse) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  const Quaternion q{w, x, y, z};
  ASSERT_IDENTICAL(q.conjugate().normalized().to_vector_wxyz(), q.inverse().to_vector_wxyz());

  // We should be able to recovery the identity symbolically:
  auto q_norm_2 = q.squared_norm();
  auto q_q_inv = (q * q.inverse());
  ASSERT_IDENTICAL(1, q_q_inv.w().collect(q_norm_2).subs(q_norm_2, 1));
  ASSERT_IDENTICAL(0, q_q_inv.x());
  ASSERT_IDENTICAL(0, q_q_inv.y());
  ASSERT_IDENTICAL(0, q_q_inv.z());

  auto q_inv_q = (q.inverse() * q);
  ASSERT_IDENTICAL(1, q_inv_q.w().collect(q_norm_2).subs(q_norm_2, 1));
  ASSERT_IDENTICAL(0, q_inv_q.x());
  ASSERT_IDENTICAL(0, q_inv_q.y());
  ASSERT_IDENTICAL(0, q_inv_q.z());
}

TEST(QuaternionTest, TestToRotationMatrix) {
  auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  const Quaternion q{w, x, y, z};

  ASSERT_IDENTICAL(make_identity(3), Quaternion().to_rotation_matrix());
  ASSERT_IDENTICAL(make_identity(3), Quaternion(-1, 0, 0, 0).to_rotation_matrix());

  // Check the matrix against rotating the vector w/ quaternion:
  auto [a, b, c] = make_symbols("a", "b", "c");
  auto v = make_vector(a, b, c);

  const Quaternion q_v_q_conj = q * Quaternion(0, a, b, c) * q.conjugate();
  const MatrixExpr R_v_expected = make_vector(q_v_q_conj.x(), q_v_q_conj.y(), q_v_q_conj.z());
  const MatrixExpr R_v = q.to_rotation_matrix() * v;

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
  const Quaternion q = Quaternion::from_angle_axis(angle, vx, vy, vz);
  ASSERT_IDENTICAL(q.w(), cos(angle / 2));
  ASSERT_IDENTICAL(q.x(), vx * sin(angle / 2));
  ASSERT_IDENTICAL(q.y(), vy * sin(angle / 2));
  ASSERT_IDENTICAL(q.z(), vz * sin(angle / 2));

  // Show that the norm is one.
  // TODO: Have the trig simplification cos^2(x) + sin^2(x) = 1 be automatic.
  const Expr half_angle = angle / 2;
  const Expr q_norm_2 = collect(q.squared_norm(), sin(half_angle));
  ASSERT_IDENTICAL(1, q_norm_2.subs(vx * vx + vy * vy + vz * vz, 1)
                          .subs(pow(cos(half_angle), 2) + pow(sin(half_angle), 2), 1));

  // V should be unmodified by the rotation matrix
  const MatrixExpr v_rot = q.to_rotation_matrix() * make_vector(vx, vy, vz);
  ASSERT_IDENTICAL(make_vector(vx, vy, vz), v_rot.distribute());

  // Compare to Rodrigues formula:
  // We need a couple of trig identities to do this:
  //  cos(theta/2)*sin(theta/2) --> sin(theta) / 2
  //  sin(theta/2)**2 --> (1 - cos(theta)) / 2
  const MatrixExpr R = q.to_rotation_matrix()
                           .subs(cos(half_angle) * sin(half_angle), sin(angle) / 2)
                           .subs(sin(half_angle) * sin(half_angle), (1 - cos(angle)) / 2)
                           .distribute();

  // clang-format off
  const MatrixExpr K = make_matrix(3, 3,
                                     0, -vz, vy,
                                     vz, 0, -vx,
                                    -vy, vx, 0);
  // clang-format on
  const MatrixExpr R_rodrigues = make_identity(3) + K * sin(angle) + (1 - cos(angle)) * K * K;
  ASSERT_IDENTICAL(R_rodrigues.distribute(), R);

  // check that this throws if we pass invalid arguments:
  ASSERT_THROW(Quaternion::from_angle_axis(0, make_vector(vx, vy)), DimensionError);
}

// Sample points uniformly on the sphere (approximately) using fibonacci sphere.
inline Eigen::Vector3d fib_sphere(int i, int n) {
  const double ratio = (1 + std::sqrt(5.0)) / 2;
  const double theta = 2 * M_PI * static_cast<double>(i) / ratio;
  const double phi = std::acos(1.0 - 2.0 * (static_cast<double>(i) + 0.5) / static_cast<double>(n));
  return {std::cos(theta) * std::sin(phi), std::sin(theta) * std::sin(phi), std::cos(phi)};
}

auto generate_angle_axis_test_pairs(int num_vectors, int num_angles) {
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
  return test_pairs;
}

TEST(QuaternionTest, FromRotationVector) {
  auto [vx, vy, vz] = make_symbols("vx", "vy", "vz");
  const Expr angle = sqrt(vx * vx + vy * vy + vz * vz);
  const Expr half_angle = angle / 2;
  const Quaternion q = Quaternion::from_rotation_vector(vx, vy, vz);
  ASSERT_IDENTICAL(q.w(), cos(angle / 2));
  ASSERT_IDENTICAL(q.x(), where(angle > 0, vx * sin(angle / 2) / angle, 0));
  ASSERT_IDENTICAL(q.y(), where(angle > 0, vy * sin(angle / 2) / angle, 0));
  ASSERT_IDENTICAL(q.z(), where(angle > 0, vz * sin(angle / 2) / angle, 0));

  const Quaternion q_ident = q.subs(vx, 0).subs(vy, 0).subs(vz, 0);
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

  ASSERT_THROW(Quaternion::from_rotation_vector(make_vector(-3, vx)), DimensionError);
  ASSERT_THROW(Quaternion::from_rotation_vector(make_identity(3)), DimensionError);
}

TEST(QuaternionTest, TestAngleConversions) {
  auto [angle] = make_symbols("theta");
  const Expr half_angle = angle / 2;
  // clang-format off
  const MatrixExpr R_x =
      make_matrix(3, 3,
                   1, 0, 0,
                   0, cos(angle), -sin(angle),
                   0, sin(angle), cos(angle));
  // clang-format on
  ASSERT_IDENTICAL(R_x, Quaternion::from_x_angle(angle)
                            .to_rotation_matrix()
                            .subs(sin(half_angle) * sin(half_angle), (1 - cos(angle)) / 2)
                            .subs(cos(half_angle) * sin(half_angle), sin(angle) / 2)
                            .distribute());

  // clang-format off
  const MatrixExpr R_y =
      make_matrix(3, 3,
                   cos(angle), 0, sin(angle),
                   0, 1, 0,
                   -sin(angle), 0, cos(angle));
  // clang-format on
  ASSERT_IDENTICAL(R_y, Quaternion::from_y_angle(angle)
                            .to_rotation_matrix()
                            .subs(sin(half_angle) * sin(half_angle), (1 - cos(angle)) / 2)
                            .subs(cos(half_angle) * sin(half_angle), sin(angle) / 2)
                            .distribute());

  // clang-format off
  const MatrixExpr R_z =
      make_matrix(3, 3,
                   cos(angle), -sin(angle), 0,
                   sin(angle), cos(angle), 0,
                   0, 0, 1);
  // clang-format on
  ASSERT_IDENTICAL(R_z, Quaternion::from_z_angle(angle)
                            .to_rotation_matrix()
                            .subs(sin(half_angle) * sin(half_angle), (1 - cos(angle)) / 2)
                            .subs(cos(half_angle) * sin(half_angle), sin(angle) / 2)
                            .distribute());
}

// Because we don't yet have assumptions, the easiest way to test this for now is numerical:
TEST(QuaternionTest, TestToAxisAngle) {
  using Eigen::Vector3d;

  auto [angle, x, y, z] = make_symbols("angle", "x", "y", "z");
  const Quaternion q = Quaternion::from_angle_axis(angle, x, y, z);

  auto [angle_recovered, axis_recovered] = q.to_angle_axis();
  ASSERT_IDENTICAL(0, angle_recovered.subs(angle, 0));
  ASSERT_IDENTICAL(0, angle_recovered.subs(x, 0).subs(y, 0).subs(z, 0));
  ASSERT_IDENTICAL(make_vector(1, 0, 0), axis_recovered.subs(angle, 0));
  ASSERT_IDENTICAL(make_vector(1, 0, 0), axis_recovered.subs(x, 0).subs(y, 0).subs(z, 0));

  for (auto [angle_num, axis_num] : get_angle_axis_test_pairs()) {
    if (angle_num < 0.0) {
      // Our solution will always be in [0, pi]
      angle_num *= -1.0;
      axis_num *= -1.0;
    }
    EXPECT_NEAR(angle_num,
                cast_checked<Float>(angle_recovered.subs(angle, angle_num)
                                        .subs(x, axis_num.x())
                                        .subs(y, axis_num.y())
                                        .subs(z, axis_num.z())
                                        .eval())
                    .get_value(),
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
}

TEST(QuaternionTest, TestFromRotationMatrix) {
  const Quaternion q_ident = Quaternion::from_rotation_matrix(make_identity(3));
  ASSERT_IDENTICAL(1, q_ident.w());
  ASSERT_IDENTICAL(0, q_ident.x());
  ASSERT_IDENTICAL(0, q_ident.y());
  ASSERT_IDENTICAL(0, q_ident.z());

  // clang-format off
  const Quaternion q_pi_over_2_x =
      Quaternion::from_rotation_matrix(make_matrix(3, 3,
        1, 0, 0,
        0, 0, -1,
        0, 1, 0));
  // clang-format on
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_x.w());
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_x.x());
  ASSERT_IDENTICAL(0, q_pi_over_2_x.y());
  ASSERT_IDENTICAL(0, q_pi_over_2_x.z());

  const Quaternion q_pi_x =
      Quaternion::from_rotation_matrix(make_matrix(3, 3, 1, 0, 0, 0, -1, 0, 0, 0, -1));
  ASSERT_IDENTICAL(0, q_pi_x.w());
  ASSERT_IDENTICAL(1, q_pi_x.x());
  ASSERT_IDENTICAL(0, q_pi_x.y());
  ASSERT_IDENTICAL(0, q_pi_x.z());

  // clang-format off
  const Quaternion q_pi_over_2_y =
      Quaternion::from_rotation_matrix(make_matrix(3, 3,
         0, 0, 1,
         0, 1, 0,
        -1, 0, 0));
  // clang-format on
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_y.w());
  ASSERT_IDENTICAL(0, q_pi_over_2_y.x());
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_y.y());
  ASSERT_IDENTICAL(0, q_pi_over_2_y.z());

  const Quaternion q_pi_y =
      Quaternion::from_rotation_matrix(make_matrix(3, 3, -1, 0, 0, 0, 1, 0, 0, 0, -1));
  ASSERT_IDENTICAL(0, q_pi_y.w());
  ASSERT_IDENTICAL(0, q_pi_y.x());
  ASSERT_IDENTICAL(1, q_pi_y.y());
  ASSERT_IDENTICAL(0, q_pi_y.z());

  // clang-format off
  const Quaternion q_pi_over_2_z =
      Quaternion::from_rotation_matrix(make_matrix(3, 3,
         0, -1, 0,
         1,  0, 0,
         0,  0, 1));
  // clang-format on
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_z.w());
  ASSERT_IDENTICAL(0, q_pi_over_2_z.x());
  ASSERT_IDENTICAL(0, q_pi_over_2_z.y());
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_z.z());

  const Quaternion q_pi_z =
      Quaternion::from_rotation_matrix(make_matrix(3, 3, -1, 0, 0, 0, -1, 0, 0, 0, 1));
  ASSERT_IDENTICAL(0, q_pi_z.w());
  ASSERT_IDENTICAL(0, q_pi_z.x());
  ASSERT_IDENTICAL(0, q_pi_z.y());
  ASSERT_IDENTICAL(1, q_pi_z.z());

  // TODO: With a bit more work on collect(), I could analytically demonstrate the mapping:
  //  S(4) -> SO(3) -> S(4)
  //  For now I'll just test this numerically.

  constexpr int num_vectors = 75;
  constexpr int num_angles = 20;
  for (auto [angle_num, axis_num] : generate_angle_axis_test_pairs(num_vectors, num_angles)) {
    const Eigen::Quaterniond q_num{Eigen::AngleAxisd(angle_num, axis_num)};

    const MatrixExpr R =
        Quaternion{q_num.w(), q_num.x(), q_num.y(), q_num.z()}.to_rotation_matrix();
    const Quaternion q = Quaternion::from_rotation_matrix(R);

    ASSERT_NEAR(q_num.w(), cast_checked<Float>(q.w()).get_value(), 1.0e-15)
        << fmt::format("q_num = [{}, {}, {}, {}]\nq = {}\nR:\n{}", q_num.w(), q_num.x(), q_num.y(),
                       q_num.z(), q.to_vector_wxyz().transposed(), R);
    ASSERT_NEAR(q_num.x(), cast_checked<Float>(q.x()).get_value(), 1.0e-15)
        << fmt::format("q_num = [{}, {}, {}, {}]\nq = {}\nR:\n{}", q_num.w(), q_num.x(), q_num.y(),
                       q_num.z(), q.to_vector_wxyz().transposed(), R);
    ASSERT_NEAR(q_num.y(), cast_checked<Float>(q.y()).get_value(), 1.0e-15)
        << fmt::format("q_num = [{}, {}, {}, {}]\nq = {}\nR:\n{}", q_num.w(), q_num.x(), q_num.y(),
                       q_num.z(), q.to_vector_wxyz().transposed(), R);
    ASSERT_NEAR(q_num.z(), cast_checked<Float>(q.z()).get_value(), 1.0e-15)
        << fmt::format("q_num = [{}, {}, {}, {}]\nq = {}\nR:\n{}", q_num.w(), q_num.x(), q_num.y(),
                       q_num.z(), q.to_vector_wxyz().transposed(), R);
  }
}

}  // namespace math
