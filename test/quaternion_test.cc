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
  auto [w, x, y, z] = Symbols("w", "x", "y", "z");
  Quaternion q{w, x, y, z};
  ASSERT_IDENTICAL(w, q.w());
  ASSERT_IDENTICAL(x, q.x());
  ASSERT_IDENTICAL(y, q.y());
  ASSERT_IDENTICAL(z, q.z());

  // Cannot pass vector or matrix arg
  ASSERT_THROW(Quaternion(Vector(x, y, z).AsExpr(), x, y, z), TypeError);

  // Default construction:
  Quaternion q_identity{};
  ASSERT_IDENTICAL(0, q_identity.x());
  ASSERT_IDENTICAL(0, q_identity.y());
  ASSERT_IDENTICAL(0, q_identity.z());
  ASSERT_IDENTICAL(1, q_identity.w());
}

TEST(QuaternionTest, TestIsIdenticalTo) {
  auto [w, x, y, z] = Symbols("w", "x", "y", "z");
  const Quaternion q1{w, x, y, z};
  ASSERT_TRUE(q1.IsIdenticalTo(q1));
  ASSERT_TRUE(q1.IsIdenticalTo({q1.w(), q1.x(), q1.y(), q1.z()}));
  ASSERT_FALSE(q1.IsIdenticalTo({1, 0, 0, 0}));
  ASSERT_FALSE(q1.IsIdenticalTo({q1.w(), 2, q1.y(), q1.z()}));
  ASSERT_FALSE(q1.IsIdenticalTo({q1.w(), q1.x(), 2, q1.z()}));
  ASSERT_FALSE(q1.IsIdenticalTo({q1.w(), q1.x(), q1.y(), 2}));
}

TEST(QuaternionTest, TestNorm) {
  auto [w, x, y, z] = Symbols("w", "x", "y", "z");
  const Quaternion q{w, x, y, z};
  ASSERT_IDENTICAL(w * w + x * x + y * y + z * z, q.SquaredNorm());
  ASSERT_IDENTICAL(sqrt(w * w + x * x + y * y + z * z), q.Norm());
  ASSERT_IDENTICAL(1, Quaternion().Norm());
  ASSERT_IDENTICAL(pow(506_s / 49, 1_s / 2), Quaternion(3, -4_s / 7, 0, 1).Norm());
  ASSERT_IDENTICAL(2 * sqrt(pow(x, 2)), Quaternion(x, x, x, x).Norm());
}

TEST(QuaternionTest, TestNormalize) {
  auto [w, x, y, z] = Symbols("w", "x", "y", "z");
  const Quaternion q{w, x, y, z};

  const auto q_normalized = q.Normalized();
  ASSERT_IDENTICAL(q.w() / q.Norm(), q_normalized.w());
  ASSERT_IDENTICAL(q.x() / q.Norm(), q_normalized.x());
  ASSERT_IDENTICAL(q.y() / q.Norm(), q_normalized.y());
  ASSERT_IDENTICAL(q.z() / q.Norm(), q_normalized.z());

  // We can demonstrate that the norm is one:
  ASSERT_IDENTICAL(1, q.Normalized().Norm().Collect(q.SquaredNorm()));
}

TEST(QuaternionTest, TestConjugate) {
  auto [w, x, y, z] = Symbols("w", "x", "y", "z");
  const Quaternion q{w, x, y, z};

  const Quaternion q_conj = q.Conjugate();
  ASSERT_IDENTICAL(q_conj.w(), q.w());
  ASSERT_IDENTICAL(-q_conj.x(), q.x());
  ASSERT_IDENTICAL(-q_conj.y(), q.y());
  ASSERT_IDENTICAL(-q_conj.z(), q.z());

  ASSERT_IDENTICAL(Quaternion().ToVectorWXYZ(), Quaternion().Conjugate().ToVectorWXYZ());
}

TEST(QuaternionTest, TestMultiply) {
  auto [w, x, y, z] = Symbols("w", "x", "y", "z");
  auto [a, b, c, d] = Symbols("a", "b", "c", "d");

  const Quaternion q1{w, x, y, z};
  const Quaternion q2{a, b, c, d};

  // Compare to matrix multiplication:
  // clang-format off
  const MatrixExpr q1_mat =
      CreateMatrix(4, 4,
                    w, -x, -y, -z,
                    x,  w, -z,  y,
                    y,  z,  w, -x,
                    z, -y,  x,  w);
  // clang-format on
  ASSERT_IDENTICAL(q1_mat * q2.ToVectorWXYZ(), (q1 * q2).ToVectorWXYZ());

  // clang-format off
  const MatrixExpr q2_mat =
      CreateMatrix(4, 4,
                   a, -b, -c, -d,
                   b,  a, -d,  c,
                   c,  d,  a, -b,
                   d, -c,  b,  a);
  // clang-format on
  ASSERT_IDENTICAL(q2_mat * q1.ToVectorWXYZ(), (q2 * q1).ToVectorWXYZ());

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
                                .Subs(q1.w(), q1_num.w())
                                .Subs(q1.x(), q1_num.x())
                                .Subs(q1.y(), q1_num.y())
                                .Subs(q1.z(), q1_num.z())
                                .Subs(q2.w(), q2_num.w())
                                .Subs(q2.x(), q2_num.x())
                                .Subs(q2.y(), q2_num.y())
                                .Subs(q2.z(), q2_num.z());
  // Won't match exactly due to floating point order:
  ASSERT_NEAR((q1_num * q2_num).w(), CastChecked<Float>(result.w()).GetValue(), 1.0e-15);
  ASSERT_NEAR((q1_num * q2_num).x(), CastChecked<Float>(result.x()).GetValue(), 1.0e-15);
  ASSERT_NEAR((q1_num * q2_num).y(), CastChecked<Float>(result.y()).GetValue(), 1.0e-15);
  ASSERT_NEAR((q1_num * q2_num).z(), CastChecked<Float>(result.z()).GetValue(), 1.0e-15);
}

TEST(QuaternionTest, TestInverse) {
  auto [w, x, y, z] = Symbols("w", "x", "y", "z");
  const Quaternion q{w, x, y, z};
  ASSERT_IDENTICAL(q.Conjugate().Normalized().ToVectorWXYZ(), q.Inverse().ToVectorWXYZ());

  // We should be able to recovery the identity symbolically:
  auto q_norm_2 = q.SquaredNorm();
  auto q_q_inv = (q * q.Inverse());
  ASSERT_IDENTICAL(1, q_q_inv.w().Collect(q_norm_2).Subs(q_norm_2, 1));
  ASSERT_IDENTICAL(0, q_q_inv.x());
  ASSERT_IDENTICAL(0, q_q_inv.y());
  ASSERT_IDENTICAL(0, q_q_inv.z());

  auto q_inv_q = (q.Inverse() * q);
  ASSERT_IDENTICAL(1, q_inv_q.w().Collect(q_norm_2).Subs(q_norm_2, 1));
  ASSERT_IDENTICAL(0, q_inv_q.x());
  ASSERT_IDENTICAL(0, q_inv_q.y());
  ASSERT_IDENTICAL(0, q_inv_q.z());
}

TEST(QuaternionTest, TestToRotationMatrix) {
  auto [w, x, y, z] = Symbols("w", "x", "y", "z");
  const Quaternion q{w, x, y, z};

  ASSERT_IDENTICAL(Identity(3), Quaternion().ToRotationMatrix());
  ASSERT_IDENTICAL(Identity(3), Quaternion(-1, 0, 0, 0).ToRotationMatrix());

  // Check the matrix against rotating the vector w/ quaternion:
  auto [a, b, c] = Symbols("a", "b", "c");
  auto v = Vector(a, b, c);

  const Quaternion q_v_q_conj = q * Quaternion(0, a, b, c) * q.Conjugate();
  const MatrixExpr R_v_expected = Vector(q_v_q_conj.x(), q_v_q_conj.y(), q_v_q_conj.z());
  const MatrixExpr R_v = q.ToRotationMatrix() * v;

  // If we did everything correctly, there should be no difference between these.
  // We need to provide a small hint, and enforce that |q|^2 = 1
  ASSERT_IDENTICAL(0, Collect((R_v_expected[0] - R_v[0]).Distribute(), a).Subs(q.SquaredNorm(), 1));
  ASSERT_IDENTICAL(0, Collect((R_v_expected[1] - R_v[1]).Distribute(), b).Subs(q.SquaredNorm(), 1));
  ASSERT_IDENTICAL(0, Collect((R_v_expected[2] - R_v[2]).Distribute(), c).Subs(q.SquaredNorm(), 1));

  // Conjugate and transpose should match:
  ASSERT_IDENTICAL(q.Conjugate().ToRotationMatrix(), q.ToRotationMatrix().Transpose());
}

TEST(QuaternionTest, TestFromAxisAngle) {
  auto [angle, vx, vy, vz] = Symbols("theta", "vx", "vy", "vz");
  const Quaternion q = Quaternion::FromAngleAxis(angle, vx, vy, vz);
  ASSERT_IDENTICAL(q.w(), cos(angle / 2));
  ASSERT_IDENTICAL(q.x(), vx * sin(angle / 2));
  ASSERT_IDENTICAL(q.y(), vy * sin(angle / 2));
  ASSERT_IDENTICAL(q.z(), vz * sin(angle / 2));

  // Show that the norm is one.
  // TODO: Have the trig simplification cos^2(x) + sin^2(x) = 1 be automatic.
  const Expr half_angle = angle / 2;
  const Expr q_norm_2 = Collect(q.SquaredNorm(), sin(half_angle));
  ASSERT_IDENTICAL(1, q_norm_2.Subs(vx * vx + vy * vy + vz * vz, 1)
                          .Subs(pow(cos(half_angle), 2) + pow(sin(half_angle), 2), 1));

  // V should be unmodified by the rotation matrix
  const MatrixExpr v_rot = q.ToRotationMatrix() * Vector(vx, vy, vz);
  ASSERT_IDENTICAL(Vector(vx, vy, vz), v_rot.Distribute());

  // Compare to Rodrigues formula:
  // We need a couple of trig identities to do this:
  //  cos(theta/2)*sin(theta/2) --> sin(theta) / 2
  //  sin(theta/2)**2 --> (1 - cos(theta)) / 2
  const MatrixExpr R = q.ToRotationMatrix()
                           .Subs(cos(half_angle) * sin(half_angle), sin(angle) / 2)
                           .Subs(sin(half_angle) * sin(half_angle), (1 - cos(angle)) / 2)
                           .Distribute();

  // clang-format off
  const MatrixExpr K = CreateMatrix(3, 3,
                                     0, -vz, vy,
                                     vz, 0, -vx,
                                    -vy, vx, 0);
  // clang-format on
  const MatrixExpr R_rodrigues = Identity(3) + K * sin(angle) + (1 - cos(angle)) * K * K;
  ASSERT_IDENTICAL(R_rodrigues.Distribute(), R);

  // check that this throws if we pass invalid arguments:
  ASSERT_THROW(Quaternion::FromAngleAxis(0, Vector(vx, vy)), DimensionError);
}

// Sample points uniformly on the sphere (approximately) using fibonacci sphere.
inline Eigen::Vector3d FibSphere(int i, int n) {
  const double ratio = (1 + std::sqrt(5.0)) / 2;
  const double theta = 2 * M_PI * static_cast<double>(i) / ratio;
  const double phi = std::acos(1.0 - 2.0 * (static_cast<double>(i) + 0.5) / static_cast<double>(n));
  return {std::cos(theta) * std::sin(phi), std::sin(theta) * std::sin(phi), std::cos(phi)};
}

auto GenerateAngleAxisTestPairs(int num_vectors, int num_angles) {
  using Eigen::Vector3d;
  std::vector<std::tuple<double, Vector3d>> test_pairs;
  for (int i = 0; i < num_vectors; ++i) {
    const Vector3d axis = FibSphere(i, num_vectors);
    for (int j = 0; j < num_angles; ++j) {
      const double angle_num =
          (static_cast<double>(j) + 0.5) / static_cast<double>(num_angles) * 2 * M_PI - M_PI;
      test_pairs.emplace_back(angle_num, axis);
    }
  }
  return test_pairs;
}

// Generate a bunch of angle-axis test pairs:
auto GetAngleAxisTestPairs() {
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
  auto pseudo_random_pairs = GenerateAngleAxisTestPairs(num_vectors, num_angles);
  test_pairs.insert(test_pairs.end(), pseudo_random_pairs.begin(), pseudo_random_pairs.end());
  return test_pairs;
}

TEST(QuaternionTest, FromRotationVector) {
  auto [vx, vy, vz] = Symbols("vx", "vy", "vz");
  const Expr angle = sqrt(vx * vx + vy * vy + vz * vz);
  const Expr half_angle = angle / 2;
  const Quaternion q = Quaternion::FromRotationVector(vx, vy, vz);
  ASSERT_IDENTICAL(q.w(), cos(angle / 2));
  ASSERT_IDENTICAL(q.x(), where(angle > 0, vx * sin(angle / 2) / angle, 0));
  ASSERT_IDENTICAL(q.y(), where(angle > 0, vy * sin(angle / 2) / angle, 0));
  ASSERT_IDENTICAL(q.z(), where(angle > 0, vz * sin(angle / 2) / angle, 0));

  const Quaternion q_ident = q.Subs(vx, 0).Subs(vy, 0).Subs(vz, 0);
  ASSERT_IDENTICAL(1, q_ident.w());
  ASSERT_IDENTICAL(0, q_ident.x());
  ASSERT_IDENTICAL(0, q_ident.y());
  ASSERT_IDENTICAL(0, q_ident.z());

  // Do some numerical tests.
  // Due to conditional logic we can't compare to rodrigues yet.
  for (auto [angle_num, axis_num] : GetAngleAxisTestPairs()) {
    const Eigen::Quaterniond q_num{Eigen::AngleAxisd{angle_num, axis_num}};
    EXPECT_EIGEN_NEAR(q_num.toRotationMatrix(),
                      EigenMatrixFromMatrixExpr(q.Subs(vx, axis_num.x() * angle_num)
                                                    .Subs(vy, axis_num.y() * angle_num)
                                                    .Subs(vz, axis_num.z() * angle_num)
                                                    .ToRotationMatrix()),
                      1.0e-15);
  }

  ASSERT_THROW(Quaternion::FromRotationVector(Vector(-3, vx)), DimensionError);
  ASSERT_THROW(Quaternion::FromRotationVector(Identity(3)), DimensionError);
}

TEST(QuaternionTest, TestAngleConversions) {
  auto [angle] = Symbols("theta");
  const Expr half_angle = angle / 2;
  // clang-format off
  const MatrixExpr R_x =
      CreateMatrix(3, 3,
                   1, 0, 0,
                   0, cos(angle), -sin(angle),
                   0, sin(angle), cos(angle));
  // clang-format on
  ASSERT_IDENTICAL(R_x, Quaternion::FromXAngle(angle)
                            .ToRotationMatrix()
                            .Subs(sin(half_angle) * sin(half_angle), (1 - cos(angle)) / 2)
                            .Subs(cos(half_angle) * sin(half_angle), sin(angle) / 2)
                            .Distribute());

  // clang-format off
  const MatrixExpr R_y =
      CreateMatrix(3, 3,
                   cos(angle), 0, sin(angle),
                   0, 1, 0,
                   -sin(angle), 0, cos(angle));
  // clang-format on
  ASSERT_IDENTICAL(R_y, Quaternion::FromYAngle(angle)
                            .ToRotationMatrix()
                            .Subs(sin(half_angle) * sin(half_angle), (1 - cos(angle)) / 2)
                            .Subs(cos(half_angle) * sin(half_angle), sin(angle) / 2)
                            .Distribute());

  // clang-format off
  const MatrixExpr R_z =
      CreateMatrix(3, 3,
                   cos(angle), -sin(angle), 0,
                   sin(angle), cos(angle), 0,
                   0, 0, 1);
  // clang-format on
  ASSERT_IDENTICAL(R_z, Quaternion::FromZAngle(angle)
                            .ToRotationMatrix()
                            .Subs(sin(half_angle) * sin(half_angle), (1 - cos(angle)) / 2)
                            .Subs(cos(half_angle) * sin(half_angle), sin(angle) / 2)
                            .Distribute());
}

// Because we don't yet have assumptions, the easiest way to test this for now is numerical:
TEST(QuaternionTest, TestToAxisAngle) {
  using Eigen::Vector3d;

  auto [angle, x, y, z] = Symbols("angle", "x", "y", "z");
  const Quaternion q = Quaternion::FromAngleAxis(angle, x, y, z);

  auto [angle_recovered, axis_recovered] = q.ToAngleAxis();
  ASSERT_IDENTICAL(0, angle_recovered.Subs(angle, 0));
  ASSERT_IDENTICAL(0, angle_recovered.Subs(x, 0).Subs(y, 0).Subs(z, 0));
  ASSERT_IDENTICAL(Vector(1, 0, 0), axis_recovered.Subs(angle, 0));
  ASSERT_IDENTICAL(Vector(1, 0, 0), axis_recovered.Subs(x, 0).Subs(y, 0).Subs(z, 0));

  for (auto [angle_num, axis_num] : GetAngleAxisTestPairs()) {
    if (angle_num < 0.0) {
      // Our solution will always be in [0, pi]
      angle_num *= -1.0;
      axis_num *= -1.0;
    }
    EXPECT_NEAR(angle_num,
                CastChecked<Float>(angle_recovered.Subs(angle, angle_num)
                                       .Subs(x, axis_num.x())
                                       .Subs(y, axis_num.y())
                                       .Subs(z, axis_num.z())
                                       .Eval())
                    .GetValue(),
                1.0e-15)
        << fmt::format("While testing axis = {}, angle = {}", axis_num.transpose(), angle_num);

    const Eigen::Vector3d axis_recovered_num =
        EigenMatrixFromMatrixExpr(axis_recovered.Subs(angle, angle_num)
                                      .Subs(x, axis_num.x())
                                      .Subs(y, axis_num.y())
                                      .Subs(z, axis_num.z()));
    EXPECT_EIGEN_NEAR(axis_num, axis_recovered_num, 1.0e-15)
        << fmt::format("While testing axis = {}, angle = {}", axis_num.transpose(), angle_num);
  }
}

TEST(QuaternionTest, TestFromRotationMatrix) {
  const Quaternion q_ident = Quaternion::FromRotationMatrix(Identity(3));
  ASSERT_IDENTICAL(1, q_ident.w());
  ASSERT_IDENTICAL(0, q_ident.x());
  ASSERT_IDENTICAL(0, q_ident.y());
  ASSERT_IDENTICAL(0, q_ident.z());

  // clang-format off
  const Quaternion q_pi_over_2_x =
      Quaternion::FromRotationMatrix(CreateMatrix(3, 3,
        1, 0, 0,
        0, 0, -1,
        0, 1, 0));
  // clang-format on
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_x.w());
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_x.x());
  ASSERT_IDENTICAL(0, q_pi_over_2_x.y());
  ASSERT_IDENTICAL(0, q_pi_over_2_x.z());

  const Quaternion q_pi_x =
      Quaternion::FromRotationMatrix(CreateMatrix(3, 3, 1, 0, 0, 0, -1, 0, 0, 0, -1));
  ASSERT_IDENTICAL(0, q_pi_x.w());
  ASSERT_IDENTICAL(1, q_pi_x.x());
  ASSERT_IDENTICAL(0, q_pi_x.y());
  ASSERT_IDENTICAL(0, q_pi_x.z());

  // clang-format off
  const Quaternion q_pi_over_2_y =
      Quaternion::FromRotationMatrix(CreateMatrix(3, 3,
         0, 0, 1,
         0, 1, 0,
        -1, 0, 0));
  // clang-format on
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_y.w());
  ASSERT_IDENTICAL(0, q_pi_over_2_y.x());
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_y.y());
  ASSERT_IDENTICAL(0, q_pi_over_2_y.z());

  const Quaternion q_pi_y =
      Quaternion::FromRotationMatrix(CreateMatrix(3, 3, -1, 0, 0, 0, 1, 0, 0, 0, -1));
  ASSERT_IDENTICAL(0, q_pi_y.w());
  ASSERT_IDENTICAL(0, q_pi_y.x());
  ASSERT_IDENTICAL(1, q_pi_y.y());
  ASSERT_IDENTICAL(0, q_pi_y.z());

  // clang-format off
  const Quaternion q_pi_over_2_z =
      Quaternion::FromRotationMatrix(CreateMatrix(3, 3,
         0, -1, 0,
         1,  0, 0,
         0,  0, 1));
  // clang-format on
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_z.w());
  ASSERT_IDENTICAL(0, q_pi_over_2_z.x());
  ASSERT_IDENTICAL(0, q_pi_over_2_z.y());
  ASSERT_IDENTICAL(1 / sqrt(2), q_pi_over_2_z.z());

  const Quaternion q_pi_z =
      Quaternion::FromRotationMatrix(CreateMatrix(3, 3, -1, 0, 0, 0, -1, 0, 0, 0, 1));
  ASSERT_IDENTICAL(0, q_pi_z.w());
  ASSERT_IDENTICAL(0, q_pi_z.x());
  ASSERT_IDENTICAL(0, q_pi_z.y());
  ASSERT_IDENTICAL(1, q_pi_z.z());

  // TODO: With a bit more work on collect(), I could analytically demonstrate the mapping:
  //  S(4) -> SO(3) -> S(4)
  //  For now I'll just test this numerically.

  constexpr int num_vectors = 75;
  constexpr int num_angles = 20;
  for (auto [angle_num, axis_num] : GenerateAngleAxisTestPairs(num_vectors, num_angles)) {
    const Eigen::Quaterniond q_num{Eigen::AngleAxisd(angle_num, axis_num)};

    const MatrixExpr R = Quaternion{q_num.w(), q_num.x(), q_num.y(), q_num.z()}.ToRotationMatrix();
    const Quaternion q = Quaternion::FromRotationMatrix(R);

    ASSERT_NEAR(q_num.w(), CastChecked<Float>(q.w()).GetValue(), 1.0e-15)
        << fmt::format("q_num = [{}, {}, {}, {}]\nq = {}\nR:\n{}", q_num.w(), q_num.x(), q_num.y(),
                       q_num.z(), q.ToVectorWXYZ().Transpose(), R);
    ASSERT_NEAR(q_num.x(), CastChecked<Float>(q.x()).GetValue(), 1.0e-15)
        << fmt::format("q_num = [{}, {}, {}, {}]\nq = {}\nR:\n{}", q_num.w(), q_num.x(), q_num.y(),
                       q_num.z(), q.ToVectorWXYZ().Transpose(), R);
    ASSERT_NEAR(q_num.y(), CastChecked<Float>(q.y()).GetValue(), 1.0e-15)
        << fmt::format("q_num = [{}, {}, {}, {}]\nq = {}\nR:\n{}", q_num.w(), q_num.x(), q_num.y(),
                       q_num.z(), q.ToVectorWXYZ().Transpose(), R);
    ASSERT_NEAR(q_num.z(), CastChecked<Float>(q.z()).GetValue(), 1.0e-15)
        << fmt::format("q_num = [{}, {}, {}, {}]\nq = {}\nR:\n{}", q_num.w(), q_num.x(), q_num.y(),
                       q_num.z(), q.ToVectorWXYZ().Transpose(), R);
  }
}

}  // namespace math
