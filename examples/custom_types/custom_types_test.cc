#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numerical_jacobian.h"
#include "wf_test_support/test_macros.h"

#define WF_SPAN_EIGEN_SUPPORT
#include "wrenfold/span_eigen.h"

using Eigen::Quaterniond;
using Eigen::Vector3d;

// Define our custom types before including the generated file.
namespace geo {

// A simple point struct.
struct Point3d {
  double x;
  double y;
  double z;

  Vector3d to_vector() const noexcept { return {x, y, z}; }
};

// A simple 3D pose. We implement accessors and constructors for this type, to demonstrate that the
// code generator can be cutomized to use these.
class Pose3d {
 public:
  Pose3d(const Quaterniond& rotation, const Vector3d& translation) noexcept
      : rotation_(rotation), translation_(translation) {}

  constexpr const Quaterniond& rotation() const noexcept { return rotation_; }
  constexpr const Vector3d& translation() const noexcept { return translation_; }

 private:
  Quaterniond rotation_;
  Vector3d translation_;
};
}  // namespace geo

#include "generated.h"

namespace wf {

// Implement manifold testing trait for our custom structs. This is to facilitate comparison to
// numerical derivatives.
template <>
struct manifold<geo::Point3d> {
  static constexpr int dimension = 3;
  using scalar_type = double;

  static Eigen::Vector3d local_coordinates(const geo::Point3d& a, const geo::Point3d& b) noexcept {
    return b.to_vector() - a.to_vector();
  }

  template <typename Derived>
  static geo::Point3d retract(const geo::Point3d& p,
                              const Eigen::MatrixBase<Derived>& dp) noexcept {
    const Eigen::Vector<double, 3> dp_eval = dp.eval();
    return geo::Point3d{p.x + dp_eval.x(), p.y + dp_eval.y(), p.z + dp_eval.z()};
  }
};

// Pose3d uses product of SO(3) and R(3) as the tangent space.
template <>
struct manifold<geo::Pose3d> {
  static constexpr int dimension = 6;
  using scalar_type = double;

  static Eigen::Vector<double, 6> local_coordinates(const geo::Pose3d& a,
                                                    const geo::Pose3d& b) noexcept {
    return (Eigen::Vector<double, 6>()
                << manifold<Quaterniond>::local_coordinates(a.rotation(), b.rotation()),
            manifold<Vector3d>::local_coordinates(a.translation(), b.translation()))
        .finished();
  }

  template <typename Derived>
  static geo::Pose3d retract(const geo::Pose3d& x, const Eigen::MatrixBase<Derived>& dx) noexcept {
    const Eigen::Vector<double, 6> dx_eval = dx.eval();
    return geo::Pose3d(manifold<Quaterniond>::retract(x.rotation(), dx_eval.head<3>()),
                       manifold<Vector3d>::retract(x.translation(), dx_eval.tail<3>()));
  }
};

Quaterniond rodrigues(const Eigen::Vector3d& w) {
  return manifold<Quaterniond>::retract(Quaterniond::Identity(), w);
}

// Some made up test poses:
std::vector<geo::Pose3d> get_test_poses() {
  return {
      geo::Pose3d(Quaterniond::Identity(), Vector3d::Identity()),
      geo::Pose3d(rodrigues({-0.01, 0.03, -0.05}), Vector3d::Identity()),
      geo::Pose3d(rodrigues({-0.5, 0.355, 0.7061}), {-0.8, 1.4, -2.13}),
      geo::Pose3d(rodrigues({0.253, -0.84, 0.123}), {10.1, -8.2, 5.3}),
  };
}

TEST(CustomTypes1Test, TestTransformPoint) {
  constexpr geo::Point3d p_body{0.35, -0.6, 0.173};

  for (const geo::Pose3d& world_T_body : get_test_poses()) {
    Eigen::Matrix<double, 3, 6> D_pose_gen;
    Eigen::Matrix<double, 3, 3> D_pt_gen;
    const geo::Point3d p_world =
        gen::transform_point<double>(world_T_body, p_body, D_pose_gen, D_pt_gen);

    // Compare to manually written transformation:
    EXPECT_EIGEN_NEAR(world_T_body.rotation().toRotationMatrix() * p_body.to_vector() +
                          world_T_body.translation(),
                      p_world.to_vector(), 1.0e-14);

    // Check derivatives numerically:
    const auto D_pose_num = numerical_jacobian(world_T_body, [&](const geo::Pose3d& w_T_b) {
      return gen::transform_point<double>(w_T_b, p_body, nullptr, nullptr);
    });
    EXPECT_EIGEN_NEAR(D_pose_num, D_pose_gen, 1.0e-12);

    const auto D_pt_num = numerical_jacobian(p_body, [&](const geo::Point3d& p_b) {
      return gen::transform_point<double>(world_T_body, p_b, nullptr, nullptr);
    });
    EXPECT_EIGEN_NEAR(D_pt_num, D_pt_gen, 1.0e-12);
  }
}

TEST(CustomTypes1Test, TestComposePoses) {
  for (const geo::Pose3d& a_T_b : get_test_poses()) {
    for (const geo::Pose3d& b_T_c : get_test_poses()) {
      Eigen::Matrix<double, 6, 6> D_first;
      Eigen::Matrix<double, 6, 6> D_second;

      // Compare to manual composition:
      const geo::Pose3d a_T_c = gen::compose_poses<double>(a_T_b, b_T_c, D_first, D_second);
      EXPECT_EIGEN_NEAR((a_T_b.rotation() * b_T_c.rotation()).toRotationMatrix(),
                        a_T_c.rotation().toRotationMatrix(), 1.0e-15);
      EXPECT_EIGEN_NEAR(
          a_T_b.rotation().toRotationMatrix() * b_T_c.translation() + a_T_b.translation(),
          a_T_c.translation(), 1.0e-14);

      // Compute numerical jacobians and compare:
      const auto D_first_num = numerical_jacobian(a_T_b, [&](const geo::Pose3d& a_T_b) {
        return gen::compose_poses<double>(a_T_b, b_T_c, nullptr, nullptr);
      });
      EXPECT_EIGEN_NEAR(D_first_num, D_first, 1.0e-12);

      const auto D_second_num = numerical_jacobian(b_T_c, [&](const geo::Pose3d& b_T_c) {
        return gen::compose_poses<double>(a_T_b, b_T_c, nullptr, nullptr);
      });
      EXPECT_EIGEN_NEAR(D_second_num, D_second, 1.0e-12);
    }
  }
}

}  // namespace wf
