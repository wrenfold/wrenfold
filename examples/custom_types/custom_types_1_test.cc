#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numerical_jacobian.h"
#include "wf_test_support/test_macros.h"

#define MATH_SPAN_EIGEN_SUPPORT
#include "wf_runtime/span_eigen.h"

// Define our custom types before including the generated file.
namespace geo {

// A very simple point type.
struct Point2D {
  double x;
  double y;
};

// A very simple 2D pose. In actual practice you might want to represent orientation with a phasor.
class Pose2D {
 public:
  constexpr Pose2D(double angle, Point2D position) : angle_(angle), position_(position) {}

  constexpr double angle() const { return angle_; }
  constexpr const Point2D& position() const { return position_; }

 private:
  double angle_;
  Point2D position_;
};
}  // namespace geo

#include "generated.h"

namespace wf {

template <>
struct manifold<geo::Point2D> {
  static constexpr int dimension = 2;
  using scalar_type = double;

  static Eigen::Vector2d local_coordinates(const geo::Point2D& a, const geo::Point2D& b) noexcept {
    return {b.x - a.x, b.y - a.y};
  }

  template <typename Derived>
  static geo::Point2D retract(const geo::Point2D& x,
                              const Eigen::MatrixBase<Derived>& dx) noexcept {
    const Eigen::Vector<double, 2> dx_eval = dx.eval();
    return geo::Point2D{x.x + dx_eval.x(), x.y + dx_eval.y()};
  }
};

template <>
struct manifold<geo::Pose2D> {
  static constexpr int dimension = 3;
  using scalar_type = double;

  static Eigen::Vector3d local_coordinates(const geo::Pose2D& a, const geo::Pose2D& b) noexcept {
    return (Eigen::Vector3d() << b.angle() - a.angle(),
            manifold<geo::Point2D>::local_coordinates(a.position(), b.position()))
        .finished();
  }

  template <typename Derived>
  static geo::Pose2D retract(const geo::Pose2D& x, const Eigen::MatrixBase<Derived>& dx) noexcept {
    const Eigen::Vector<double, 3> dx_eval = dx.eval();
    return geo::Pose2D{x.angle() + dx_eval[0],
                       manifold<geo::Point2D>::retract(x.position(), dx_eval.tail<2>())};
  }
};

TEST(CustomTypes1Test, TestTransformPoint) {
  constexpr geo::Pose2D world_T_body{-0.431, {-0.8, 1.4}};
  constexpr geo::Point2D p_body{0.35, -0.6};
  Eigen::Matrix<double, 2, 3> D_pose_gen;
  Eigen::Matrix<double, 2, 2> D_pt_gen;
  const auto [p_world_x, p_world_y] =
      gen::transform_point<double>(world_T_body, p_body, D_pose_gen, D_pt_gen);

  // Compare to manual computation of the transform:
  const double c = std::cos(world_T_body.angle());
  const double s = std::sin(world_T_body.angle());
  EXPECT_NEAR(c * p_body.x - s * p_body.y + world_T_body.position().x, p_world_x, 1.0e-15);
  EXPECT_NEAR(s * p_body.x + c * p_body.y + world_T_body.position().y, p_world_y, 1.0e-15);

  // Check derivatives numerically.
  const auto D_pose_num = numerical_jacobian(world_T_body, [&](geo::Pose2D w_T_b) {
    return gen::transform_point<double>(w_T_b, p_body, nullptr, nullptr);
  });
  EXPECT_EIGEN_NEAR(D_pose_num, D_pose_gen, 1.0e-12);

  const auto D_pt_num = numerical_jacobian(p_body, [&](geo::Point2D p_b) {
    return gen::transform_point<double>(world_T_body, p_b, nullptr, nullptr);
  });
  EXPECT_EIGEN_NEAR(D_pt_num, D_pt_gen, 1.0e-12);
}

TEST(CustomTypes1Test, TestComposePoses) {}

}  // namespace wf
