#pragma once
#include "assertions.h"
#include "constants.h"
#include "expression.h"
#include "expressions/matrix.h"
#include "functions.h"
#include "matrix_expression.h"
#include "matrix_functions.h"

#include <array>

namespace math {

// Symbolic quaternion type.
class Quaternion {
 public:
  // Initialize with elements in order [w, x, y, z].
  // No normalization occurs by default.
  Quaternion(Expr w, Expr x, Expr y, Expr z)
      : wxyz_{std::move(w), std::move(x), std::move(y), std::move(z)} {}

  // Default initialize to identity (w = 1, xyz = 0).
  Quaternion() : Quaternion(Constants::One, Constants::Zero, Constants::Zero, Constants::Zero) {}

  // Access components:
  constexpr const Expr& w() const noexcept { return wxyz_[0]; }
  constexpr const Expr& x() const noexcept { return wxyz_[1]; }
  constexpr const Expr& y() const noexcept { return wxyz_[2]; }
  constexpr const Expr& z() const noexcept { return wxyz_[3]; }

  // True if all sub-expressions in the quaternion are identical to those in the argument.
  bool is_identical_to(const Quaternion& other) const {
    return std::equal(wxyz_.begin(), wxyz_.end(), other.wxyz_.begin(), IsIdenticalOperator<Expr>{});
  }

  // Convert to a column vector in [w,x,y,z] order.
  const MatrixExpr to_vector_wxyz() const { return make_vector(w(), x(), y(), z()); }

  // Create a new quaternion by substituting.
  Quaternion subs(const Expr& target, const Expr& replacement) const {
    return Quaternion(w().subs(target, replacement), x().subs(target, replacement),
                      y().subs(target, replacement), z().subs(target, replacement));
  }

  // The squared norm.
  Expr squared_norm() const {
    // TODO: Add a faster path for this type of summation.
    return wxyz_[0] * wxyz_[0] + wxyz_[1] * wxyz_[1] + wxyz_[2] * wxyz_[2] + wxyz_[3] * wxyz_[3];
  }

  // The norm of the quaternion.
  Expr norm() const { return sqrt(squared_norm()); }

  // Return a copy of this quaternion that has been normalized.
  [[nodiscard]] Quaternion normalized() const {
    const Expr n = norm();
    return {w() / n, x() / n, y() / n, z() / n};
  }

  // Conjugate the quaternion by negating the complex coefficients:
  // [w, x, y, z] --> [w, -x, -y, -z].
  [[nodiscard]] Quaternion conjugate() const { return {w(), -x(), -y(), -z()}; }

  // Invert the quaternion by conjugating and normalizing.
  [[nodiscard]] Quaternion inverse() const {
    const Expr n = norm();
    const Expr negative_norm = -n;
    return {w() / n, x() / negative_norm, y() / negative_norm, z() / negative_norm};
  }

  // Convert a unit-norm to a rotation matrix. If the input does not have unit norm, the
  // result will not be a valid member of SO(3).
  MatrixExpr to_rotation_matrix() const {
    const Expr x2 = x() * 2;
    const Expr y2 = y() * 2;
    const Expr z2 = z() * 2;
    const Expr wx2 = x2 * w();
    const Expr wy2 = y2 * w();
    const Expr wz2 = z2 * w();
    const Expr xx2 = x2 * x();
    const Expr xy2 = y2 * x();
    const Expr xz2 = z2 * x();
    const Expr yy2 = y2 * y();
    const Expr yz2 = z2 * y();
    const Expr zz2 = z2 * z();
    // clang-format off
    return make_matrix(3, 3,
                        1 - yy2 - zz2,      xy2 - wz2,      xz2 + wy2,
                        xy2 + wz2,      1 - xx2 - zz2,      yz2 - wx2,
                        xz2 - wy2,          yz2 + wx2,  1 - xx2 - yy2
                        );
    // clang-format on
  }

  // Construct quaternion from axis and angle.
  // It is expected that [vx, vy, vz] form a unit vector. Angle is in radians.
  static Quaternion from_angle_axis(const Expr& angle, const Expr& vx, const Expr& vy,
                                    const Expr& vz) {
    Expr half_angle = angle / 2;
    Expr sin_angle = sin(half_angle);
    return {cos(half_angle), vx * sin_angle, vy * sin_angle, vz * sin_angle};
  }

  // Construct quaternion from axis and angle. It is expected that `v` has unit norm.
  // Angle is in radians.
  static Quaternion from_angle_axis(const Expr& angle, const MatrixExpr& v) {
    if (v.rows() != 3 || v.cols() != 1) {
      throw DimensionError("Axis vector must be 3x1. Received: [{}, {}]", v.rows(), v.cols());
    }
    return from_angle_axis(angle, v[0], v[1], v[2]);
  }

  // Construct quaternion from a rotation vector.
  static Quaternion from_rotation_vector(const Expr& vx, const Expr& vy, const Expr& vz,
                                         const bool insert_conditional = true) {
    const Expr angle = sqrt(vx * vx + vy * vy + vz * vz);
    const Expr half_angle = angle / 2;
    const Expr sinc_half_angle = sin(half_angle) / angle;
    if (insert_conditional) {
      return {
          cos(half_angle),
          where(angle > 0, vx * sinc_half_angle, Constants::Zero),
          where(angle > 0, vy * sinc_half_angle, Constants::Zero),
          where(angle > 0, vz * sinc_half_angle, Constants::Zero),
      };
    } else {
      return {
          cos(half_angle),
          vx * sinc_half_angle,
          vy * sinc_half_angle,
          vz * sinc_half_angle,
      };
    }
  }

  // Construct quaternion from a rotation vector.
  static Quaternion from_rotation_vector(const MatrixExpr& v) {
    if (v.rows() != 3 || v.cols() != 1) {
      throw DimensionError("Rotation vector must be 3x1. Received: [{}, {}]", v.rows(), v.cols());
    }
    return from_rotation_vector(v[0], v[1], v[2]);
  }

  // Convenience method for X-axis rotation. Angle is in radians.
  static Quaternion from_x_angle(const Expr& angle) {
    return from_angle_axis(angle, Constants::One, Constants::Zero, Constants::Zero);
  }

  // Convenience method for Y-axis rotation. Angle is in radians.
  static Quaternion from_y_angle(const Expr& angle) {
    return from_angle_axis(angle, Constants::Zero, Constants::One, Constants::Zero);
  }

  // Convenience method for Z-axis rotation. Angle is in radians.
  static Quaternion from_z_angle(const Expr& angle) {
    return from_angle_axis(angle, Constants::Zero, Constants::Zero, Constants::One);
  }

  // Convert to axis-angle representation.
  // Angle is converted into the range [0, pi]. If the norm of the vector component falls below
  // `zero_epsilon`, the rotation cannot be recovered, and we return a zero angle.
  std::tuple<Expr, MatrixExpr> to_angle_axis(const Expr& zero_epsilon = Constants::Zero) const {
    // We want to recover angle and axis from:
    // [cos(angle/2), vx * sin(angle/2), vy * sin(angle/2), vz * sin(angle/2)]
    // http://www.neil.dantam.name/note/dantam-quaternion.pdf (equation 19)
    const Expr vector_norm = sqrt(x() * x() + y() * y() + z() * z());

    static const auto unit_x = make_vector(Constants::One, Constants::Zero, Constants::Zero);
    if (is_zero(vector_norm)) {
      // If the norm is analytically zero, we can't do anything else here:
      return std::make_tuple(Constants::Zero, unit_x);
    }

    // Compute half-angle in range [0, pi/2] --> times 2 --> [0, pi]
    Expr angle = atan2(vector_norm, abs(w())) * 2;
    Expr flipped_norm = where(w() < 0, -vector_norm, vector_norm);

    // TODO: MatrixExpr should be an entirely separate type so this gross casting is not required.
    return std::make_tuple(
        where(vector_norm > zero_epsilon, angle, Constants::Zero),
        where(vector_norm > zero_epsilon,
              make_vector(x() / flipped_norm, y() / flipped_norm, z() / flipped_norm), unit_x));
  }

  // Construct a quaternion from a rotation matrix using Caley's method.
  // If `R` is not a member of SO(3), the behavior is undefined. See:
  // Section 3.5 of: "A survey on the Computation of Quaternions from Rotation Matrices"
  // By S. Sarabandi and F. Thomas
  // This method avoids introducing any divisions.
  static Quaternion from_rotation_matrix(const MatrixExpr& R_in) {
    if (R_in.rows() != 3 || R_in.cols() != 3) {
      throw DimensionError("Rotation matrix must be 3x3. Received: [{}, {}]", R_in.rows(),
                           R_in.cols());
    }
    const Matrix& R = R_in.as_matrix();
    // clang-format off
    Expr a = pow(R(0, 0) + R(1, 1) + R(2, 2) + 1, 2) +
             pow(R(2, 1) - R(1, 2), 2) +
             pow(R(0, 2) - R(2, 0), 2) +
             pow(R(1, 0) - R(0, 1), 2);
    Expr b = pow(R(2, 1) - R(1, 2), 2) +
             pow(R(0, 0) - R(1, 1) - R(2, 2) + 1, 2) +
             pow(R(1, 0) + R(0, 1), 2) +
             pow(R(2, 0) + R(0, 2), 2);
    Expr c = pow(R(0, 2) - R(2, 0), 2) +
             pow(R(1, 0) + R(0, 1), 2) +
             pow(R(1, 1) - R(0, 0) - R(2, 2) + 1, 2) +
             pow(R(2, 1) + R(1, 2), 2);
    Expr d = pow(R(1, 0) - R(0, 1), 2) +
             pow(R(2, 0) + R(0, 2), 2) +
             pow(R(2, 1) + R(1, 2), 2) +
             pow(R(2, 2) - R(0, 0) - R(1, 1) + 1, 2);
    // clang-format on
    return {sqrt(a) / 4,
            // TODO: Add a sign-no-zero method to avoid conditional here?
            where(R(2, 1) - R(1, 2) >= 0, 1, -1) * sqrt(b) / 4,
            where(R(0, 2) - R(2, 0) >= 0, 1, -1) * sqrt(c) / 4,
            where(R(1, 0) - R(0, 1) >= 0, 1, -1) * sqrt(d) / 4};
  }

 private:
  // Quaternion elements in order [w, x, y, z].
  std::array<Expr, 4> wxyz_;
};

// Multiply two quaternions together.
inline Quaternion operator*(const Quaternion& a, const Quaternion& b) {
  return {a.w() * b.w() - a.x() * b.x() - a.y() * b.y() - a.z() * b.z(),
          a.w() * b.x() + a.x() * b.w() + a.y() * b.z() - a.z() * b.y(),
          a.w() * b.y() + a.y() * b.w() + a.z() * b.x() - a.x() * b.z(),
          a.w() * b.z() + a.z() * b.w() + a.x() * b.y() - a.y() * b.x()};
}

}  // namespace math