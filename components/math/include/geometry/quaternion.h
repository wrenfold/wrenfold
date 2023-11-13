// Copyright 2023 Gareth Cross
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

  // Construct from a vector ordered `wxyz`.
  static Quaternion from_vector_wxyz(const MatrixExpr& q) {
    if (q.rows() != 4 || q.cols() != 1) {
      throw DimensionError("Quaternion storage must be 4x1. Received [{} x {}]", q.rows(),
                           q.cols());
    }
    return Quaternion{q[0], q[1], q[2], q[3]};
  }

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
  Quaternion subs(const Expr& target, const Expr& replacement) const;

  // The squared L2 norm of the quaternion elements.
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
  MatrixExpr to_rotation_matrix() const;

  // Construct quaternion from axis and angle.
  // It is expected that [vx, vy, vz] form a unit vector. Angle is in radians.
  static Quaternion from_angle_axis(const Expr& angle, const Expr& vx, const Expr& vy,
                                    const Expr& vz);

  // Construct quaternion from axis and angle. It is expected that `v` has unit norm.
  // Angle is in radians.
  static Quaternion from_angle_axis(const Expr& angle, const MatrixExpr& v);

  // Construct quaternion from a rotation vector.
  // When the rotation angle < epsilon, the first order taylor series is used instead.
  static Quaternion from_rotation_vector(const Expr& vx, const Expr& vy, const Expr& vz,
                                         std::optional<Expr> epsilon);

  // Construct quaternion from a rotation vector.
  // When the rotation angle < epsilon, the first order taylor series is used instead.
  static Quaternion from_rotation_vector(const MatrixExpr& v, std::optional<Expr> epsilon);

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
  // `epsilon`, the rotation cannot be recovered, and we return a zero angle.
  std::tuple<Expr, MatrixExpr> to_angle_axis(std::optional<Expr> epsilon) const;

  // Convert quaternion to a Rodrigues rotation vector.
  // When the rotation angle < epsilon, the first order taylor series is used instead.
  MatrixExpr to_rotation_vector(std::optional<Expr> epsilon) const;

  // Construct a quaternion from a rotation matrix using Caley's method.
  // If `R` is not a member of SO(3), the behavior is undefined. See:
  // Section 3.5 of: "A survey on the Computation of Quaternions from Rotation Matrices"
  // By S. Sarabandi and F. Thomas
  static Quaternion from_rotation_matrix(const MatrixExpr& R_in);

  // Compute the 4x3 tangent-space derivative of this quaternion with respect to a right-multiplied
  // perturbation. This is the derivative:
  //
  //  d(Q * exp(w)) / dw evaluated at w = 0
  //
  // Where `Q` is this quaternion, and `w` is an infinitesimal rotation vector, retracted on the
  // right side of Q. It is assumed that `Q` has unit norm, otherwise the result is undefined.
  MatrixExpr right_retract_derivative() const;

  // Compute the 3x4 tangent-space derivative of the operation:
  //
  //  d(log(Q^T * (Q + dq)) / dq evaluated at dq = 0
  //
  // Where `Q` is this (unit norm) quaternion, and Q^T denotes the conjugation operation. dQ is an
  // infinitesimal additive perturbation to the quaternion.
  MatrixExpr right_local_coordinates_derivative() const;

 private:
  // Quaternion elements in order [w, x, y, z].
  std::array<Expr, 4> wxyz_;
};

// Multiply two quaternions together.
Quaternion operator*(const Quaternion& a, const Quaternion& b);

}  // namespace math
