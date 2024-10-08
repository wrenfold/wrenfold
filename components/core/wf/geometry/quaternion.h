// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/constants.h"
#include "wf/expression.h"
#include "wf/matrix_expression.h"
#include "wf/utility/assertions.h"

#include <array>

namespace wf {

// Symbolic quaternion type.
class quaternion {
 public:
  // Initialize with elements in order [w, x, y, z].
  // No normalization occurs by default.
  quaternion(scalar_expr w, scalar_expr x, scalar_expr y, scalar_expr z)
      : wxyz_{std::move(w), std::move(x), std::move(y), std::move(z)} {}

  // Default initialize to identity (w = 1, xyz = 0).
  quaternion() : quaternion(constants::one, constants::zero, constants::zero, constants::zero) {}

  // Construct from a vector ordered `wxyz`.
  static quaternion from_vector_wxyz(const matrix_expr& q) {
    if (q.rows() != 4 || q.cols() != 1) {
      throw dimension_error("Quaternion storage must be 4x1. Received [{} x {}]", q.rows(),
                            q.cols());
    }
    return quaternion{q[0], q[1], q[2], q[3]};
  }

  // Construct from a vector ordered `xyzw` (scalar last).
  static quaternion from_vector_xyzw(const matrix_expr& q) {
    if (q.rows() != 4 || q.cols() != 1) {
      throw dimension_error("Quaternion storage must be 4x1. Received [{} x {}]", q.rows(),
                            q.cols());
    }
    return quaternion{q[3], q[0], q[1], q[2]};
  }

  // Construct a quaternion of variables w/ the given name prefix.
  // Members will have the names `{name}_[w|x|y|z]`.
  static quaternion from_name_prefix(std::string_view name);

  // Access components:
  constexpr const scalar_expr& w() const noexcept { return wxyz_[0]; }
  constexpr const scalar_expr& x() const noexcept { return wxyz_[1]; }
  constexpr const scalar_expr& y() const noexcept { return wxyz_[2]; }
  constexpr const scalar_expr& z() const noexcept { return wxyz_[3]; }

  // Access coeff array as wxyz array.
  constexpr const std::array<scalar_expr, 4>& wxyz() const noexcept { return wxyz_; }

  // True if all sub-expressions in the quaternion are identical to those in the argument.
  bool is_identical_to(const quaternion& other) const {
    return std::equal(wxyz_.begin(), wxyz_.end(), other.wxyz_.begin(),
                      is_identical_struct<scalar_expr>{});
  }

  // Convert to a column vector in [w,x,y,z] order.
  matrix_expr to_vector_wxyz() const;

  // Convert to a column vector in [x,y,z,w] order.
  matrix_expr to_vector_xyzw() const;

  // Create a new quaternion by substituting.
  quaternion subs(const scalar_expr& target, const scalar_expr& replacement) const;

  // The squared L2 norm of the quaternion elements.
  scalar_expr squared_norm() const;

  // The norm of the quaternion.
  scalar_expr norm() const;

  // Return a copy of this quaternion that has been normalized.
  [[nodiscard]] quaternion normalized() const {
    const scalar_expr n = norm();
    return {w() / n, x() / n, y() / n, z() / n};
  }

  // Conjugate the quaternion by negating the complex coefficients:
  // [w, x, y, z] --> [w, -x, -y, -z].
  [[nodiscard]] quaternion conjugate() const { return {w(), -x(), -y(), -z()}; }

  // Invert the quaternion by conjugating and dividing by squared norm.
  [[nodiscard]] quaternion inverse() const;

  // Negate every element of the quaternion.
  quaternion operator-() const;

  // Convert a unit-norm to a rotation matrix. If the input does not have unit norm, the
  // result will not be a valid member of SO(3).
  matrix_expr to_rotation_matrix() const;

  // Construct quaternion from axis and angle.
  // It is expected that [vx, vy, vz] form a unit vector. Angle is in radians.
  static quaternion from_angle_axis(const scalar_expr& angle, const scalar_expr& vx,
                                    const scalar_expr& vy, const scalar_expr& vz);

  // Construct quaternion from axis and angle. It is expected that `v` has unit norm.
  // Angle is in radians.
  static quaternion from_angle_axis(const scalar_expr& angle, const matrix_expr& v);

  // Construct quaternion from a rotation vector.
  // When the rotation angle < epsilon, the first order taylor series is used instead.
  static quaternion from_rotation_vector(const scalar_expr& vx, const scalar_expr& vy,
                                         const scalar_expr& vz,
                                         const std::optional<scalar_expr>& epsilon);

  // Construct quaternion from a rotation vector.
  // When the rotation angle < epsilon, the first order taylor series is used instead.
  static quaternion from_rotation_vector(const matrix_expr& v,
                                         const std::optional<scalar_expr>& epsilon);

  // Convenience method for X-axis rotation. Angle is in radians.
  static quaternion from_x_angle(const scalar_expr& angle) {
    return from_angle_axis(angle, constants::one, constants::zero, constants::zero);
  }

  // Convenience method for Y-axis rotation. Angle is in radians.
  static quaternion from_y_angle(const scalar_expr& angle) {
    return from_angle_axis(angle, constants::zero, constants::one, constants::zero);
  }

  // Convenience method for Z-axis rotation. Angle is in radians.
  static quaternion from_z_angle(const scalar_expr& angle) {
    return from_angle_axis(angle, constants::zero, constants::zero, constants::one);
  }

  // Convert to axis-angle representation.
  // Angle is converted into the range [0, pi]. If the norm of the vector component falls below
  // `epsilon`, the rotation cannot be recovered, and we return a zero angle.
  std::tuple<scalar_expr, matrix_expr> to_angle_axis(
      const std::optional<scalar_expr>& epsilon) const;

  // Convert quaternion to a Rodrigues rotation vector.
  // When the rotation angle < epsilon, the first order taylor series is used instead.
  matrix_expr to_rotation_vector(const std::optional<scalar_expr>& epsilon,
                                 bool use_atan2 = true) const;

  // Construct a quaternion from a rotation matrix using Sheppards's method.
  // If `R` is not a member of SO(3), the behavior is undefined. See:
  // Section 3.3 of: "A survey on the Computation of Quaternions from Rotation Matrices"
  // By S. Sarabandi and F. Thomas
  static quaternion from_rotation_matrix(const matrix_expr& R_in);

  // Compute 4xN jacobian of this quaternion with respect to the `N` input variables `vars`.
  // The rows of the jacobian are ordered in the storage order of the quaternion: [w,x,y,z].
  matrix_expr jacobian(
      absl::Span<const scalar_expr> vars,
      non_differentiable_behavior behavior = non_differentiable_behavior::constant) const;

  // Compute the 4xN jacobian of this quaternion with respect to the `Nx1` column vector `vars`.
  // The rows of the jacobian are ordered in the storage order of the quaternion: [w,x,y,z].
  matrix_expr jacobian(const matrix_expr& vars, non_differentiable_behavior behavior =
                                                    non_differentiable_behavior::constant) const;

  // Compute the 4x4 jacobian of this quaternion with respect to the variables in quaternion `vars`.
  // The rows and columns of the output jacobian are ordered in the storage order of the quaternion:
  // [w,x,y,z].
  matrix_expr jacobian(const quaternion& vars, non_differentiable_behavior behavior =
                                                   non_differentiable_behavior::constant) const {
    return jacobian(vars.wxyz(), behavior);
  }

  // Compute the 4x3 tangent-space derivative of this quaternion with respect to a right-multiplied
  // perturbation. This is the derivative:
  //
  //  d(Q * exp(w)) / dw evaluated at w = 0
  //
  // Where `Q` is this quaternion, and `w` is an infinitesimal rotation vector, retracted on the
  // right side of Q. It is assumed that `Q` has unit norm, otherwise the result is undefined.
  matrix_expr right_retract_derivative() const;

  // Compute the 3x4 tangent-space derivative of the operation:
  //
  //  d(log(Q^T * (Q + dq)) / dq evaluated at dq = 0
  //
  // Where `Q` is this (unit norm) quaternion, and Q^T denotes the conjugation operation. dQ is an
  // infinitesimal additive perturbation to the quaternion.
  matrix_expr right_local_coordinates_derivative() const;

 private:
  // Quaternion elements in order [w, x, y, z].
  std::array<scalar_expr, 4> wxyz_;
};

// Multiply two quaternions together.
quaternion operator*(const quaternion& a, const quaternion& b);

// Compute the left Jacobian of SO(3) as a function of a rodrigues vector `w`.
// This is the integral of `exp(w * alpha)` over the interval alpha = [0, 1], where
// `exp(v)` is the exponential map of SO(3). If `v` is a rodrigues vector, then `exp(v)` is
// the matching 3x3 rotation matrix.
//
// This is also the derivative of: log(exp(w + dw) * R^T) with respect to the additive
// perturbation `dw`, evaluated about `dw = 0`.
//
// You can obtain the right jacobian of SO(3) by transposing this matrix.
//
// When the norm of `w` falls below `epsilon`, the small angle approximation is used.
matrix_expr left_jacobian_of_so3(const matrix_expr& w, const std::optional<scalar_expr>& epsilon);

// Compute the _inverse_ of the left Jacobian of SO(3) as a function of a rodrigues vector `w`.
// If `left_jacobian_of_so3` produces V(w), then `inverse_left_jacobian_of_so3` produces V^-1(w)
// such that V^-1(w) * V(w) = I3.
//
// When the norm of `w` falls below epsilon, the small angle approximation is used.
matrix_expr inverse_left_jacobian_of_so3(const matrix_expr& w,
                                         const std::optional<scalar_expr>& epsilon);

template <>
struct hash_struct<quaternion> {
  std::size_t operator()(const quaternion& q) const { return hash_all(0, q.wxyz()); }
};

template <>
struct is_identical_struct<quaternion> {
  bool operator()(const quaternion& a, const quaternion& b) const {
    return std::equal(a.wxyz().begin(), a.wxyz().end(), b.wxyz().begin(),
                      is_identical_struct<scalar_expr>{});
  }
};

}  // namespace wf
