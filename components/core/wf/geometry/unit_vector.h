// wrenfold symbolic code generator.
// Copyright (c) 2026 wrenfold contributors
// For license information refer to accompanying LICENSE file.
#pragma once

#include "wf/matrix_expression.h"

namespace wf {

// Symbolic unit vector stored as an Nx1 column vector, with N >= 2.
// Construction does not normalize or constrain the coordinates. Callers should supply unit-norm
// coordinates when representing a point on the unit sphere; the manifold methods preserve the
// input norm, as does Ceres SphereManifold. Its Householder tangent frame is discontinuous at
// some points on the sphere.
class unit_vector {
 public:
  // Initialize from an Nx1 column vector of ambient coordinates, where N >= 2.
  // A zero vector cannot be used with normalization or the manifold methods.
  explicit unit_vector(matrix_expr value);

  // Construct symbolic ambient coordinates with the given name prefix.
  // For example, from_name_prefix("u", 3) stores [u_0, u_1, u_2]^T.
  // The variables are not constrained to have unit norm.
  static unit_vector from_name_prefix(std::string_view name, index_t dimension);

  // Number of stored coordinates N. The tangent space has dimension N - 1.
  index_t dimension() const { return value_.rows(); }

  // Access ambient coordinate `i` in storage order.
  const scalar_expr& operator[](index_t i) const { return value_[i]; }

  // Return the stored coordinates as an Nx1 column vector.
  const matrix_expr& to_vector() const { return value_; }

  // True if all sub-expressions in the vector are identical to those in the argument.
  bool is_identical_to(const unit_vector& other) const {
    return value_.is_identical_to(other.value_);
  }

  // Create a new unit_vector by substituting in every ambient coordinate. No normalization occurs.
  unit_vector subs(const scalar_expr& target, const scalar_expr& replacement) const {
    return unit_vector{value_.subs(target, replacement)};
  }

  // The squared L2 norm of the ambient coordinates.
  scalar_expr squared_norm() const { return value_.squared_norm(); }

  // The L2 norm of the ambient coordinates.
  scalar_expr norm() const { return value_.norm(); }

  // Return a copy normalized to unit length. Undefined for a zero vector.
  unit_vector normalized() const { return unit_vector{value_ / norm()}; }

  // Compute the NxM Jacobian of the stored ambient coordinates with respect to the M variables
  // in `vars`. This does not map the derivative into the (N-1)-dimensional tangent space.
  // Non-differentiable operations are treated as constants unless `behavior` is `abstract`.
  matrix_expr jacobian(const matrix_expr& vars, non_differentiable_behavior behavior =
                                                    non_differentiable_behavior::constant) const {
    return value_.jacobian(vars, behavior);
  }

  // Retract an (N-1)x1 tangent perturbation onto the sphere about this vector.
  // Coordinates follow Ceres SphereManifold's Householder frame, which uses the last ambient
  // coordinate as its pivot. A zero perturbation returns this vector; its norm is preserved.
  unit_vector retract(const matrix_expr& delta) const;

  // Return the (N-1)x1 tangent displacement from this vector to `other`.
  // Both vectors must have the same ambient dimension and should have the same nonzero norm.
  // At the antipode the direction is ambiguous; the last tangent axis is used, as in Ceres.
  matrix_expr local_coordinates(const unit_vector& other) const;

  // Compute the Nx(N-1) derivative of retract(delta) with respect to `delta` at delta = 0.
  // This matches Ceres SphereManifold::PlusJacobian for the stored ambient coordinates.
  matrix_expr retract_derivative() const;

  // Compute the (N-1)xN derivative of local_coordinates(unit_vector{value + dv}) with respect
  // to an additive ambient perturbation `dv` at dv = 0. This matches Ceres
  // SphereManifold::MinusJacobian for the stored ambient coordinates.
  matrix_expr local_coordinates_derivative() const;

 private:
  // Ceres-style Householder reflection with the last coordinate as pivot.
  matrix_expr householder() const;

  // Ambient coordinates in storage order.
  matrix_expr value_;
};

// Convenience alias for 3D use; construction does not enforce a dimension of three.
using unit3 = unit_vector;

template <>
struct hash_struct<unit_vector> {
  std::size_t operator()(const unit_vector& x) const {
    return hash_all(0, x.to_vector().to_vector());
  }
};

template <>
struct is_identical_struct<unit_vector> {
  bool operator()(const unit_vector& a, const unit_vector& b) const { return a.is_identical_to(b); }
};

}  // namespace wf
