// Copyright 2023 Gareth Cross
#pragma once
#include "wf/expression.h"
#include "wf/functions.h"
#include "wf/geometry/quaternion.h"
#include "wf/matrix_functions.h"
#include "wf/type_annotations.h"

// Some symbolic functions we use in unit tests.
namespace math {

namespace ta = type_annotations;

// A very simple function that combines three input arguments.
inline Expr simple_multiply_add(Expr x, Expr y, Expr z) { return x * y + z; }

// Rotate a 2D vector by an angle. Output the vector, and its derivative with respect
// to the angle.
inline auto vector_rotation_2d(Expr theta, ta::static_matrix<2, 1> v) {
  using namespace matrix_operator_overloads;
  MatrixExpr R = make_matrix(2, 2, cos(theta), -sin(theta), sin(theta), cos(theta));
  MatrixExpr v_rot{R * v};
  ta::static_matrix<2, 1> v_dot_D_theta{v_rot.diff(theta)};
  return std::make_tuple(return_value(v_rot), optional_output_arg("D_theta", v_dot_D_theta));
}

// Norm of a 3D vector + the 1x3 derivative.
inline auto vector_norm_3d(ta::static_matrix<3, 1> v) {
  Expr len = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
  auto D_v = ta::static_matrix<1, 3>::create(len.diff(v[0]), len.diff(v[1]), len.diff(v[2]));
  return std::make_tuple(return_value(len), output_arg("D_v", D_v));
}

// Heaviside step function - a very simple test of conditionals.
inline Expr heaviside(Expr x) { return where(x >= 0, 1, 0); }

// Exclusive or on the conditions x > 0 and y > 0 (a simple nested conditional).
inline Expr exclusive_or(Expr x, Expr y) {
  Expr cond_x = x > 0;
  Expr cond_y = y > 0;
  return where(cond_x, where(cond_y, 0, 1), where(cond_y, 1, 0));
}

// Test generation of signum and abs.
inline auto signum_and_abs(Expr x) {
  return std::make_tuple(return_value(signum(x)), output_arg("abs", abs(x)));
}

// Arc-tangent w/ derivatives.
inline auto atan2_with_derivatives(Expr y, Expr x) {
  Expr f = atan2(y, x);
  return std::make_tuple(return_value(f), output_arg("D_y", f.diff(y)),
                         output_arg("D_x", f.diff(x)));
}

// Three layers of nested conditionals.
inline auto nested_conditionals_1(Expr x, Expr y) {
  Expr c0 = where(y > 0, cos(x * y), cos(x) + 2);
  Expr c1 = where(x > 0, log(abs(y)), atan2(y, x) * 3);
  Expr c2 = where(abs(x) > abs(y), c0 * 3 - sqrt(abs(c0)), pow(abs(c1), 1.0 / 3.0) / 5);
  return c2;
}

// Nested conditionals, but only on the if-branch side.
inline auto nested_conditionals_2(Expr x, Expr y) {
  Expr c0 = where(y > 0, cos(x * y * constants::pi), sin(22 / y) - 3 * x);
  Expr c1 = where(x > 0, c0, atan2(y * 0.4, x * 0.1) * 19 - y);
  Expr c2 = where(abs(x) > abs(y), c1, sqrt(abs(x * y + 2 * x)));
  return c2;
}

// Create a rotation matrix from a Rodrigues vector, and the 9x3 Jacobian of the rotation matrix
// elements with respect to the vector.
inline auto create_rotation_matrix(ta::static_matrix<3, 1> w) {
  MatrixExpr R = Quaternion::from_rotation_vector(w.inner(), 1.0e-16).to_rotation_matrix();
  MatrixExpr R_diff = vectorize_matrix(R).jacobian(w);
  return std::make_tuple(output_arg("R", ta::static_matrix<3, 3>{R}),
                         optional_output_arg("R_D_w", ta::static_matrix<9, 3>{R_diff}));
}

// Recover a Rodrigues rotation vector from a 3x3 rotation matrix.
inline auto rotation_vector_from_matrix(ta::static_matrix<3, 3> R) {
  Quaternion q = Quaternion::from_rotation_matrix(R);
  ta::static_matrix<3, 1> w = q.to_rotation_vector(1.0e-16);
  return std::make_tuple(output_arg("w", w));
}

}  // namespace math