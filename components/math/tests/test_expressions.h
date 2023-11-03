// Copyright 2023 Gareth Cross
#pragma once
#include "expression.h"
#include "functions.h"
#include "geometry/quaternion.h"
#include "matrix_functions.h"
#include "type_annotations.h"

// Some symbolic functions we use in unit tests.
namespace math {

namespace ta = type_annotations;

inline Expr simple_multiply_add(Expr x, Expr y, Expr z) { return x * y + z; }

inline auto vector_rotation_2d(Expr theta, ta::StaticMatrix<2, 1> v) {
  using namespace matrix_operator_overloads;
  MatrixExpr R = make_matrix(2, 2, cos(theta), -sin(theta), sin(theta), cos(theta));
  MatrixExpr v_rot{R * v};
  ta::StaticMatrix<2, 1> v_dot_D_theta{v_rot.diff(theta)};
  return std::make_tuple(ReturnValue(v_rot), OptionalOutputArg("D_theta", v_dot_D_theta));
}

// Norm of a 3D vector + the 1x3 derivative.
inline auto vector_norm_3d(ta::StaticMatrix<3, 1> v) {
  Expr len = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
  auto D_v = ta::StaticMatrix<1, 3>::create(len.diff(v[0]), len.diff(v[1]), len.diff(v[2]));
  return std::make_tuple(ReturnValue(len), OutputArg("D_v", D_v));
}

// Heaviside step function - a very simple conditional.
inline Expr heaviside(Expr x) { return where(x >= 0, 1, 0); }

// Exclusive or on the conditions x > 0 and y > 0.
inline Expr exclusive_or(Expr x, Expr y) {
  Expr cond_x = x > 0;
  Expr cond_y = y > 0;
  return where(cond_x, where(cond_y, 0, 1), where(cond_y, 1, 0));
}

// Test generation of signum and abs.
inline auto signum_and_abs(Expr x) {
  return std::make_tuple(ReturnValue(signum(x)), OutputArg("abs", abs(x)));
}

// Arc-tangent w/ derivatives.
inline auto atan2_with_derivatives(Expr y, Expr x) {
  Expr f = atan2(y, x);
  return std::make_tuple(ReturnValue(f), OutputArg("D_y", f.diff(y)), OutputArg("D_x", f.diff(x)));
}

// Create a rotation matrix from a rodrigues vector, and the 9x3 Jacobian of the rotation matrix
// elements with respect to the vector.
inline auto create_rotation_matrix(ta::StaticMatrix<3, 1> w) {
  MatrixExpr R = Quaternion::from_rotation_vector(w.inner(), 1.0e-16).to_rotation_matrix();
  MatrixExpr R_diff = vectorize_matrix(R).jacobian(w);
  return std::make_tuple(OutputArg("R", ta::StaticMatrix<3, 3>{R}),
                         OptionalOutputArg("R_D_w", ta::StaticMatrix<9, 3>{R_diff}));
}

}  // namespace math
