// Copyright 2023 Gareth Cross
#pragma once
#include "expression.h"
#include "functions.h"
#include "matrix_functions.h"
#include "type_annotations.h"

// Some symbolic functions we use in unit tests.
namespace math {

namespace ta = type_annotations;

inline Expr SimpleMultiplyAdd(Expr x, Expr y, Expr z) { return x * y + z; }

inline auto VectorRotation2D(Expr theta, ta::StaticMatrix<2, 1> v) {
  using namespace matrix_operator_overloads;
  MatrixExpr R = CreateMatrix(2, 2, cos(theta), -sin(theta), sin(theta), cos(theta));
  MatrixExpr v_rot{R * v};
  ta::StaticMatrix<2, 1> v_dot_D_theta{v_rot.Diff(theta)};
  return std::make_tuple(ReturnValue(v_rot), OptionalOutputArg("D_theta", v_dot_D_theta));
}

// Norm of a 3D vector + the 1x3 derivative.
inline auto VectorNorm3D(ta::StaticMatrix<3, 1> v) {
  Expr len = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
  auto D_v = ta::StaticMatrix<1, 3>::Create(len.Diff(v[0]), len.Diff(v[1]), len.Diff(v[2]));
  return std::make_tuple(ReturnValue(len), OutputArg("D_v", D_v));
}

// Heaviside step function - a very simple conditional.
inline Expr Heaviside(Expr x) { return where(x >= 0, 1, 0); }

// Exclusive or on the conditions x > 0 and y > 0.
inline Expr ExclusiveOr(Expr x, Expr y) {
  Expr cond_x = x > 0;
  Expr cond_y = y > 0;
  return where(cond_x, where(cond_y, 0, 1), where(cond_y, 1, 0));
}

// A handwritten signum implemented with conditionals and abs().
// Not very useful, but we use it as a unit test case.
inline Expr HandwrittenSignum(Expr x) { return where(x == 0, 0, x / abs(x)); }

// A handwritten `abs` implemented with signum.
// Not very useful, but used as a unit test case.
inline Expr HandwrittenAbs(Expr x) { return signum(x) * x; }

// Arc-tangent w/ derivatives.
inline auto Atan2WithDerivatives(Expr y, Expr x) {
  Expr f = atan2(y, x);
  return std::make_tuple(ReturnValue(f), OutputArg("D_y", f.Diff(y)), OutputArg("D_x", f.Diff(x)));
}

}  // namespace math
