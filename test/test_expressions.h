// Copyright 2023 Gareth Cross
#pragma once
#include "expression.h"
#include "matrix_functions.h"
#include "type_annotations.h"

// Some symbolic functions we use in unit tests.
namespace math {

namespace ta = type_annotations;
using namespace matrix_operator_overloads;

inline Expr SimpleMultiplyAdd(Expr x, Expr y, Expr z) { return x * y + z; }

inline ta::StaticMatrix<2, 1> VectorRotation2D(Expr theta, ta::StaticMatrix<2, 1> v,
                                               ta::StaticMatrix<2, 1>& D_theta) {
  MatrixExpr R = CreateMatrix(2, 2, cos(theta), -sin(theta), sin(theta), cos(theta));
  MatrixExpr v_rot{R * v};
  D_theta = v_rot.Diff(theta);
  return v_rot;
}

inline Expr VectorNorm3D(ta::StaticMatrix<3, 1> v, ta::StaticMatrix<1, 3>& D_v) {
  Expr len = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
  D_v = ta::StaticMatrix<1, 3>::Create(len.Diff(v[0]), len.Diff(v[1]), len.Diff(v[2]));
  return len;
}

}  // namespace math
