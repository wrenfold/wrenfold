// Copyright 2023 Gareth Cross
#pragma once
#include "expression.h"
#include "matrix_functions.h"
#include "type_annotations.h"

// Some symbolic functions we use in unit tests.
namespace math {
namespace ta = type_annotations;

inline Expr SimpleMultiplyAdd(Expr x, Expr y, Expr z) { return x * y + z; }

#if 0
inline ta::StaticMatrix<2, 1> VectorRotation2D(Expr theta, ta::StaticMatrix<2, 1> v) {
  MatrixExpr R = CreateMatrix(2, 2, cos(theta), -sin(theta), sin(theta), cos(theta));
  MatrixExpr v_rot = R * v;
  return ta::StaticMatrix<2, 1>(v_rot);
}
#endif

}  // namespace math
