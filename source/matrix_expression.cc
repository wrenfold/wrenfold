// Copyright 2023 Gareth Cross
#include "matrix_expression.h"

#include "expressions/matrix.h"

namespace math {

MatrixExpr::MatrixExpr(Expr&& arg) : expr_(std::move(arg)) {
  ASSERT(expr_.Impl(), "Expr is non-nullable");
  // TODO: Need to handle other matrix types here eventually.
  if (!TryCast<Matrix>(expr_)) {
    throw TypeError(
        "Attempted to construct MatrixExpr from expression of type: {}. Expression = {}",
        expr_.TypeName(), expr_.ToString());
  }
}

MatrixExpr::MatrixExpr(const Expr& arg) : MatrixExpr(Expr{arg}) {}

MatrixExpr MatrixExpr::Create(index_t rows, index_t cols, std::vector<Expr> args) {
  return MatrixExpr{MakeExpr<Matrix>(rows, cols, std::move(args))};
}

// For now, a lot of these methods just forward to the `Matrix` type. In the future,
// we may have different matrix types.

index_t MatrixExpr::NumRows() const { return AsMatrix().NumRows(); }

index_t MatrixExpr::NumCols() const { return AsMatrix().NumCols(); }

const Expr& MatrixExpr::operator[](index_t i) const { return AsMatrix()[i]; }

const Expr& MatrixExpr::operator()(index_t i, index_t j) const { return AsMatrix()(i, j); }

MatrixExpr MatrixExpr::GetBlock(index_t row, index_t col, index_t nrows, index_t ncols) const {
  Matrix result = AsMatrix().GetBlock(row, col, nrows, ncols);
  return MatrixExpr{MakeExpr<Matrix>(std::move(result))};
}

MatrixExpr MatrixExpr::Transpose() const {
  return MatrixExpr{MakeExpr<Matrix>(AsMatrix().Transpose())};
}

const Matrix& MatrixExpr::AsMatrix() const {
  // Cast is safe since the constructor checked this condition.
  return static_cast<const Matrix&>(*expr_.Impl());
}

}  // namespace math
