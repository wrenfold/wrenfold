// Copyright 2023 Gareth Cross
#pragma once
#include "matrix_expression.h"

namespace math {

// Create a column vector from the arguments.
template <typename... Ts>
static MatrixExpr Vector(Ts&&... args) {
  return MatrixExpr::Create(sizeof...(Ts), 1, {Expr{std::forward<Ts>(args)}...});
}

// Create a row vector from the arguments.
template <typename... Ts>
static MatrixExpr RowVector(Ts&&... args) {
  return MatrixExpr::Create(1, sizeof...(Ts), {Expr{std::forward<Ts>(args)}...});
}

// Create a matrix from the arguments (args specified in row-major order).
template <typename... Ts>
static MatrixExpr CreateMatrix(index_t rows, index_t cols, Ts&&... args) {
  return MatrixExpr::Create(rows, cols, {Expr{std::forward<Ts>(args)}...});
}

// Create a matrix of symbols.
MatrixExpr MatrixOfSymbols(std::string_view prefix, index_t rows, index_t cols);

// Create a matrix of zeros.
MatrixExpr Zeros(index_t rows, index_t cols);

// Create an identity matrix.
MatrixExpr Identity(index_t rows);

// Create a vector by flattening a matrix in column-major order.
MatrixExpr Vec(const MatrixExpr& m);

}  // namespace math