// Copyright 2023 Gareth Cross
#pragma once
#include "expression.h"

namespace math {

// Create a column vector from the arguments.
template <typename... Ts>
static MatrixExpr Vector(Ts&&... args) {
  return MatrixExpr::CreateMatrix(sizeof...(Ts), 1, {Expr{std::forward<Ts>(args)}...});
}

// Create a row vector from the arguments.
template <typename... Ts>
static MatrixExpr RowVector(Ts&&... args) {
  return MatrixExpr::CreateMatrix(1, sizeof...(Ts), {Expr{std::forward<Ts>(args)}...});
}

// Create a matrix from the arguments (args specified in row-major order).
template <typename... Ts>
static MatrixExpr CreateMatrix(std::size_t rows, std::size_t cols, Ts&&... args) {
  return MatrixExpr::CreateMatrix(rows, cols, {Expr{std::forward<Ts>(args)}...});
}

// Create a matrix of zeros.
MatrixExpr Zeros(std::size_t rows, std::size_t cols);

// Create an identity matrix.
MatrixExpr Identity(std::size_t rows);

}  // namespace math
