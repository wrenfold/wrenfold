// Copyright 2023 Gareth Cross
#pragma once
#include <tuple>

#include "wf/matrix_expression.h"

namespace wf {

// Create a column vector from the arguments.
template <typename... Ts>
static MatrixExpr make_vector(Ts&&... args) {
  return MatrixExpr::create(sizeof...(Ts), 1, {Expr{std::forward<Ts>(args)}...});
}

// Create a row vector from the arguments.
template <typename... Ts>
static MatrixExpr make_row_vector(Ts&&... args) {
  return MatrixExpr::create(1, sizeof...(Ts), {Expr{std::forward<Ts>(args)}...});
}

// Create a matrix from the arguments (args specified in row-major order).
template <typename... Ts>
static MatrixExpr make_matrix(index_t rows, index_t cols, Ts&&... args) {
  return MatrixExpr::create(rows, cols, {Expr{std::forward<Ts>(args)}...});
}

// Create a matrix of symbols.
MatrixExpr make_matrix_of_symbols(std::string_view prefix, index_t rows, index_t cols);

// Create a matrix of zeros.
MatrixExpr make_zeros(index_t rows, index_t cols);

// Create an identity matrix.
MatrixExpr make_identity(index_t rows);

// Create a vector by flattening a matrix in column-major order.
MatrixExpr vectorize_matrix(const MatrixExpr& m);

// Horizontally stack matrices.
MatrixExpr hstack(absl::Span<const MatrixExpr> values);

// Vertically stack matrices.
MatrixExpr vstack(absl::Span<const MatrixExpr> values);

// Diagonally stack matrices (filling in off-diagonal elements with zero).
MatrixExpr diagonal_stack(absl::Span<const MatrixExpr> values);

// Perform full-pivoting gaussian elimination on a symbolic matrix.
// Returns matrices [P, L, U, Q] such that: A = P*L*U*Q
// Where `L` is lower triangular, `U` is upper triangular, and both `P` and `Q` are permutation
// matrices.
std::tuple<MatrixExpr, MatrixExpr, MatrixExpr, MatrixExpr> factorize_full_piv_lu(
    const MatrixExpr& A);

// Compute determinant of a matrix. Only valid for square matrices.
Expr determinant(const MatrixExpr& m);

}  // namespace wf
