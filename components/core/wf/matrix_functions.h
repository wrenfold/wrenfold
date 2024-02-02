// Copyright 2023 Gareth Cross
#pragma once
#include <tuple>

#include "wf/matrix_expression.h"

namespace wf {

// Create a column vector from the arguments.
template <typename... Ts>
static matrix_expr make_vector(Ts&&... args) {
  return matrix_expr::create(sizeof...(Ts), 1, {Expr{std::forward<Ts>(args)}...});
}

// Create a row vector from the arguments.
template <typename... Ts>
static matrix_expr make_row_vector(Ts&&... args) {
  return matrix_expr::create(1, sizeof...(Ts), {Expr{std::forward<Ts>(args)}...});
}

// Create a matrix from the arguments (args specified in row-major order).
template <typename... Ts>
static matrix_expr make_matrix(index_t rows, index_t cols, Ts&&... args) {
  return matrix_expr::create(rows, cols, {Expr{std::forward<Ts>(args)}...});
}

// Create a matrix of symbols.
matrix_expr make_matrix_of_symbols(std::string_view prefix, index_t rows, index_t cols);

// Create a matrix of zeros.
matrix_expr make_zeros(index_t rows, index_t cols);

// Create an identity matrix.
matrix_expr make_identity(index_t rows);

// Create a vector by flattening a matrix in column-major order.
matrix_expr vectorize_matrix(const matrix_expr& m);

// Horizontally stack matrices.
matrix_expr hstack(absl::Span<const matrix_expr> values);

// Vertically stack matrices.
matrix_expr vstack(absl::Span<const matrix_expr> values);

// Diagonally stack matrices (filling in off-diagonal elements with zero).
matrix_expr diagonal_stack(absl::Span<const matrix_expr> values);

// Perform full-pivoting gaussian elimination on a symbolic matrix.
// Returns matrices [P, L, U, Q] such that: A = P*L*U*Q
// Where `L` is lower triangular, `U` is upper triangular, and both `P` and `Q` are permutation
// matrices.
std::tuple<matrix_expr, matrix_expr, matrix_expr, matrix_expr> factorize_full_piv_lu(
    const matrix_expr& A);

// Compute determinant of a matrix. Only valid for square matrices.
Expr determinant(const matrix_expr& m);

}  // namespace wf
