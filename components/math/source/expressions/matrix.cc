// Copyright 2023 Gareth Cross
#include "expressions/matrix.h"

#include "expressions/addition.h"
#include "matrix_functions.h"

namespace math {

Matrix Matrix::GetBlock(const index_t row, const index_t col, const index_t nrows,
                        const index_t ncols) const {
  if (row < 0 || row + nrows > rows_ || col < 0 || col + ncols > cols_) {
    throw DimensionError(
        "Block [position: ({}, {}), size: ({}, {})] is out of bounds for matrix of shape ({}, {})",
        row, col, nrows, ncols, rows_, cols_);
  }
  std::vector<Expr> data;
  data.reserve(nrows * ncols);
  IterMatrix(nrows, ncols,
             [&](index_t i, index_t j) { data.push_back(GetUnchecked(i + row, j + col)); });
  return Matrix(nrows, ncols, std::move(data));
}

Matrix Matrix::Transpose() const {
  std::vector<Expr> output{};
  output.reserve(Size());
  const index_t output_rows = NumCols();
  const index_t output_cols = NumRows();
  IterMatrix(output_rows, output_cols,
             [&](index_t r, index_t c) { output.push_back(operator()(c, r)); });
  return Matrix(output_rows, output_cols, std::move(output));
}

void Matrix::MultiplyByScalarInPlace(const Expr& arg) {
  if (arg.Is<Matrix>()) {
    throw TypeError("Expected a scalar expression, but got a matrix of type: {}", arg.TypeName());
  }
  for (Expr& expr : data_) {
    expr = expr * arg;
  }
}

Matrix operator*(const Matrix& a, const Matrix& b) {
  if (a.NumCols() != b.NumRows()) {
    throw DimensionError(
        "Dimension mismatch in matrix multiplication: ({}, {}) x ({}, {}). Inner dimensions must "
        "match.",
        a.NumRows(), a.NumCols(), b.NumRows(), b.NumCols());
  }
  const index_t output_rows = a.NumRows();
  const index_t output_cols = b.NumCols();

  std::vector<Expr> output;
  output.reserve(output_rows * output_cols);

  std::vector<Expr> addition_args;
  IterMatrix(output_rows, output_cols, [&](index_t i, index_t j) {
    // Multiply row times column:
    addition_args.clear();
    for (index_t k = 0; k < a.NumCols(); ++k) {
      Expr prod = a(i, k) * b(k, j);
      if (!IsZero(prod)) {
        addition_args.push_back(std::move(prod));
      }
    }
    output.push_back(Addition::FromOperands(addition_args));
  });
  return Matrix(output_rows, output_cols, std::move(output));
}

Matrix operator+(const Matrix& a, const Matrix& b) {
  if (a.NumRows() != b.NumRows() || a.NumCols() != b.NumCols()) {
    throw DimensionError("Dimension mismatch in matrix addition: ({}, {}) + ({}, {}).", a.NumRows(),
                         a.NumCols(), b.NumRows(), b.NumCols());
  }
  std::vector<Expr> output;
  output.reserve(a.Size());
  IterMatrix(a.NumRows(), a.NumCols(),
             [&](index_t i, index_t j) { output.push_back(a(i, j) + b(i, j)); });

  return Matrix(a.NumRows(), a.NumCols(), std::move(output));
}

}  // namespace math
