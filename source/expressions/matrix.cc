// Copyright 2023 Gareth Cross
#include "expressions/matrix.h"

#include "expressions/addition.h"

namespace math {

// Iterate over a matrix and run `callable` on each element. The purpose of this method is
// to have one place (or fewer places) where the traversal order (row-major) is specified.
template <typename Callable>
inline void IterMatrix(std::size_t rows, std::size_t cols, Callable&& callable) {
  for (std::size_t i = 0; i < rows; ++i) {
    for (std::size_t j = 0; j < cols; ++j) {
      callable(i, j);
    }
  }
}

Matrix Matrix::Transpose() const {
  std::vector<Expr> output{};
  output.reserve(Size());
  const std::size_t output_rows = NumCols();
  const std::size_t output_cols = NumRows();
  IterMatrix(output_rows, output_cols,
             [&](std::size_t r, std::size_t c) { output.push_back(operator()(c, r)); });
  return Matrix(output_rows, output_cols, std::move(output));
}

Matrix operator*(const Matrix& a, const Matrix& b) {
  if (a.NumCols() != b.NumRows()) {
    throw DimensionError(
        "Dimension mismatch in matrix multiplication: ({}, {}) x ({}, {}). Inner dimensions must "
        "match.",
        a.NumRows(), a.NumCols(), b.NumRows(), b.NumCols());
  }
  const std::size_t output_rows = a.NumRows();
  const std::size_t output_cols = b.NumCols();

  std::vector<Expr> output;
  output.reserve(output_rows * output_cols);

  IterMatrix(output_rows, output_cols, [&](std::size_t i, std::size_t j) {
    // Multiply row times column:
    std::vector<Expr> addition_args;
    addition_args.reserve(a.NumCols());
    for (std::size_t k = 0; k < a.NumCols(); ++k) {
      Expr prod = a(i, k) * b(k, j);
      if (!IsZero(prod)) {
        addition_args.push_back(std::move(prod));
      }
    }
    output.push_back(Addition::FromOperands(addition_args));
  });
  return Matrix(output_rows, output_cols, std::move(output));
}

}  // namespace math
