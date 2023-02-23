// Copyright 2023 Gareth Cross
#include "matrix_functions.h"

#include "constants.h"
#include "expressions/matrix.h"
#include "expressions/variable.h"

namespace math {

template <typename Callable>
MatrixExpr CreateMatrixWithLambda(index_t rows, index_t cols, Callable&& callable) {
  std::vector<Expr> data;
  data.reserve(static_cast<std::size_t>(rows * cols));
  IterMatrix(rows, cols, [callable = std::move(callable), &data](index_t i, index_t j) {
    data.push_back(callable(i, j));
  });
  return MatrixExpr::Create(rows, cols, std::move(data));
}

MatrixExpr MatrixOfSymbols(const std::string_view prefix, index_t rows, index_t cols) {
  if (rows <= 0 || cols <= 0) {
    throw DimensionError("Cannot construct symbolic matrix with shape: ({}, {})", rows, cols);
  }
  return CreateMatrixWithLambda(rows, cols, [&](index_t i, index_t j) {
    std::string name = fmt::format("{}_{}_{}", prefix, i, j);
    return MakeExpr<Variable>(std::move(name));
  });
}

MatrixExpr Zeros(index_t rows, index_t cols) {
  if (rows <= 0 || cols <= 0) {
    throw DimensionError("Cannot construct zero matrix with shape: ({}, {})", rows, cols);
  }
  // Eventually we might have a symbolic zero matrix, and this won't be required.
  std::vector<Expr> data(static_cast<std::size_t>(rows * cols), Constants::Zero);
  return MatrixExpr::Create(rows, cols, std::move(data));
}

// Create an identity matrix.
MatrixExpr Identity(index_t rows) {
  if (rows <= 0) {
    throw DimensionError("Cannot construct identity matrix with dimension: {}", rows);
  }
  return CreateMatrixWithLambda(
      rows, rows, [&](index_t i, index_t j) { return i == j ? Constants::One : Constants::Zero; });
}

MatrixExpr Vec(const MatrixExpr& m) {
  std::vector<Expr> flattened;
  const auto flat_size = m.NumRows() * m.NumCols();
  flattened.reserve(static_cast<std::size_t>(flat_size));
  // Iterate over columns first, transposing the underlying data:
  for (index_t j = 0; j < m.NumCols(); ++j) {
    for (index_t i = 0; i < m.NumRows(); ++i) {
      flattened.push_back(m(i, j));
    }
  }
  return MatrixExpr::Create(flat_size, 1, std::move(flattened));
}

}  // namespace math
