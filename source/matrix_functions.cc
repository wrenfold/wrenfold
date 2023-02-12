// Copyright 2023 Gareth Cross
#include "matrix_functions.h"

#include "constants.h"
#include "expressions/matrix.h"

namespace math {

MatrixExpr Zeros(std::size_t rows, std::size_t cols) {
  // Eventually we might have a symbolic zero matrix, and this won't be required.
  std::vector<Expr> data(rows * cols, Constants::Zero);
  return MatrixExpr::CreateMatrix(rows, cols, std::move(data));
}

// Create an identity matrix.
MatrixExpr Identity(std::size_t rows) {
  std::vector<Expr> data;
  data.reserve(rows * rows);
  IterMatrix(rows, rows, [&](std::size_t i, std::size_t j) {
    data.push_back(i == j ? Constants::One : Constants::Zero);
  });
  return MatrixExpr::CreateMatrix(rows, rows, std::move(data));
}

}  // namespace math
