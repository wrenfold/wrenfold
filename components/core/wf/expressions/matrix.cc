// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/expressions/matrix.h"

#include "wf/expression_visitor.h"
#include "wf/expressions/addition.h"
#include "wf/utility/assertions.h"

namespace wf {

matrix::matrix(const index_t rows, const index_t cols, container_type data)
    : rows_(rows), cols_(cols), data_(std::move(data)) {
  if (data_.size() != static_cast<std::size_t>(rows_) * static_cast<std::size_t>(cols_)) {
    throw dimension_error("Mismatch between shape and # of elements. size = {}, shape = [{}, {}]",
                          data_.size(), rows_, cols_);
  }
  WF_ASSERT_GE(rows_, 0);
  WF_ASSERT_GE(cols_, 0);
}

matrix matrix::get_block(const index_t row, const index_t col, const index_t nrows,
                         const index_t ncols) const {
  if (row < 0 || row + nrows > rows_ || col < 0 || col + ncols > cols_) {
    throw dimension_error(
        "Block [position: ({}, {}), size: ({}, {})] is out of bounds for matrix of shape ({}, {})",
        row, col, nrows, ncols, rows_, cols_);
  }
  std::vector<scalar_expr> data{};
  data.reserve(nrows * ncols);
  iter_matrix(nrows, ncols, [&](const index_t i, const index_t j) {
    data.push_back(get_unchecked(i + row, j + col));
  });
  return matrix(nrows, ncols, std::move(data));
}

void matrix::set_block(const index_t row, const index_t col, const index_t nrows,
                       const index_t ncols, const matrix& block) {
  if (row < 0 || row + nrows > rows_ || col < 0 || col + ncols > cols_) {
    throw dimension_error(
        "Block [position: ({}, {}), size: ({}, {})] is out of bounds for matrix of shape ({}, {})",
        row, col, nrows, ncols, rows_, cols_);
  }
  if (block.rows() != nrows || block.cols() != ncols) {
    throw dimension_error("Block shape ({}, {}) does not match requested shape ({}, {})",
                          block.rows(), block.cols(), nrows, ncols);
  }
  iter_matrix(nrows, ncols, [&](index_t i, index_t j) {
    set_unchecked(i + row, j + col, block.get_unchecked(i, j));
  });
}

matrix matrix::transposed() const {
  std::vector<scalar_expr> output{};
  output.reserve(size());
  const index_t output_rows = cols();
  const index_t output_cols = rows();
  iter_matrix(output_rows, output_cols,
              [&](index_t r, index_t c) { output.push_back(operator()(c, r)); });
  return matrix(output_rows, output_cols, std::move(output));
}

matrix operator*(const matrix& a, const matrix& b) {
  if (a.cols() != b.rows()) {
    throw dimension_error(
        "dimension mismatch in matrix multiplication: ({}, {}) x ({}, {}). Inner dimensions must "
        "match.",
        a.rows(), a.cols(), b.rows(), b.cols());
  }
  const index_t output_rows = a.rows();
  const index_t output_cols = b.cols();

  std::vector<scalar_expr> output;
  output.reserve(output_rows * output_cols);

  std::vector<scalar_expr> addition_args;
  iter_matrix(output_rows, output_cols, [&](index_t i, index_t j) {
    // Multiply row times column:
    addition_args.clear();
    for (index_t k = 0; k < a.cols(); ++k) {
      if (scalar_expr prod = a(i, k) * b(k, j); !is_zero(prod)) {
        addition_args.push_back(std::move(prod));
      }
    }
    if (addition_args.empty()) {
      output.push_back(constants::zero);
    } else {
      output.push_back(addition::from_operands(addition_args));
    }
  });
  return matrix(output_rows, output_cols, std::move(output));
}

matrix operator+(const matrix& a, const matrix& b) {
  if (a.rows() != b.rows() || a.cols() != b.cols()) {
    throw dimension_error("dimension mismatch in matrix addition: ({}, {}) + ({}, {}).", a.rows(),
                          a.cols(), b.rows(), b.cols());
  }
  std::vector<scalar_expr> output;
  output.reserve(a.size());
  iter_matrix(a.rows(), a.cols(),
              [&](index_t i, index_t j) { output.push_back(a(i, j) + b(i, j)); });

  return matrix(a.rows(), a.cols(), std::move(output));
}

matrix operator-(const matrix& a, const matrix& b) {
  if (a.rows() != b.rows() || a.cols() != b.cols()) {
    throw dimension_error("dimension mismatch in matrix subtraction: ({}, {}) - ({}, {}).",
                          a.rows(), a.cols(), b.rows(), b.cols());
  }
  std::vector<scalar_expr> output;
  output.reserve(a.size());
  iter_matrix(a.rows(), a.cols(),
              [&](index_t i, index_t j) { output.push_back(a(i, j) - b(i, j)); });

  return matrix(a.rows(), a.cols(), std::move(output));
}

}  // namespace wf
