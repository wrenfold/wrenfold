// Copyright 2023 Gareth Cross
#pragma once
#include <vector>

#include "assertions.h"
#include "error_types.h"
#include "expression_concept.h"
#include "expression_impl.h"

namespace math {

// A matrix of expressions.
// Stores a 2D grid of sub-expressions in row-major order.
class Matrix : public ExpressionImpl<Matrix> {
 public:
  static constexpr std::string_view NameStr = "Matrix";

  // Construct from data vector.
  Matrix(std::size_t rows, std::size_t cols, std::vector<Expr> data)
      : rows_(rows), cols_(cols), data_(std::move(data)) {
    if (data_.size() != rows_ * cols_) {
      throw DimensionError("Mismatch between shape and # of elements. size = {}, shape = [{}, {}]",
                           data_.size(), rows_, cols_);
    }
    ASSERT_GREATER(rows_, 0);
    ASSERT_GREATER(cols_, 0);
  }

  // All elements must match.
  bool IsIdenticalToImplTyped(const Matrix& other) const {
    return rows_ == other.rows_ && cols_ == other.cols_ &&
           std::equal(data_.begin(), data_.end(), other.data_.begin(),
                      [](const Expr& x, const Expr& y) { return x.IsIdenticalTo(y); });
  }

  // Access element in a vector. Only valid if `cols` or `rows` is 1.
  const Expr& operator[](std::size_t i) const;

  // Access element in a matrix.
  const Expr& operator()(std::size_t i, std::size_t j) const;

  // Transpose the matrix.
  Matrix Transpose() const;

  // Dimensions of the matrix.
  std::size_t NumRows() const { return rows_; }
  std::size_t NumCols() const { return cols_; }

  // Total size (product of elements).
  std::size_t Size() const { return data_.size(); }

  // Access elements.
  const std::vector<Expr>& Data() const { return data_; }

  // Iterators:
  auto begin() const { return data_.begin(); }
  auto end() const { return data_.end(); }

  // Compute flattened row-major index.
  std::size_t Index(std::size_t i, std::size_t j) const { return i * cols_ + j; }

 private:
  std::size_t rows_;
  std::size_t cols_;
  std::vector<Expr> data_;  //  TODO: Small vector up to size 4x4.
};

inline const Expr& Matrix::operator[](std::size_t i) const {
  if (rows_ != 1 && cols_ != 1) {
    throw DimensionError(
        "Array-style accessor is only valid on vectors. Matrix has dimensions ({}, {}).", rows_,
        cols_);
  }
  if (i >= data_.size()) {
    throw DimensionError("Index {} is out of bounds for vector of length {}.", i, data_.size());
  }
  return data_[i];
}

inline const Expr& Matrix::operator()(std::size_t i, std::size_t j) const {
  if (i >= rows_ || j >= cols_) {
    throw DimensionError("Index ({}, {}) is out of bounds for matrix of size ({}, {})", i, j, rows_,
                         cols_);
  }
  return data_[Index(i, j)];
}

// Multiply matrices w/ dimension checking.
Matrix operator*(const Matrix& a, const Matrix& b);

}  // namespace math
