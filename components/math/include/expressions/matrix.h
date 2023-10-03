// Copyright 2023 Gareth Cross
#pragma once
#include <vector>

#include "assertions.h"
#include "error_types.h"
#include "expression_concept.h"
#include "expression_impl.h"
#include "visitor_impl.h"

namespace math {

// A matrix of expressions.
// Stores a 2D grid of sub-expressions in row-major order.
class Matrix {
 public:
  static constexpr std::string_view NameStr = "Matrix";
  static constexpr bool IsLeafNode = false;

  // Construct from data vector.
  Matrix(index_t rows, index_t cols, std::vector<Expr> data)
      : rows_(rows), cols_(cols), data_(std::move(data)) {
    if (data_.size() != static_cast<std::size_t>(rows_) * static_cast<std::size_t>(cols_)) {
      throw DimensionError("Mismatch between shape and # of elements. size = {}, shape = [{}, {}]",
                           data_.size(), rows_, cols_);
    }
    ASSERT_GREATER_OR_EQ(rows_, 0);
    ASSERT_GREATER_OR_EQ(cols_, 0);
  }

  // All elements must match.
  bool is_identical_to(const Matrix& other) const {
    return rows_ == other.rows_ && cols_ == other.cols_ &&
           std::equal(data_.begin(), data_.end(), other.data_.begin(), IsIdenticalOperator<Expr>{});
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void for_each(Operation&& operation) const {
    std::for_each(data_.begin(), data_.end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Matrix map_children(Operation&& operation) const {
    std::vector<Expr> transformed;
    transformed.reserve(size());
    std::transform(data_.begin(), data_.end(), std::back_inserter(transformed),
                   std::forward<Operation>(operation));
    return {rows(), cols(), std::move(transformed)};
  }

  // Access element in a vector. Only valid if `cols` or `rows` is 1.
  const Expr& operator[](index_t i) const;

  // Access element in a matrix.
  const Expr& operator()(index_t i, index_t j) const;

  // Get with no bounds checking.
  const Expr& get_unchecked(index_t i, index_t j) const noexcept {
    return data_[compute_index(i, j)];
  }

  // Non-const accessor with no bounds checking.
  Expr& get_unchecked(index_t i, index_t j) noexcept { return data_[compute_index(i, j)]; }

  // Get a sub-block from this matrix.
  Matrix get_block(index_t row, index_t col, index_t nrows, index_t ncols) const;

  // Transpose the matrix.
  Matrix transposed() const;

  // Dimensions of the matrix.
  constexpr index_t rows() const noexcept { return rows_; }
  constexpr index_t cols() const noexcept { return cols_; }

  // Total size (product of elements).
  std::size_t size() const noexcept { return data_.size(); }

  // Access elements.
  const std::vector<Expr>& data() const { return data_; }

  // Iterators:
  auto begin() const noexcept { return data_.begin(); }
  auto end() const noexcept { return data_.end(); }

  // Compute flattened row-major index.
  constexpr std::size_t compute_index(index_t i, index_t j) const noexcept {
    return static_cast<std::size_t>(i * cols_ + j);
  }

 private:
  index_t rows_;
  index_t cols_;
  std::vector<Expr> data_;  //  TODO: Small vector up to size 4x4.
};

static_assert(std::is_move_constructible_v<Matrix> && std::is_move_assignable_v<Matrix>);

inline const Expr& Matrix::operator[](index_t i) const {
  if (rows_ != 1 && cols_ != 1) {
    throw DimensionError(
        "Array-style accessor is only valid on vectors. Matrix has dimensions ({}, {}).", rows_,
        cols_);
  }
  if (i < 0 || static_cast<std::size_t>(i) >= data_.size()) {
    throw DimensionError("Index {} is out of bounds for vector of length {}.", i, data_.size());
  }
  return data_[static_cast<std::size_t>(i)];
}

inline const Expr& Matrix::operator()(index_t i, index_t j) const {
  if (i >= rows_ || i < 0 || j >= cols_ || j < 0) {
    throw DimensionError("Index ({}, {}) is out of bounds for matrix of size ({}, {})", i, j, rows_,
                         cols_);
  }
  return data_[compute_index(i, j)];
}

// Multiply matrices w/ dimension checking.
Matrix operator*(const Matrix& a, const Matrix& b);

// Add matrices w/ dimension checking.
Matrix operator+(const Matrix& a, const Matrix& b);

// Subtract matrices w/ dimension checking.
Matrix operator-(const Matrix& a, const Matrix& b);

// Iterate over a matrix and run `callable` on each element. The purpose of this method is
// to have one place (or fewer places) where the traversal order (row-major) is specified.
template <typename Callable>
inline void iter_matrix(index_t rows, index_t cols, Callable&& callable) {
  for (index_t i = 0; i < rows; ++i) {
    for (index_t j = 0; j < cols; ++j) {
      callable(i, j);
    }
  }
}

template <>
struct hash_struct<Matrix> {
  std::size_t operator()(const Matrix& m) const {
    const std::size_t seed =
        hash_combine(std::hash<std::size_t>{}(m.rows()), std::hash<std::size_t>{}(m.cols()));
    return hash_all(seed, m.begin(), m.end());
  }
};

}  // namespace math
