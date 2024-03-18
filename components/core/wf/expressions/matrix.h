// Copyright 2023 Gareth Cross
#pragma once
#include <vector>

#include "wf/algorithm_utils.h"
#include "wf/error_types.h"
#include "wf/matrix_expression.h"

namespace wf {

// A matrix of expressions.
// Stores a 2D grid of sub-expressions in row-major order.
class matrix {
 public:
  static constexpr std::string_view name_str = "Matrix";
  static constexpr bool is_leaf_node = false;
  using container_type = std::vector<scalar_expr>;

  // Construct from data vector.
  matrix(index_t rows, index_t cols, container_type data);

  // Implement ExpressionImpl::Map
  template <typename Operation>
  matrix_expr map_children(Operation&& operation) const {
    return matrix_expr{std::in_place_type_t<matrix>{}, rows(), cols(),
                       transform_map<container_type>(data_, std::forward<Operation>(operation))};
  }

  // Access element in a vector. Only valid if `cols` or `rows` is 1.
  const scalar_expr& operator[](index_t i) const;

  // Access element in a matrix.
  const scalar_expr& operator()(index_t i, index_t j) const;

  // Get with no bounds checking.
  const scalar_expr& get_unchecked(const index_t i, const index_t j) const noexcept {
    return data_[compute_index(i, j)];
  }

  // Non-const accessor with no bounds checking.
  scalar_expr& get_unchecked(const index_t i, const index_t j) noexcept {
    return data_[compute_index(i, j)];
  }

  // Set with no bounds checking.
  void set_unchecked(const index_t i, const index_t j, const scalar_expr& value) noexcept {
    data_[compute_index(i, j)] = value;
  }

  // Get a sub-block from this matrix.
  matrix get_block(index_t row, index_t col, index_t nrows, index_t ncols) const;

  // Set a sub-block of this matrix.
  void set_block(index_t row, index_t col, index_t nrows, index_t ncols, const matrix& block);

  // Transpose the matrix.
  matrix transposed() const;

  // Dimensions of the matrix.
  constexpr index_t rows() const noexcept { return rows_; }
  constexpr index_t cols() const noexcept { return cols_; }

  // Dimensions as tuple.
  constexpr std::tuple<index_t, index_t> dimensions() const noexcept {
    return std::make_tuple(rows_, cols_);
  }

  // Total size (product of elements).
  std::size_t size() const noexcept { return data_.size(); }

  // Access elements.
  constexpr const std::vector<scalar_expr>& data() const noexcept { return data_; }

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
  container_type data_;  //  TODO: Small vector up to size 4x4.
};

static_assert(std::is_move_constructible_v<matrix> && std::is_move_assignable_v<matrix>);

inline const scalar_expr& matrix::operator[](index_t i) const {
  if (rows_ != 1 && cols_ != 1) {
    throw dimension_error(
        "Array-style accessor is only valid on vectors. Matrix has dimensions ({}, {}).", rows_,
        cols_);
  }
  if (i < 0 || static_cast<std::size_t>(i) >= data_.size()) {
    throw dimension_error("Index {} is out of bounds for vector of length {}.", i, data_.size());
  }
  return data_[static_cast<std::size_t>(i)];
}

inline const scalar_expr& matrix::operator()(index_t i, index_t j) const {
  if (i >= rows_ || i < 0 || j >= cols_ || j < 0) {
    throw dimension_error("Index ({}, {}) is out of bounds for matrix of size ({}, {})", i, j,
                          rows_, cols_);
  }
  return data_[compute_index(i, j)];
}

// Multiply matrices w/ dimension checking.
matrix operator*(const matrix& a, const matrix& b);

// Add matrices w/ dimension checking.
matrix operator+(const matrix& a, const matrix& b);

// Subtract matrices w/ dimension checking.
matrix operator-(const matrix& a, const matrix& b);

// Iterate over a matrix and run `callable` on each element. The purpose of this method is
// to have one place (or fewer places) where the traversal order (row-major) is specified.
template <typename Callable>
void iter_matrix(index_t rows, index_t cols, Callable&& callable) {
  for (index_t i = 0; i < rows; ++i) {
    for (index_t j = 0; j < cols; ++j) {
      callable(i, j);
    }
  }
}

template <>
struct hash_struct<matrix> {
  std::size_t operator()(const matrix& m) const noexcept {
    const std::size_t seed =
        hash_combine(static_cast<std::size_t>(m.rows()), static_cast<std::size_t>(m.cols()));
    return hash_all(seed, m.begin(), m.end());
  }
};

template <>
struct is_identical_struct<matrix> {
  bool operator()(const matrix& a, const matrix& b) const {
    return a.rows() == b.rows() && a.cols() == b.cols() &&
           std::equal(a.begin(), a.end(), b.begin(), is_identical_struct<scalar_expr>{});
  }
};

template <>
struct order_struct<matrix> {
  relative_order operator()(const matrix& a, const matrix& b) const {
    if (const relative_order order = order_by_comparison(a.dimensions(), b.dimensions());
        order != relative_order::equal) {
      return order;
    }
    return lexicographical_order(a, b, order_struct<scalar_expr>{});
  }
};

}  // namespace wf
