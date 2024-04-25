// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <vector>

#include "wf/expression.h"
#include "wf/expression_variant.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/types/span.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

// For now, we only have one expression type for `matrix`. That said, there are runtime
// optimizations that are easier to implement if we have an abstract matrix multiplication
// expression. So we'll generalize this to be an expression type, and add more expressions
// in a follow-up PR.
struct matrix_meta_type {};
template <>
struct type_list_trait<matrix_meta_type> {
  // clang-format off
  using types = type_list<
    class matrix
  >;
  // clang-format on
};

// A matrix expression is an abstract expression of a 2D matrix.
class matrix_expr final : public expression_base<matrix_expr, matrix_meta_type> {
 public:
  using expression_base::expression_base;

  // Static constructor: Create a dense matrix of expressions.
  static matrix_expr create(index_t rows, index_t cols, std::vector<scalar_expr> args);

  // Create from a pair of iterators.
  template <typename Iterator>
  static matrix_expr create(const index_t rows, const index_t cols, Iterator begin, Iterator end) {
    return create(rows, cols, std::vector<scalar_expr>{begin, end});
  }

  // Convert to string.
  std::string to_string() const;

  // Format to graphical tree representation.
  std::string to_expression_tree_string() const;

  // Negation operator.
  matrix_expr operator-() const;

  // Differentiate with respect to a single variable. Reps defines how many derivatives to take.
  // The returned matrix is the element-wise derivative.
  matrix_expr diff(
      const scalar_expr& var, int reps = 1,
      non_differentiable_behavior behavior = non_differentiable_behavior::constant) const;

  // Differentiate a vector with respect to another vector, producing a Jacobian.
  // If the input is an [N,1] vector and `vars` has M expressions, the result will be an NxM matrix.
  // Throws if `this` is not a column vector.
  matrix_expr jacobian(
      absl::Span<const scalar_expr> vars,
      non_differentiable_behavior behavior = non_differentiable_behavior::constant) const;

  // Version of `jacobian` that accepts `matrix_expr` directly.
  matrix_expr jacobian(const matrix_expr& vars, non_differentiable_behavior behavior =
                                                    non_differentiable_behavior::constant) const;

  // Distribute terms in this expression.
  matrix_expr distribute() const;

  // Create a new expression by recursively substituting `replacement` for `target`.
  matrix_expr subs(const scalar_expr& target, const scalar_expr& replacement) const;

  // Collect terms in every element of this matrix.
  matrix_expr collect(absl::Span<const scalar_expr> terms) const;

  // Evaluate to matrix of floats.
  matrix_expr eval() const;

  // Get # of rows.
  index_t rows() const;

  // Get # of columns.
  index_t cols() const;

  // Size as size_t.
  std::size_t size() const { return static_cast<std::size_t>(rows() * cols()); }

  // For vectors or row-vectors only. Access element `i`.
  const scalar_expr& operator[](index_t i) const;

  // Access row `i` and column `j`.
  const scalar_expr& operator()(index_t i, index_t j) const;

  // Get a block of rows [start, start + length).
  matrix_expr get_block(index_t row, index_t col, index_t nrows, index_t ncols) const;

  // Transpose the matrix.
  [[nodiscard]] matrix_expr transposed() const;

  // Reshape the matrix (returns a copy).
  [[nodiscard]] matrix_expr reshape(index_t nrows, index_t ncols) const;

  // Get the squared norm of the matrix.
  scalar_expr squared_norm() const;

  // Get the norm of the matrix.
  scalar_expr norm() const;

  // Cast to underlying matrix type.
  const matrix& as_matrix() const;

  // Convert to vector of expressions, in row-major order.
  std::vector<scalar_expr> to_vector() const;
};

// Relative order of matrices (first by dimensions, then by lexicographical order).
template <>
struct order_struct<matrix_expr> {
  relative_order operator()(const matrix_expr& a, const matrix_expr& b) const;
};

// Math operators:
matrix_expr operator+(const matrix_expr& a, const matrix_expr& b);
matrix_expr operator-(const matrix_expr& a, const matrix_expr& b);
matrix_expr operator*(const matrix_expr& a, const matrix_expr& b);
matrix_expr operator*(const matrix_expr& a, const scalar_expr& b);
inline matrix_expr operator*(const scalar_expr& a, const matrix_expr& b) { return b * a; }
inline matrix_expr operator/(const matrix_expr& a, const scalar_expr& b) { return a * (1 / b); }

// ostream support
inline std::ostream& operator<<(std::ostream& stream, const matrix_expr& x) {
  stream << x.to_string();
  return stream;
}

}  // namespace wf

// libfmt support:
template <>
struct fmt::formatter<wf::matrix_expr> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::matrix_expr& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.to_string());
  }
};
