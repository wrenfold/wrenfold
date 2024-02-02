// Copyright 2023 Gareth Cross
#pragma once
#include <vector>

#include "absl_imports.h"
#include "expression.h"

namespace wf {

// Matrix type that stores a dense block of expressions. For context, this was originally
// part of the `Expr` type hierarchy. However, this proved to be a mistake because the rules for
// matrices are sufficiently different from scalars. For now, it is just a wrapper around a shared
// ptr to `matrix`. In future a symbolic matrix expression might exist, and then this will be more
// like the `Expr` type.
class matrix_expr {
 public:
  // Construct w/ matrix content.
  explicit matrix_expr(class matrix&& content);

  // Static constructor: Create a dense matrix of expressions.
  static matrix_expr create(index_t rows, index_t cols, std::vector<Expr> args);

  // Create from a pair of iterators.
  template <typename Iterator>
  static matrix_expr create(index_t rows, index_t cols, Iterator begin, Iterator end) {
    return create(rows, cols, std::vector<Expr>{begin, end});
  }

  // Test if the two expressions are identical.
  bool is_identical_to(const matrix_expr& other) const;

  // Test if the two expressions have the same underlying address.
  bool has_same_address(const matrix_expr& other) const {
    return matrix_.get() == other.matrix_.get();
  }

  // Get the underlying type name as a string.
  std::string_view type_name() const;

  // Convert to string.
  std::string to_string() const;

  // Defined in tree_formatter.cc
  std::string to_expression_tree_string() const;

  // Negation operator.
  matrix_expr operator-() const;

  // Differentiate with respect to a single variable. Reps defines how many derivatives to take.
  // The returned matrix is the element-wise derivative.
  matrix_expr diff(
      const Expr& var, int reps = 1,
      non_differentiable_behavior behavior = non_differentiable_behavior::constant) const;

  // Differentiate a vector with respect to another vector, producing a Jacobian.
  // If the input is an [N,1] vector and `vars` has M expressions, the result will be an NxM matrix.
  // Throws if `this` is not a column vector.
  matrix_expr jacobian(
      absl::Span<const Expr> vars,
      non_differentiable_behavior behavior = non_differentiable_behavior::constant) const;

  // Version of `jacobian` that accepts `matrix_expr` directly.
  matrix_expr jacobian(const matrix_expr& vars, non_differentiable_behavior behavior =
                                                    non_differentiable_behavior::constant) const;

  // Distribute terms in this expression.
  matrix_expr distribute() const;

  // Create a new expression by recursively substituting `replacement` for `target`.
  matrix_expr subs(const Expr& target, const Expr& replacement) const;

  // Collect terms in every element of this matrix.
  matrix_expr collect(absl::Span<const Expr> terms) const;

  // Evaluate to matrix of floats.
  matrix_expr eval() const;

  // Get # of rows.
  index_t rows() const;

  // Get # of columns.
  index_t cols() const;

  // Size as size_t.
  std::size_t size() const { return static_cast<std::size_t>(rows() * cols()); }

  // For vectors or row-vectors only. Access element `i`.
  const Expr& operator[](index_t i) const;

  // Access row `i` and column `j`.
  const Expr& operator()(index_t i, index_t j) const;

  void set(index_t i, index_t j, const Expr& value);

  // Get a block of rows [start, start + length).
  matrix_expr get_block(index_t row, index_t col, index_t nrows, index_t ncols) const;

  // Set a block of rows [start, start + length).
  void set_block(index_t row, index_t col, index_t nrows, index_t ncols, const matrix_expr& block);

  // Transpose the matrix.
  [[nodiscard]] matrix_expr transposed() const;

  // Reshape the matrix (returns a copy).
  [[nodiscard]] matrix_expr reshape(index_t nrows, index_t ncols) const;

  // Get the squared norm of the matrix.
  Expr squared_norm() const;

  // Get the norm of the matrix.
  Expr norm() const;

  // Static cast to underlying matrix type.
  const matrix& as_matrix() const;

  matrix& as_matrix_mut() { return *matrix_.get(); }

  // Convert to vector of expressions.
  std::vector<Expr> to_vector() const;

 private:
  std::shared_ptr<matrix> matrix_;
};

static_assert(std::is_move_assignable_v<matrix_expr> && std::is_move_constructible_v<matrix_expr>,
              "Should be movable");

// Hash matrix.
template <>
struct hash_struct<matrix_expr> {
  std::size_t operator()(const matrix_expr& mat) const;
};

// Relative order of matrices (first by dimensions, then by lexicographical order).
template <>
struct order_struct<matrix_expr> {
  relative_order operator()(const matrix_expr& a, const matrix_expr& b) const;
};

// Are two matrices identical.
template <>
struct is_identical_struct<matrix_expr> {
  bool operator()(const matrix_expr& a, const matrix_expr& b) const { return a.is_identical_to(b); }
};

// Math operators:
namespace matrix_operator_overloads {

matrix_expr operator+(const matrix_expr& a, const matrix_expr& b);
matrix_expr operator-(const matrix_expr& a, const matrix_expr& b);
matrix_expr operator*(const matrix_expr& a, const matrix_expr& b);
matrix_expr operator*(const matrix_expr& a, const Expr& b);
inline matrix_expr operator*(const Expr& a, const matrix_expr& b) { return b * a; }

}  // namespace matrix_operator_overloads

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
