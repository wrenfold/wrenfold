// Copyright 2023 Gareth Cross
#pragma once
#include <vector>

#include "expression.h"

namespace math {

// Matrix type that stores a dense block of expressions. For context, this was originally
// part of the `Expr` type hierarchy. However, this proved to be a mistake because the rules for
// matrices are sufficiently different from scalars. For now, it is just a wrapper around a shared
// ptr to `Matrix`. In future a symbolic matrix expression might exist, and then this will be more
// like the `Expr` type.
class MatrixExpr {
 public:
  // Construct w/ matrix content.
  explicit MatrixExpr(class Matrix&& content);

  // Static constructor: Create a dense matrix of expressions.
  static MatrixExpr create(index_t rows, index_t cols, std::vector<Expr> args);

  // Test if the two expressions are identical.
  bool is_identical_to(const MatrixExpr& other) const;

  // Test if the two expressions have the same underlying address.
  bool has_same_address(const MatrixExpr& other) const {
    return matrix_.get() == other.matrix_.get();
  }

  // Get the underlying type name as a string.
  std::string_view type_name() const;

  // Convert to string.
  std::string to_string() const;

  // Defined in tree_formatter.cc
  std::string to_expression_tree_string() const;

  // Negation operator.
  MatrixExpr operator-() const;

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  MatrixExpr diff(const Expr& var, int reps = 1) const;

  // Distribute terms in this expression.
  MatrixExpr distribute() const;

  // Create a new expression by recursively substituting `replacement` for `target`.
  MatrixExpr subs(const Expr& target, const Expr& replacement) const;

  // Evaluate to matrix of floats.
  MatrixExpr eval() const;

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

  // Get a block of rows [start, start + length).
  MatrixExpr get_block(index_t row, index_t col, index_t nrows, index_t ncols) const;

  // Transpose the matrix.
  [[nodiscard]] MatrixExpr transposed() const;

  // Static cast to underlying matrix type.
  const Matrix& as_matrix() const;

 private:
  std::shared_ptr<const Matrix> matrix_;
};

static_assert(std::is_move_assignable_v<MatrixExpr> && std::is_move_constructible_v<MatrixExpr>,
              "Should be movable");

// Math operators:
namespace matrix_operator_overloads {

MatrixExpr operator+(const MatrixExpr& a, const MatrixExpr& b);
MatrixExpr operator-(const MatrixExpr& a, const MatrixExpr& b);
MatrixExpr operator*(const MatrixExpr& a, const MatrixExpr& b);
MatrixExpr operator*(const MatrixExpr& a, const Expr& b);
inline MatrixExpr operator*(const Expr& a, const MatrixExpr& b) { return b * a; }

}  // namespace matrix_operator_overloads

// ostream support
inline std::ostream& operator<<(std::ostream& stream, const MatrixExpr& x) {
  stream << x.to_string();
  return stream;
}

}  // namespace math

// libfmt support:
template <>
struct fmt::formatter<math::MatrixExpr> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::MatrixExpr& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.to_string());
  }
};
