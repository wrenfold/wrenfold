// Copyright 2023 Gareth Cross
#pragma once
#include <vector>

#include "expression.h"

namespace math {

// Variation of `Expr` that exposes certain operations only valid on matrices.
// Rather than inheriting, we store another `Expr` via composition.
class MatrixExpr {
 public:
  // Construct from expression. The underlying type is checked and an exception will be thrown
  // if the argument is not a matrix.
  explicit MatrixExpr(Expr&& arg);
  explicit MatrixExpr(const Expr& arg);

  // Static constructor: Create a dense matrix of expressions.
  static MatrixExpr Create(index_t rows, index_t cols, std::vector<Expr> args);

  // Test if the two expressions are identical.
  bool IsIdenticalTo(const MatrixExpr& other) const { return expr_.IsIdenticalTo(other.expr_); }

  // Test if the two expressions have the same underlying address.
  bool HasSameAddress(const MatrixExpr& other) const { return expr_.HasSameAddress(other.expr_); }

  // Get the underlying type name as a string.
  std::string_view TypeName() const { return expr_.TypeName(); }

  // Convert to string.
  std::string ToString() const { return expr_.ToString(); }

  // Negation operator.
  MatrixExpr operator-() const { return MatrixExpr(-expr_); }

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  MatrixExpr Diff(const Expr& var, int reps = 1) const { return MatrixExpr{expr_.Diff(var, reps)}; }

  // Distribute terms in this expression.
  MatrixExpr Distribute() const { return MatrixExpr{expr_.Distribute()}; }

  // Create a new expression by recursively substituting `replacement` for `target`.
  MatrixExpr Subs(const Expr& target, const Expr& replacement) const {
    return MatrixExpr{expr_.Subs(target, replacement)};
  }

  // Receive a visitor.
  void Receive(VisitorBase& visitor) const { expr_.Receive(visitor); }

  // Get # of rows.
  index_t NumRows() const;

  // Get # of columns.
  index_t NumCols() const;

  // Size as size_t.
  std::size_t Size() const { return static_cast<std::size_t>(NumRows() * NumCols()); }

  // For vectors or row-vectors only. Access element `i`.
  const Expr& operator[](index_t i) const;

  // Access row `i` and column `j`.
  const Expr& operator()(index_t i, index_t j) const;

  // Get a block of rows [start, start + length).
  MatrixExpr GetBlock(index_t row, index_t col, index_t nrows, index_t ncols) const;

  // Transpose the matrix.
  [[nodiscard]] MatrixExpr Transpose() const;

  // Static cast to underlying matrix type.
  const Matrix& AsMatrix() const;

  // Explicit cast to expr.
  explicit operator const Expr&() const { return expr_; }
  const Expr& AsExpr() const { return expr_; }

 private:
  Expr expr_;
};

static_assert(std::is_move_assignable_v<MatrixExpr> && std::is_move_constructible_v<MatrixExpr>,
              "Should be movable");

// Math operators:
namespace matrix_operator_overloads {
// TODO: These can potentially take shortcuts instead of just casting to Expr.
inline MatrixExpr operator+(const MatrixExpr& a, const MatrixExpr& b) {
  return MatrixExpr{static_cast<Expr>(a) + static_cast<Expr>(b)};
}
inline MatrixExpr operator-(const MatrixExpr& a, const MatrixExpr& b) {
  return MatrixExpr{static_cast<Expr>(a) - static_cast<Expr>(b)};
}
inline Expr operator*(const MatrixExpr& a, const MatrixExpr& b) {
  // This returns `Expr` since matrix multiplication can reduce to a scalar.
  return static_cast<Expr>(a) * static_cast<Expr>(b);
}
inline Expr operator*(const MatrixExpr& a, const Expr& b) {
  return static_cast<Expr>(a) * static_cast<Expr>(b);
}
inline Expr operator*(const Expr& a, const MatrixExpr& b) {
  return static_cast<Expr>(a) * static_cast<Expr>(b);
}
}  // namespace matrix_operator_overloads

// ostream support
inline std::ostream& operator<<(std::ostream& stream, const MatrixExpr& x) {
  stream << x.ToString();
  return stream;
}

}  // namespace math
