// Copyright 2023 Gareth Cross
#include "matrix_expression.h"

#include "constants.h"
#include "expressions/matrix.h"
#include "plain_formatter.h"

namespace math {

MatrixExpr::MatrixExpr(Matrix&& content)
    : matrix_(std::make_shared<const Matrix>(std::move(content))) {}

MatrixExpr MatrixExpr::Create(index_t rows, index_t cols, std::vector<Expr> args) {
  return MatrixExpr{Matrix{rows, cols, std::move(args)}};
}

// For now, a lot of these methods just forward to the `Matrix` type. In the future,
// we may have different matrix types.

// Test if the two expressions are identical.
bool MatrixExpr::is_identical_to(const MatrixExpr& other) const {
  return AsMatrix().is_identical_to(other.AsMatrix());
}

std::string_view MatrixExpr::TypeName() const { return Matrix::NameStr; }

std::string MatrixExpr::ToString() const {
  PlainFormatter formatter{};
  formatter(AsMatrix());
  return formatter.TakeOutput();
}

index_t MatrixExpr::NumRows() const { return AsMatrix().rows(); }

index_t MatrixExpr::NumCols() const { return AsMatrix().cols(); }

const Expr& MatrixExpr::operator[](index_t i) const { return AsMatrix()[i]; }

const Expr& MatrixExpr::operator()(index_t i, index_t j) const { return AsMatrix()(i, j); }

MatrixExpr MatrixExpr::get_block(index_t row, index_t col, index_t nrows, index_t ncols) const {
  Matrix result = AsMatrix().get_block(row, col, nrows, ncols);
  return MatrixExpr{std::move(result)};
}

MatrixExpr MatrixExpr::Transpose() const { return MatrixExpr{AsMatrix().transposed()}; }

const Matrix& MatrixExpr::AsMatrix() const { return *matrix_.get(); }

MatrixExpr MatrixExpr::operator-() const {
  return matrix_operator_overloads::operator*(*this, Constants::NegativeOne);
}

MatrixExpr MatrixExpr::Diff(const Expr& var, int reps) const {
  return MatrixExpr{AsMatrix().map_children([&](const Expr& x) { return x.Diff(var, reps); })};
}

MatrixExpr MatrixExpr::Distribute() const { return MatrixExpr{AsMatrix().map_children(&math::Distribute)}; }

MatrixExpr MatrixExpr::Subs(const Expr& target, const Expr& replacement) const {
  return MatrixExpr{AsMatrix().map_children([&](const Expr& x) { return x.Subs(target, replacement); })};
}

MatrixExpr MatrixExpr::Eval() const { return MatrixExpr{AsMatrix().map_children(&math::Eval)}; }

namespace matrix_operator_overloads {

MatrixExpr operator+(const MatrixExpr& a, const MatrixExpr& b) {
  return MatrixExpr{a.AsMatrix() + b.AsMatrix()};
}

MatrixExpr operator-(const MatrixExpr& a, const MatrixExpr& b) {
  return MatrixExpr{a.AsMatrix() - b.AsMatrix()};
}

MatrixExpr operator*(const MatrixExpr& a, const MatrixExpr& b) {
  return MatrixExpr{a.AsMatrix() * b.AsMatrix()};
}

MatrixExpr operator*(const MatrixExpr& a, const Expr& b) {
  const Matrix& a_mat = a.AsMatrix();

  std::vector<Expr> data{};
  data.reserve(a_mat.size());
  std::transform(a_mat.begin(), a_mat.end(), std::back_inserter(data),
                 [&b](const Expr& a_expr) { return a_expr * b; });

  return MatrixExpr{Matrix(a_mat.rows(), a_mat.cols(), std::move(data))};
}

}  // namespace matrix_operator_overloads
}  // namespace math
