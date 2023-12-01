// Copyright 2023 Gareth Cross
#include "wf/matrix_expression.h"

#include "wf/constants.h"
#include "wf/derivative.h"
#include "wf/expressions/addition.h"
#include "wf/expressions/matrix.h"
#include "wf/functions.h"
#include "wf/plain_formatter.h"
#include "wf_runtime/span.h"

namespace math {

MatrixExpr::MatrixExpr(Matrix&& content)
    : matrix_(std::make_shared<const Matrix>(std::move(content))) {}

MatrixExpr MatrixExpr::create(index_t rows, index_t cols, std::vector<Expr> args) {
  return MatrixExpr{Matrix{rows, cols, std::move(args)}};
}

// For now, a lot of these methods just forward to the `Matrix` type. In the future,
// we may have different matrix types.

// Test if the two expressions are identical.
bool MatrixExpr::is_identical_to(const MatrixExpr& other) const {
  return as_matrix().is_identical_to(other.as_matrix());
}

std::string_view MatrixExpr::type_name() const { return Matrix::NameStr; }

std::string MatrixExpr::to_string() const {
  PlainFormatter formatter{};
  formatter(as_matrix());
  return formatter.take_output();
}

index_t MatrixExpr::rows() const { return as_matrix().rows(); }

index_t MatrixExpr::cols() const { return as_matrix().cols(); }

const Expr& MatrixExpr::operator[](index_t i) const { return as_matrix()[i]; }

const Expr& MatrixExpr::operator()(index_t i, index_t j) const { return as_matrix()(i, j); }

MatrixExpr MatrixExpr::get_block(index_t row, index_t col, index_t nrows, index_t ncols) const {
  Matrix result = as_matrix().get_block(row, col, nrows, ncols);
  return MatrixExpr{std::move(result)};
}

MatrixExpr MatrixExpr::transposed() const { return MatrixExpr{as_matrix().transposed()}; }

Expr MatrixExpr::squared_norm() const {
  const Matrix& m = as_matrix();
  std::vector<Expr> operands{};
  operands.reserve(m.size());
  for (const auto& x : m) {
    operands.push_back(pow(x, 2));
  }
  return Addition::from_operands(operands);
}

const Matrix& MatrixExpr::as_matrix() const { return *matrix_.get(); }

std::vector<Expr> MatrixExpr::to_vector() const { return as_matrix().data(); }

MatrixExpr MatrixExpr::operator-() const {
  return matrix_operator_overloads::operator*(*this, Constants::NegativeOne);
}

MatrixExpr MatrixExpr::diff(const Expr& var, int reps) const {
  DiffVisitor visitor{var};
  return MatrixExpr{as_matrix().map_children([&visitor, reps](const Expr& x) {
    Expr result = x;
    for (int i = 0; i < reps; ++i) {
      result = visitor.apply(result);
    }
    return result;
  })};
}

MatrixExpr MatrixExpr::jacobian(const absl::Span<const Expr> vars) const {
  if (cols() != 1) {
    throw DimensionError(
        "Jacobian can only be computed on column-vectors. Received dimensions: [{}, {}]", rows(),
        cols());
  }
  const auto& m = as_matrix();
  return math::jacobian(m.data(), vars);
}

MatrixExpr MatrixExpr::jacobian(const MatrixExpr& vars) const {
  if (vars.rows() != 1 && vars.cols() != 1) {
    throw DimensionError("Variables must be a row or column vector. Received dimensions: [{}, {}]",
                         vars.rows(), vars.cols());
  }
  const auto& m = vars.as_matrix();
  return jacobian(m.data());  //  Call span version.
}

MatrixExpr MatrixExpr::distribute() const {
  return MatrixExpr{as_matrix().map_children(&math::distribute)};
}

MatrixExpr MatrixExpr::subs(const Expr& target, const Expr& replacement) const {
  return MatrixExpr{
      as_matrix().map_children([&](const Expr& x) { return x.subs(target, replacement); })};
}

MatrixExpr MatrixExpr::collect(absl::Span<const Expr> terms) const {
  return MatrixExpr{
      as_matrix().map_children([&terms](const Expr& x) { return collect_many(x, terms); })};
}

MatrixExpr MatrixExpr::eval() const {
  return MatrixExpr{as_matrix().map_children(&math::evaluate)};
}

namespace matrix_operator_overloads {

MatrixExpr operator+(const MatrixExpr& a, const MatrixExpr& b) {
  return MatrixExpr{a.as_matrix() + b.as_matrix()};
}

MatrixExpr operator-(const MatrixExpr& a, const MatrixExpr& b) {
  return MatrixExpr{a.as_matrix() - b.as_matrix()};
}

MatrixExpr operator*(const MatrixExpr& a, const MatrixExpr& b) {
  return MatrixExpr{a.as_matrix() * b.as_matrix()};
}

MatrixExpr operator*(const MatrixExpr& a, const Expr& b) {
  const Matrix& a_mat = a.as_matrix();

  std::vector<Expr> data{};
  data.reserve(a_mat.size());
  std::transform(a_mat.begin(), a_mat.end(), std::back_inserter(data),
                 [&b](const Expr& a_expr) { return a_expr * b; });

  return MatrixExpr{Matrix(a_mat.rows(), a_mat.cols(), std::move(data))};
}

}  // namespace matrix_operator_overloads
}  // namespace math
