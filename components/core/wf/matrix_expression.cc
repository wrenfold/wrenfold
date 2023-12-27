// Copyright 2023 Gareth Cross
#include "wf/matrix_expression.h"

#include "wf/constants.h"
#include "wf/derivative.h"
#include "wf/expressions/addition.h"
#include "wf/expressions/matrix.h"
#include "wf/functions.h"
#include "wf/plain_formatter.h"
#include "wf_runtime/span.h"

namespace wf {

MatrixExpr::MatrixExpr(matrix&& content) : matrix_(std::make_shared<matrix>(std::move(content))) {}

MatrixExpr MatrixExpr::create(index_t rows, index_t cols, std::vector<Expr> args) {
  return MatrixExpr{matrix{rows, cols, std::move(args)}};
}

// For now, a lot of these methods just forward to the `Matrix` type. In the future,
// we may have different matrix types.

// Test if the two expressions are identical.
bool MatrixExpr::is_identical_to(const MatrixExpr& other) const {
  return as_matrix().is_identical_to(other.as_matrix());
}

std::string_view MatrixExpr::type_name() const { return matrix::name_str; }

std::string MatrixExpr::to_string() const {
  plain_formatter formatter{};
  formatter(as_matrix());
  return formatter.take_output();
}

index_t MatrixExpr::rows() const { return as_matrix().rows(); }

index_t MatrixExpr::cols() const { return as_matrix().cols(); }

const Expr& MatrixExpr::operator[](index_t i) const { return as_matrix()[i]; }

const Expr& MatrixExpr::operator()(index_t i, index_t j) const { return as_matrix()(i, j); }

void MatrixExpr::set(index_t i, index_t j, const Expr& value) {
  as_matrix_mut().set_unchecked(i, j, value);
}

MatrixExpr MatrixExpr::get_block(index_t row, index_t col, index_t nrows, index_t ncols) const {
  matrix result = as_matrix().get_block(row, col, nrows, ncols);
  return MatrixExpr{std::move(result)};
}

void MatrixExpr::set_block(index_t row, index_t col, index_t nrows, index_t ncols,
                           const MatrixExpr& block) {
  as_matrix_mut().set_block(row, col, nrows, ncols, block.as_matrix());
}

MatrixExpr MatrixExpr::transposed() const { return MatrixExpr{as_matrix().transposed()}; }

MatrixExpr MatrixExpr::reshape(index_t nrows, index_t ncols) const {
  if (nrows < 0 || ncols < 0) {
    throw dimension_error("Dimensions must be non-negative. Received [{}, {}]", nrows, ncols);
  }
  if (static_cast<std::size_t>(nrows * ncols) != size()) {
    throw dimension_error(
        "Reshaped dimensions [{} x {} = {}] does not match number of input elements [{} x {} = {}]",
        nrows, ncols, nrows * ncols, rows(), cols(), size());
  }
  return MatrixExpr::create(nrows, ncols, to_vector());
}

Expr MatrixExpr::squared_norm() const {
  const matrix& m = as_matrix();
  std::vector<Expr> operands{};
  operands.reserve(m.size());
  for (const auto& x : m) {
    operands.push_back(pow(x, 2));
  }
  return addition::from_operands(operands);
}

Expr MatrixExpr::norm() const { return sqrt(squared_norm()); }

const matrix& MatrixExpr::as_matrix() const { return *matrix_.get(); }

std::vector<Expr> MatrixExpr::to_vector() const { return as_matrix().data(); }

MatrixExpr MatrixExpr::operator-() const {
  return matrix_operator_overloads::operator*(*this, constants::negative_one);
}

MatrixExpr MatrixExpr::diff(const Expr& var, int reps, non_differentiable_behavior behavior) const {
  derivative_visitor visitor{var, behavior};
  return MatrixExpr{as_matrix().map_children([&visitor, reps](const Expr& x) {
    Expr result = x;
    for (int i = 0; i < reps; ++i) {
      result = visitor.apply(result);
    }
    return result;
  })};
}

MatrixExpr MatrixExpr::jacobian(const absl::Span<const Expr> vars,
                                non_differentiable_behavior behavior) const {
  if (cols() != 1) {
    throw dimension_error(
        "Jacobian can only be computed on column-vectors. Received dimensions: [{}, {}]", rows(),
        cols());
  }
  const auto& m = as_matrix();
  return wf::jacobian(m.data(), vars, behavior);
}

MatrixExpr MatrixExpr::jacobian(const MatrixExpr& vars,
                                non_differentiable_behavior behavior) const {
  if (vars.rows() != 1 && vars.cols() != 1) {
    throw dimension_error("Variables must be a row or column vector. Received dimensions: [{}, {}]",
                          vars.rows(), vars.cols());
  }
  const auto& m = vars.as_matrix();
  return jacobian(m.data(), behavior);  //  Call span version.
}

MatrixExpr MatrixExpr::distribute() const {
  return MatrixExpr{as_matrix().map_children(&wf::distribute)};
}

MatrixExpr MatrixExpr::subs(const Expr& target, const Expr& replacement) const {
  return MatrixExpr{
      as_matrix().map_children([&](const Expr& x) { return x.subs(target, replacement); })};
}

MatrixExpr MatrixExpr::collect(absl::Span<const Expr> terms) const {
  return MatrixExpr{
      as_matrix().map_children([&terms](const Expr& x) { return collect_many(x, terms); })};
}

MatrixExpr MatrixExpr::eval() const { return MatrixExpr{as_matrix().map_children(&wf::evaluate)}; }

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
  const matrix& a_mat = a.as_matrix();

  std::vector<Expr> data{};
  data.reserve(a_mat.size());
  std::transform(a_mat.begin(), a_mat.end(), std::back_inserter(data),
                 [&b](const Expr& a_expr) { return a_expr * b; });

  return MatrixExpr{matrix(a_mat.rows(), a_mat.cols(), std::move(data))};
}

}  // namespace matrix_operator_overloads
}  // namespace wf