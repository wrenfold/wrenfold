// Copyright 2023 Gareth Cross
#include "wf/matrix_expression.h"

#include "wf_runtime/span.h"

#include "wf/constants.h"
#include "wf/derivative.h"
#include "wf/distribute.h"
#include "wf/expressions/all_expressions.h"
#include "wf/functions.h"
#include "wf/plain_formatter.h"

namespace wf {

static_assert(std::is_move_assignable_v<matrix_expr> && std::is_move_constructible_v<matrix_expr>,
              "Should be movable");

matrix_expr matrix_expr::create(index_t rows, index_t cols, std::vector<scalar_expr> args) {
  return matrix_expr{std::in_place_type_t<matrix>{}, rows, cols, std::move(args)};
}

std::string matrix_expr::to_string() const {
  plain_formatter formatter{};
  formatter(*this);
  return formatter.take_output();
}

index_t matrix_expr::rows() const { return as_matrix().rows(); }

index_t matrix_expr::cols() const { return as_matrix().cols(); }

const scalar_expr& matrix_expr::operator[](index_t i) const { return as_matrix()[i]; }

const scalar_expr& matrix_expr::operator()(index_t i, index_t j) const { return as_matrix()(i, j); }

void matrix_expr::set(index_t i, index_t j, const scalar_expr& value) {
  as_matrix_mut().set_unchecked(i, j, value);
}

matrix_expr matrix_expr::get_block(index_t row, index_t col, index_t nrows, index_t ncols) const {
  matrix result = as_matrix().get_block(row, col, nrows, ncols);
  return matrix_expr{std::move(result)};
}

void matrix_expr::set_block(index_t row, index_t col, index_t nrows, index_t ncols,
                            const matrix_expr& block) {
  as_matrix_mut().set_block(row, col, nrows, ncols, block.as_matrix());
}

matrix_expr matrix_expr::transposed() const { return matrix_expr{as_matrix().transposed()}; }

matrix_expr matrix_expr::reshape(index_t nrows, index_t ncols) const {
  if (nrows < 0 || ncols < 0) {
    throw dimension_error("Dimensions must be non-negative. Received [{}, {}]", nrows, ncols);
  }
  if (static_cast<std::size_t>(nrows * ncols) != size()) {
    throw dimension_error(
        "Reshaped dimensions [{} x {} = {}] does not match number of input elements [{} x {} = {}]",
        nrows, ncols, nrows * ncols, rows(), cols(), size());
  }
  return matrix_expr::create(nrows, ncols, to_vector());
}

scalar_expr matrix_expr::squared_norm() const {
  const matrix& m = as_matrix();
  std::vector<scalar_expr> operands{};
  operands.reserve(m.size());
  for (const auto& x : m) {
    operands.push_back(pow(x, 2));
  }
  return addition::from_operands(operands);
}

scalar_expr matrix_expr::norm() const { return sqrt(squared_norm()); }

const matrix& matrix_expr::as_matrix() const { return cast_unchecked<const matrix>(*this); }

matrix& matrix_expr::as_matrix_mut() { return cast_unchecked<matrix>(*this); }

std::vector<scalar_expr> matrix_expr::to_vector() const { return as_matrix().data(); }

matrix_expr matrix_expr::operator-() const { return operator*(*this, constants::negative_one); }

matrix_expr matrix_expr::diff(const scalar_expr& var, const int reps,
                              const non_differentiable_behavior behavior) const {
  derivative_visitor visitor{var, behavior};
  return matrix_expr{as_matrix().map_children([&visitor, reps](const scalar_expr& x) {
    scalar_expr result = x;
    for (int i = 0; i < reps; ++i) {
      result = visitor.apply(result);
    }
    return result;
  })};
}

matrix_expr matrix_expr::jacobian(const absl::Span<const scalar_expr> vars,
                                  const non_differentiable_behavior behavior) const {
  if (cols() != 1) {
    throw dimension_error(
        "Jacobian can only be computed on column-vectors. Received dimensions: [{}, {}]", rows(),
        cols());
  }
  const auto& m = as_matrix();
  return wf::jacobian(m.data(), vars, behavior);
}

matrix_expr matrix_expr::jacobian(const matrix_expr& vars,
                                  const non_differentiable_behavior behavior) const {
  if (vars.rows() != 1 && vars.cols() != 1) {
    throw dimension_error("Variables must be a row or column vector. Received dimensions: [{}, {}]",
                          vars.rows(), vars.cols());
  }
  const auto& m = vars.as_matrix();
  return jacobian(m.data(), behavior);  //  Call span version.
}

matrix_expr matrix_expr::distribute() const {
  return matrix_expr{as_matrix().map_children(distribute_visitor{})};
}

matrix_expr matrix_expr::subs(const scalar_expr& target, const scalar_expr& replacement) const {
  return matrix_expr{
      as_matrix().map_children([&](const scalar_expr& x) { return x.subs(target, replacement); })};
}

matrix_expr matrix_expr::collect(absl::Span<const scalar_expr> terms) const {
  return matrix_expr{
      as_matrix().map_children([&terms](const scalar_expr& x) { return collect_many(x, terms); })};
}

matrix_expr matrix_expr::eval() const {
  return matrix_expr{as_matrix().map_children(&wf::evaluate)};
}

relative_order order_struct<matrix_expr>::operator()(const matrix_expr& a,
                                                     const matrix_expr& b) const {
  const matrix& am = a.as_matrix();
  const matrix& bm = b.as_matrix();
  if (const relative_order order = order_by_comparison(am.dimensions(), bm.dimensions());
      order != relative_order::equal) {
    return order;
  }
  return determine_order(am.data(), bm.data());
}

matrix_expr operator+(const matrix_expr& a, const matrix_expr& b) {
  return matrix_expr{a.as_matrix() + b.as_matrix()};
}

matrix_expr operator-(const matrix_expr& a, const matrix_expr& b) {
  return matrix_expr{a.as_matrix() - b.as_matrix()};
}

matrix_expr operator*(const matrix_expr& a, const matrix_expr& b) {
  return matrix_expr{a.as_matrix() * b.as_matrix()};
}

matrix_expr operator*(const matrix_expr& a, const scalar_expr& b) {
  const matrix& a_mat = a.as_matrix();

  std::vector<scalar_expr> data{};
  data.reserve(a_mat.size());
  std::transform(a_mat.begin(), a_mat.end(), std::back_inserter(data),
                 [&b](const scalar_expr& a_expr) { return a_expr * b; });

  return matrix_expr{matrix(a_mat.rows(), a_mat.cols(), std::move(data))};
}

}  // namespace wf
