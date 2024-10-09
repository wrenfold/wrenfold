// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/matrix_expression.h"

#include "wf/collect.h"
#include "wf/constants.h"
#include "wf/derivative.h"
#include "wf/distribute.h"
#include "wf/evaluate.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"
#include "wf/functions.h"
#include "wf/plain_formatter.h"
#include "wf/substitute.h"
#include "wf/tree_formatter.h"

namespace wf {

static_assert(std::is_move_assignable_v<matrix_expr> && std::is_move_constructible_v<matrix_expr>,
              "Should be movable");

matrix_expr matrix_expr::create(index_t rows, index_t cols, std::vector<scalar_expr> args) {
  return matrix_expr{std::in_place_type_t<matrix>{}, rows, cols, std::move(args)};
}

std::string matrix_expr::to_string() const { return plain_formatter::convert(*this); }

std::string matrix_expr::to_expression_tree_string() const {
  tree_formatter_visitor formatter{};
  formatter(*this);
  return formatter.take_output();
}

index_t matrix_expr::rows() const { return as_matrix().rows(); }

index_t matrix_expr::cols() const { return as_matrix().cols(); }

const scalar_expr& matrix_expr::operator[](const index_t i) const { return as_matrix()[i]; }

const scalar_expr& matrix_expr::operator()(const index_t i, const index_t j) const {
  return as_matrix()(i, j);
}

matrix_expr matrix_expr::get_block(const index_t row, const index_t col, const index_t nrows,
                                   const index_t ncols) const {
  matrix result = as_matrix().get_block(row, col, nrows, ncols);
  return matrix_expr{std::move(result)};
}

matrix_expr matrix_expr::transposed() const { return matrix_expr{as_matrix().transposed()}; }

matrix_expr matrix_expr::reshape(index_t nrows, index_t ncols) const {
  if (nrows < 0 || ncols < 0) {
    throw dimension_error("Dimensions must be non-negative. Received [{}, {}]", nrows, ncols);
  }
  if (static_cast<std::size_t>(nrows) * static_cast<std::size_t>(ncols) != size()) {
    throw dimension_error(
        "Reshaped dimensions [{} x {} = {}] does not match number of input elements [{} x {} = {}]",
        nrows, ncols, nrows * ncols, rows(), cols(), size());
  }
  return create(nrows, ncols, to_vector());
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

const matrix& matrix_expr::as_matrix() const { return get_unchecked<const matrix>(*this); }

std::vector<scalar_expr> matrix_expr::to_vector() const { return as_matrix().children(); }

matrix_expr matrix_expr::operator-() const { return operator*(*this, constants::negative_one); }

// Expand a matrix expression, and apply `f` to every `scalar_expr` it contains.
template <typename F>
static matrix_expr map_matrix_expression(const matrix_expr& expr, F&& f) {
  return visit(expr, [&](const auto& mat) { return mat.map_children(std::forward<F>(f)); });
}

matrix_expr matrix_expr::diff(const scalar_expr& var, const int reps,
                              const non_differentiable_behavior behavior) const {
  derivative_visitor visitor{var, behavior};
  return map_matrix_expression(*this, [&visitor, reps](const scalar_expr& x) {
    scalar_expr result = x;
    for (int i = 0; i < reps; ++i) {
      result = visitor(result);
    }
    return result;
  });
}

matrix_expr matrix_expr::jacobian(const absl::Span<const scalar_expr> vars,
                                  const non_differentiable_behavior behavior) const {
  if (rows() != 1 && cols() != 1) {
    throw dimension_error("Jacobian can only be computed on vectors. Received dimensions: [{}, {}]",
                          rows(), cols());
  }
  const auto& m = as_matrix();
  return wf::jacobian(m.children(), vars, behavior);
}

matrix_expr matrix_expr::jacobian(const matrix_expr& vars,
                                  const non_differentiable_behavior behavior) const {
  if (vars.rows() != 1 && vars.cols() != 1) {
    throw dimension_error("Variables must be a row or column vector. Received dimensions: [{}, {}]",
                          vars.rows(), vars.cols());
  }
  const auto& m = vars.as_matrix();
  return jacobian(m.children(), behavior);  //  Call span version.
}

matrix_expr matrix_expr::distribute() const {
  return map_matrix_expression(*this, distribute_visitor{});
}

matrix_expr matrix_expr::subs(const scalar_expr& target, const scalar_expr& replacement) const {
  const std::array<scalar_or_boolean_pair, 1> pairs{std::make_tuple(target, replacement)};
  return substitute(*this, pairs);
}

matrix_expr matrix_expr::collect(const absl::Span<const scalar_expr> terms) const {
  return map_matrix_expression(*this,
                               [&terms](const scalar_expr& x) { return collect_many(x, terms); });
}

matrix_expr matrix_expr::eval() const { return map_matrix_expression(*this, &wf::evaluate); }

relative_order order_struct<matrix_expr>::operator()(const matrix_expr& a,
                                                     const matrix_expr& b) const {
  const matrix& am = a.as_matrix();
  const matrix& bm = b.as_matrix();
  if (const relative_order order = order_by_comparison(am.dimensions(), bm.dimensions());
      order != relative_order::equal) {
    return order;
  }
  return determine_order(am.children(), bm.children());
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
  auto data = transform_map<matrix::container_type>(
      a_mat.children(), [&b](const scalar_expr& a_expr) { return a_expr * b; });
  return matrix_expr{std::in_place_type_t<matrix>{}, a_mat.rows(), a_mat.cols(), std::move(data)};
}

}  // namespace wf
