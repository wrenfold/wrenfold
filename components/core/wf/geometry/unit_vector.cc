// wrenfold symbolic code generator.
// Copyright (c) 2026 wrenfold contributors
// For license information refer to accompanying LICENSE file.
#include "wf/geometry/unit_vector.h"

#include <limits>

#include "wf/constants.h"
#include "wf/functions.h"
#include "wf/matrix_functions.h"

namespace wf {

namespace {

// Map a tangent displacement to a unit vector centered on the last basis axis.
std::pair<matrix_expr, scalar_expr> sphere_exp(const matrix_expr& delta) {
  // exp_sphere(delta) = [sin(a)/a * delta; cos(a)], where a = ||delta||.
  const scalar_expr angle = delta.norm();
  const scalar_expr scale = where(angle > 0, sin(angle) / angle, 1);
  std::vector<scalar_expr> exp;
  exp.reserve(delta.rows() + 1);
  for (index_t i = 0; i < delta.rows(); ++i) {
    exp.push_back(delta[i] * scale);
  }
  exp.push_back(cos(angle));
  return {matrix_expr::create(delta.rows() + 1, 1, std::move(exp)), angle};
}

// Map a unit vector centered on the last basis axis to tangent coordinates.
matrix_expr sphere_log(const matrix_expr& z) {
  // z = [v; w] where v has n-1 entries and w is the last entry. The tangent coordinates are
  // log_sphere([v; w]) = atan2(||v||, w) / ||v|| * v when ||v|| > 0.
  const index_t tangent_dim = z.rows() - 1;
  const scalar_expr v_norm = z.get_block(0, 0, tangent_dim, 1).norm();
  const scalar_expr w = z[tangent_dim];
  const scalar_expr scale = where(v_norm > 0, atan2(v_norm, w) / v_norm, 1);

  // When ||v|| = 0, w < 0 is the antipode. Ceres puts pi on the last tangent
  // axis in that case; otherwise the zero-tangent result is zero.
  const scalar_expr antipode_angle = where(w < 0, constants::pi, constants::zero);
  std::vector<scalar_expr> result;
  result.reserve(tangent_dim);
  for (index_t i = 0; i < tangent_dim; ++i) {
    const scalar_expr fallback = i == tangent_dim - 1 ? antipode_angle : constants::zero;
    result.push_back(where(v_norm > 0, scale * z[i], fallback));
  }
  return matrix_expr::create(tangent_dim, 1, std::move(result));
}
}  // namespace

unit_vector::unit_vector(matrix_expr value) : value_{std::move(value)} {
  if (value_.cols() != 1 || value_.rows() < 2) {
    throw dimension_error("UnitN storage must be Nx1 with N >= 2. Received [{} x {}]",
                          value_.rows(), value_.cols());
  }
}

unit_vector unit_vector::from_name_prefix(const std::string_view name, const index_t dimension) {
  if (dimension < 2) {
    throw dimension_error("UnitN dimension must be >= 2. Received {}", dimension);
  }
  std::vector<scalar_expr> values;
  values.reserve(dimension);
  for (index_t i = 0; i < dimension; ++i) {
    values.emplace_back(fmt::format("{}_{}", name, i), number_set::unknown);
  }
  return unit_vector{matrix_expr::create(dimension, 1, std::move(values))};
}

matrix_expr unit_vector::householder() const {
  // Hertzberg et al., Appendix B.2, Eq. (106) uses R_x = H_x X: X flips the
  // second coordinate, making R_x a rotation with the first coordinate as pivot.
  // Ceres instead uses the reflection H_x directly with the last coordinate
  // as pivot. This gives Ceres' tangent coordinates and Jacobians.
  // https://arxiv.org/abs/1107.1119

  const index_t n = dimension();
  // sigma = sum_{i=0}^{n-2} x_i^2 (squared norm of the first n-1 elements of the unit vector)
  const scalar_expr sigma = value_.get_block(0, 0, n - 1, 1).squared_norm();
  // pivot = x_{n-1} (last element of the unit vector)
  const scalar_expr pivot = value_[n - 1];
  // mu = ||x||
  const scalar_expr mu = norm();

  // p = pivot - mu if pivot <= 0, otherwise p = -sigma / (pivot + mu).
  const scalar_expr p = where(pivot <= 0, pivot - mu, -sigma / (pivot + mu));
  // beta = 2 p^2 / (sigma + p^2)
  const scalar_expr beta = 2 * p * p / (sigma + p * p);
  const boolean_expr general = sigma > std::numeric_limits<double>::epsilon();

  // v = (x_0/p, ..., x_{n-2}/p, 1)
  std::vector<scalar_expr> v;
  v.reserve(n);
  for (index_t i = 0; i < n - 1; ++i) {
    v.push_back(value_[i] / p);
  }
  v.push_back(1);

  // For sigma <= epsilon, Ceres sets v = (x_0, ..., x_{n-2}, 1)
  // and beta = 2 if pivot < 0, otherwise beta = 0.
  const scalar_expr beta_pole = where(pivot < 0, 2, 0);
  std::vector<scalar_expr> entries;
  entries.reserve(n * n);
  // H_x = I - beta v v^T.
  for (index_t row = 0; row < n; ++row) {
    for (index_t col = 0; col < n; ++col) {
      const scalar_expr identity = row == col ? constants::one : constants::zero;
      const scalar_expr pole_row = row == n - 1 ? constants::one : value_[row];
      const scalar_expr pole_col = col == n - 1 ? constants::one : value_[col];
      const scalar_expr pole = identity - beta_pole * pole_row * pole_col;
      entries.push_back(where(general, identity - beta * v[row] * v[col], pole));
    }
  }
  return matrix_expr::create(n, n, std::move(entries));
}

unit_vector unit_vector::retract(const matrix_expr& delta) const {
  // Ceres convention for Hertzberg et al., Appendix B.2, Eqs. (107) and (109).
  if (delta.rows() != dimension() - 1 || delta.cols() != 1) {
    throw dimension_error("UnitN tangent must be [{} x 1]. Received [{} x {}]", dimension() - 1,
                          delta.rows(), delta.cols());
  }
  const auto [exp, angle] = sphere_exp(delta);

  // For a > 0, x plus delta = H_x @ exp_sphere(delta) * ||x||.
  // The paper assumes ||x|| = 1. Ceres preserves the input norm: H_x is orthogonal
  // and exp_sphere(delta) has unit norm, so multiply by ||x|| to keep that radius.
  const matrix_expr updated = householder() * exp * norm();

  // x plus delta = { updated if a > 0; x if a = 0 }.
  return unit_vector{where(angle > 0, updated, value_)};
}

matrix_expr unit_vector::local_coordinates(const unit_vector& other) const {
  // Ceres convention for Hertzberg et al., Appendix B.2, Eqs. (108) and (109).
  if (other.dimension() != dimension()) {
    throw dimension_error("UnitN dimensions must agree: {} and {}", dimension(), other.dimension());
  }
  if (is_zero((other.value_ - value_).squared_norm())) {
    return make_zeros(dimension() - 1, 1);
  }
  // An exact symbolic y = -x is the antipode; runtime cases go through sphere_log().
  if (is_zero((other.value_ + value_).squared_norm())) {
    std::vector<scalar_expr> antipodal(dimension() - 1, constants::zero);
    antipodal.back() = constants::pi;
    return matrix_expr::create(dimension() - 1, 1, std::move(antipodal));
  }

  // z = H_x y / ||x|| = [v; w], where v has n-1 entries.
  const matrix_expr local = (householder() * other.value_) / norm();
  return sphere_log(local);
}

matrix_expr unit_vector::retract_derivative() const {
  // d(x plus delta)/d(delta) at delta = 0: ||x|| H_x[:, 0..n-2].
  return householder().get_block(0, 0, dimension(), dimension() - 1) * norm();
}

matrix_expr unit_vector::local_coordinates_derivative() const {
  // d(y minus x)/d(y) at y = x: H_x[:, 0..n-2]^T / ||x||.
  return householder().get_block(0, 0, dimension(), dimension() - 1).transposed() / norm();
}

}  // namespace wf
