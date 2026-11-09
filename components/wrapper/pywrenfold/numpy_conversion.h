// wrenfold symbolic code generator.
// Copyright (c) 2026 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <nanobind/nanobind.h>
#include <Eigen/Core>

#include "wf/matrix_expression.h"

namespace wf {

using numerical_array_variant = std::variant<
    Eigen::Matrix<std::int64_t, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>,
    Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>,
    Eigen::Matrix<std::complex<double>, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>;

// Convert matrix to a numpy array with default numeric promotion.
numerical_array_variant numpy_from_matrix(const matrix_expr& self, const nanobind::kwargs&);

// Apply an explicit dtype before promoting mixed numeric elements, preserving exact integers.
nanobind::object numpy_from_matrix_with_dtype(const matrix_expr& self,
                                              const nanobind::object& dtype);

}  // namespace wf
