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

// Convert matrix to numpy array.
// We need `kwargs` here because numpy might pass copy=True. This argument doesn't matter to us,
// because we never copy expressions. But we need to be able to accept the argument for nanobind to
// do method resolution.
numerical_array_variant numpy_from_matrix(const matrix_expr& self, const nanobind::kwargs&);

}  // namespace wf
