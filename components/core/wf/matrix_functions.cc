// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/matrix_functions.h"

#include <numeric>  //  iota

#include "wf/constants.h"
#include "wf/expressions/all_expressions.h"
#include "wf/expressions/matrix.h"
#include "wf/expressions/variable.h"
#include "wrenfold/span.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/types/span.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

matrix_expr make_matrix_of_symbols(const std::string_view prefix, index_t rows, index_t cols) {
  if (rows <= 0 || cols <= 0) {
    throw dimension_error("Cannot construct symbolic matrix with shape: ({}, {})", rows, cols);
  }
  return create_matrix(rows, cols, [&](index_t i, index_t j) {
    std::string name = fmt::format("{}_{}_{}", prefix, i, j);
    return make_expr<variable>(std::move(name), number_set::unknown);
  });
}

matrix_expr make_zeros(index_t rows, index_t cols) {
  if (rows <= 0 || cols <= 0) {
    throw dimension_error("Cannot construct zero matrix with shape: ({}, {})", rows, cols);
  }
  // Eventually we might have a symbolic zero matrix, and this won't be required.
  std::vector<scalar_expr> data(static_cast<std::size_t>(rows * cols), constants::zero);
  return matrix_expr::create(rows, cols, std::move(data));
}

matrix_expr make_identity(const index_t rows, const std::optional<index_t> cols_opt) {
  const index_t cols = cols_opt.value_or(rows);
  if (rows <= 0 || cols <= 0) {
    throw dimension_error("Cannot construct identity matrix with dimensions: [{}, {}]", rows, cols);
  }
  return create_matrix(rows, cols, [&](const index_t i, const index_t j) {
    return i == j ? constants::one : constants::zero;
  });
}

matrix_expr vectorize_matrix(const matrix_expr& m) {
  std::vector<scalar_expr> flattened;
  const auto flat_size = m.rows() * m.cols();
  flattened.reserve(static_cast<std::size_t>(flat_size));
  // Iterate over columns first, transposing the underlying data:
  for (index_t j = 0; j < m.cols(); ++j) {
    for (index_t i = 0; i < m.rows(); ++i) {
      flattened.push_back(m(i, j));
    }
  }
  return matrix_expr::create(flat_size, 1, std::move(flattened));
}

static matrix_expr stack(const absl::Span<const matrix_expr> values, const index_t num_rows,
                         const index_t num_cols) {
  std::vector<scalar_expr> result{};
  result.resize(static_cast<std::size_t>(num_rows) * static_cast<std::size_t>(num_cols),
                constants::zero);

  constexpr constant<1> col_stride{};
  const auto output_span = make_span(result.data(), make_value_pack(num_rows, num_cols),
                                     make_value_pack(num_cols, col_stride));

  // We call this method for horizontal, vertical, and diagonal stacking. So check if we need to
  // increment both or just one dimension.
  const bool increment_rows = num_rows > values[0].rows();
  const bool increment_cols = num_cols > values[0].cols();

  // Maybe not the fastest way of doing this iteration in some cases...
  index_t row_offset = 0;
  index_t col_offset = 0;
  for (const matrix_expr& m : values) {
    const matrix& m_concrete = m.as_matrix();
    for (index_t i = 0; i < m_concrete.rows(); ++i) {
      for (index_t j = 0; j < m_concrete.cols(); ++j) {
        output_span(i + row_offset, j + col_offset) = m_concrete.get_unchecked(i, j);
      }
    }
    row_offset += m_concrete.rows() * static_cast<index_t>(increment_rows);
    col_offset += m_concrete.cols() * static_cast<index_t>(increment_cols);
  }
  return matrix_expr::create(num_rows, num_cols, std::move(result));
}

matrix_expr hstack(const absl::Span<const matrix_expr> values) {
  if (values.empty()) {
    throw dimension_error("Need at least one matrix to stack.");
  }

  const index_t num_rows = values[0].rows();

  index_t total_cols = 0;
  for (const matrix_expr& m : values) {
    total_cols += m.cols();
    if (m.rows() != num_rows) {
      throw dimension_error(
          "All input matrices must have the same number of rows. Received mixed dimensions {} and "
          "{}.",
          num_rows, m.rows());
    }
  }

  return stack(values, num_rows, total_cols);
}

matrix_expr vstack(const absl::Span<const matrix_expr> values) {
  if (values.empty()) {
    throw dimension_error("Need at least one matrix to stack.");
  }

  const index_t num_cols = values[0].cols();

  index_t total_rows = 0;
  for (const matrix_expr& m : values) {
    total_rows += m.rows();
    if (m.cols() != num_cols) {
      throw dimension_error(
          "All input matrices must have the same number of cols. Received mixed dimensions {} and "
          "{}.",
          num_cols, m.cols());
    }
  }
  return stack(values, total_rows, num_cols);
}

matrix_expr diagonal_stack(const absl::Span<const matrix_expr> values) {
  if (values.empty()) {
    throw dimension_error("Need at least one matrix to stack.");
  }
  index_t total_rows = 0;
  index_t total_cols = 0;
  for (const matrix_expr& m : values) {
    total_rows += m.rows();
    total_cols += m.cols();
  }
  return stack(values, total_rows, total_cols);
}

matrix_expr diagonal(const absl::Span<const scalar_expr> values) {
  if (values.empty()) {
    throw dimension_error("Need at least one scalar to stack.");
  }

  const index_t dims = static_cast<index_t>(values.size());
  return create_matrix(dims, dims, [&](auto i, auto j) {
    if (i == j) {
      return values[static_cast<std::size_t>(i)];
    } else {
      return constants::zero;
    }
  });
}

// A simple permutation "matrix".
// Stores a mapping from `permuted row` --> `original row`.
struct permutation_matrix {
 public:
  using container_type = absl::InlinedVector<index_t, 8>;

  explicit permutation_matrix(const std::size_t size) {
    p_.resize(size);
    std::iota(p_.begin(), p_.end(), static_cast<index_t>(0));
  }
  explicit permutation_matrix(container_type&& p, std::size_t num_swaps = 0)
      : p_(std::move(p)), num_swaps_(num_swaps) {}

  // The row index in the input matrix to read from.
  index_t permuted_row(const index_t i) const noexcept { return p_[static_cast<std::size_t>(i)]; }

  // Equivalent to `PermutedRow`, but if this matrix were transposed.
  index_t permuted_row_transposed(const index_t i) const noexcept {
    auto it = std::find(p_.begin(), p_.end(), i);
    return static_cast<index_t>(std::distance(p_.begin(), it));
  }

  // Number of rows in the permutation.
  std::size_t rows() const noexcept { return p_.size(); }

  // Insert a new row at the start, and then swap row `0` and `row`.
  void shift_down_and_swap(const index_t row) {
    for (index_t& index : p_) {
      index += 1;
    }
    p_.insert(p_.begin(), 0);
    if (row != 0) {
      WF_ASSERT_LT(static_cast<std::size_t>(row), p_.size());
      std::swap(p_[0], p_[row]);
      ++num_swaps_;
    }
  }

  // Insert a new row at the start, then swap whatever row contains `row` with 0.
  void shift_down_and_swap_right(index_t row) {
    for (index_t& index : p_) {
      index += 1;
    }
    p_.insert(p_.begin(), 0);
    auto it = std::find(p_.begin(), p_.end(), row);
    WF_ASSERT(it != p_.end());
    if (it != p_.begin()) {
      std::swap(*it, p_[0]);
      ++num_swaps_;
    }
  }

  permutation_matrix transposed() const {
    container_type p_transpose{};
    p_transpose.resize(rows());
    for (std::size_t i = 0; i < p_.size(); ++i) {
      p_transpose[p_[i]] = static_cast<index_t>(i);
    }
    return permutation_matrix{std::move(p_transpose), num_swaps_};
  }

  // Determinant of this permutation matrix. Either 1 or -1.
  constexpr int determinant() const noexcept {
    if (num_swaps_ & 1) {
      return -1;
    } else {
      return 1;
    }
  }

 private:
  container_type p_{};
  std::size_t num_swaps_{0};
};

using dynamic_row_major_span =
    span<scalar_expr, value_pack<dynamic, dynamic>, value_pack<dynamic, constant<1>>>;

inline std::optional<std::tuple<std::size_t, std::size_t>> find_pivot(
    const dynamic_row_major_span& U) {
  for (std::size_t p_row = 0; p_row < U.rows(); ++p_row) {
    for (std::size_t p_col = 0; p_col < U.cols(); ++p_col) {
      if (const scalar_expr& el = U(p_row, p_col); !is_zero(el)) {
        // We can't really know for sure this isn't zero, since it is symbolic. But we can avoid
        // things that are analytically zero.
        return std::make_tuple(p_row, p_col);
      }
    }
  }
  return std::nullopt;
}

// Full-pivoting gaussian elimination.
//
// This method operates by recursively performing gaussian elimination on A.
// We first scan `A` for a suitable non-zero pivot. Once the pivot is found, we move it to the
// top left and form:
//
//  B = P * A * Q, where P and Q are permutation matrices.
//  B = [[  B(0, 0)  r^t
//       [  c        B_inner  ]]
//
// Then we reduce `B` using: C = B_inner - c * r^T / B(0, 0), followed by recursion on C:
//
//  C = P' * C * Q'
//
// This method cannot guarantee a successful decomposition, because the symbolic variable selected
// as the pivot could turn out to be zero after substitution. But we can do best-effort, and avoid
// analytical zeros.
static std::tuple<permutation_matrix, permutation_matrix> factorize_full_piv_lu_internal(
    dynamic_row_major_span L, dynamic_row_major_span U) {
  if (L.rows() == 1) {
    WF_ASSERT_EQ(1, L.cols());
    L(0, 0) = constants::one;
    return std::make_tuple(permutation_matrix(1), permutation_matrix(U.cols()));
  }

  WF_ASSERT_GE(U.rows(), 2);

  // Search for a non-zero pivot.
  auto pivot_indices = find_pivot(U);
  if (!pivot_indices) {
    // no non-zero pivot - just return identity:
    for (std::size_t i = 0; i < L.rows(); ++i) {
      L(i, i) = constants::one;
    }
    return std::make_tuple(permutation_matrix(L.rows()), permutation_matrix(U.cols()));
  }
  const auto [p_row, p_col] = *pivot_indices;

  // permute A - first by swapping rows, then by swapping cols
  if (p_row != 0) {
    for (std::size_t col = 0; col < U.cols(); ++col) {
      std::swap(U(p_row, col), U(0, col));
    }
  }
  if (p_col != 0) {
    for (std::size_t row = 0; row < U.rows(); ++row) {
      std::swap(U(row, p_col), U(row, 0));
    }
  }

  // now modify the bottom-right sub-matrix:
  const scalar_expr pivot = U(0, 0);

  auto L_inner =
      L.block(make_constant_value_pack<1, 1>(), make_value_pack(L.rows() - 1, L.cols() - 1));

  // break A into bottom-right panel
  auto U_inner =
      U.block(make_constant_value_pack<1, 1>(), make_value_pack(U.rows() - 1, U.cols() - 1));
  auto c = U.block(make_constant_value_pack<1, 0>(), make_value_pack(U.rows() - 1, 1));
  auto r_t = U.block(make_constant_value_pack<0, 1>(), make_value_pack(1, U.cols() - 1));

  for (std::size_t i = 0; i < U_inner.rows(); ++i) {
    for (std::size_t j = 0; j < U_inner.cols(); ++j) {
      U_inner(i, j) = U_inner(i, j) - (c(i, 0) * r_t(0, j) / pivot);
    }
  }

  auto permutation_matrices = factorize_full_piv_lu_internal(L_inner, U_inner);
  auto [P, Q] = std::move(permutation_matrices);

  // fill in the upper left element of `L`
  L(0, 0) = constants::one;

  // then the column underneath it:
  WF_ASSERT_EQ(static_cast<std::size_t>(P.rows()), c.rows());
  for (std::size_t i = 0; i < c.rows(); ++i) {
    L(i + 1, 0) = c(P.permuted_row_transposed(static_cast<index_t>(i)), 0) / pivot;
  }

  // Copy r_t so we can permute it:
  std::vector<scalar_expr> r_t_copied;
  r_t_copied.reserve(r_t.cols());
  for (std::size_t j = 0; j < r_t.cols(); ++j) {
    r_t_copied.push_back(r_t(0, j));
  }

  // Permute the top-right row of U:
  WF_ASSERT_EQ(static_cast<std::size_t>(Q.rows()), r_t.cols());
  for (std::size_t j = 0; j < r_t.cols(); ++j) {
    U(0, j + 1) = r_t_copied[Q.permuted_row(static_cast<index_t>(j))];
  }

  // now zero out U below the diagonal
  for (std::size_t j = 1; j < U.rows(); ++j) {
    U(j, 0) = constants::zero;
  }

  P.shift_down_and_swap(static_cast<index_t>(p_row));
  Q.shift_down_and_swap_right(static_cast<index_t>(p_col));

  return std::make_tuple(std::move(P), std::move(Q));
}

static matrix_expr create_matrix_from_permutations(const permutation_matrix& P) {
  std::vector<scalar_expr> data(P.rows() * P.rows(), constants::zero);
  const auto span = make_span(data.data(), make_value_pack(P.rows(), P.rows()),
                              make_value_pack(P.rows(), constant<1>{}));

  for (index_t row = 0; row < P.rows(); ++row) {
    span(row, P.permuted_row(row)) = constants::one;
  }
  return matrix_expr::create(static_cast<index_t>(P.rows()), static_cast<index_t>(P.rows()),
                             std::move(data));
}

static std::tuple<permutation_matrix, matrix, matrix, permutation_matrix>
factorize_full_piv_lu_internal(const matrix& A) {
  // TODO: See https://github.com/wrenfold/wrenfold/issues/235
  if (A.rows() > A.cols()) {
    throw dimension_error(
        "Factorization of matrices with more rows than columns is not supported yet. `A` has shape "
        "[{}, {}]",
        A.rows(), A.cols());
  } else {
    // We copy A and then use this as storage for the output `U`.
    std::vector<scalar_expr> U_storage{A.begin(), A.end()};
    auto U_span = make_span(U_storage.data(), make_value_pack(A.rows(), A.cols()),
                            make_value_pack(A.cols(), constant<1>{}));

    std::vector<scalar_expr> L_storage(static_cast<std::size_t>(A.rows() * A.rows()),
                                       constants::zero);
    auto L_span = make_span(L_storage.data(), make_value_pack(A.rows(), A.rows()),
                            make_value_pack(A.rows(), constant<1>{}));

    auto [P, Q] = factorize_full_piv_lu_internal(L_span, U_span);

    // convert L and U to `Matrix` type
    matrix L{static_cast<index_t>(L_span.rows()), static_cast<index_t>(L_span.cols()),
             std::move(L_storage)};
    matrix U{static_cast<index_t>(U_span.rows()), static_cast<index_t>(U_span.cols()),
             std::move(U_storage)};

    return std::make_tuple(std::move(P), std::move(L), std::move(U), std::move(Q));
  }
}

std::tuple<matrix_expr, matrix_expr, matrix_expr, matrix_expr> factorize_full_piv_lu(
    const matrix_expr& A_in) {
  auto results = factorize_full_piv_lu_internal(A_in.as_matrix());

  const auto& P = std::get<0>(results);
  const auto& Q = std::get<3>(results);

  auto L = std::move(std::get<1>(results));
  auto U = std::move(std::get<2>(results));

  // Convert to matrix_expr:
  return std::make_tuple(create_matrix_from_permutations(P), matrix_expr{std::move(L)},
                         matrix_expr{std::move(U)}, create_matrix_from_permutations(Q));
}

scalar_expr determinant(const matrix_expr& m) {
  const matrix& mat = m.as_matrix();
  if (mat.rows() != mat.cols()) {
    throw dimension_error(
        "Determinant can only be computed for square matrices. Dimensions = [{}, {}]", mat.rows(),
        mat.cols());
  }

  // Hardcoded solutions for 1x1, 2x2, and 3x3
  if (mat.rows() == 1) {
    return mat.get_unchecked(0, 0);
  } else if (mat.rows() == 2) {
    return mat.get_unchecked(0, 0) * mat.get_unchecked(1, 1) -
           mat.get_unchecked(0, 1) * mat.get_unchecked(1, 0);
  } else if (mat.rows() == 3) {
    return mat.get_unchecked(0, 0) * mat.get_unchecked(1, 1) * mat.get_unchecked(2, 2) -
           mat.get_unchecked(0, 0) * mat.get_unchecked(1, 2) * mat.get_unchecked(2, 1) -
           mat.get_unchecked(0, 1) * mat.get_unchecked(1, 0) * mat.get_unchecked(2, 2) +
           mat.get_unchecked(0, 1) * mat.get_unchecked(1, 2) * mat.get_unchecked(2, 0) +
           mat.get_unchecked(0, 2) * mat.get_unchecked(1, 0) * mat.get_unchecked(2, 1) -
           mat.get_unchecked(0, 2) * mat.get_unchecked(1, 1) * mat.get_unchecked(2, 0);
  }

  // General case, use full-piv LU:
  // TODO: Not sure if this is preferable, or if symbolic co-factor method is better?
  auto factorization = factorize_full_piv_lu_internal(mat);

  const auto& P = std::get<0>(factorization);
  const auto& U = std::get<2>(factorization);
  const auto& Q = std::get<3>(factorization);

  // The product of the diagonal is the product of eigenvalues, which equals the determinant:
  scalar_expr prod = P.determinant() * Q.determinant();
  for (index_t i = 0; i < U.rows(); ++i) {
    prod = prod * U.get_unchecked(i, i);
  }
  return prod;
}

}  // namespace wf
