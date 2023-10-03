// Copyright 2023 Gareth Cross
#include "matrix_functions.h"

#include <numeric>  //  iota

#include "absl_imports.h"
#include "constants.h"
#include "expressions/matrix.h"
#include "expressions/variable.h"
#include "span.h"

namespace math {

template <typename Callable>
MatrixExpr CreateMatrixWithLambda(index_t rows, index_t cols, Callable&& callable) {
  std::vector<Expr> data;
  data.reserve(static_cast<std::size_t>(rows * cols));
  iter_matrix(rows, cols, [callable = std::move(callable), &data](index_t i, index_t j) {
    data.push_back(callable(i, j));
  });
  return MatrixExpr::create(rows, cols, std::move(data));
}

MatrixExpr make_matrix_of_symbols(const std::string_view prefix, index_t rows, index_t cols) {
  if (rows <= 0 || cols <= 0) {
    throw DimensionError("Cannot construct symbolic matrix with shape: ({}, {})", rows, cols);
  }
  return CreateMatrixWithLambda(rows, cols, [&](index_t i, index_t j) {
    std::string name = fmt::format("{}_{}_{}", prefix, i, j);
    return make_expr<Variable>(std::move(name));
  });
}

MatrixExpr make_zeros(index_t rows, index_t cols) {
  if (rows <= 0 || cols <= 0) {
    throw DimensionError("Cannot construct zero matrix with shape: ({}, {})", rows, cols);
  }
  // Eventually we might have a symbolic zero matrix, and this won't be required.
  std::vector<Expr> data(static_cast<std::size_t>(rows * cols), Constants::Zero);
  return MatrixExpr::create(rows, cols, std::move(data));
}

// Create an identity matrix.
MatrixExpr make_identity(index_t rows) {
  if (rows <= 0) {
    throw DimensionError("Cannot construct identity matrix with dimension: {}", rows);
  }
  return CreateMatrixWithLambda(
      rows, rows, [&](index_t i, index_t j) { return i == j ? Constants::One : Constants::Zero; });
}

MatrixExpr vectorize_matrix(const MatrixExpr& m) {
  std::vector<Expr> flattened;
  const auto flat_size = m.rows() * m.cols();
  flattened.reserve(static_cast<std::size_t>(flat_size));
  // Iterate over columns first, transposing the underlying data:
  for (index_t j = 0; j < m.cols(); ++j) {
    for (index_t i = 0; i < m.rows(); ++i) {
      flattened.push_back(m(i, j));
    }
  }
  return MatrixExpr::create(flat_size, 1, std::move(flattened));
}

// A simple permutation "matrix".
// Stores a mapping from `permuted row` --> `original row`.
struct PermutationMatrix {
 public:
  using Container = absl::InlinedVector<index_t, 8>;

  explicit PermutationMatrix(std::size_t size) {
    p_.resize(size);
    std::iota(p_.begin(), p_.end(), static_cast<index_t>(0));
  }
  explicit PermutationMatrix(Container&& p) : p_(std::move(p)) {}

  // The row index in the input matrix to read from.
  index_t PermutedRow(index_t i) const noexcept { return p_[static_cast<std::size_t>(i)]; }

  // Equivalent to `PermutedRow`, but if this matrix were transposed.
  index_t PermutedRowTransposed(index_t i) const noexcept {
    auto it = std::find(p_.begin(), p_.end(), i);
    return static_cast<index_t>(std::distance(p_.begin(), it));
  }

  // Number of rows in the permutation.
  std::size_t NumRows() const noexcept { return p_.size(); }

  // Insert a new row at the start, and then swap row `0` and `row`.
  void ShiftDownAndSwap(index_t row) {
    for (index_t& index : p_) {
      index += 1;
    }
    p_.insert(p_.begin(), 0);
    if (row != 0) {
      ASSERT_LESS(static_cast<std::size_t>(row), p_.size());
      std::swap(p_[0], p_[row]);
      ++num_swaps_;
    }
  }

  // Insert a new row at the start, then swap whatever row contains `row` with 0.
  void ShiftDownAndSwapRight(index_t row) {
    for (index_t& index : p_) {
      index += 1;
    }
    p_.insert(p_.begin(), 0);
    auto it = std::find(p_.begin(), p_.end(), row);
    ASSERT(it != p_.end());
    if (it != p_.begin()) {
      std::swap(*it, p_[0]);
      ++num_swaps_;
    }
  }

  PermutationMatrix Transpose() const {
    Container p_transpose{};
    p_transpose.resize(NumRows());
    for (std::size_t i = 0; i < p_.size(); ++i) {
      p_transpose[p_[i]] = static_cast<index_t>(i);
    }
    return PermutationMatrix{std::move(p_transpose)};
  }

  // Determinant of this permutation matrix. Either 1 or -1.
  constexpr int Determinant() const noexcept {
    if (num_swaps_ & 1) {
      return -1;
    } else {
      return 1;
    }
  }

 private:
  Container p_{};
  std::size_t num_swaps_{0};
};

using dynamic_row_major_span =
    span<Expr, value_pack<dynamic, dynamic>, value_pack<dynamic, constant<1>>>;

static inline std::optional<std::tuple<std::size_t, std::size_t>> FindPivot(
    dynamic_row_major_span U) {
  for (std::size_t p_row = 0; p_row < U.rows(); ++p_row) {
    for (std::size_t p_col = 0; p_col < U.cols(); ++p_col) {
      const Expr& el = U(p_row, p_col);
      if (!is_zero(el)) {
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
static std::tuple<PermutationMatrix, PermutationMatrix> FactorizeFullPivLUInternal(
    dynamic_row_major_span L, dynamic_row_major_span U) {
  if (L.rows() == 1) {
    ASSERT_EQUAL(1, L.cols());
    L(0, 0) = Constants::One;
    return std::make_tuple(PermutationMatrix(1), PermutationMatrix(U.cols()));
  }

  ASSERT_GREATER_OR_EQ(U.rows(), 2);

  // Search for a non-zero pivot.
  auto pivot_indices = FindPivot(U);
  if (!pivot_indices) {
    // no non-zero pivot - just return identity:
    for (std::size_t i = 0; i < L.rows(); ++i) {
      L(i, i) = Constants::One;
    }
    return std::make_tuple(PermutationMatrix(L.rows()), PermutationMatrix(U.cols()));
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
  const Expr pivot = U(0, 0);

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

  auto permutation_matrices = FactorizeFullPivLUInternal(L_inner, U_inner);
  auto [P, Q] = std::move(permutation_matrices);

  // fill in the upper left element of `L`
  L(0, 0) = Constants::One;

  // then the column underneath it:
  ASSERT_EQUAL(static_cast<std::size_t>(P.NumRows()), c.rows());
  for (std::size_t i = 0; i < c.rows(); ++i) {
    L(i + 1, 0) = c(P.PermutedRowTransposed(static_cast<index_t>(i)), 0) / pivot;
  }

  // Copy r_t so we can permute it:
  std::vector<Expr> r_t_copied;
  r_t_copied.reserve(r_t.cols());
  for (std::size_t j = 0; j < r_t.cols(); ++j) {
    r_t_copied.push_back(r_t(0, j));
  }

  // Permute the top-right row of U:
  ASSERT_EQUAL(static_cast<std::size_t>(Q.NumRows()), r_t.cols());
  for (std::size_t j = 0; j < r_t.cols(); ++j) {
    U(0, j + 1) = r_t_copied[Q.PermutedRow(static_cast<index_t>(j))];
  }

  // now zero out U below the diagonal
  for (std::size_t j = 1; j < U.rows(); ++j) {
    U(j, 0) = Constants::Zero;
  }

  P.ShiftDownAndSwap(static_cast<index_t>(p_row));
  Q.ShiftDownAndSwapRight(static_cast<index_t>(p_col));

  return std::make_tuple(std::move(P), std::move(Q));
}

static std::tuple<PermutationMatrix, Matrix, Matrix, PermutationMatrix> FactorizeFullPivLUInternal(
    const Matrix& A) {
  if (A.rows() > A.cols()) {
    // To simplify the implementation, we factorize the transpose and then do a fix-up step:
    auto [P, L, U, Q] = FactorizeFullPivLUInternal(A.transposed());

    // First transpose the outputs:
    Matrix U_out = L.transposed();
    Matrix L_out = U.transposed();

    ASSERT_EQUAL(L_out.rows(), A.rows());
    ASSERT_EQUAL(L_out.cols(), A.cols());
    ASSERT_EQUAL(U_out.rows(), A.cols());
    ASSERT_EQUAL(U_out.rows(), U_out.cols());

    // Then we need to normalize the diagonal of L
    for (index_t col = 0; col < L_out.cols(); ++col) {
      Expr v = L_out.get_unchecked(col, col);
      L_out.get_unchecked(col, col) = Constants::One;

      if (!is_zero(v)) {
        for (index_t row = col + 1; row < L_out.rows(); ++row) {
          L_out.get_unchecked(row, col) = L_out.get_unchecked(row, col) / v;
        }
      }

      // Multiply onto rows of U:
      for (index_t u_col = 0; u_col < U_out.cols(); ++u_col) {
        U_out.get_unchecked(col, u_col) = U_out.get_unchecked(col, u_col) * v;
      }
    }
    return std::make_tuple(Q.Transpose(), std::move(L_out), std::move(U_out), P.Transpose());
  } else {
    // We copy A and then use this as storage for the output `U`.
    std::vector<Expr> U_storage{A.begin(), A.end()};
    auto U_span = make_span(U_storage.data(), make_value_pack(A.rows(), A.cols()),
                            make_value_pack(A.cols(), constant<1>{}));

    std::vector<Expr> L_storage(static_cast<std::size_t>(A.rows() * A.rows()),
                                Constants::Zero);
    auto L_span = make_span(L_storage.data(), make_value_pack(A.rows(), A.rows()),
                            make_value_pack(A.rows(), constant<1>{}));

    auto [P, Q] = FactorizeFullPivLUInternal(L_span, U_span);

    // convert L and U to `Matrix` type
    Matrix L{static_cast<index_t>(L_span.rows()), static_cast<index_t>(L_span.cols()),
             std::move(L_storage)};
    Matrix U{static_cast<index_t>(U_span.rows()), static_cast<index_t>(U_span.cols()),
             std::move(U_storage)};

    return std::make_tuple(std::move(P), std::move(L), std::move(U), std::move(Q));
  }
}

static MatrixExpr CreateMatrixFromPermutations(const PermutationMatrix& P) {
  std::vector<Expr> data(P.NumRows() * P.NumRows(), Constants::Zero);
  auto span = make_span(data.data(), make_value_pack(P.NumRows(), P.NumRows()),
                        make_value_pack(P.NumRows(), constant<1>{}));

  for (index_t row = 0; row < P.NumRows(); ++row) {
    span(row, P.PermutedRow(row)) = Constants::One;
  }
  return MatrixExpr::create(static_cast<index_t>(P.NumRows()), static_cast<index_t>(P.NumRows()),
                            std::move(data));
}

std::tuple<MatrixExpr, MatrixExpr, MatrixExpr, MatrixExpr> factorize_full_piv_lu(
    const MatrixExpr& A_in) {
  auto results = FactorizeFullPivLUInternal(A_in.as_matrix());

  const auto& P = std::get<0>(results);
  const auto& Q = std::get<3>(results);

  auto L = std::move(std::get<1>(results));
  auto U = std::move(std::get<2>(results));

  // Convert to MatrixExpr:
  return std::make_tuple(CreateMatrixFromPermutations(P), MatrixExpr{std::move(L)},
                         MatrixExpr{std::move(U)}, CreateMatrixFromPermutations(Q));
}

Expr determinant(const MatrixExpr& m) {
  const Matrix& mat = m.as_matrix();
  if (mat.rows() != mat.cols()) {
    throw DimensionError(
        "Determinant can only be computed for square matrices. Dimensions = [{}, {}]",
        mat.rows(), mat.cols());
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
  auto factorization = FactorizeFullPivLUInternal(mat);

  const auto& P = std::get<0>(factorization);
  const auto& U = std::get<2>(factorization);
  const auto& Q = std::get<3>(factorization);

  // The product of the diagonal is the product of eigenvalues, which equals the determinant:
  Expr prod = P.Determinant() * Q.Determinant();
  for (index_t i = 0; i < U.rows(); ++i) {
    prod = prod * U.get_unchecked(i, i);
  }
  return prod;
}

}  // namespace math
