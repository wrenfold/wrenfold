// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <array>
#include <random>

#include "wf/constants.h"
#include "wf/functions.h"
#include "wf/matrix_functions.h"
#include "wf/utility/error_types.h"
#include "wf_test_support/test_macros.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/types/span.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {
using namespace custom_literals;

TEST(MatrixOperationsTest, TestConstruct) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  const scalar_expr z{"z"};

  // Construct vector:
  const matrix_expr v = make_vector(x, 4, y + z);
  ASSERT_EQ(v.rows(), 3);
  ASSERT_EQ(v.cols(), 1);
  ASSERT_IDENTICAL(x, v[0]);
  ASSERT_IDENTICAL(x, v(0, 0));
  ASSERT_IDENTICAL(4, v[1]);
  ASSERT_IDENTICAL(4, v(1, 0));
  ASSERT_IDENTICAL(y + z, v[2]);
  ASSERT_IDENTICAL(y + z, v(2, 0));

  // Invalid access:
  ASSERT_THROW(v[4], dimension_error);
  ASSERT_THROW(v(10, 2), dimension_error);

  // Compare to row-vector constructor
  ASSERT_IDENTICAL(v.transposed(), make_row_vector(x, 4, y + z));

  // Construct matrix:
  // clang-format off
  const matrix_expr m = make_matrix(2, 3,
                                    x + y, 0, 5 - z,
                                    z * y, x, 2 / x);
  // clang-format on
  ASSERT_EQ(2, m.rows());
  ASSERT_EQ(3, m.cols());
  ASSERT_IDENTICAL(x + y, m(0, 0));  //  first row
  ASSERT_IDENTICAL(0, m(0, 1));
  ASSERT_IDENTICAL(5 - z, m(0, 2));
  ASSERT_IDENTICAL(y * z, m(1, 0));  //  second row
  ASSERT_IDENTICAL(x, m(1, 1));
  ASSERT_IDENTICAL(2 / x, m(1, 2));

  // Invalid access:
  ASSERT_THROW(m(-1, 0), dimension_error);
  ASSERT_THROW(m(0, -1), dimension_error);
  ASSERT_THROW(m(5, 0), dimension_error);
  ASSERT_THROW(m(1, 10), dimension_error);
  ASSERT_THROW(m[0], dimension_error);
}

TEST(MatrixOperationsTest, TestGetBlock) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  const scalar_expr z{"z"};
  const scalar_expr a{"a"};
  const scalar_expr b{"b"};
  const scalar_expr c{"c"};
  // clang-format off
  const matrix_expr m1 = make_matrix(3, 2,
                                     x, a,
                                     y, b,
                                     z, c);
  // clang-format on
  ASSERT_IDENTICAL(make_vector(x, y, z), m1.get_block(0, 0, 3, 1));
  ASSERT_IDENTICAL(make_vector(y, z), m1.get_block(1, 0, 2, 1));
  ASSERT_IDENTICAL(make_vector(a, b), m1.get_block(0, 1, 2, 1));
  ASSERT_IDENTICAL(make_row_vector(y, b), m1.get_block(1, 0, 1, 2));
  ASSERT_IDENTICAL(make_row_vector(y), m1.get_block(1, 0, 1, 1));
  ASSERT_IDENTICAL(make_matrix(2, 2, y, b, z, c), m1.get_block(1, 0, 2, 2));

  // clang-format off
  const matrix_expr m2 = make_matrix(3, 4,
                                     x, constants::pi, 2, x - z,
                                     3.0, -5, y, pow(z, 2),
                                     2 * z, constants::euler, -1, sin(x));
  // clang-format on
  ASSERT_IDENTICAL(make_row_vector(x, constants::pi, 2, x - z), m2.get_block(0, 0, 1, 4));
  ASSERT_IDENTICAL(make_vector(x, 3.0, 2 * z), m2.get_block(0, 0, 3, 1));
  ASSERT_IDENTICAL(make_matrix(2, 2, y, pow(z, 2), -1, sin(x)), m2.get_block(1, 2, 2, 2));

  // bounds checking:
  ASSERT_THROW(m2.get_block(-1, 0, 2, 1), dimension_error);
  ASSERT_THROW(m2.get_block(1, -5, 1, 1), dimension_error);
  ASSERT_THROW(m2.get_block(1, 1, 4, 1), dimension_error);
  ASSERT_THROW(m2.get_block(1, 1, 2, 5), dimension_error);
}

TEST(MatrixOperationsTest, TestTranspose) {
  const scalar_expr a{"a"};
  const scalar_expr b{"b"};
  const scalar_expr c{"c"};
  // clang-format off
  const matrix_expr m = make_matrix(2, 5,
                                    cos(a), sin(b), -1, constants::pi * 3, 0.0,
                                    tan(c), c*a, 4, 5.0, a);
  // clang-format on
  const matrix_expr m_t = m.transposed();
  ASSERT_EQ(5, m_t.rows());
  ASSERT_EQ(2, m_t.cols());
  for (int i = 0; i < m_t.rows(); ++i) {
    for (int j = 0; j < m_t.cols(); ++j) {
      ASSERT_IDENTICAL(m_t(i, j), m(j, i));
    }
  }
  ASSERT_IDENTICAL(m, m.transposed().transposed());
}

TEST(MatrixOperationsTest, TestReshape) {
  const auto [a, b, c] = make_symbols("a", "b", "c");
  const matrix_expr m = make_matrix(2, 4, a * 2, -b, cos(c), 5, c / a, 8 * a, b * c, c / 2);

  const matrix_expr m_reshape = m.reshape(4, 2);
  ASSERT_EQ(4, m_reshape.rows());
  ASSERT_EQ(2, m_reshape.cols());

  const auto old_contents = m.to_vector();
  const auto new_contents = m_reshape.to_vector();
  ASSERT_TRUE(std::equal(old_contents.begin(), old_contents.end(), new_contents.begin(),
                         new_contents.end(), is_identical_struct<scalar_expr>{}));
}

TEST(MatrixOperationsTest, TestVec) {
  const scalar_expr a{"a"};
  const scalar_expr b{"b"};
  const scalar_expr c{"c"};
  const scalar_expr d{"d"};
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  ASSERT_IDENTICAL(make_vector(a, b, c), vectorize_matrix(make_row_vector(a, b, c)));
  ASSERT_IDENTICAL(make_vector(a, c, b, d, x, y),
                   vectorize_matrix(make_matrix(2, 3, a, b, x, c, d, y)));
}

TEST(MatrixOperationsTest, TestStack) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  const auto m0 = make_matrix(2, 3, x, 2 * y, z - 2, sin(x), -x, 5 * z);
  const auto m1 = make_matrix(3, 3, pow(x, z), z - y, -2, 5 - x, sin(y) * cos(z), -x, 7, 6, y * z);
  const auto m2 = make_matrix(1, 3, y * y - z, -3 * x, cos(y + z));

  const auto vertical = vstack({m0, m1, m2});
  ASSERT_IDENTICAL(m0, vertical.get_block(0, 0, 2, 3));
  ASSERT_IDENTICAL(m1, vertical.get_block(2, 0, 3, 3));
  ASSERT_IDENTICAL(m2, vertical.get_block(5, 0, 1, 3));

  const auto horizontal = hstack({m0.transposed(), m1, m2.transposed()});
  ASSERT_IDENTICAL(m0.transposed(), horizontal.get_block(0, 0, 3, 2));
  ASSERT_IDENTICAL(m1, horizontal.get_block(0, 2, 3, 3));
  ASSERT_IDENTICAL(m2.transposed(), horizontal.get_block(0, 5, 3, 1));

  // stack diagonally and fill in with zeros:
  const auto diagonal = diagonal_stack({m0, m1, m2});
  ASSERT_EQ(6, diagonal.rows());
  ASSERT_EQ(9, diagonal.cols());
  ASSERT_IDENTICAL(m0, diagonal.get_block(0, 0, 2, 3));
  ASSERT_IDENTICAL(m1, diagonal.get_block(2, 3, 3, 3));
  ASSERT_IDENTICAL(m2, diagonal.get_block(5, 6, 1, 3));

  // check off diagonal blocks:
  for (index_t i = 0; i < diagonal.rows(); ++i) {
    for (index_t j = 0; j < diagonal.cols(); ++j) {
      const bool in_first_block = (i < 2) && (j < 3);
      const bool in_second_block = (i >= 2 && i < 5) && (j >= 3 && j < 6);
      const bool in_third_block = (i >= 5 && i < 6) && (j >= 6 && j < 9);
      if (!in_first_block && !in_second_block && !in_third_block) {
        ASSERT_IDENTICAL(0, diagonal(i, j));
      }
    }
  }

  ASSERT_THROW(hstack({}), wf::dimension_error);
  ASSERT_THROW(vstack({}), wf::dimension_error);
  ASSERT_THROW(diagonal_stack({}), wf::dimension_error);
}

TEST(MatrixOperationsTest, TestAddition) {
  const scalar_expr a{"a"};
  const scalar_expr b{"b"};
  const scalar_expr c{"c"};
  const scalar_expr d{"d"};
  ASSERT_IDENTICAL(make_vector(a + c, b - d, b * 2 + 5),
                   make_vector(a, -d, b) + make_vector(c, b, b + 5));
  ASSERT_IDENTICAL(make_vector(0, 0, 0), make_vector(a, b, c) + make_vector(-a, -b, -c));
  ASSERT_IDENTICAL(make_vector(a, b) + make_row_vector(c, d).transposed(),
                   make_vector(a + c, b + d));

  // Dimension mismatch:
  ASSERT_THROW(make_vector(a, b) + make_row_vector(c, d), dimension_error);
  ASSERT_THROW(make_vector(a, b) + make_matrix(2, 2, c, d, 2, -3_s / 5), dimension_error);

  const matrix_expr m = make_matrix(2, 2, a, b, c, d);
  ASSERT_IDENTICAL(m, m + make_zeros(2, 2));
  ASSERT_IDENTICAL(make_matrix(2, 2, a + 1, b, c, d + 1), m + make_identity(2));
  ASSERT_IDENTICAL(make_matrix(2, 2, 2 * a, b + c, c + b, d + d), m + m.transposed());
  ASSERT_IDENTICAL(make_zeros(2, 2), make_matrix(2, 2, a, b, c, d) - make_matrix(2, 2, a, b, c, d));

  ASSERT_THROW(make_zeros(2, 3) + make_identity(2), wf::dimension_error);
  ASSERT_THROW(make_vector(a, b, c) + make_row_vector(c, b, a), wf::dimension_error);
}

TEST(MatrixOperationsTest, TestMultiplication) {
  const scalar_expr a{"a"};
  const scalar_expr b{"b"};
  const scalar_expr c{"c"};
  const scalar_expr d{"d"};
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  const scalar_expr z{"z"};
  const scalar_expr w{"w"};

  ASSERT_IDENTICAL(make_identity(4), make_identity(4) * make_identity(4));
  ASSERT_IDENTICAL(make_vector(a, b, c, d), make_identity(4) * make_vector(a, b, c, d));
  ASSERT_IDENTICAL(make_vector(0, 0), make_zeros(2, 4) * make_vector(a, b, c, d));

  ASSERT_IDENTICAL(make_vector(a * x + 2 * b * y + 2 * (c - 3), d * a * x - cos(d) * y + c * 2),
                   make_matrix(2, 3, a, -2 * b, c - 3, d * a, cos(d), c) * make_vector(x, -y, 2));
  ASSERT_IDENTICAL(make_matrix(2, 2, a * c, 0, 0, b * d),
                   make_matrix(2, 2, a, 0, 0, b) * make_matrix(2, 2, c, 0, 0, d));

  // clang-format off
  ASSERT_IDENTICAL(make_matrix(2, 2,
                                a * x + b * z, a * y + b * w,
                                c * x + d * z, c * y + d * w),
                   make_matrix(2, 2, a, b, c, d) * make_matrix(2, 2, x, y, z, w));
  // clang-format on

  // Outer product:
  ASSERT_IDENTICAL(make_matrix(2, 2, x * a, x * b, y * a, y * b),
                   make_vector(x, y) * make_row_vector(a, b));

  // Distribute scalars into matrix:
  ASSERT_IDENTICAL(make_matrix(2, 2, x + 5, 0, 0, x + 5), make_identity(2) * (x + 5));
  ASSERT_IDENTICAL(make_matrix(2, 3, sin(x) * 3, z * 3, 9, cos(y) * 3, 2, 0),
                   make_matrix(2, 3, sin(x), z, 3, cos(y), 2_s / 3, 0) * 3);

  // 1xn * nx1 --> 1x1
  ASSERT_IDENTICAL(make_matrix(1, 1, a * c + b * d), make_row_vector(a, b) * make_vector(c, d));
  ASSERT_IDENTICAL(make_matrix(1, 1, a * c * x + b * d * x),
                   (make_row_vector(a, b) * make_vector(c, d) * x).distribute());

  // Inner dimension mismatch
  ASSERT_THROW(make_zeros(2, 3) * make_zeros(4, 2), dimension_error);
  ASSERT_THROW(make_zeros(10, 10) * make_vector(x, y, z), dimension_error);
  ASSERT_THROW(make_row_vector(1, -3, z) * make_vector(z, sin(y)), dimension_error);
}

TEST(MatrixOperationsTest, TestSquaredNorm) {
  auto [x, y, z] = make_symbols("x", "y", "z");
  ASSERT_IDENTICAL(x * x + y * y + z * z, make_vector(x, y, z).squared_norm());
  ASSERT_IDENTICAL(4 * x * x + 9 * y * y + 36, make_vector(2 * x, 3 * y, 6).squared_norm());
  ASSERT_IDENTICAL(0, make_zeros(2, 3).squared_norm());
  ASSERT_IDENTICAL(3, make_identity(3).squared_norm());
}

TEST(MatrixOperationsTest, TestJacobian) {
  auto [x, y, z] = make_symbols("x", "y", "z");
  const auto v1 = make_vector(x * x + z, sin(y) * x);

  const auto v1_jacobian = make_matrix(2, 3, v1[0].diff(x), v1[0].diff(y), v1[0].diff(z),
                                       v1[1].diff(x), v1[1].diff(y), v1[1].diff(z));
  ASSERT_IDENTICAL(v1_jacobian, v1.jacobian({x, y, z}));

  const auto v2 = make_vector(x * y * z * sin(z), pow(y, x), cos(x * y) - sin(z));
  const auto v2_jacobian =
      make_matrix(3, 3, v2[0].diff(x), v2[0].diff(y), v2[0].diff(z), v2[1].diff(x), v2[1].diff(y),
                  v2[1].diff(z), v2[2].diff(x), v2[2].diff(y), v2[2].diff(z));
  ASSERT_IDENTICAL(v2_jacobian, v2.jacobian({x, y, z}));

  ASSERT_THROW(v1.jacobian(make_zeros(2, 3)), wf::dimension_error);
  ASSERT_THROW(make_matrix(2, 2, x * x, y - 3, 0, z).jacobian({x, y, z}), wf::dimension_error);
}

void check_full_piv_lu_solution(
    const matrix_expr& A_in,
    const std::tuple<matrix_expr, matrix_expr, matrix_expr, matrix_expr>& solution) {
  auto [P, L, U, Q] = solution;

  // print verbosely
#if 0
  fmt::print("P:\n{}\n", P);
  fmt::print("L:\n{}\n", L);
  fmt::print("U:\n{}\n", U);
  fmt::print("Q:\n{}\n", Q);
#endif

  // check that P/Q are pivot matrices:
  // this is only a partial check, since any orthonormal matrix would satisfy
  ASSERT_IDENTICAL(make_identity(A_in.rows()), P * P.transposed());
  ASSERT_IDENTICAL(make_identity(A_in.cols()), Q * Q.transposed());

  // Check that L is lower triangular
  ASSERT_EQ(L.rows(), A_in.rows());
  for (int row = 0; row < L.rows(); ++row) {
    for (int col = 0; col < L.cols(); ++col) {
      if (row < col) {
        ASSERT_IDENTICAL(0, L(row, col)) << fmt::format("row = {}, col = {}\nL = {}", row, col, L);
      }
    }
  }

  // Check that U is upper triangular
  for (int row = 0; row < U.rows(); ++row) {
    for (int col = 0; col < U.cols(); ++col) {
      if (row > col) {
        ASSERT_IDENTICAL(0, U(row, col)) << fmt::format("row = {}, col = {}\nU = {}", row, col, U);
      }
    }
  }

  const matrix_expr A_out{P * L * U * Q};
  ASSERT_IDENTICAL(A_in, A_out) << fmt::format("P = {}\nL={}\nU={}\nQ={}\n", P, L, U, Q);
}

matrix_expr check_permutation_matrix(absl::Span<const int> permutation) {
  std::vector<scalar_expr> elements(permutation.size() * permutation.size(), constants::zero);

  for (std::size_t row = 0; row < permutation.size(); ++row) {
    const int col_index = permutation[row];
    elements[row * permutation.size() + static_cast<std::size_t>(col_index)] = constants::one;
  }
  const auto dim = static_cast<index_t>(permutation.size());
  return matrix_expr::create(dim, dim, std::move(elements));
}

const auto& get_four_element_permutations() {
  // all permutations of 4 indices: https://oeis.org/A159880
  static const std::vector<std::array<int, 4>> permutations_4 = {
      {0, 1, 2, 3}, {1, 0, 2, 3}, {2, 0, 1, 3}, {0, 2, 1, 3}, {1, 2, 0, 3}, {2, 1, 0, 3},
      {3, 1, 0, 2}, {0, 1, 3, 2}, {1, 0, 3, 2}, {3, 0, 1, 2}, {0, 3, 1, 2}, {1, 3, 0, 2},
      {2, 3, 0, 1}, {3, 2, 0, 1}, {0, 2, 3, 1}, {2, 0, 3, 1}, {3, 0, 2, 1}, {0, 3, 2, 1},
      {1, 3, 2, 0}, {2, 3, 1, 0}, {3, 2, 1, 0}, {1, 2, 3, 0}, {2, 1, 3, 0}, {3, 1, 2, 0},
  };
  return permutations_4;
}

TEST(MatrixOperationsTest, TestFactorizeLU1) {
  const std::vector<std::array<int, 4>>& permutations_4 = get_four_element_permutations();

  // dimension 2:
  for (std::size_t i = 0; i < 2; ++i) {
    matrix_expr A =
        check_permutation_matrix(absl::Span<const int>(permutations_4[i]).subspan(0, 2));
    check_full_piv_lu_solution(A, factorize_full_piv_lu(A));
  }

  // dimension 3:
  for (std::size_t i = 0; i < 3; ++i) {
    matrix_expr A =
        check_permutation_matrix(absl::Span<const int>(permutations_4[i]).subspan(0, 3));
    check_full_piv_lu_solution(A, factorize_full_piv_lu(A));
  }

  // dimension 4:
  for (std::size_t i = 0; i < permutations_4.size(); ++i) {
    matrix_expr A = check_permutation_matrix(absl::Span<const int>(permutations_4[i]));
    check_full_piv_lu_solution(A, factorize_full_piv_lu(A));
  }

  // fully symbolic matrices:
  matrix_expr A2 = make_matrix_of_symbols("x", 2, 2);
  check_full_piv_lu_solution(A2, factorize_full_piv_lu(A2));

  matrix_expr A3 = make_matrix_of_symbols("x", 3, 3);
  check_full_piv_lu_solution(A3, factorize_full_piv_lu(A3));

  matrix_expr A4 = make_matrix_of_symbols("x", 4, 4);
  check_full_piv_lu_solution(A4, factorize_full_piv_lu(A4));

  // zeros:
  matrix_expr Z2 = make_zeros(2, 2);
  check_full_piv_lu_solution(Z2, factorize_full_piv_lu(Z2));

  matrix_expr Z3 = make_zeros(3, 3);
  check_full_piv_lu_solution(Z3, factorize_full_piv_lu(Z3));
}

TEST(MatrixOperationsTest, TestFactorizeLU2) {
  // some non-square symbolic matrices:
  const std::vector<std::pair<index_t, index_t>> dims = {
      {2, 3}, {2, 4}, {2, 5}, {3, 4}, {3, 5}, {3, 6}, {4, 5}, {4, 6},
  };

  for (const auto& [row, col] : dims) {
    matrix_expr A = make_matrix_of_symbols("x", row, col);
    check_full_piv_lu_solution(A, factorize_full_piv_lu(A));
  }

#if 0
  for (const auto& [col, row] : dims) {
    // Transposed version:
    matrix_expr A = make_matrix_of_symbols("x", row, col);
    check_full_piv_lu_solution(A, factorize_full_piv_lu(A));
  }
#endif

  matrix_expr Z24 = make_zeros(2, 4);
  check_full_piv_lu_solution(Z24, factorize_full_piv_lu(Z24));

#if 0
  matrix_expr Z42 = make_zeros(4, 2);
  check_full_piv_lu_solution(Z42, factorize_full_piv_lu(Z42));

  matrix_expr Z53 = make_zeros(5, 3);
  check_full_piv_lu_solution(Z53, factorize_full_piv_lu(Z53));
#endif
}

TEST(MatrixOperationsTest, TestFactorizeLU3) {
  // Some singular matrices that require full pivot:
  matrix_expr A = make_matrix(3, 3, 0, 0, 0, 0, 0, "x", 0, 0, 0);
  check_full_piv_lu_solution(A, factorize_full_piv_lu(A));

  A = make_matrix(4, 4, 0, 0, 0, 0, 0, "y", 0, 0, 0, 0, 0, 0, 0, 0, 0, -5);
  check_full_piv_lu_solution(A, factorize_full_piv_lu(A));

  A = make_matrix(3, 5, 0, 0, 0, 0, "z", 0, 0, 0, 0, 0, 0, 0, 0, 0, 8);
  check_full_piv_lu_solution(A, factorize_full_piv_lu(A));
}

TEST(MatrixOperationsTest, TestFactorizeRandomLU) {
  // Test some random integer matrices to see if we can find failures.
  std::default_random_engine engine{1399};
  std::uniform_int_distribution<int> distribution{-10, 10};

  const std::vector<std::pair<index_t, index_t>> dims = {{2, 2}, {2, 3}, {3, 3}, {3, 4}, {3, 5},
                                                         {4, 4}, {4, 6}, {5, 7}, {5, 9}};

  for (const auto& [rows, cols] : dims) {
    constexpr int num_trials = 500;
    for (int i = 0; i < num_trials; ++i) {
      std::vector<scalar_expr> data{};
      data.reserve(rows * cols);
      for (int j = 0; j < rows * cols; ++j) {
        data.emplace_back(distribution(engine));
      }
      matrix_expr M = matrix_expr::create(rows, cols, std::move(data));
      check_full_piv_lu_solution(M, factorize_full_piv_lu(M));
    }
  }
}

TEST(MatrixOperationsTest, TestFactorizeLU4) {
  // clang-format off
  auto M = make_matrix(4, 4,
                        0, -2, -7, 7,
                        3, 0, 1, -7,
                        5, -3, 0, 1,
                        -1, -1, 4, 0);
  // clang-format on
  check_full_piv_lu_solution(M, factorize_full_piv_lu(M));
}

// See https://github.com/wrenfold/wrenfold/issues/235
#if 0
TEST(MatrixOperationsTest, TestFactorizeLU5) {
  // clang-format off
  auto M = make_matrix(5, 3,
                       2, 3, -7,
                       0, 0, 0,
                       5, 7, -9,
                       10, 7, -1,
                       2, 8, 9);
  // clang-format on
  check_full_piv_lu_solution(M, factorize_full_piv_lu(M));
}
#endif

TEST(MatrixOperationsTest, TestDeterminantNonSquare) {
  ASSERT_THROW(determinant(make_zeros(2, 3)), wf::dimension_error);
  ASSERT_THROW(determinant(make_zeros(3, 2)), wf::dimension_error);
}

TEST(MatrixOperationsTest, TestDeterminant1x1) {
  ASSERT_IDENTICAL(1, determinant(make_identity(1)));
  ASSERT_IDENTICAL(5, determinant(make_vector(5)));
}

TEST(MatrixOperationsTest, TestDeterminant2x2) {
  auto I2 = make_identity(2);
  ASSERT_IDENTICAL(1, determinant(I2));
  ASSERT_IDENTICAL(1, determinant(-I2));

  // 2x2 version
  auto [a, b, c, d] = make_symbols("a", "b", "c", "d");
  auto A2 = make_matrix(2, 2, a, b, c, d);
  ASSERT_IDENTICAL(a * d - c * b, determinant(A2));
}

TEST(MatrixOperationsTest, TestDeterminant3x3) {
  // Compare 3x3 to cofactor method:
  auto A = make_matrix_of_symbols("x", 3, 3);
  auto det_cofactor = A(0, 0) * determinant(A.get_block(1, 1, 2, 2)) -
                      A(0, 1) * determinant(make_matrix(2, 2, A(1, 0), A(1, 2), A(2, 0), A(2, 2))) +
                      A(0, 2) * determinant(A.get_block(1, 0, 2, 2));
  ASSERT_IDENTICAL(det_cofactor.distribute(), determinant(A));

  auto I3 = make_identity(3);
  ASSERT_IDENTICAL(1, determinant(I3));
  ASSERT_IDENTICAL(-1, determinant(-I3));
  ASSERT_IDENTICAL(0, determinant(make_zeros(3, 3)));

  // test the 3x3 version against some hardcoded matrices
  auto M1 = make_matrix(3, 3, -2, 2, -4, 2, -4, -3, -3, 0, 4);
  ASSERT_IDENTICAL(82, determinant(M1));

  auto M2 = make_matrix(3, 3, 1, -1, -2, -3, 4, 4, -1, -1, -1);
  ASSERT_IDENTICAL(-7, determinant(M2));

  auto M3 = make_matrix(3, 3, 4, -3, -3, 3, -3, 0, 4, 3, -1);
  ASSERT_IDENTICAL(-60, determinant(M3));
}

TEST(MatrixOperationsTest, TestDeterminant) {
  auto M1 = make_matrix(4, 4, 4, -1, -1, -3, 0, 3, -2, -1, 4, 1, 2, 1, -1, 3, -1, 1);
  ASSERT_IDENTICAL(28, determinant(M1));

  auto M2 = make_matrix(4, 4, -2, 0, 6, 2, 6, 6, 2, -7, 7, 1, 3, -6, -8, 9, 0, 4);
  ASSERT_IDENTICAL(-136, determinant(M2));

  auto M3 = make_matrix(4, 4, -9, 6, -2, 0, -3, -2, -9, 0, -1, -5, 3, 0, -3, -1, -9, 0);
  ASSERT_IDENTICAL(0, determinant(M3));

  auto M4 = make_matrix(4, 4, 0, -2, -7, 7, 3, 0, 1, -7, 5, -3, 0, 1, -1, -1, 4, 0);
  ASSERT_IDENTICAL(-411, determinant(M4));

  auto M5 = make_matrix(5, 5, 5, -9, 3, -7, 3, 5, 1, 5, 6, -3, 5, -8, 0, 3, 6, -9, 9, -2, -4, -7,
                        -1, 0, -6, -1, 0);
  ASSERT_IDENTICAL(6870, determinant(M5));

  auto M6 = make_matrix(6, 6, 1, -4, 4, -1, 1, 2, -1, -3, 2, 4, 1, -1, 4, -3, -1, 2, 2, -3, 0, 3, 2,
                        4, 2, 1, -4, 0, 0, 4, -3, 2, 4, -1, -2, -1, -4, 0);
  ASSERT_IDENTICAL(6995, determinant(M6));

  auto M7 = make_matrix(6, 6, 0, 4, -1, -1, 4, -2, 2, 0, -2, 2, -2, 3, -4, -4, 0, -2, 2, -1, 0, 0,
                        -1, 0, -3, -3, 0, -4, -4, 1, 0, -2, 3, 3, 1, 4, 2, 0);
  ASSERT_IDENTICAL(3714, determinant(M7));
}

}  // namespace wf
