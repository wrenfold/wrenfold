// Copyright 2023 Gareth Cross
#include <array>
#include <random>

#include "absl_imports.h"
#include "constants.h"
#include "error_types.h"
#include "functions.h"
#include "matrix_functions.h"
#include "test_helpers.h"

namespace math {
using namespace custom_literals;
using namespace matrix_operator_overloads;

TEST(MatrixOperationsTest, TestConstruct) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};

  // Construct vector:
  const MatrixExpr v = make_vector(x, 4, y + z);
  ASSERT_EQ(v.rows(), 3);
  ASSERT_EQ(v.cols(), 1);
  ASSERT_IDENTICAL(x, v[0]);
  ASSERT_IDENTICAL(x, v(0, 0));
  ASSERT_IDENTICAL(4, v[1]);
  ASSERT_IDENTICAL(4, v(1, 0));
  ASSERT_IDENTICAL(y + z, v[2]);
  ASSERT_IDENTICAL(y + z, v(2, 0));

  // Invalid access:
  ASSERT_THROW(v[4], DimensionError);
  ASSERT_THROW(v(10, 2), DimensionError);

  // Compare to row-vector constructor
  ASSERT_IDENTICAL(v.transposed(), make_row_vector(x, 4, y + z));

  // Construct matrix:
  // clang-format off
  const MatrixExpr m = make_matrix(2, 3,
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
  ASSERT_THROW(m(-1, 0), DimensionError);
  ASSERT_THROW(m(0, -1), DimensionError);
  ASSERT_THROW(m(5, 0), DimensionError);
  ASSERT_THROW(m(1, 10), DimensionError);
}

TEST(MatrixOperationsTest, TestGetBlock) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  const Expr a{"a"};
  const Expr b{"b"};
  const Expr c{"c"};
  // clang-format off
  const MatrixExpr m1 = make_matrix(3, 2,
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
  const MatrixExpr m2 = make_matrix(3, 4,
                                     x, Constants::Pi, 2, x - z,
                                     3.0, -5, y, pow(z, 2),
                                     2 * z, Constants::Euler, -1, sin(x));
  // clang-format on
  ASSERT_IDENTICAL(make_row_vector(x, Constants::Pi, 2, x - z), m2.get_block(0, 0, 1, 4));
  ASSERT_IDENTICAL(make_vector(x, 3.0, 2 * z), m2.get_block(0, 0, 3, 1));
  ASSERT_IDENTICAL(make_matrix(2, 2, y, pow(z, 2), -1, sin(x)), m2.get_block(1, 2, 2, 2));

  // bounds checking:
  ASSERT_THROW(m2.get_block(-1, 0, 2, 1), DimensionError);
  ASSERT_THROW(m2.get_block(1, -5, 1, 1), DimensionError);
  ASSERT_THROW(m2.get_block(1, 1, 4, 1), DimensionError);
  ASSERT_THROW(m2.get_block(1, 1, 2, 5), DimensionError);
}

TEST(MatrixOperationsTest, TestTranspose) {
  const Expr a{"a"};
  const Expr b{"b"};
  const Expr c{"c"};
  // clang-format off
  const MatrixExpr m = make_matrix(2, 5,
                                    cos(a), sin(b), -1, Constants::Pi * 3, 0.0,
                                    tan(c), c*a, 4, 5.0, a);
  // clang-format on
  const MatrixExpr m_t = m.transposed();
  ASSERT_EQ(5, m_t.rows());
  ASSERT_EQ(2, m_t.cols());
  for (int i = 0; i < m_t.rows(); ++i) {
    for (int j = 0; j < m_t.cols(); ++j) {
      ASSERT_IDENTICAL(m_t(i, j), m(j, i));
    }
  }
  ASSERT_IDENTICAL(m, m.transposed().transposed());
}

TEST(MatrixOperationsTest, TestVec) {
  const Expr a{"a"};
  const Expr b{"b"};
  const Expr c{"c"};
  const Expr d{"d"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(make_vector(a, b, c), vectorize_matrix(make_row_vector(a, b, c)));
  ASSERT_IDENTICAL(make_vector(a, c, b, d, x, y),
                   vectorize_matrix(make_matrix(2, 3, a, b, x, c, d, y)));
}

TEST(MatrixOperationsTest, TestAddition) {
  const Expr a{"a"};
  const Expr b{"b"};
  const Expr c{"c"};
  const Expr d{"d"};
  ASSERT_IDENTICAL(make_vector(a + c, b - d, b * 2 + 5),
                   make_vector(a, -d, b) + make_vector(c, b, b + 5));
  ASSERT_IDENTICAL(make_vector(0, 0, 0), make_vector(a, b, c) + make_vector(-a, -b, -c));
  ASSERT_IDENTICAL(make_vector(a, b) + make_row_vector(c, d).transposed(),
                   make_vector(a + c, b + d));

  // Dimension mismatch:
  ASSERT_THROW(make_vector(a, b) + make_row_vector(c, d), DimensionError);
  ASSERT_THROW(make_vector(a, b) + make_matrix(2, 2, c, d, 2, -3_s / 5), DimensionError);

  const MatrixExpr m = make_matrix(2, 2, a, b, c, d);
  ASSERT_IDENTICAL(m, m + make_zeros(2, 2));
  ASSERT_IDENTICAL(make_matrix(2, 2, a + 1, b, c, d + 1), m + make_identity(2));
  ASSERT_IDENTICAL(make_matrix(2, 2, 2 * a, b + c, c + b, d + d), m + m.transposed());
  ASSERT_IDENTICAL(make_zeros(2, 2), make_matrix(2, 2, a, b, c, d) - make_matrix(2, 2, a, b, c, d));
}

TEST(MatrixOperationsTest, TestMultiplication) {
  const Expr a{"a"};
  const Expr b{"b"};
  const Expr c{"c"};
  const Expr d{"d"};
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  const Expr w{"w"};

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
  ASSERT_THROW(make_zeros(2, 3) * make_zeros(4, 2), DimensionError);
  ASSERT_THROW(make_zeros(10, 10) * make_vector(x, y, z), DimensionError);
  ASSERT_THROW(make_row_vector(1, -3, z) * make_vector(z, sin(y)), DimensionError);
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
}

void check_full_piv_lu_solution(
    const MatrixExpr& A_in,
    const std::tuple<MatrixExpr, MatrixExpr, MatrixExpr, MatrixExpr>& solution) {
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

  const MatrixExpr A_out{P * L * U * Q};
  ASSERT_IDENTICAL(A_in, A_out) << fmt::format("P = {}\nL={}\nU={}\nQ={}\n", P, L, U, Q);
}

MatrixExpr check_permutation_matrix(absl::Span<const int> permutation) {
  std::vector<Expr> elements(permutation.size() * permutation.size(), Constants::Zero);

  for (std::size_t row = 0; row < permutation.size(); ++row) {
    const int col_index = permutation[row];
    elements[row * permutation.size() + static_cast<std::size_t>(col_index)] = Constants::One;
  }
  const auto dim = static_cast<index_t>(permutation.size());
  return MatrixExpr::create(dim, dim, std::move(elements));
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
    MatrixExpr A = check_permutation_matrix(absl::Span<const int>(permutations_4[i]).subspan(0, 2));
    check_full_piv_lu_solution(A, factorize_full_piv_lu(A));
  }

  // dimension 3:
  for (std::size_t i = 0; i < 3; ++i) {
    MatrixExpr A = check_permutation_matrix(absl::Span<const int>(permutations_4[i]).subspan(0, 3));
    check_full_piv_lu_solution(A, factorize_full_piv_lu(A));
  }

  // dimension 4:
  for (std::size_t i = 0; i < permutations_4.size(); ++i) {
    MatrixExpr A = check_permutation_matrix(absl::Span<const int>(permutations_4[i]));
    check_full_piv_lu_solution(A, factorize_full_piv_lu(A));
  }

  // fully symbolic matrices:
  MatrixExpr A2 = make_matrix_of_symbols("x", 2, 2);
  check_full_piv_lu_solution(A2, factorize_full_piv_lu(A2));

  MatrixExpr A3 = make_matrix_of_symbols("x", 3, 3);
  check_full_piv_lu_solution(A3, factorize_full_piv_lu(A3));

  MatrixExpr A4 = make_matrix_of_symbols("x", 4, 4);
  check_full_piv_lu_solution(A4, factorize_full_piv_lu(A4));

  // zeros:
  MatrixExpr Z2 = make_zeros(2, 2);
  check_full_piv_lu_solution(Z2, factorize_full_piv_lu(Z2));

  MatrixExpr Z3 = make_zeros(3, 3);
  check_full_piv_lu_solution(Z3, factorize_full_piv_lu(Z3));
}

TEST(MatrixOperationsTest, TestFactorizeLU2) {
  // some non-square symbolic matrices:
  const std::vector<std::pair<index_t, index_t>> dims = {
      {2, 3}, {2, 4}, {2, 5}, {3, 4}, {3, 5}, {3, 6}, {4, 5}, {4, 6},
  };

  for (const auto [row, col] : dims) {
    MatrixExpr A = make_matrix_of_symbols("x", row, col);
    check_full_piv_lu_solution(A, factorize_full_piv_lu(A));
  }

  for (const auto [col, row] : dims) {
    // Transposed version:
    MatrixExpr A = make_matrix_of_symbols("x", row, col);
    check_full_piv_lu_solution(A, factorize_full_piv_lu(A));
  }

  MatrixExpr Z24 = make_zeros(2, 4);
  check_full_piv_lu_solution(Z24, factorize_full_piv_lu(Z24));

  MatrixExpr Z42 = make_zeros(4, 2);
  check_full_piv_lu_solution(Z42, factorize_full_piv_lu(Z42));

  MatrixExpr Z53 = make_zeros(5, 3);
  check_full_piv_lu_solution(Z53, factorize_full_piv_lu(Z53));
}

TEST(MatrixOperationsTest, TestFactorizeLU3) {
  // Some singular matrices that require full pivot:
  MatrixExpr A = make_matrix(3, 3, 0, 0, 0, 0, 0, "x", 0, 0, 0);
  check_full_piv_lu_solution(A, factorize_full_piv_lu(A));

  A = make_matrix(4, 4, 0, 0, 0, 0, 0, "y", 0, 0, 0, 0, 0, 0, 0, 0, 0, -5);
  check_full_piv_lu_solution(A, factorize_full_piv_lu(A));

  A = make_matrix(5, 3, 0, 0, 0, 0, "z", 0, 0, 0, 0, 0, 0, 0, 0, 0, 8);
  check_full_piv_lu_solution(A, factorize_full_piv_lu(A));
}

TEST(MatrixOperationsTest, TestFactorizeRandomLU) {
  // Test some random integer matrices to see if we can find failures.
  std::default_random_engine engine{1399};
  std::uniform_int_distribution<int> distribution{-10, 10};

  const std::vector<std::pair<index_t, index_t>> dims = {{2, 2}, {2, 3}, {3, 3}, {3, 4},
                                                         {5, 3}, {4, 4}, {4, 6}};

  for (const auto [rows, cols] : dims) {
    constexpr int num_trials = 25;
    for (int i = 0; i < num_trials; ++i) {
      std::vector<Expr> data{};
      data.reserve(rows * cols);
      for (int j = 0; j < rows * cols; ++j) {
        data.emplace_back(distribution(engine));
      }
      MatrixExpr M = MatrixExpr::create(rows, cols, std::move(data));
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

// TODO: Test the LU factors against the cofactor method?
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

}  // namespace math
