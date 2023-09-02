// Copyright 2023 Gareth Cross
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
  const MatrixExpr v = Vector(x, 4, y + z);
  ASSERT_EQ(v.NumRows(), 3);
  ASSERT_EQ(v.NumCols(), 1);
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
  ASSERT_IDENTICAL(v.Transpose(), RowVector(x, 4, y + z));

  // Construct matrix:
  // clang-format off
  const MatrixExpr m = CreateMatrix(2, 3,
                                    x + y, 0, 5 - z,
                                    z * y, x, 2 / x);
  // clang-format on
  ASSERT_EQ(2, m.NumRows());
  ASSERT_EQ(3, m.NumCols());
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

  // Cannot make a matrix from sub-matrices:
  ASSERT_THROW(RowVector(2.0, m), TypeError);
  ASSERT_THROW(CreateMatrix(1, 1, m), TypeError);
}

TEST(MatrixOperationsTest, TestGetBlock) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  const Expr a{"a"};
  const Expr b{"b"};
  const Expr c{"c"};
  // clang-format off
  const MatrixExpr m1 = CreateMatrix(3, 2,
                                     x, a,
                                     y, b,
                                     z, c);
  // clang-format on
  ASSERT_IDENTICAL(Vector(x, y, z), m1.GetBlock(0, 0, 3, 1));
  ASSERT_IDENTICAL(Vector(y, z), m1.GetBlock(1, 0, 2, 1));
  ASSERT_IDENTICAL(Vector(a, b), m1.GetBlock(0, 1, 2, 1));
  ASSERT_IDENTICAL(RowVector(y, b), m1.GetBlock(1, 0, 1, 2));
  ASSERT_IDENTICAL(RowVector(y), m1.GetBlock(1, 0, 1, 1));
  ASSERT_IDENTICAL(CreateMatrix(2, 2, y, b, z, c), m1.GetBlock(1, 0, 2, 2));

  // clang-format off
  const MatrixExpr m2 = CreateMatrix(3, 4,
                                     x, Constants::Pi, 2, x - z,
                                     3.0, -5, y, pow(z, 2),
                                     2 * z, Constants::Euler, -1, sin(x));
  // clang-format on
  ASSERT_IDENTICAL(RowVector(x, Constants::Pi, 2, x - z), m2.GetBlock(0, 0, 1, 4));
  ASSERT_IDENTICAL(Vector(x, 3.0, 2 * z), m2.GetBlock(0, 0, 3, 1));
  ASSERT_IDENTICAL(CreateMatrix(2, 2, y, pow(z, 2), -1, sin(x)), m2.GetBlock(1, 2, 2, 2));

  // bounds checking:
  ASSERT_THROW(m2.GetBlock(-1, 0, 2, 1), DimensionError);
  ASSERT_THROW(m2.GetBlock(1, -5, 1, 1), DimensionError);
  ASSERT_THROW(m2.GetBlock(1, 1, 4, 1), DimensionError);
  ASSERT_THROW(m2.GetBlock(1, 1, 2, 5), DimensionError);
}

TEST(MatrixOperationsTest, TestTranspose) {
  const Expr a{"a"};
  const Expr b{"b"};
  const Expr c{"c"};
  // clang-format off
  const MatrixExpr m = CreateMatrix(2, 5,
                                    cos(a), sin(b), -1, Constants::Pi * 3, 0.0,
                                    tan(c), c*a, 4, 5.0, a);
  // clang-format on
  const MatrixExpr m_t = m.Transpose();
  ASSERT_EQ(5, m_t.NumRows());
  ASSERT_EQ(2, m_t.NumCols());
  for (int i = 0; i < m_t.NumRows(); ++i) {
    for (int j = 0; j < m_t.NumCols(); ++j) {
      ASSERT_IDENTICAL(m_t(i, j), m(j, i));
    }
  }
  ASSERT_IDENTICAL(m, m.Transpose().Transpose());
}

TEST(MatrixOperationsTest, TestVec) {
  const Expr a{"a"};
  const Expr b{"b"};
  const Expr c{"c"};
  const Expr d{"d"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(Vector(a, b, c), Vec(RowVector(a, b, c)));
  ASSERT_IDENTICAL(Vector(a, c, b, d, x, y), Vec(CreateMatrix(2, 3, a, b, x, c, d, y)));
}

TEST(MatrixOperationsTest, TestAddition) {
  const Expr a{"a"};
  const Expr b{"b"};
  const Expr c{"c"};
  const Expr d{"d"};
  ASSERT_IDENTICAL(Vector(a + c, b - d, b * 2 + 5), Vector(a, -d, b) + Vector(c, b, b + 5));
  ASSERT_IDENTICAL(Vector(0, 0, 0), Vector(a, b, c) + Vector(-a, -b, -c));
  ASSERT_IDENTICAL(Vector(a, b) + RowVector(c, d).Transpose(), Vector(a + c, b + d));

  // Dimension mismatch:
  ASSERT_THROW(Vector(a, b) + RowVector(c, d), DimensionError);
  ASSERT_THROW(Vector(a, b) + CreateMatrix(2, 2, c, d, 2, -3_s / 5), DimensionError);
  ASSERT_THROW(static_cast<Expr>(RowVector(c, d)) + a, TypeError);
  ASSERT_THROW(static_cast<Expr>(RowVector(c, d)) - d, TypeError);

  const MatrixExpr m = CreateMatrix(2, 2, a, b, c, d);
  ASSERT_IDENTICAL(m, m + Zeros(2, 2));
  ASSERT_IDENTICAL(CreateMatrix(2, 2, a + 1, b, c, d + 1), m + Identity(2));
  ASSERT_IDENTICAL(CreateMatrix(2, 2, 2 * a, b + c, c + b, d + d), m + m.Transpose());
  ASSERT_IDENTICAL(Zeros(2, 2), CreateMatrix(2, 2, a, b, c, d) - CreateMatrix(2, 2, a, b, c, d));
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

  ASSERT_IDENTICAL(Identity(4), Identity(4) * Identity(4));
  ASSERT_IDENTICAL(Vector(a, b, c, d), Identity(4) * Vector(a, b, c, d));
  ASSERT_IDENTICAL(Vector(0, 0), Zeros(2, 4) * Vector(a, b, c, d));

  ASSERT_IDENTICAL(Vector(a * x + 2 * b * y + 2 * (c - 3), d * a * x - cos(d) * y + c * 2),
                   CreateMatrix(2, 3, a, -2 * b, c - 3, d * a, cos(d), c) * Vector(x, -y, 2));
  ASSERT_IDENTICAL(CreateMatrix(2, 2, a * c, 0, 0, b * d),
                   CreateMatrix(2, 2, a, 0, 0, b) * CreateMatrix(2, 2, c, 0, 0, d));

  // clang-format off
  ASSERT_IDENTICAL(CreateMatrix(2, 2,
                                a * x + b * z, a * y + b * w,
                                c * x + d * z, c * y + d * w),
                   CreateMatrix(2, 2, a, b, c, d) * CreateMatrix(2, 2, x, y, z, w));
  // clang-format on

  // Outer product:
  ASSERT_IDENTICAL(CreateMatrix(2, 2, x * a, x * b, y * a, y * b), Vector(x, y) * RowVector(a, b));

  // Distribute scalars into matrix:
  ASSERT_IDENTICAL(CreateMatrix(2, 2, x + 5, 0, 0, x + 5), Identity(2) * (x + 5));
  ASSERT_IDENTICAL(CreateMatrix(2, 3, sin(x) * 3, z * 3, 9, cos(y) * 3, 2, 0),
                   CreateMatrix(2, 3, sin(x), z, 3, cos(y), 2_s / 3, 0) * 3);

  // Reduction to scalar:
  ASSERT_IDENTICAL(a * c + b * d, RowVector(a, b) * Vector(c, d));
  ASSERT_IDENTICAL(a * c * x + b * d * x, (RowVector(a, b) * Vector(c, d) * x).Distribute());

  // Inner dimension mismatch
  ASSERT_THROW(Zeros(2, 3) * Zeros(4, 2), DimensionError);
  ASSERT_THROW(Zeros(10, 10) * Vector(x, y, z), DimensionError);
  ASSERT_THROW(RowVector(1, -3, z) * Vector(z, sin(y)), DimensionError);

  // Cannot divide by matrix.
  ASSERT_THROW(1 / static_cast<Expr>(Vector(1, 2, w)), TypeError);
}

}  // namespace math
