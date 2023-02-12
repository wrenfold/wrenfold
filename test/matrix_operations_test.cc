// Copyright 2023 Gareth Cross
#include "error_types.h"
#include "matrix_functions.h"
#include "test_helpers.h"

namespace math {
using namespace custom_literals;

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
  ASSERT_IDENTICAL(v.Transpose().Transpose(), v);

  // Invalid access:
  ASSERT_THROW(v[4], DimensionError);
  ASSERT_THROW(v(10, 2), DimensionError);

  // Compare to row-vector constructor
  ASSERT_IDENTICAL(v.Transpose(), RowVector(x, 4, y + z));

  // Construct matrix:
  const MatrixExpr m = CreateMatrix(2, 3, x + y, 0, 5 - z, z * y, x, 2 / x);
  ASSERT_EQ(2, m.NumRows());
  ASSERT_EQ(3, m.NumCols());
  ASSERT_IDENTICAL(x + y, m(0, 0));  //  first row
  ASSERT_IDENTICAL(0, m(0, 1));
  ASSERT_IDENTICAL(5 - z, m(0, 2));
  ASSERT_IDENTICAL(y * z, m(1, 0));  //  second row
  ASSERT_IDENTICAL(x, m(1, 1));
  ASSERT_IDENTICAL(2 / x, m(1, 2));
  ASSERT_IDENTICAL(m.Transpose().Transpose(), m);

  // Invalid access:
  ASSERT_THROW(m(5, 0), DimensionError);
  ASSERT_THROW(m(1, 10), DimensionError);

  // Cannot make a matrix from sub-matrices:
  ASSERT_THROW(RowVector(2.0, m), TypeError);
}

TEST(MatrixOperationsTest, TestAddition) {
  const Expr a{"a"};
  const Expr b{"b"};
  const Expr c{"c"};
  const Expr d{"d"};
  ASSERT_IDENTICAL(Vector(a + c, b - d, b * 2 + 5), Vector(a, -d, b) + Vector(c, b, b + 5));
  ASSERT_IDENTICAL(Vector(0, 0, 0), Vector(a, b, c) + Vector(-a, -b, -c));
  ASSERT_IDENTICAL(Vector(a, b) + RowVector(c, d).Transpose(), Vector(a + c, b + d));
  ASSERT_THROW(Vector(a, b) + RowVector(c, d), DimensionError);
  ASSERT_THROW(Vector(a, b) + CreateMatrix(2, 2, c, d, 2, -3_s / 5), DimensionError);

  const MatrixExpr m = CreateMatrix(2, 2, a, b, c, d);
  ASSERT_IDENTICAL(m, m + Zeros(2, 2));
  ASSERT_IDENTICAL(CreateMatrix(2, 2, a + 1, b, c, d + 1), m + Identity(2));
  ASSERT_IDENTICAL(CreateMatrix(2, 2, 2 * a, b + c, c + b, d + d), m + m.Transpose());
}

TEST(MatrixOperationsTest, TestMultiplication) {
  const Expr a{"a"};
  const Expr b{"b"};
  const Expr c{"c"};
  const Expr d{"d"};

  ASSERT_IDENTICAL(Identity(4), Identity(4) * Identity(4));
  ASSERT_IDENTICAL(Vector(a, b, c, d), Identity(4) * Vector(a, b, c, d));

  // Reduction to scalar:
  ASSERT_IDENTICAL(a * c + b * d, RowVector(a, b) * Vector(c, d));

  //
}

}  // namespace math
