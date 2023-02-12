// Copyright 2023 Gareth Cross
#include "error_types.h"
#include "functions.h"
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

  // Dimension mismatch:
  ASSERT_THROW(Vector(a, b) + RowVector(c, d), DimensionError);
  ASSERT_THROW(Vector(a, b) + CreateMatrix(2, 2, c, d, 2, -3_s / 5), DimensionError);

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
  ASSERT_IDENTICAL(CreateMatrix(2, 2, a * x + b * z, a * y + b * w, c * x + d * z, c * y + d * w),
                   CreateMatrix(2, 2, a, b, c, d) * CreateMatrix(2, 2, x, y, z, w));

  // Outer product:
  ASSERT_IDENTICAL(CreateMatrix(2, 2, x * a, x * b, y * a, y * b), Vector(x, y) * RowVector(a, b));

  // Distribute scalars into matrix:
  ASSERT_IDENTICAL(CreateMatrix(2, 2, x + 5, 0, 0, x + 5), Identity(2) * (x + 5));

  // Reduction to scalar:
  ASSERT_IDENTICAL(a * c + b * d, RowVector(a, b) * Vector(c, d));
  ASSERT_IDENTICAL(a * c * x + b * d * x, (RowVector(a, b) * Vector(c, d) * x).Distribute());

  // Inner dimension mismatch
  ASSERT_THROW(Zeros(2, 3) * Zeros(4, 2), DimensionError);

  // Cannot divide by matrix.
  ASSERT_THROW(1 / Vector(1, 2, w), TypeError);
}

}  // namespace math
