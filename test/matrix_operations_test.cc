// Copyright 2022 Gareth Cross
#include "expressions/addition.h"
#include "expressions/multiplication.h"
#include "expressions/power.h"
#include "functions.h"
#include "test_helpers.h"

namespace math {

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
}

}  // namespace math
