#include "constants.h"
#include "functions.h"
#include "test_helpers.h"

// Test derivatives of scalar functions.
namespace math {

TEST(ScalarDerivativesTest, TestConstants) {
  const Expr x{"x"};
  ASSERT_IDENTICAL(0, Expr{5}.Diff(x));
  ASSERT_IDENTICAL(0, Expr{22.6}.Diff(x, 4));
  ASSERT_IDENTICAL(0, Constants::Pi.Diff(x));
  ASSERT_THROW(x.Diff(5), std::runtime_error);
}

TEST(ScalarDerivativesTest, TestAdditionAndSubtraction) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(0, (x + y).Diff(w));
  ASSERT_IDENTICAL(0, (x - y).Diff(w, 2));
  ASSERT_IDENTICAL(1, (x + y).Diff(y));
  ASSERT_IDENTICAL(-Expr{1}, (x - y).Diff(y));
  ASSERT_IDENTICAL(0, (x - y).Diff(y, 2));
  ASSERT_IDENTICAL(Constants::One + Constants::One, (x + y + x).Diff(x));
}

TEST(ScalarDerivativesTest, TestMultiplication) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(0, (x * y).Diff(w));
  ASSERT_IDENTICAL(y, (x * y).Diff(x));
  ASSERT_IDENTICAL(x, (x * y).Diff(y));
  ASSERT_IDENTICAL(3, (x * 3).Diff(x));
  ASSERT_IDENTICAL(0, (x * y).Diff(x, 2));
  ASSERT_IDENTICAL(x + x, (x * x).Diff(x));
  ASSERT_IDENTICAL(Constants::One + Constants::One, (x * x).Diff(x, 2));
  // Diff() does not simplify these yet:
  ASSERT_IDENTICAL(x * x + (x + x) * x, (x * x * x).Diff(x));
  ASSERT_IDENTICAL(x * y + y * x, (x * y * x).Diff(x));
  ASSERT_IDENTICAL(y + y, (x * y * x).Diff(x, 2));
}

TEST(ScalarDerivativesTest, TestDivision) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(y / (y * y), (x / y).Diff(x));
  ASSERT_IDENTICAL(-y / (x * x), (y / x).Diff(x));
  ASSERT_IDENTICAL(-(x + x) * -y / (x * x * x * x), (y / x).Diff(x, 2));
//  ASSERT_IDENTICAL(y / x, (y / x).Diff(w));
}

TEST(ScalarDerivativesTest, TestPower) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
}

TEST(ScalarDerivativesTest, TestFunctions) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
}

}  // namespace math
