#include "constants.h"
#include "functions.h"
#include "operations_inline.h"
#include "test_helpers.h"

// Test derivatives of scalar functions.
namespace math {

TEST(ScalarDerivativesTest, TestConstants) {
  const Expr x{"x"};
  ASSERT_IDENTICAL(Constants::Zero, (5_s).Diff(x));
  ASSERT_IDENTICAL(Constants::Zero, (22.5_s).Diff(x, 4));
  ASSERT_IDENTICAL(Constants::Zero, Constants::Pi.Diff(x));
  ASSERT_THROW(x.Diff(5_s), std::runtime_error);
}

TEST(ScalarDerivativesTest, TestAdditionAndSubtraction) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(Constants::Zero, (x + y).Diff(w));
  ASSERT_IDENTICAL(Constants::Zero, (x - y).Diff(w, 2));
  ASSERT_IDENTICAL(Constants::One, (x + y).Diff(y));
  ASSERT_IDENTICAL(-1_s, (x - y).Diff(y));
  ASSERT_IDENTICAL(Constants::Zero, (x - y).Diff(y, 2));
  ASSERT_IDENTICAL(Constants::One + Constants::One, (x + y + x).Diff(x));
}

TEST(ScalarDerivativesTest, TestMultiplication) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(Constants::Zero, (x * y).Diff(w));
  ASSERT_IDENTICAL(y, (x * y).Diff(x));
  ASSERT_IDENTICAL(x, (x * y).Diff(y));
  ASSERT_IDENTICAL(3_s, (x * 3_s).Diff(x));
  ASSERT_IDENTICAL(Constants::Zero, (x * y).Diff(x, 2));
  ASSERT_IDENTICAL(x + x, (x * x).Diff(x));
  ASSERT_IDENTICAL(Constants::One + Constants::One, (x * x).Diff(x, 2));
  // Diff() does not simplify these yet:
  ASSERT_IDENTICAL((x * x) + (x * x) + (x * x), (x * x * x).Diff(x));
  ASSERT_IDENTICAL(y * x + x * y, (x * y * x).Diff(x));
  ASSERT_IDENTICAL(y + y, (x * y * x).Diff(x, 2));
}

TEST(ScalarDerivativesTest, TestDivision) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(1_s / y, (x / y).Diff(x));
  ASSERT_IDENTICAL(-y / (x * x), (y / x).Diff(x));
  ASSERT_IDENTICAL(-((x + x) * -y) / ((x * x) * (x * x)), (y / x).Diff(x, 2));
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
