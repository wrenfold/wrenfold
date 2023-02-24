#include "constants.h"
#include "error_types.h"
#include "functions.h"
#include "matrix_functions.h"
#include "test_helpers.h"
#include "tree_formatter.h"

// Test `diff` operation.
namespace math {
using namespace math::custom_literals;

TEST(DerivativesTest, TestConstants) {
  const Expr x{"x"};
  ASSERT_IDENTICAL(Constants::Zero, (5_s).Diff(x));
  ASSERT_IDENTICAL(Constants::Zero, (22.5_s).Diff(x, 4));
  ASSERT_IDENTICAL(Constants::Zero, Constants::Pi.Diff(x));
  ASSERT_THROW(x.Diff(5_s), AssertionError);
}

TEST(DerivativesTest, TestAdditionAndSubtraction) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(Constants::Zero, (x + y).Diff(w));
  ASSERT_IDENTICAL(Constants::Zero, (x - y).Diff(w, 2));
  ASSERT_IDENTICAL(Constants::One, (x + y).Diff(y));
  ASSERT_IDENTICAL(-1_s, (x - y).Diff(y));
  ASSERT_IDENTICAL(-5_s / 7_s, ((x - y * 5_s) / 7_s).Diff(y));
  ASSERT_IDENTICAL(Constants::Zero, (x - y).Diff(y, 2));
  ASSERT_IDENTICAL(2_s, (x + y + x).Diff(x));
}

TEST(DerivativesTest, TestMultiplication) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(Constants::Zero, (x * y).Diff(w));
  ASSERT_IDENTICAL(y, (x * y).Diff(x));
  ASSERT_IDENTICAL(x, (x * y).Diff(y));
  ASSERT_IDENTICAL(3_s, (x * 3_s).Diff(x));
  ASSERT_IDENTICAL(Constants::Zero, (x * y).Diff(x, 2));
  ASSERT_IDENTICAL(2_s * x, (x * x).Diff(x));
  ASSERT_IDENTICAL(2_s, (x * x).Diff(x, 2));
  ASSERT_IDENTICAL(3_s * pow(x, 2_s), (x * x * x).Diff(x));
  ASSERT_IDENTICAL(2_s * x * y, (x * y * x).Diff(x));
  ASSERT_IDENTICAL(2_s * y, (x * y * x).Diff(x, 2));
  ASSERT_IDENTICAL(5_s * pow(y, 4_s), pow(y, 5_s).Diff(y));
  ASSERT_IDENTICAL(20_s * pow(y, 3_s), pow(y, 5_s).Diff(y, 2));
  ASSERT_IDENTICAL(1_s / y, (x / y).Diff(x));
  ASSERT_IDENTICAL(-y / (x * x), (y / x).Diff(x));
  ASSERT_IDENTICAL(2_s * y / pow(x, 3_s), (y / x).Diff(x, 2));
}

TEST(DerivativesTest, TestPower) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(Constants::Zero, pow(x, y).Diff(w));

  ASSERT_IDENTICAL(y * pow(x, y - 1_s), pow(x, y).Diff(x));
  ASSERT_IDENTICAL(y * (y - 1_s) * pow(x, y - 2_s), pow(x, y).Diff(x, 2));
  ASSERT_IDENTICAL(y * (y - 1_s) * (y - 2_s) * pow(x, y - 3_s), pow(x, y).Diff(x, 3));

  ASSERT_IDENTICAL(pow(y, w) * log(y), pow(y, w).Diff(w));
  ASSERT_IDENTICAL(pow(y, w) * pow(log(y), 2_s), pow(y, w).Diff(w, 2));
  ASSERT_IDENTICAL(pow(y, w) * pow(log(y), 3_s), pow(y, w).Diff(w, 3));
  ASSERT_IDENTICAL(pow(x, x) * log(x) + pow(x, x), pow(x, x).Diff(x));

  const Expr coeff = (5_s / 7_s) * pow(2_s, (5_s / 7_s) * x) * pow(x, (5_s / 7_s) * x);
  ASSERT_IDENTICAL((coeff * (1_s + log(2_s) + log(x))).Distribute(),
                   pow(x * 2_s, x * 5_s / 7_s).Diff(x).Distribute());
}

TEST(DerivativesTest, TestLog) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(1 / x, log(x).Diff(x));
  ASSERT_IDENTICAL(Constants::Zero, log(x).Diff(y));
  ASSERT_IDENTICAL(1 / y, log(x * y).Diff(y));
  ASSERT_IDENTICAL(1 / (2 * x), log(sqrt(x)).Diff(x));
}

TEST(DerivativesTest, TestTrig) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(cos(x), sin(x).Diff(x));
  ASSERT_IDENTICAL(-sin(x), cos(x).Diff(x));
  ASSERT_IDENTICAL(y * cos(x * y), sin(x * y).Diff(x));
  ASSERT_IDENTICAL(-2_s * sin(2_s * x + y), cos(2_s * x + y).Diff(x));
  ASSERT_IDENTICAL(1_s / pow(cos(x), 2_s), tan(x).Diff(x));
  ASSERT_IDENTICAL(-y / (x * x) / (cos(y / x) * cos(y / x)), tan(y / x).Diff(x));
}

TEST(DerivativesTest, TestInverseTrig) {
  const Expr x{"x"};
  const Expr y{"y"};

  ASSERT_IDENTICAL(Constants::Zero, acos(5_s).Diff(x));
  ASSERT_IDENTICAL(-1_s / sqrt(1_s - x * x), acos(x).Diff(x));
  ASSERT_IDENTICAL(-y / sqrt(1_s - x * x * y * y), acos(x * y).Diff(x));

  ASSERT_IDENTICAL(Constants::Zero, asin(y).Diff(x));
  ASSERT_IDENTICAL(1_s / sqrt(1_s - x * x), asin(x).Diff(x));
  ASSERT_IDENTICAL(sqrt(x) / sqrt(1_s - y * y * x), asin(y * sqrt(x)).Diff(y));

  ASSERT_IDENTICAL(Constants::Zero, atan(Constants::Euler).Diff(y));
  ASSERT_IDENTICAL(1_s / (x * x + 1), atan(x).Diff(x));
  ASSERT_IDENTICAL(3_s * (x * x) / (pow(x, 6_s) + 1_s), atan(pow(x, 3_s)).Diff(x));
}

TEST(DerivativesTest, TestMatrix) {
  // Matrix derivative should do element-wise differentiation.
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_IDENTICAL(Vector(0, 0, 0), Vector(x, y, 1).Diff(z));
  ASSERT_IDENTICAL(Vector(0, 1, 0), Vector(x, y, z).Diff(y));
  ASSERT_IDENTICAL(Vector(cos(y), 2 * x, -z / pow(x, 2)),
                   Vector(x * cos(y), y + x * x, z / x).Diff(x));
}

}  // namespace math
