#include "constants.h"
#include "fmt_imports.h"
#include "functions.h"
#include "matrix_functions.h"
#include "test_helpers.h"

namespace math {
using namespace math::custom_literals;

TEST(PlainFormatterTest, TestAdditionAndSubtraction) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_STR_EQ("w + x + y", x + y + w);
  ASSERT_STR_EQ("2 + x", x + 2);
  ASSERT_STR_EQ("-10 + y", y - 10);
  ASSERT_STR_EQ("5 - 10 / 3 * w", 5 - 10 / 3_s * w);
  ASSERT_STR_EQ("-5 - x", -5 - x);
  ASSERT_STR_EQ("14 - 3 * x", 14 - 3 * x);
  ASSERT_STR_EQ("1.5 + w + y", w + 1.5 + y);
  ASSERT_STR_EQ("w + x", x + 0 + w);
  ASSERT_STR_EQ("y", x + y - x);
  ASSERT_STR_EQ("x - y", x - y);
  ASSERT_STR_EQ("5 / 6 - x + y", -x + y + 5 / 6_s);
  ASSERT_STR_EQ("-w - x + y", -w + -x + y);
}

TEST(PlainFormatterTest, TestMultiplication) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_STR_EQ("x * y", x * y);
  ASSERT_STR_EQ("x * y * z", x * z * y);
  ASSERT_STR_EQ("z * (x + y)", (x + y) * z);
  ASSERT_STR_EQ("(3.14 + x) * (x + z)", (x + 3.14_s) * (z + x));
  ASSERT_STR_EQ("2 * x + y * z", x + y * z + x);
  ASSERT_STR_EQ("z + x * y * z", z + z * (x * y));
  ASSERT_STR_EQ("-6 + 2 * w + x", x + 2 * (w - 3));
  ASSERT_STR_EQ("-5 / 3 * x - 5 / 3 * y + x * z", x * z - 5_s / 3 * (x + y));
  ASSERT_STR_EQ("5 - 2 * y", -2 * y + 5);
  ASSERT_STR_EQ("-x + 2 * y", 2 * y - x);

  // Negations:
  ASSERT_STR_EQ("-x", -x);
  ASSERT_STR_EQ("-x - y", -(x + y));
  ASSERT_STR_EQ("-x * y", -(x * y));

  // Test division:
  ASSERT_STR_EQ("z / x", z / x);
  ASSERT_STR_EQ("x / (y * z)", (x / y) / z);
  ASSERT_STR_EQ("x * y / z", x * y / z);
  ASSERT_STR_EQ("(1 + x) / (y * z)", (x + 1) / (y * z));
  ASSERT_STR_EQ("z * (1 + x) / y", (x + 1) / y * z);
  ASSERT_STR_EQ("-2 * x ** 3 * y", -2 * y * x * x * x);
  ASSERT_STR_EQ("-2 * y / x ** 3", -(x + x) * y / (x * x * x * x));
  ASSERT_STR_EQ("-y * z * (x + z) / x", -y / x * z * (z + x));
  ASSERT_STR_EQ("z / (x + y)", z / (x + y));
  ASSERT_STR_EQ("y - z / x", -z / x + y);
  ASSERT_STR_EQ("-z / (x * y)", -z / (x * y));

  // Divisions including rationals:
  ASSERT_STR_EQ("5 * x / 7", (5 / 7_s) * x);
  ASSERT_STR_EQ("-6 * x / (11 * z)", (6 / 11_s) * x / -z);
  ASSERT_STR_EQ("2 ** (1 / 3) * 10 * x / 7", (5 / 7_s) * pow(2, 4 / 3_s) * x);
  ASSERT_STR_EQ("2 * x / (3 * y)", (-2 / 3_s) * x / -y);

  // Multiplications involving powers:
  ASSERT_STR_EQ("y ** x / (5 * x ** z)", pow(y, x) / (pow(x, z) * 5));
  ASSERT_STR_EQ("y ** (3 * x / 5) / (x ** 5.22 * z ** (2 * w))",
                pow(y, 3 / 5_s * x) * pow(z, -2 * w) * pow(x, -5.22_s));
}

TEST(PlainFormatterTest, TestPower) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_STR_EQ("x ** y", pow(x, y));
  ASSERT_STR_EQ("8", pow(2, 3));
  ASSERT_STR_EQ("(x + y) ** z", pow(x + y, z));
  ASSERT_STR_EQ("x ** (-y + z)", pow(x, z - y));
  ASSERT_STR_EQ("x ** z * y ** z", pow(y * x, z));
  ASSERT_STR_EQ("x ** (z / y) * y ** (z / y)", pow(y * x, z / y));
  ASSERT_STR_EQ("(-y + x * y) ** (x + y + z)", pow(y * x - y, z + y + x));
  ASSERT_STR_EQ("y ** (x ** z)", pow(y, pow(x, z)));
  ASSERT_STR_EQ("(x ** (-x + y)) ** (x ** (-3 * z / 7))", pow(pow(x, y - x), pow(x, 3 * z / -7_s)));
  ASSERT_STR_EQ("-5 ** (2 / 3) * 7 ** (2 / 3) + x - y * z",
                pow(5 * 7, 2 / 3_s) * Constants::NegativeOne + x - y * z);
}

TEST(PlainFormatterTest, TestRelationals) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  ASSERT_STR_EQ("x < y", x < y);
  ASSERT_STR_EQ("x <= y", x <= y);
  ASSERT_STR_EQ("x == y", x == y);
  ASSERT_STR_EQ("y < x", x > y);
  ASSERT_STR_EQ("y <= x", x >= y);

  // Test precedence:
  ASSERT_STR_EQ("x * y < 5", x * y < 5);
  ASSERT_STR_EQ("sin(y) < 2 * x + z", z + 2 * x > sin(y));
  ASSERT_STR_EQ("x < (z < y)", x < (z < y));
  ASSERT_STR_EQ("x < (y <= z)", x < (z >= y));
}

TEST(PlainFormatterTest, TestConditionals) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  ASSERT_STR_EQ("where(0 < x, 2 * y, cos(z))", where(x > 0, y * 2, cos(z)));

  // TODO: This output is a bit gross for min/max. Maybe automatically re-write?
  ASSERT_STR_EQ("where(x < y, y, x)", max(x, y));
  ASSERT_STR_EQ("where(where(x < y, x, y) < y, y, where(x < y, x, y))", max(min(y, x), y));
}

TEST(PlainFormatterTest, TestBuiltInFunctions) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_STR_EQ("ln(x)", log(x));
  ASSERT_STR_EQ("ln(x * y)", log(x * y));
  ASSERT_STR_EQ("-x * ln(x / y)", -x * log(x / y));
  ASSERT_STR_EQ("cos(x)", cos(x));
  ASSERT_STR_EQ("sin(cos(y))", sin(cos(y)));
  ASSERT_STR_EQ("atan(x * y)", atan(x * y));
  ASSERT_STR_EQ("acos(-5 * y)", acos(y * -5));
  ASSERT_STR_EQ("acos(x) * asin(y)", acos(x) * asin(y));
  ASSERT_STR_EQ("atan2(y, x)", atan2(y, x));
  ASSERT_STR_EQ("atan2(y + cos(x), 6 - y)", atan2(y + cos(x), 6 - y));
  ASSERT_STR_EQ("abs(x)", abs(x));
  ASSERT_STR_EQ("abs(1 + x - sin(y))", abs(x + 1 - sin(y)));
  ASSERT_STR_EQ("signum(3 * y)", signum(3 * y));
}

TEST(PlainFormatterTest, TestMatrix) {
  const Expr a{"a"};
  const Expr b{"b"};
  const Expr c{"c"};
  const Expr d{"d"};
  ASSERT_STR_EQ("[[a, b],\n [c, d]]", make_matrix(2, 2, a, b, c, d));
  ASSERT_STR_EQ("[[2 * a, b - c],\n [    c, 3 * d]]", make_matrix(2, 2, a * 2, b - c, c, d * 3));
  ASSERT_STR_EQ("[[a],\n [b],\n [c]]", make_vector(a, b, c));
  ASSERT_STR_EQ("[[-3 + a],\n [     b],\n [cos(c)]]", make_vector(a - 3, b, cos(c)));
  ASSERT_STR_EQ("[[2, a * b * c, sin(d)]]", make_row_vector(2, a * b * c, sin(d)));
}

TEST(PlainFormatterTest, TestDerivativeExpression) {
  const Expr a{"a"};
  ASSERT_STR_EQ("Derivative(signum(a), a)", signum(a).diff(a));
  ASSERT_STR_EQ("Derivative(signum(a), a, 2)", signum(a).diff(a, 2));
}

TEST(PlainFormatterTest, TestComplexInfinity) { ASSERT_STR_EQ("zoo", Constants::ComplexInfinity); }

TEST(PlainFormatterTest, TestUndefined) { ASSERT_STR_EQ("nan", Constants::Undefined); }

}  // namespace math