// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/constants.h"
#include "wf/expressions/substitute_expression.h"
#include "wf/functions.h"
#include "wf/matrix_functions.h"

#include "wf_test_support/test_macros.h"

namespace wf {
using namespace wf::custom_literals;

TEST(PlainFormatterTest, TestAdditionAndSubtraction) {
  const scalar_expr w{"w"};
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  ASSERT_STR_EQ("w + x + y", x + y + w);
  ASSERT_STR_EQ("2 + x", x + 2);
  ASSERT_STR_EQ("-10 + y", y - 10);
  ASSERT_STR_EQ("5 - 10*w/3", 5 - 10 / 3_s * w);
  ASSERT_STR_EQ("-5 - x", -5 - x);
  ASSERT_STR_EQ("14 - 3*x", 14 - 3 * x);
  ASSERT_STR_EQ("1.5 + w + y", w + 1.5 + y);
  ASSERT_STR_EQ("w + x", x + 0 + w);
  ASSERT_STR_EQ("y", x + y - x);
  ASSERT_STR_EQ("x - y", x - y);
  ASSERT_STR_EQ("5/6 - x + y", -x + y + 5 / 6_s);
  ASSERT_STR_EQ("-w - x + y", -w + -x + y);
  ASSERT_STR_EQ("x + I*y", x + constants::imaginary_unit * y);
}

TEST(PlainFormatterTest, TestMultiplication) {
  const scalar_expr w{"w"};
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  const scalar_expr z{"z"};
  ASSERT_STR_EQ("x*y", x * y);
  ASSERT_STR_EQ("x*y*z", x * z * y);
  ASSERT_STR_EQ("z*(x + y)", (x + y) * z);
  ASSERT_STR_EQ("(3.14 + x)*(x + z)", (x + 3.14_s) * (z + x));
  ASSERT_STR_EQ("2*x + y*z", x + y * z + x);
  ASSERT_STR_EQ("z + x*y*z", z + z * (x * y));
  ASSERT_STR_EQ("-6 + 2*w + x", x + 2 * (w - 3));
  ASSERT_STR_EQ("-5*x/3 - 5*y/3 + x*z", x * z - 5_s / 3 * (x + y));
  ASSERT_STR_EQ("5 - 2*y", -2 * y + 5);
  ASSERT_STR_EQ("-x + 2*y", 2 * y - x);

  // Negations:
  ASSERT_STR_EQ("-x", -x);
  ASSERT_STR_EQ("-x - y", -(x + y));
  ASSERT_STR_EQ("-x*y", -(x * y));

  // Test division:
  ASSERT_STR_EQ("z/x", z / x);
  ASSERT_STR_EQ("5/y", 5 / y);
  ASSERT_STR_EQ("x/(y*z)", (x / y) / z);
  ASSERT_STR_EQ("x*y/z", x * y / z);
  ASSERT_STR_EQ("(1 + x)/(y*z)", (x + 1) / (y * z));
  ASSERT_STR_EQ("z*(1 + x)/y", (x + 1) / y * z);
  ASSERT_STR_EQ("-2*x**3*y", -2 * y * x * x * x);
  ASSERT_STR_EQ("-2*y/x**3", -(x + x) * y / (x * x * x * x));
  ASSERT_STR_EQ("-y*z*(x + z)/x", -y / x * z * (z + x));
  ASSERT_STR_EQ("z/(x + y)", z / (x + y));
  ASSERT_STR_EQ("y - z/x", -z / x + y);
  ASSERT_STR_EQ("-z/(x*y)", -z / (x * y));

  // Divisions including rationals:
  ASSERT_STR_EQ("5*x/7", (5 / 7_s) * x);
  ASSERT_STR_EQ("-6*x/(11*z)", (6 / 11_s) * x / -z);
  ASSERT_STR_EQ("2**(1/3)*10*x/7", (5 / 7_s) * pow(2, 4 / 3_s) * x);
  ASSERT_STR_EQ("2*x/(3*y)", (-2 / 3_s) * x / -y);

  // Multiplications involving powers:
  ASSERT_STR_EQ("y**x/(5*x**z)", pow(y, x) / (pow(x, z) * 5));
  ASSERT_STR_EQ("y**(3*x/5)/(x**5.22*z**(2*w))",
                pow(y, 3 / 5_s * x) * pow(z, -2 * w) * pow(x, -5.22_s));
}

TEST(PlainFormatterTest, TestPower) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  ASSERT_STR_EQ("x**y", pow(x, y));
  ASSERT_STR_EQ("8", pow(2, 3));
  ASSERT_STR_EQ("1/y", pow(y, -1));
  ASSERT_STR_EQ("5/y", 5 * pow(y, -1));
  ASSERT_STR_EQ("(x + y)**z", pow(x + y, z));
  ASSERT_STR_EQ("x**(-y + z)", pow(x, z - y));
  ASSERT_STR_EQ("(-x)**y", pow(-x, y));
  ASSERT_STR_EQ("(x*y)**z", pow(y * x, z));
  ASSERT_STR_EQ("(x*y)**(z/y)", pow(y * x, z / y));
  ASSERT_STR_EQ("(-y + x*y)**(x + y + z)", pow(y * x - y, z + y + x));
  ASSERT_STR_EQ("y**(x**z)", pow(y, pow(x, z)));
  ASSERT_STR_EQ("(x**(-x + y))**(x**(-3*z/7))", pow(pow(x, y - x), pow(x, 3 * z / -7_s)));
  ASSERT_STR_EQ("x - y*z - 35**(2/3)", pow(35, 2 / 3_s) * constants::negative_one + x - y * z);
  ASSERT_STR_EQ("-I**(3/2)", pow(constants::imaginary_unit, 7_s / 2));
  ASSERT_STR_EQ("1/(-3 + x + y)", pow(-3 + x + y, -1));

  // Make sure we add brackets when the base is a negative number:
  ASSERT_STR_EQ("(-3)**x", pow(-3, x));
  ASSERT_STR_EQ("(-3.12)**x", pow(-3.12, x));
  ASSERT_STR_EQ("(-4/5)**x", pow(-4_s / 5, x));
}

TEST(PlainFormatterTest, TestRelationals) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  ASSERT_STR_EQ("x < y", x < y);
  ASSERT_STR_EQ("x <= y", x <= y);
  ASSERT_STR_EQ("x == y", x == y);
  ASSERT_STR_EQ("y < x", x > y);
  ASSERT_STR_EQ("y <= x", x >= y);

  // Test precedence:
  ASSERT_STR_EQ("x*y < 5", x * y < 5);
  ASSERT_STR_EQ("sin(y) < 2*x + z", z + 2 * x > sin(y));
  ASSERT_STR_EQ("x < iverson(z < y)", x < iverson(z < y));
  ASSERT_STR_EQ("x < iverson(y <= z)", x < iverson(z >= y));
}

TEST(PlainFormatterTest, TestConditionals) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  ASSERT_STR_EQ("where(0 < x, 2*y, cos(z))", where(x > 0, y * 2, cos(z)));

  // TODO: This output is a bit gross for min/max. Maybe automatically re-write?
  ASSERT_STR_EQ("where(x < y, y, x)", max(x, y));
  ASSERT_STR_EQ("where(where(x < y, x, y) < y, y, where(x < y, x, y))", max(min(y, x), y));
}

TEST(PlainFormatterTest, TestBuiltInFunctions) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  ASSERT_STR_EQ("log(x)", log(x));
  ASSERT_STR_EQ("log(x*y)", log(x * y));
  ASSERT_STR_EQ("-x*log(x/y)", -x * log(x / y));
  ASSERT_STR_EQ("cos(x)", cos(x));
  ASSERT_STR_EQ("sin(cos(y))", sin(cos(y)));
  ASSERT_STR_EQ("atan(x*y)", atan(x * y));
  ASSERT_STR_EQ("acos(-5*y)", acos(y * -5));
  ASSERT_STR_EQ("acos(x)*asin(y)", acos(x) * asin(y));
  ASSERT_STR_EQ("atan2(y, x)", atan2(y, x));
  ASSERT_STR_EQ("atan2(y + cos(x), 6 - y)", atan2(y + cos(x), 6 - y));
  ASSERT_STR_EQ("abs(x)", abs(x));
  ASSERT_STR_EQ("abs(1 + x - sin(y))", abs(x + 1 - sin(y)));
  ASSERT_STR_EQ("sign(3*y)", signum(3 * y));
  ASSERT_STR_EQ("floor(x/y)", floor(x / y));
  ASSERT_STR_EQ("cosh(x + y)", cosh(x + y));
  ASSERT_STR_EQ("sinh(5*x)", sinh(5 * x));
  ASSERT_STR_EQ("tanh(x/y)", tanh(x / y));
  ASSERT_STR_EQ("acosh(x + y)", acosh(x + y));
  ASSERT_STR_EQ("asinh(5*x)", asinh(5 * x));
  ASSERT_STR_EQ("atanh(x/y)", atanh(x / y));
}

TEST(PlainFormatterTest, TestMatrix) {
  const auto [a, b, c, d] = make_symbols("a", "b", "c", "d");
  ASSERT_STR_EQ("[[a, b], [c, d]]", make_matrix(2, 2, a, b, c, d));
  ASSERT_STR_EQ("[[2*a, b - c], [c, 3*d]]", make_matrix(2, 2, a * 2, b - c, c, d * 3));
  ASSERT_STR_EQ("[[a], [b], [c]]", make_vector(a, b, c));
  ASSERT_STR_EQ("[[-3 + a], [b], [cos(c)]]", make_vector(a - 3, b, cos(c)));
  ASSERT_STR_EQ("[[2, a*b*c, sin(d)]]", make_row_vector(2, a * b * c, sin(d)));
}

TEST(PlainFormatterTest, TestDerivativeExpression) {
  const scalar_expr a{"a"};
  ASSERT_STR_EQ("Derivative(sign(a), a)",
                signum(a).diff(a, 1, non_differentiable_behavior::abstract));
  ASSERT_STR_EQ("Derivative(sign(a), a, 2)",
                signum(a).diff(a, 2, non_differentiable_behavior::abstract));
}

TEST(PlainFormatterTest, TestStopDerivative) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_STR_EQ("StopDerivative(x)", stop_diff(x));
  ASSERT_STR_EQ("StopDerivative(x*y)", stop_diff(x * y));
}

TEST(PlainFormatterTest, TestSubstitutionExpression) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_STR_EQ("Subs(x**2, x, 2 + y)", substitution::create(x * x, x, y + 2));
}

TEST(PlainFormatterTest, TestComplexInfinity) { ASSERT_STR_EQ("zoo", constants::complex_infinity); }

TEST(PlainFormatterTest, TestUndefined) { ASSERT_STR_EQ("nan", constants::undefined); }

TEST(PlainFormatterTest, TestUnevaluated) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_STR_EQ("(x*y)", make_unevaluated(x * y));
}

TEST(PlainFormatterTest, TestScalarConstants) {
  ASSERT_STR_EQ("pi", constants::pi);
  ASSERT_STR_EQ("E", constants::euler);
  ASSERT_STR_EQ("I", constants::imaginary_unit);
}

TEST(PlainFormatterTest, TestBooleanConstants) {
  ASSERT_STR_EQ("True", constants::boolean_true);
  ASSERT_STR_EQ("False", constants::boolean_false);
}

}  // namespace wf
