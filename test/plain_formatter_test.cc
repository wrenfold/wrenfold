#include <fmt/format.h>

#include "constants.h"
#include "functions.h"
#include "test_helpers.h"
#include "tree_formatter.h"

namespace math {
using namespace math::custom_literals;

#define ASSERT_STR_EQ(val1, val2) ASSERT_PRED_FORMAT2(StringEqualTestHelper, val1, val2)

testing::AssertionResult StringEqualTestHelper(const std::string&, const std::string& name_b,
                                               const std::string& a, const Expr& b) {
  const std::string b_str = b.ToString();
  if (a == b_str) {
    return testing::AssertionSuccess();
  }
  return testing::AssertionFailure() << fmt::format(
             "String `{}` does not match ({}).ToString(), where:\n({}).ToString() = {}\n"
             "The expression tree for `{}` is:\n{}",
             a, name_b, name_b, b_str, name_b, FormatDebugTree(b));
}

TEST(PlainFormatterTest, TestAdditionAndSubtraction) {
  const Expr w{"w"};
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_STR_EQ("w + x + y", x + y + w);
  ASSERT_STR_EQ("2 + x", x + 2_s);
  ASSERT_STR_EQ("-10 + y", y - 10_s);
  ASSERT_STR_EQ("5 - 10 / 3 * w", 5_s - 10_s / 3_s * w);
  ASSERT_STR_EQ("-5 - x", -5_s - x);
  ASSERT_STR_EQ("14 - 3 * x", 14_s - 3_s * x);
  ASSERT_STR_EQ("1.5 + w + y", w + 1.5_s + y);
  ASSERT_STR_EQ("w + x", x + 0_s + w);
  ASSERT_STR_EQ("y", x + y - x);
  ASSERT_STR_EQ("x - y", x - y);
  ASSERT_STR_EQ("5 / 6 - x + y", -x + y + 5_s / 6_s);
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

  // Negations:
  ASSERT_STR_EQ("-x", -x);
  ASSERT_STR_EQ("-(x + y)", -(x + y));
  ASSERT_STR_EQ("-x * y", -(x * y));

  // Test division:
  ASSERT_STR_EQ("z / x", z / x);
  ASSERT_STR_EQ("x / (y * z)", (x / y) / z);
  ASSERT_STR_EQ("x * y / z", x * y / z);
  ASSERT_STR_EQ("(1 + x) / (y * z)", (x + 1_s) / (y * z));
  ASSERT_STR_EQ("z * (1 + x) / y", (x + 1_s) / y * z);
  ASSERT_STR_EQ("-2 * x ^ 3 * y", -2_s * y * x * x * x);
  ASSERT_STR_EQ("-2 * y / x ^ 3", -(x + x) * y / (x * x * x * x));
  ASSERT_STR_EQ("-y * z * (x + z) / x", -y / x * z * (z + x));
  ASSERT_STR_EQ("z / (x + y)", z / (x + y));
  ASSERT_STR_EQ("y - z / x", -z / x + y);
  ASSERT_STR_EQ("-z / (x * y)", -z / (x * y));

  // Divisions including rationals:
  ASSERT_STR_EQ("5 * x / 7", (5_s / 7_s) * x);
  ASSERT_STR_EQ("-6 * x / (11 * z)", (6_s / 11_s) * x / -z);
  ASSERT_STR_EQ("10 * 2 ^ (1 / 3) * x / 7", (5_s / 7_s) * pow(2_s, 4_s / 3_s) * x);
  ASSERT_STR_EQ("2 * x / (3 * y)", (-2_s / 3_s) * x / -y);

  // Multiplications involving powers:
  ASSERT_STR_EQ("y ^ x / (5 * x ^ z)", pow(y, x) / (pow(x, z) * 5_s));
  ASSERT_STR_EQ("y ^ (3 * x / 5) / (x ^ 5.22 * z ^ (2 * w))",
                pow(y, 3_s / 5_s * x) * pow(z, -2_s * w) * pow(x, -5.22_s));
}

TEST(PlainFormatterTest, TestPower) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_STR_EQ("x ^ y", pow(x, y));
  ASSERT_STR_EQ("8", pow(2_s, 3_s));
  ASSERT_STR_EQ("(x + y) ^ z", pow(x + y, z));
  ASSERT_STR_EQ("x ^ (-y + z)", pow(x, z - y));
  ASSERT_STR_EQ("x ^ z * y ^ z", pow(y * x, z));
  ASSERT_STR_EQ("x ^ (z / y) * y ^ (z / y)", pow(y * x, z / y));
  ASSERT_STR_EQ("(-y + x * y) ^ (x + y + z)", pow(y * x - y, z + y + x));
  ASSERT_STR_EQ("y ^ (x ^ z)", pow(y, pow(x, z)));
  ASSERT_STR_EQ("(x ^ (-x + y)) ^ (x ^ (-3 * z / 7))", pow(pow(x, y - x), pow(x, 3_s * z / -7_s)));
  ASSERT_STR_EQ("-5 ^ (2 / 3) * 7 ^ (2 / 3) + x - y * z",
                pow(5_s * 7_s, 2_s / 3_s) * Constants::NegativeOne + x - y * z);
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
  ASSERT_STR_EQ("acos(-5 * y)", acos(y * -5_s));
  ASSERT_STR_EQ("acos(x) * asin(y)", acos(x) * asin(y));
}

}  // namespace math
