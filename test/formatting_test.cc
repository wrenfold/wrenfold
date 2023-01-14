#include <fmt/format.h>

#include "constants.h"
#include "functions.h"
#include "test_helpers.h"
#include "tree_formatter.h"

namespace math {

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
  ASSERT_STR_EQ("x + y + w", x + y + w);
  ASSERT_STR_EQ("x + 2", x + 2_s);
  ASSERT_STR_EQ("w + 1.5 + y", w + 1.5_s + y);
  ASSERT_STR_EQ("x + w", x + 0_s + w);
  ASSERT_STR_EQ("x + y - x", x + y - x);
  ASSERT_STR_EQ("x - y", x - y);
}

TEST(PlainFormatterTest, TestMultiplication) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_STR_EQ("x * y", x * y);
  ASSERT_STR_EQ("x * z * y", x * z * y);
  ASSERT_STR_EQ("(x + y) * z", (x + y) * z);
  ASSERT_STR_EQ("(x + 3.14) * (z + x)", (x + 3.14_s) * (z + x));
  ASSERT_STR_EQ("x + y * z + x", x + y * z + x);
  ASSERT_STR_EQ("z + z * x * y", z + z * (x * y));
}

TEST(PlainFormatterTest, TestNegation) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_STR_EQ("-x", -x);
  ASSERT_STR_EQ("-(x + y)", -(x + y));
  ASSERT_STR_EQ("-x * y", -(x * y));
  ASSERT_STR_EQ("x / y", -x / -y);
}

TEST(PlainFormatterTest, TestDivision) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_STR_EQ("z / x", z / x);
  ASSERT_STR_EQ("x / (y * z)", (x / y) / z);
  ASSERT_STR_EQ("x * y / z", x * y / z);
  ASSERT_STR_EQ("(x + 1) / (y * z)", (x + 1_s) / (y * z));
  ASSERT_STR_EQ("(x + 1) * z / y", (x + 1_s) / y * z);
  ASSERT_STR_EQ("-(x + x) * y * x * x * x / x", -(x + x) * y / x * x * x * x);
  ASSERT_STR_EQ("-(x + x) * y / (x * x * x * x)", -(x + x) * y / (x * x * x * x));
  ASSERT_STR_EQ("-x * y * z * (z + x) / x", -x * y / x * z * (z + x));
  ASSERT_STR_EQ("z / (x + y)", z / (x + y));
  ASSERT_STR_EQ("-z / x + y", -z / x + y);
  ASSERT_STR_EQ("y / ln(x)", y / math::log(x));
  ASSERT_STR_EQ("y ^ x / (x ^ z * 5)", math::pow(y, x) / (math::pow(x, z) * 5_s));
}

TEST(PlainFormatterTest, TestPower) {
  const Expr x{"x"};
  const Expr y{"y"};
  const Expr z{"z"};
  ASSERT_STR_EQ("x ^ y", math::pow(x, y));
  ASSERT_STR_EQ("8", math::pow(2_s, 3_s));
  ASSERT_STR_EQ("(x + y) ^ z", math::pow(x + y, z));
  ASSERT_STR_EQ("x ^ (z - y)", math::pow(x, z - y));
  ASSERT_STR_EQ("(y * x) ^ z", math::pow(y * x, z));
  ASSERT_STR_EQ("(y * x) ^ (z / y)", math::pow(y * x, z / y));
  ASSERT_STR_EQ("(y * x - y) ^ (z + y + x)", math::pow(y * x - y, z + y + x));
  ASSERT_STR_EQ("y ^ (x ^ z)", math::pow(y, math::pow(x, z)));
  ASSERT_STR_EQ("(x ^ (y - x)) ^ (x ^ z)", math::pow(math::pow(x, y - x), math::pow(x, z)));
}

TEST(PlainFormatterTest, TestLog) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_STR_EQ("ln(x)", math::log(x));
  ASSERT_STR_EQ("ln(x * y)", math::log(x * y));
  ASSERT_STR_EQ("-x * ln(x / y)", -x * math::log(x / y));
}

}  // namespace math
