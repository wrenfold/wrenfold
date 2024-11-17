// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/constants.h"
#include "wf/expressions/function_expressions.h"
#include "wf/expressions/substitute_expression.h"
#include "wf/functions.h"

#include "wf_test_support/test_macros.h"

#define ASSERT_TREE_STR_EQ(val1, val2) \
  ASSERT_PRED_FORMAT2(wf::tree_formatter_test_helper, val1, val2)

namespace wf {
using namespace wf::custom_literals;

template <typename ExprType>
testing::AssertionResult tree_formatter_test_helper(const std::string_view,
                                                    const std::string_view name_b,
                                                    const std::string_view a, const ExprType& b) {
  const std::string b_str = b.to_expression_tree_string();
  if (a == b_str) {
    return testing::AssertionSuccess();
  }
  return testing::AssertionFailure() << fmt::format(
             "String `{}` does not match ({}).to_expression_tree_string(), "
             "where:\n({}).to_expression_tree_string() is:\n"
             "{}\n",
             escape_newlines(a), name_b, name_b, b_str);
}

TEST(TreeFormatterTest, TestAddition) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  ASSERT_TREE_STR_EQ(R"(Addition:
├─ Variable (x, unknown)
├─ Variable (y, unknown)
└─ Variable (z, unknown))",
                     x + y + z);
}

TEST(TreeFormatterTest, TestBuiltInFunctionInvocation) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_TREE_STR_EQ(R"(BuiltInFunctionInvocation (asin):
└─ Multiplication:
   ├─ Variable (x, unknown)
   └─ Variable (y, unknown))",
                     asin(x * y));
  ASSERT_TREE_STR_EQ(R"(BuiltInFunctionInvocation (sign):
└─ Variable (y, unknown))",
                     signum(y));
}

TEST(TreeFormatterTest, TestConstants) {
  ASSERT_TREE_STR_EQ("BooleanConstant (true)", constants::boolean_true);
  ASSERT_TREE_STR_EQ("BooleanConstant (false)", constants::boolean_false);
  ASSERT_TREE_STR_EQ("ComplexInfinity", constants::complex_infinity);
  ASSERT_TREE_STR_EQ("Undefined", constants::undefined);
  ASSERT_TREE_STR_EQ("ImaginaryUnit", constants::imaginary_unit);
  ASSERT_TREE_STR_EQ("IntegerConstant (1)", 1_s);
  ASSERT_TREE_STR_EQ("RationalConstant (2 / 3)", 2_s / 3);
  ASSERT_TREE_STR_EQ("FloatConstant (1.32)", 1.32_s);
  ASSERT_TREE_STR_EQ("SymbolicConstant (pi)", constants::pi);
  ASSERT_TREE_STR_EQ("SymbolicConstant (E)", constants::euler);
}

TEST(TreeFormatterTest, TestConditional) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  ASSERT_TREE_STR_EQ(R"(Conditional:
├─ Relational (<)
│  ├─ IntegerConstant (0)
│  └─ Variable (x, unknown)
├─ Variable (y, unknown)
└─ Variable (z, unknown))",
                     where(x > 0, y, z));
}

TEST(TreeFormatterTest, TestDerivative) {
  const auto [x, y] = make_symbols("x", "y");
  const symbolic_function f{"foo"};
  ASSERT_TREE_STR_EQ(R"(Derivative (order = 2):
├─ SymbolicFunctionInvocation (foo):
│  ├─ Variable (x, unknown)
│  └─ Variable (y, unknown)
└─ Variable (y, unknown))",
                     f(x, y).diff(y, 2));
}

TEST(TreeFormatterTest, TestIverson) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_TREE_STR_EQ(R"(IversonBracket:
└─ Relational (==)
   ├─ Variable (x, unknown)
   └─ Variable (y, unknown))",
                     iverson(x == y));
}

TEST(TreeFormatterTest, TestMatrix) {
  ASSERT_TREE_STR_EQ(R"(Matrix (2, 3):
├─ IntegerConstant (1)
├─ IntegerConstant (0)
├─ IntegerConstant (0)
├─ IntegerConstant (0)
├─ IntegerConstant (1)
└─ IntegerConstant (0))",
                     make_identity(2, 3));
}

TEST(TreeFormatterTest, TestMultiplication) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  ASSERT_TREE_STR_EQ(R"(Multiplication:
├─ Variable (x, unknown)
├─ Variable (y, unknown)
└─ Variable (z, unknown))",
                     x * y * z);
}

TEST(TreeFormatterTest, TestPower) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_TREE_STR_EQ(R"(Power:
├─ Addition:
│  ├─ Variable (x, unknown)
│  └─ Variable (y, unknown)
└─ Multiplication:
   ├─ IntegerConstant (2)
   └─ Variable (y, unknown))",
                     pow(x + y, 2 * y));
}

TEST(TreeFormatterTest, TestStopDerivative) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_TREE_STR_EQ(R"(StopDerivative:
└─ Addition:
   ├─ Variable (x, unknown)
   └─ Multiplication:
      ├─ IntegerConstant (-1)
      └─ Variable (y, unknown))",
                     stop_diff(x - y));
}

TEST(TreeFormatterTest, TestSubstitution) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_TREE_STR_EQ(R"(Substitution:
├─ Power:
│  ├─ Variable (x, unknown)
│  └─ IntegerConstant (2)
├─ Variable (x, unknown)
└─ Variable (y, unknown))",
                     substitution::create(x * x, x, y));
}

TEST(TreeFormatterTest, TestUnevaluated) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_TREE_STR_EQ(R"(Unevaluated:
└─ Multiplication:
   ├─ IntegerConstant (5)
   ├─ Variable (x, unknown)
   └─ Variable (y, unknown))",
                     make_unevaluated(x * 5 * y));
}

}  // namespace wf
