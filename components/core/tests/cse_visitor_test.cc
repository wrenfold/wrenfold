// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <unordered_set>

#include "wf/cse.h"
#include "wf/functions.h"
#include "wf/matrix_functions.h"

#include "wf_test_support/test_macros.h"

// Tests for the cse_visitor.
namespace wf {
using namespace wf::custom_literals;

scalar_expr make_var(std::size_t index) { return scalar_expr{fmt::format("v{}", index)}; }

void check_replacements(const absl::Span<const scalar_expr> expected,
                        const absl::Span<const std::tuple<scalar_expr, scalar_expr>> actual) {
  std::unordered_set<scalar_expr, hash_struct<scalar_expr>, is_identical_struct<scalar_expr>>
      actual_set{};
  actual_set.reserve(expected.size());
  std::transform(actual.begin(), actual.end(), std::inserter(actual_set, actual_set.end()),
                 [](const auto& tup) { return std::get<1>(tup); });

  ASSERT_EQ(expected.size(), actual.size()) << fmt::format(
      "expected = [{}], actual = [{}]", fmt::join(expected, ", "), fmt::join(actual_set, ", "));
  for (const scalar_expr& expected_expr : expected) {
    ASSERT_TRUE(actual_set.count(expected_expr))
        << fmt::format("Expression `{}` is not in the output set: [{}]", expected_expr,
                       fmt::join(actual_set, ", "));
  }
}

template <typename T>
auto apply_substitutions(
    T expr, const absl::Span<const std::tuple<scalar_expr, scalar_expr>> replacements) {
  // Need to apply in reverse order since v2 uses v1, which uses v0, etc.
  for (auto it = replacements.rbegin(); it != replacements.rend(); ++it) {
    expr = expr.subs(std::get<0>(*it), std::get<1>(*it));
  }
  return expr;
}

TEST(CseVisitorTest, Test1) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  // z*x and 3*y should get eliminated
  const scalar_expr f = z * x + 3 * y - cos(3 * y) * sin(z * x);

  const auto [output, replacements] = eliminate_subexpressions(f, make_var, 2);
  check_replacements({3 * y, z * x}, replacements);
  ASSERT_IDENTICAL(f, apply_substitutions(output, replacements));
}

TEST(CseVisitorTest, Test2) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  const scalar_expr f = cos(y) * 2 + 3 * sin(x) - log(z + cos(y) * 2) + abs(cos(y));

  // cos(y) is one sub-expression, which is referenced by another
  const auto [output, replacements] = eliminate_subexpressions(f, make_var, 2);
  check_replacements({make_var(0) * 2, cos(y)}, replacements);
  ASSERT_IDENTICAL(f, apply_substitutions(output, replacements));
}

TEST(CseVisitorTest, Test3) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  const scalar_expr f = (x * y + abs(z)) + cos(z / 2) / log(x * y) - pow(cos(z / 2), abs(z)) -
                        abs(cos(z / 2)) * pow(abs(z), z / 2) - sin(z / 2);

  // Replace only things occuring three times or more:
  const auto [output, replacements] = eliminate_subexpressions(f, make_var, 3);
  check_replacements({cos(make_var(1)), abs(z), z / 2}, replacements);
  ASSERT_IDENTICAL(f, apply_substitutions(output, replacements));
}

TEST(CseVisitorTest, Test4) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  // Construct an expression with two levels of substitution:
  const scalar_expr f = abs(cos(z) + 22 * x) - pow(z * 2, 3 * cos(z)) +
                        pow(abs(cos(z) + 22 * x), z) - z * (y + 22 * x) - acosh(cos(z) + 22 * x);

  const auto [output, replacements] = eliminate_subexpressions(f, make_var, 2);
  check_replacements({abs(make_var(2)), make_var(1) + make_var(0), cos(z), 22 * x}, replacements);
  ASSERT_IDENTICAL(f, apply_substitutions(output, replacements));
}

TEST(CseVisitorTest, Test5) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  // Test out a matrix:
  const matrix_expr v = make_vector(x * y, 4 + abs(x * y), cos(abs(x * y)));

  const auto [output, replacements] = eliminate_subexpressions(v, make_var, 2);
  check_replacements({x * y, abs(make_var(0))}, replacements);
  ASSERT_IDENTICAL(v, apply_substitutions(output, replacements));
}

TEST(CseVisitorTest, Test6) {
  const auto [x, y] = make_symbols("x", "y");

  // There are no replacements, everything appears as part of one expression:
  const scalar_expr f = cos(x) * cos(x) + y;

  const auto [output, replacements] = eliminate_subexpressions(f, make_var, 2);
  check_replacements({}, replacements);
  ASSERT_IDENTICAL(f, apply_substitutions(output, replacements));
}

}  // namespace wf
