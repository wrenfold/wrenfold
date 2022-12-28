#include "test_helpers.h"

#include <fmt/format.h>

#include "formatting.h"

namespace math {

testing::AssertionResult IdenticalTestHelper(const std::string& name_a, const std::string& name_b,
                                             const Expr& a, const Expr& b) {
  if (a.IsIdenticalTo(b)) {
    return testing::AssertionSuccess();
  }
  return testing::AssertionFailure() << fmt::format(
             "{} is not identical to {}, where:\n{} = {}\nand {} = {}\n"
             "expression tree for `{}`:\n{}\n"
             "expression tree for `{}`:\n{}",
             name_a, name_b, name_a, a.ToString(), name_b, b.ToString(), name_a, FormatDebugTree(a),
             name_b, FormatDebugTree(b));
}

testing::AssertionResult NotIdenticalTestHelper(const std::string& name_a,
                                                const std::string& name_b, const Expr& a,
                                                const Expr& b) {
  if (!a.IsIdenticalTo(b)) {
    return testing::AssertionSuccess();
  }
  return testing::AssertionFailure() << fmt::format(
             "{} is identical to {}, where:\n{} = {}\nand {} = {}\n"
             "expression tree for `{}`:\n{}\n"
             "expression tree for `{}`:\n{}",
             name_a, name_b, name_a, a.ToString(), name_b, b.ToString(), name_a, FormatDebugTree(a),
             name_b, FormatDebugTree(b));
}

}  // namespace math
