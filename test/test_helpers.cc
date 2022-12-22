#include "test_helpers.h"

#include <fmt/format.h>

namespace math {

testing::AssertionResult IdenticalTestHelper(const std::string& name_a, const std::string& name_b,
                                             const Expr& a, const Expr& b) {
  if (a.IsIdenticalTo(b)) {
    return testing::AssertionSuccess();
  }
  return testing::AssertionFailure()
         << fmt::format("{} is not identical to {}, where:\n{} = {}\nand {} = {}\n", name_a, name_b,
                        name_a, a.ToString(), name_b, b.ToString());
}

testing::AssertionResult NotIdenticalTestHelper(const std::string& name_a,
                                                const std::string& name_b, const Expr& a,
                                                const Expr& b) {
  if (!a.IsIdenticalTo(b)) {
    return testing::AssertionSuccess();
  }
  return testing::AssertionFailure()
         << fmt::format("{} is identical to {}, where:\n{} = {}\nand {} = {}\n", name_a, name_b,
                        name_a, a.ToString(), name_b, b.ToString());
}

}  // namespace math