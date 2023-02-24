// Copyright 2023 Gareth Cross
#include "code_generation.h"
#include "test_helpers.h"

#include <fmt/format.h>

namespace math {

TEST(CodeGenerationTest, TestSSA1) {
  const auto [w, x, y, z] = Symbols("w", "x", "y", "z");
  const Expr f = ((w + x) + y * (z - 2)) * ((w + x) + z * (z - 2)) * 5;

  std::vector<ssa::AssignmentVariant> output{};
  CreateSSA(f, output);
  fmt::print("{}\n", FormatSSA(output));

  const Expr f_rebuilt = ExpressionFromSSA(output);
  ASSERT_IDENTICAL(f, f_rebuilt);

  fmt::print("{}\n", f_rebuilt.ToString());
}

}  // namespace math
