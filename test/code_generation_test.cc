// Copyright 2023 Gareth Cross
#include "code_generation.h"
#include "test_helpers.h"

#include <fmt/format.h>

namespace math {

TEST(CodeGenerationTest, TestSSA1) {
  const auto [w, x, y, z] = Symbols("w", "x", "y", "z");
  Expr f = ((w + x) + y * (z - 2)) * ((w + x) + y * (z - 2) + 1) * 5;

  fmt::print("input: {}\n", f.ToString());

//  std::vector<Expr> subs = IdentifySequenceCounts(f);
//  for (int i = 0; i < subs.size(); ++i) {
//    fmt::print("$temp{:03} <-- {}\n", i, subs[i].ToString());
//  }

  std::unordered_map<std::size_t, ssa::OperationVariant> output{};
  const std::size_t expr_value = CreateSSA(f, output);
  fmt::print("-- IR ({} operations):\n{}\n", output.size(), FormatSSA(output));

  const Expr f_rebuilt = ExpressionFromSSA(output, expr_value);
  ASSERT_IDENTICAL(f, f_rebuilt);

  EliminateDuplicates(output, {expr_value});
  fmt::print("-- Simplified ({} operations):\n{}\n", output.size(), FormatSSA(output));

  const Expr f_rebuilt_2 = ExpressionFromSSA(output, expr_value);
  //  fmt::print("\n{}\n", f_rebuilt_2.ToString());
}

}  // namespace math
