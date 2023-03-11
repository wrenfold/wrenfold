// Copyright 2023 Gareth Cross
#include "code_generation.h"
#include "test_helpers.h"

namespace math {

TEST(CodeGenerationTest, TestCreateIRConstant) {
  const auto [w, x] = Symbols("w", "x");
  {
    // A constant
    Expr f = 5;
    IrBuilder ir{{f}};
    ASSERT_IDENTICAL(f, ir.CreateExpressionForOutput(0)) << ir.FormatIRForOutput(0);
  }
  {
    // A constant
    IrBuilder ir{{Constants::Pi}};
    ASSERT_IDENTICAL(Constants::Pi, ir.CreateExpressionForOutput(0)) << ir.FormatIRForOutput(0);
  }
  {
    // Single variable
    IrBuilder ir{{x}};
    ASSERT_IDENTICAL(x, ir.CreateExpressionForOutput(0)) << ir.FormatIRForOutput(0);

    // Simplifying should have no effect
    ir.EliminateDuplicates();
    ASSERT_IDENTICAL(x, ir.CreateExpressionForOutput(0)) << ir.FormatIRForOutput(0);
  }
  {
    // Two outputs:
    IrBuilder ir{{x, w}};
    ASSERT_IDENTICAL(x, ir.CreateExpressionForOutput(0)) << ir.FormatIRForOutput(0);
    ASSERT_IDENTICAL(w, ir.CreateExpressionForOutput(1)) << ir.FormatIRForOutput(1);
  }
}

TEST(CodeGenerationTest, TestCreateIR) {
  const auto [w, x, y, z] = Symbols("w", "x", "y", "z");

  {
    IrBuilder ir{{w + x, y * z + 3}};
    ASSERT_EQ(3, ir.NumOperations());
    ASSERT_IDENTICAL(w + x, ir.CreateExpressionForOutput(0)) << ir.FormatIRForOutput(0);
    ASSERT_IDENTICAL(y * z + 3, ir.CreateExpressionForOutput(1)) << ir.FormatIRForOutput(1);
  }

  {
    const std::vector<Expr> expressions = {w * x + cos(y) / 2, pow(z, 5) - w * tan(x)};
    IrBuilder ir{expressions};
    ASSERT_EQ(9, ir.NumOperations());
    ASSERT_IDENTICAL(expressions[0], ir.CreateExpressionForOutput(0)) << ir.FormatIRForOutput(0);
    ASSERT_IDENTICAL(expressions[1], ir.CreateExpressionForOutput(1)) << ir.FormatIRForOutput(1);
  }

  // Some expressions with duplication:
  {
    const std::vector<Expr> expressions = {w * x * y, y * x * 5, (cos(x * y) + 3)};
    IrBuilder ir{expressions};
    ASSERT_EQ(7, ir.NumOperations());
    for (std::size_t i = 0; i < expressions.size(); ++i) {
      ASSERT_IDENTICAL(expressions[i], ir.CreateExpressionForOutput(i)) << ir.FormatIRForOutput(i);
    }

    // Eliminate duplicated `x * y`
    ir.EliminateDuplicates();

    ASSERT_EQ(5, ir.NumOperations());
    for (std::size_t i = 0; i < expressions.size(); ++i) {
      ASSERT_IDENTICAL(expressions[i], ir.CreateExpressionForOutput(i)) << ir.FormatIRForOutput(i);
    }
  }

  // The two multiplied operands have several repeated elements:
  {
    const Expr f = ((w + x) + y * (z - 2) / (x * w)) * ((w + x) + y * (z - 2) + 1) * 5;
    IrBuilder ir{{f}};
    ASSERT_EQ(15, ir.NumOperations());
    ASSERT_IDENTICAL(f, ir.CreateExpressionForOutput(0)) << ir.FormatIRForOutput(0);

    ir.EliminateDuplicates();
    ASSERT_EQ(12, ir.NumOperations());
    ASSERT_IDENTICAL(f, ir.CreateExpressionForOutput(0)) << ir.FormatIRForOutput(0);
  }
}

}  // namespace math
