// Copyright 2023 Gareth Cross
#include "code_generation.h"
#include "test_helpers.h"

#include "test_expressions.h"

namespace math {

TEST(CodeGenerationTest, TestCreateIRConstant) {
  const auto [w, x] = Symbols("w", "x");
  {
    // A constant
    Expr f = 5;
    IrBuilder ir{{f}};
    ASSERT_IDENTICAL(f, ir.CreateExpressionForOutput(0)) << ir.ToString();
  }
  {
    // A constant
    IrBuilder ir{{Constants::Pi}};
    ASSERT_IDENTICAL(Constants::Pi, ir.CreateExpressionForOutput(0)) << ir.ToString();
  }
  {
    // Single variable
    IrBuilder ir{{x}};
    ASSERT_IDENTICAL(x, ir.CreateExpressionForOutput(0)) << ir.ToString();

    // Simplifying should have no effect
    ir.EliminateDuplicates();
    ASSERT_IDENTICAL(x, ir.CreateExpressionForOutput(0)) << ir.ToString();
  }
  {
    // Two outputs:
    IrBuilder ir{{x, w}};
    ASSERT_IDENTICAL(x, ir.CreateExpressionForOutput(0)) << ir.ToString();
    ASSERT_IDENTICAL(w, ir.CreateExpressionForOutput(1)) << ir.ToString();
  }
}

TEST(CodeGenerationTest, TestCreateIR1) {
  const auto [w, x, y, z] = Symbols("w", "x", "y", "z");
  IrBuilder ir{{w + x, y * z + 3}};
  ASSERT_EQ(3, ir.NumOperations());
  ASSERT_IDENTICAL(w + x, ir.CreateExpressionForOutput(0)) << ir.ToString();
  ASSERT_IDENTICAL(y * z + 3, ir.CreateExpressionForOutput(1)) << ir.ToString();
}

TEST(CodeGenerationTest, TestCreateIR2) {
  const auto [w, x, y, z] = Symbols("w", "x", "y", "z");
  const std::vector<Expr> expressions = {w * x + cos(y) / 2, pow(z, 5) - w * tan(x)};
  IrBuilder ir{expressions};
  ASSERT_EQ(9, ir.NumOperations());
  ASSERT_IDENTICAL(expressions[0], ir.CreateExpressionForOutput(0)) << ir.ToString();
  ASSERT_IDENTICAL(expressions[1], ir.CreateExpressionForOutput(1)) << ir.ToString();
}

// Some expressions with duplication:
TEST(CodeGenerationTest, TestCreateIR3) {
  const auto [w, x, y, z] = Symbols("w", "x", "y", "z");
  const std::vector<Expr> expressions = {w * x * y, y * x * 5, (cos(x * y) + 3)};
  IrBuilder ir{expressions};
  ASSERT_EQ(7, ir.NumOperations());
  for (std::size_t i = 0; i < expressions.size(); ++i) {
    ASSERT_IDENTICAL(expressions[i], ir.CreateExpressionForOutput(i)) << ir.ToString();
  }

  // Eliminate duplicated `x * y`
  ir.EliminateDuplicates();

  ASSERT_EQ(5, ir.NumOperations());
  for (std::size_t i = 0; i < expressions.size(); ++i) {
    ASSERT_IDENTICAL(expressions[i], ir.CreateExpressionForOutput(i)) << ir.ToString();
  }
}

TEST(CodeGenerationTest, TestCreateIR4) {
  const auto [w, x, y, z] = Symbols("w", "x", "y", "z");

  // The two multiplied operands have several repeated elements:
  const Expr f = ((w + x) + y * (z - 2) / (x * w)) * ((w + x) + y * (z - 2) + 1) * 5;
  IrBuilder ir{{f}};
  ASSERT_EQ(15, ir.NumOperations());
  ASSERT_IDENTICAL(f, ir.CreateExpressionForOutput(0)) << ir.ToString();

  ir.EliminateDuplicates();
  ASSERT_EQ(12, ir.NumOperations());
  ASSERT_IDENTICAL(f, ir.CreateExpressionForOutput(0)) << ir.ToString();
}

TEST(CodeGenerationTest, TestCreateIR5) {
  const auto [x, y, theta] = Symbols("x", "y", "theta");

  ta::StaticMatrix<2, 1> D_theta = ta::StaticMatrix<2, 1>::Create(0, 0);
  MatrixExpr v = VectorRotation2D(theta, ta::StaticMatrix<2, 1>::Create(x, y), D_theta);

  // The two multiplied operands have several repeated elements:
  IrBuilder ir{{v[0], v[1], D_theta[0], D_theta[1]}};

  ASSERT_IDENTICAL(v[0], ir.CreateExpressionForOutput(0)) << ir.ToString();
  ASSERT_IDENTICAL(v[1], ir.CreateExpressionForOutput(1)) << ir.ToString();
  ASSERT_IDENTICAL(D_theta[0], ir.CreateExpressionForOutput(2)) << ir.ToString();
  ASSERT_IDENTICAL(D_theta[1], ir.CreateExpressionForOutput(3)) << ir.ToString();

  ir.EliminateDuplicates();
  ASSERT_IDENTICAL(v[0], ir.CreateExpressionForOutput(0)) << ir.ToString();
  ASSERT_IDENTICAL(v[1], ir.CreateExpressionForOutput(1)) << ir.ToString();
  ASSERT_IDENTICAL(D_theta[0], ir.CreateExpressionForOutput(2)) << ir.ToString();
  ASSERT_IDENTICAL(D_theta[1], ir.CreateExpressionForOutput(3)) << ir.ToString();
}

TEST(CodeGenerationTest, TestCreateIR6) {
  // Test a single conditional expression.
  const auto [x, eps] = Symbols("x", "eps");
  const auto f = where(pow(x, 2) > eps, sin(x) / x, 1 - pow(x, 2) / 6 + pow(x, 4) / 120);
  IrBuilder ir{{f}};
  ASSERT_IDENTICAL(f, ir.CreateExpressionForOutput(0)) << ir.ToString();
  ir.EliminateDuplicates();
  ASSERT_IDENTICAL(f, ir.CreateExpressionForOutput(0)) << ir.ToString();
}

TEST(CodeGenerationTest, TestCreateIR7) {
  // Test nested conditional expressions
  const auto [x, a, b, c, d] = Symbols("x", "a", "b", "c", "d");
  const auto l1 = a * x + b;
  const auto a1 = where(l1 > 0, l1, 0);
  const auto l2 = c * l1 + d;
  const auto a2 = where(l2 > 0, l2, l2 / 10);

  IrBuilder ir{{a1, a2}};
  ASSERT_IDENTICAL(a1, ir.CreateExpressionForOutput(0)) << ir.ToString();
  ASSERT_IDENTICAL(a2, ir.CreateExpressionForOutput(1)) << ir.ToString();
  ASSERT_EQ(21, ir.NumOperations());

  fmt::print("{}\n\n", ir.ToString());

  ir.EliminateDuplicates();
  ASSERT_IDENTICAL(a1, ir.CreateExpressionForOutput(0)) << ir.ToString();
  ASSERT_IDENTICAL(a2, ir.CreateExpressionForOutput(1)) << ir.ToString();
  ASSERT_EQ(9, ir.NumOperations());

  fmt::print("{}\n\n", ir.ToString());
}

TEST(CodeGenerationTest, TestCreateIR8) {
  const auto [x, y, a, b, c, d] = Symbols("x", "y", "a", "b", "c", "d");

//  const auto foo = where(x > 0, a + 2, b - 3);
//  const auto bar = sin(foo) * cos(foo);
//  const auto baz = (bar + y) * foo;
//  const auto buzz = bar * d;
//  const auto fizz = where(y < 0, baz, buzz);

  //  const auto f1 = where(x > 0, a * b, b - c);
  //  const auto f2 = where(y > 0, f1, 2 / d);

  //  const auto f1 = where(x > 0, a + 2, b - 3);
  //  const auto f2 = where(y <= 2, x * x, a + 2);
  //  const auto f3 = where(x > 0, c * d, a + 2);
  //  const auto f4 = where(y <= 2, f2, f1 * (a + 2));

  const auto f1 = where(x > 0, cos(x), sin(x));
  const auto f2 = 5 * f1;
  const auto f3 = where(x > 0, f2 - 5, f2 + 3);

  IrBuilder ir{{f3}};
  fmt::print("Num operations: {}\n", ir.NumOperations());
  fmt::print("Num conditional jumps: {}\n", ir.NumJumps());
  fmt::print("{}\n\n", ir.ToString());

  ASSERT_IDENTICAL(f3, ir.CreateExpressionForOutput(0));

  ir.EliminateDuplicates();

  fmt::print("Num operations (after): {}\n", ir.NumOperations());
  fmt::print("Num conditional jumps (after): {}\n", ir.NumJumps());
  fmt::print("{}\n\n", ir.ToString());

  //  ir.EliminateDuplicates();
  //  fmt::print("{}\n\n", ir.ToString());

  //    ir.TopologicallySort();
  //    ::print("{}\n\n", ir.ToString());

  //  ir.GroupConditionals();
  //  fmt::print("{}\n\n", ir.ToString());
}

}  // namespace math
