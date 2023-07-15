// Copyright 2023 Gareth Cross
#include "code_generation.h"
#include "test_helpers.h"

#include "test_expressions.h"

#include <random>

#include "cpp_code_generator.h"

namespace math {
using namespace math::custom_literals;

template <typename Func, typename... Args>
void GenerateFunc(std::string& output, Func func, const std::string_view name, Args&&... args) {
  auto tuple = BuildFunctionDescription(func, name, std::forward<Args>(args)...);
  ast::FunctionSignature& signature = std::get<0>(tuple);
  const std::vector<ExpressionGroup>& expressions = std::get<1>(tuple);

  IrBuilder ir{expressions};

  fmt::print("IR:\n{}\n\n\n", ir.ToString());

  fmt::print("Count: {}\n", ir.NumOperations());

//  ir.EliminateDuplicates();
//  ir.StripUnusedValues();
  ir.DropValues();
  fmt::print("After de-dup:\n{}\n\n\n", ir.ToString());
  fmt::print("Count: {}\n", ir.NumOperations());

  ir.ConvertTernaryConditionalsToJumps();

  const std::string ir_string = ir.ToString();
  fmt::print("IR:\n{}\n\n\n", ir_string);

  // Generate syntax tree:
  ast::FunctionDefinition ast = ir.CreateAST(signature);

  CppCodeGenerator generator{};
  const std::string code = generator.Generate(ast);
  fmt::format_to(std::back_inserter(output), "{}\n\n", code);
}

// TEST(CodeGenerationTest, TestCreateIRConstant) {
//   const auto [w, x] = Symbols("w", "x");
//   {
//     // A constant
//     Expr f = 5;
//     IrBuilder ir{{f}};
//     ASSERT_IDENTICAL(f, ir.CreateExpressionForOutput(0)) << ir.ToString();
//   }
//   {
//     // A constant
//     IrBuilder ir{{Constants::Pi}};
//     ASSERT_IDENTICAL(Constants::Pi, ir.CreateExpressionForOutput(0)) << ir.ToString();
//   }
//   {
//     // Single variable
//     IrBuilder ir{{x}};
//     ASSERT_IDENTICAL(x, ir.CreateExpressionForOutput(0)) << ir.ToString();
//
//     // Simplifying should have no effect
//     ir.EliminateDuplicates();
//     ASSERT_IDENTICAL(x, ir.CreateExpressionForOutput(0)) << ir.ToString();
//   }
//   {
//     // Two outputs:
//     IrBuilder ir{{x, w}};
//     ASSERT_IDENTICAL(x, ir.CreateExpressionForOutput(0)) << ir.ToString();
//     ASSERT_IDENTICAL(w, ir.CreateExpressionForOutput(1)) << ir.ToString();
//   }
// }
//
// TEST(CodeGenerationTest, TestCreateIR1) {
//   const auto [w, x, y, z] = Symbols("w", "x", "y", "z");
//   IrBuilder ir{{w + x, y * z + 3}};
//   ASSERT_EQ(3, ir.NumOperations());
//   ASSERT_IDENTICAL(w + x, ir.CreateExpressionForOutput(0)) << ir.ToString();
//   ASSERT_IDENTICAL(y * z + 3, ir.CreateExpressionForOutput(1)) << ir.ToString();
// }
//
// TEST(CodeGenerationTest, TestCreateIR2) {
//   const auto [w, x, y, z] = Symbols("w", "x", "y", "z");
//   const std::vector<Expr> expressions = {w * x + cos(y) / 2, pow(z, 5) - w * tan(x)};
//   IrBuilder ir{expressions};
//   ASSERT_EQ(9, ir.NumOperations());
//   ASSERT_IDENTICAL(expressions[0], ir.CreateExpressionForOutput(0)) << ir.ToString();
//   ASSERT_IDENTICAL(expressions[1], ir.CreateExpressionForOutput(1)) << ir.ToString();
// }
//
//// Some expressions with duplication:
// TEST(CodeGenerationTest, TestCreateIR3) {
//   const auto [w, x, y, z] = Symbols("w", "x", "y", "z");
//   const std::vector<Expr> expressions = {w * x * y, y * x * 5, (cos(x * y) + 3)};
//   IrBuilder ir{expressions};
//   ASSERT_EQ(7, ir.NumOperations());
//   for (std::size_t i = 0; i < expressions.size(); ++i) {
//     ASSERT_IDENTICAL(expressions[i], ir.CreateExpressionForOutput(i)) << ir.ToString();
//   }
//
//   // Eliminate duplicated `x * y`
//   ir.EliminateDuplicates();
//
//   ASSERT_EQ(5, ir.NumOperations());
//   for (std::size_t i = 0; i < expressions.size(); ++i) {
//     ASSERT_IDENTICAL(expressions[i], ir.CreateExpressionForOutput(i)) << ir.ToString();
//   }
// }
//
// TEST(CodeGenerationTest, TestCreateIR4) {
//   const auto [w, x, y, z] = Symbols("w", "x", "y", "z");
//
//   // The two multiplied operands have several repeated elements:
//   const Expr f = ((w + x) + y * (z - 2) / (x * w)) * ((w + x) + y * (z - 2) + 1) * 5;
//   IrBuilder ir{{f}};
//   ASSERT_EQ(15, ir.NumOperations());
//   ASSERT_IDENTICAL(f, ir.CreateExpressionForOutput(0)) << ir.ToString();
//
//   ir.EliminateDuplicates();
//   ASSERT_EQ(12, ir.NumOperations());
//   ASSERT_IDENTICAL(f, ir.CreateExpressionForOutput(0)) << ir.ToString();
// }
//
// TEST(CodeGenerationTest, TestCreateIR5) {
//   const auto [x, y, theta] = Symbols("x", "y", "theta");
//
//   ta::StaticMatrix<2, 1> D_theta = ta::StaticMatrix<2, 1>::Create(0, 0);
//   MatrixExpr v = VectorRotation2D(theta, ta::StaticMatrix<2, 1>::Create(x, y), D_theta);
//
//   // The two multiplied operands have several repeated elements:
//   IrBuilder ir{{v[0], v[1], D_theta[0], D_theta[1]}};
//
//   ASSERT_IDENTICAL(v[0], ir.CreateExpressionForOutput(0)) << ir.ToString();
//   ASSERT_IDENTICAL(v[1], ir.CreateExpressionForOutput(1)) << ir.ToString();
//   ASSERT_IDENTICAL(D_theta[0], ir.CreateExpressionForOutput(2)) << ir.ToString();
//   ASSERT_IDENTICAL(D_theta[1], ir.CreateExpressionForOutput(3)) << ir.ToString();
//
//   ir.EliminateDuplicates();
//   ASSERT_IDENTICAL(v[0], ir.CreateExpressionForOutput(0)) << ir.ToString();
//   ASSERT_IDENTICAL(v[1], ir.CreateExpressionForOutput(1)) << ir.ToString();
//   ASSERT_IDENTICAL(D_theta[0], ir.CreateExpressionForOutput(2)) << ir.ToString();
//   ASSERT_IDENTICAL(D_theta[1], ir.CreateExpressionForOutput(3)) << ir.ToString();
// }
//
// TEST(CodeGenerationTest, TestCreateIR6) {
//   // Test a single conditional expression.
//   const auto [x, eps] = Symbols("x", "eps");
//   const auto f = where(pow(x, 2) > eps, sin(x) / x, 1 - pow(x, 2) / 6 + pow(x, 4) / 120);
//   IrBuilder ir{{f}};
//   ASSERT_IDENTICAL(f, ir.CreateExpressionForOutput(0)) << ir.ToString();
//   ir.EliminateDuplicates();
//   ASSERT_IDENTICAL(f, ir.CreateExpressionForOutput(0)) << ir.ToString();
// }
//
// TEST(CodeGenerationTest, TestCreateIR7) {
//   // Test nested conditional expressions
//   const auto [x, a, b, c, d] = Symbols("x", "a", "b", "c", "d");
//   const auto l1 = a * x + b;
//   const auto a1 = where(l1 > 0, l1, 0);
//   const auto l2 = c * l1 + d;
//   const auto a2 = where(l2 > 0, l2, l2 / 10);
//
//   IrBuilder ir{{a1, a2}};
//   ASSERT_IDENTICAL(a1, ir.CreateExpressionForOutput(0)) << ir.ToString();
//   ASSERT_IDENTICAL(a2, ir.CreateExpressionForOutput(1)) << ir.ToString();
//   ASSERT_EQ(21, ir.NumOperations());
//
//   fmt::print("{}\n\n", ir.ToString());
//
//   ir.EliminateDuplicates();
//   ASSERT_IDENTICAL(a1, ir.CreateExpressionForOutput(0)) << ir.ToString();
//   ASSERT_IDENTICAL(a2, ir.CreateExpressionForOutput(1)) << ir.ToString();
//   ASSERT_EQ(9, ir.NumOperations());
//
//   fmt::print("{}\n\n", ir.ToString());
// }

MatrixExpr Skew3(const ta::StaticMatrix<3, 1>& v) {
  // clang-format off
  return CreateMatrix(3, 3,
                            0, -v[2],  v[1],
                         v[2],     0, -v[0],
                        -v[1],  v[0],     0);
  // clang-format on
}

ta::StaticMatrix<3, 3> CreateRotationMatrix(const ta::StaticMatrix<3, 1>& w) {
  const MatrixExpr w_skew = Skew3(w);
  const Expr omega = sqrt(w[0] * w[0] + w[1] * w[1] + w[2] * w[2]);

  const Expr omega_sub{"omega_tmp"};
  Expr c0 = sin(omega_sub) / omega_sub;
  Expr c1 = (1 - cos(omega_sub)) / pow(omega_sub, 2);

  c0 = where(omega > 1.0e-6, c0, 1 - pow(omega_sub, 2) / 3).Subs(omega_sub, omega);
  c1 = where(omega > 1.0e-6, c1, 1_s / 2 - pow(omega_sub, 2) / 12).Subs(omega_sub, omega);

  return MatrixExpr{Identity(3).AsExpr() + (w_skew * c0) + (w_skew * w_skew * c1)};
}

inline std::vector<Expr> FlattenMatrix(const MatrixExpr& m) {
  std::vector<Expr> result{};
  result.reserve(m.Size());
  for (index_t row = 0; row < m.NumRows(); ++row) {
    for (index_t col = 0; col < m.NumCols(); ++col) {
      result.push_back(m(row, col));
    }
  }
  return result;
}

ta::StaticMatrix<2, 1> FooFunc(Expr x, Expr y, Expr z, Expr a, Expr b, Expr c, Expr d,
                               ta::StaticMatrix<2, 1>& f_out_1, Expr& f_out_2) {
  const auto f0 = where(y < 0, log(a), a * b * c);
  const auto f1 = where(x > 0, cos(x), sin(x) + f0);
  const auto f2 = 5 * f1;
  const auto f3 = where(x > 0, f2 - 5, f2 + 3);
  const auto f4 = where(y < 0, f2, f3);
  f_out_1 = CreateMatrix(2, 1, f4, f0 - 2 * d);
  f_out_2 = f0;
  return CreateMatrix(2, 1, f3, f3 * z);
}

TEST(CodeGenerationTest, TestCreateIR8) {
  const auto [x, y, z, a, b, c, d] = Symbols("x", "y", "z", "a", "b", "c", "d");

  MatrixExpr R1 = CreateRotationMatrix(CreateMatrix(3, 1, x, y, z));
  MatrixExpr R2 = CreateRotationMatrix(CreateMatrix(3, 1, a, b, c));

  MatrixExpr R_combo{R1 * R2};

  std::vector<Expr> diffs{};
  std::vector<Expr> diffs_2{};
  ta::StaticMatrix<9, 1> R_dot_x = Vec(R_combo).Diff(x);
  ta::StaticMatrix<9, 1> R_dot_y = Vec(R_combo).Diff(y);
  ta::StaticMatrix<9, 1> R_dot_z = Vec(R_combo).Diff(z);

  ta::StaticMatrix<9, 1> R_dot_a = Vec(R_combo).Diff(a);
  ta::StaticMatrix<9, 1> R_dot_b = Vec(R_combo).Diff(b);
  ta::StaticMatrix<9, 1> R_dot_c = Vec(R_combo).Diff(c);

  for (int i = 0; i < 9; ++i) {
    diffs.push_back(R_dot_x[i]);
  }
  for (int i = 0; i < 9; ++i) {
    diffs.push_back(R_dot_y[i]);
  }
  for (int i = 0; i < 9; ++i) {
    diffs.push_back(R_dot_z[i]);
  }

  for (int i = 0; i < 9; ++i) {
    diffs_2.push_back(R_dot_x[i]);
  }
  for (int i = 0; i < 9; ++i) {
    diffs_2.push_back(R_dot_y[i]);
  }
  for (int i = 0; i < 9; ++i) {
    diffs_2.push_back(R_dot_z[i]);
  }

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

  const auto f0 = where(y < 0, log(a), a * b * c);
  const auto f1 = where(x > 0, cos(x), sin(x) + f0);
  const auto f2 = 5 * f1;
  const auto f3 = where(x > 0, f2 - 5, f2 + 3);

  const auto f4 = where(y < 0, f2, f3);

  //  std::vector<ExpressionGroup> groups;
  //  groups.emplace_back(std::vector<Expr>{f3}, OutputKey{ExpressionUsage::ReturnValue, 0});
  //  groups.emplace_back(std::vector<Expr>{f4}, OutputKey{ExpressionUsage::OptionalOutputArgument,
  //  1}); groups.emplace_back(std::vector<Expr>{f0},
  //  OutputKey{ExpressionUsage::OptionalOutputArgument, 2});

  //  groups.emplace_back(FlattenMatrix(R_combo), false);
  //  groups.emplace_back(std::move(diffs), true);
  //  groups.emplace_back(std::move(diffs_2), true);

  //  IrBuilder ir{groups};
  //  fmt::print("Num operations: {}\n", ir.NumOperations());
  //  fmt::print("Num jumps: {}\n", ir.NumJumps());
  //  fmt::print("{}\n\n", ir.ToString());
  //
  //  //  std::vector<Expr> output_expr = ir.CreateOutputExpressions();
  //  //  ASSERT_IDENTICAL(f1, output_expr.front());
  //  //  ASSERT_IDENTICAL(exprs.back(), output_expr.back());
  //
  //  //  ir.CombineSequentialBlocks();
  //  ir.EliminateUnreachableBlocks();
  //  ir.EliminateDuplicates();
  //  ir.StripUnusedValues();
  //  //  ir.EliminateDuplicates();
  //  //  ir.StripUnusedValues();
  //
  //  fmt::print("-- after eliminating duplicates:\n");
  //  fmt::print("Num operations (after): {}\n", ir.NumOperations());
  //  fmt::print("Num jumps (after): {}\n", ir.NumJumps());
  //  fmt::print("{}\n\n", ir.ToString());
  //
  //  ir.ConvertTernaryConditionalsToJumps(true);
  //
  //  fmt::print("-- after re-ordering conditionals:\n");
  //  fmt::print("Num operations (after): {}\n", ir.NumOperations());
  //  fmt::print("Num jumps (after): {}\n", ir.NumJumps());
  //  fmt::print("{}\n\n", ir.ToString());
  //
  //  ir.ConvertTernaryConditionalsToJumps(false);

  //  ir.DropValues();
  //
  //  fmt::print("-- after making block:\n");
  //  fmt::print("Num operations (after): {}\n", ir.NumOperations());
  //  fmt::print("Num jumps (after): {}\n", ir.NumJumps());
  //  fmt::print("{}\n\n", ir.ToString());

  std::string output;
  GenerateFunc(output, &FooFunc, "foo_func", Arg("x"), Arg("y"), Arg("z"), Arg("a"), Arg("b"),
               Arg("c"), Arg("d"), Arg("f_out_1", true), Arg("f_out_2", true));

  GenerateFunc(output, &CreateRotationMatrix, "create_rotation_matrix", Arg("w"));

  fmt::print("output:\n{}\n", output);

  //  auto tokens = ir.CreateAST2();
}

}  // namespace math
