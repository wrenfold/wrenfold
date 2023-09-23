// Copyright 2023 Gareth Cross
#include <filesystem>
#include <fstream>

#include <fmt/format.h>
#include <cstdio>
#include <iostream>

#include "code_generation/ir_builder.h"
#include "cpp_code_generator.h"
#include "function_evaluator.h"
#include "type_annotations.h"

#include "test_expressions.h"

namespace math {

namespace ta = type_annotations;

template <typename Func, typename... Args>
void GenerateFunc(std::string& output, Func&& func, const std::string_view name, Args&&... args) {
  auto tuple =
      BuildFunctionDescription(std::forward<Func>(func), name, std::forward<Args>(args)...);
  const ast::FunctionSignature& signature = std::get<0>(tuple);
  const std::vector<ExpressionGroup>& expressions = std::get<1>(tuple);

  FlatIr ir{expressions};
  ir.EliminateDuplicates();

  OutputIr output_ir{std::move(ir)};
#if 0
  fmt::print("IR ({}):\n{}\n", name, output_ir.ToString());
  std::fflush(nullptr);
#endif

  // Generate syntax tree:
  ast::FunctionDefinition ast = ast::CreateAST(output_ir, signature);

  CppCodeGenerator generator{};
  const std::string code = generator.Generate(ast);
  fmt::format_to(std::back_inserter(output), "{}\n\n", code);
}

}  // namespace math

int main() {
  using namespace math;
  std::string code = "// Machine generated code.\n#include <cmath>\n\n#include <span.h>\n\n";

  code += "namespace gen {\n\n";

  GenerateFunc(code, &SimpleMultiplyAdd, "simple_multiply_add", "x", "y", "z");
  GenerateFunc(
      code,
      [](Expr theta, ta::StaticMatrix<2, 1> v) {
        auto [v_rot, v_rot_D_theta] = VectorRotation2D(theta, v);
        return std::make_tuple(v_rot.ToOutputArg("v_rot"), std::move(v_rot_D_theta));
      },
      "vector_rotation_2d", "theta", "v");
  GenerateFunc(code, &VectorNorm3D, "vector_norm_3d", "v");
  GenerateFunc(code, &Heaviside, "heaviside", Arg("x"));
  GenerateFunc(code, &ExclusiveOr, "exclusive_or", Arg("x"), Arg("y"));
  GenerateFunc(code, &HandwrittenSignum, "handwritten_signum", Arg("x"));
  GenerateFunc(code, &HandwrittenAbs, "handwritten_abs", Arg("x"));
  GenerateFunc(code, &Atan2WithDerivatives, "atan2_with_derivatives", Arg("y"), Arg("x"));

  code += "\n\n} // namespace gen";

  std::ofstream output{"generated.h"};
  output << code;
  output.flush();
  return 0;
}
