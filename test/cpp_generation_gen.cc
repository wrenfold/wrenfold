// Copyright 2023 Gareth Cross
#include <filesystem>
#include <fstream>

#include <fmt/format.h>

#include "code_generation/ir_builder.h"
#include "cpp_code_generator.h"
#include "function_evaluator.h"
#include "type_annotations.h"

#include "test_expressions.h"

namespace math {

namespace ta = type_annotations;

template <typename Func, typename... Args>
void GenerateFunc(std::string& output, Func func, const std::string_view name, Args&&... args) {
  auto tuple = BuildFunctionDescription(func, name, std::forward<Args>(args)...);
  ast::FunctionSignature& signature = std::get<0>(tuple);
  const std::vector<ExpressionGroup>& expressions = std::get<1>(tuple);

  IrBuilder ir{expressions};
  ir.EliminateDuplicates();
  ir.StripUnusedValues();
  ir.ConvertTernaryConditionalsToJumps();
  ir.DropValues();

  const std::string ir_string = ir.ToString();
  fmt::print("IR:\n{}\n\n\n", ir_string);

  // Generate syntax tree:
  ast::FunctionDefinition ast = ir.CreateAST(signature);

  CppCodeGenerator generator{};
  const std::string code = generator.Generate(ast);
  fmt::format_to(std::back_inserter(output), "{}\n\n", code);
}

}  // namespace math

int main() {
  using namespace math;
  std::string code =
      "// Machine generated code.\n#include <cmath>\n#include <optional>\n#include "
      "<tuple>\n\n#include "
      "<Eigen/Core>\n\n";

  code += "namespace gen {\n\n";

  GenerateFunc(code, &SimpleMultiplyAdd, "simple_multiply_add", "x", "y", "z");
  GenerateFunc(code, &VectorRotation2D, "vector_rotation_2d", "theta", "v", Arg("D_theta", true));
  GenerateFunc(code, &VectorNorm3D, "vector_norm_3d", "v", Arg("D_v", false));
  GenerateFunc(code, &Heaviside, "heaviside", Arg("x"));

  code += "\n\n} // namespace gen";

  std::ofstream output{"generated.h"};
  output << code;
  output.flush();
  return 0;
}
