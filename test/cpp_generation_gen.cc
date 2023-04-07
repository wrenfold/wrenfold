// Copyright 2023 Gareth Cross
#include <filesystem>

#include <fmt/format.h>
#include <fmt/os.h>

#include "code_generation.h"
#include "cpp_code_generator.h"
#include "function_evaluator.h"
#include "type_annotations.h"

#include "test_expressions.h"

namespace math {

namespace ta = type_annotations;

template <typename Func, typename... Args>
void GenerateFunc(std::string& output, Func func, const std::string_view name, Args&&... args) {
  auto [desc, expressions] = BuildFunctionDescription(func, name, std::forward<Args>(args)...);

  IrBuilder ir{expressions};
  ir.EliminateDuplicates();
  auto ast = ir.CreateAST(desc);

  // Create function definition:
  ast::FunctionDefinition definition{std::move(desc), std::move(ast)};

  CppCodeGenerator generator{};
  const std::string code = generator.Generate(definition);
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

  code += "\n\n} // namespace gen";
  fmt::output_file("generated.h").print("{}", code);
  return 0;
}
