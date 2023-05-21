// Copyright 2023 Gareth Cross
#include <filesystem>
#include <fstream>

#include <fmt/format.h>

#include "code_generation.h"
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
  const std::vector<Expr>& expressions = std::get<1>(tuple);

  std::vector<ExpressionGroup> groups{};
  groups.reserve(signature.return_values.size() + signature.output_args.size());

  std::size_t index = 0;
  for (const std::shared_ptr<const ast::Argument>& arg : signature.output_args) {
    ASSERT_LESS(index + arg->TypeDimension(), expressions.size());
    // Create an expression group for every argument:
    std::vector<Expr> group_expressions{expressions.begin() + index,
                                        expressions.begin() + index + arg->TypeDimension()};
    groups.emplace_back(std::move(group_expressions), arg->IsOptional()
                                                          ? ExpressionGroupUsage::OptionalOutputArg
                                                          : ExpressionGroupUsage::OutputArg);
    index += arg->TypeDimension();
  }

  IrBuilder ir{groups};
  ir.EliminateDuplicates();
  auto ast = ir.CreateAST(signature);

  // Create function definition:
  ast::FunctionDefinition definition{std::move(signature), std::move(ast)};

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

  std::ofstream output{"generated.h"};
  output << code;
  output.flush();
  return 0;
}
