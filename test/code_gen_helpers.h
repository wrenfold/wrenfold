// Copyright 2023 Gareth Cross
#pragma once

#include "code_generation/ir_builder.h"
#include "fmt_imports.h"
#include "function_evaluator.h"
#include "type_annotations.h"

namespace math {

template <typename CodeGenerator, typename Func, typename... Args>
void GenerateFunc(CodeGenerator&& generator, std::string& output, Func&& func,
                  const std::string_view name, Args&&... args) {
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
  std::vector<ast::Variant> body = ast::create_ast(output_ir, signature);

  const std::string code = generator.generate_code(signature, body);
  fmt::format_to(std::back_inserter(output), "{}\n\n", code);
}

}  // namespace math
