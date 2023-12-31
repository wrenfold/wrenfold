// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/ast_conversion.h"
#include "wf/code_generation/function_evaluator.h"
#include "wf/code_generation/ir_builder.h"
#include "wf/fmt_imports.h"

namespace wf {

template <typename CodeGenerator, typename Func, typename... Args>
void generate_func(CodeGenerator&& generator, std::string& output, Func&& func,
                   const std::string_view name, Args&&... args) {
  const function_description description =
      build_function_description(std::forward<Func>(func), name, std::forward<Args>(args)...);

  flat_ir ir{description.output_expressions()};
  ir.eliminate_duplicates();
  const output_ir output_ir{std::move(ir)};
#if 0
  fmt::print("IR ({}, {} operations):\n{}\n", name, output_ir.num_operations(),
             output_ir.to_string());
  std::fflush(nullptr);
#endif

  // Generate syntax tree:
  const ast::function_definition definition = ast::create_ast(output_ir, description);

  // Convert to output code:
  const std::string code = generator(definition);
  fmt::format_to(std::back_inserter(output), "{}\n\n", code);
}

}  // namespace wf
