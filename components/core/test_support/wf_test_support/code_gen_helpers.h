// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/ast_conversion.h"
#include "wf/code_generation/ir_builder.h"
#include "wf/fmt_imports.h"
#include "wf/function_evaluator.h"

namespace wf {

template <typename CodeGenerator, typename Func, typename... Args>
void generate_func(CodeGenerator&& generator, std::string& output, Func&& func,
                   const std::string_view name, Args&&... args) {
  auto tuple =
      build_function_description(std::forward<Func>(func), name, std::forward<Args>(args)...);
  const function_signature& signature = std::get<0>(tuple);
  const std::vector<expression_group>& expressions = std::get<1>(tuple);

  flat_ir ir{expressions};
  ir.eliminate_duplicates();

  const output_ir output_ir{std::move(ir)};
#if 0
  fmt::print("IR ({}, {} operations):\n{}\n", name, output_ir.num_operations(),
             output_ir.to_string());
  std::fflush(nullptr);
#endif

  // Generate syntax tree:
  const ast::function_definition definition = ast::create_ast(output_ir, signature);

  const std::string code = std::invoke(generator, definition);
  fmt::format_to(std::back_inserter(output), "{}\n\n", code);
}

}  // namespace wf
