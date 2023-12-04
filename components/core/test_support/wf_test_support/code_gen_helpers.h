// Copyright 2023 Gareth Cross
#pragma once
#include <chrono>

#include "wf/code_generation/ir_builder.h"
#include "wf/fmt_imports.h"
#include "wf/function_evaluator.h"
#include "wf/type_annotations.h"

namespace wf {

template <typename CodeGenerator, typename Func, typename... Args>
void generate_func(CodeGenerator&& generator, std::string& output, Func&& func,
                   const std::string_view name, Args&&... args) {
  const auto start = std::chrono::steady_clock::now();
  auto tuple =
      build_function_description(std::forward<Func>(func), name, std::forward<Args>(args)...);
  const ast::function_signature& signature = std::get<0>(tuple);
  const std::vector<expression_group>& expressions = std::get<1>(tuple);

  flat_ir ir{expressions};
  ir.eliminate_duplicates();

  output_ir output_ir{std::move(ir)};
#if 0
  fmt::print("IR ({}, {} operations):\n{}\n", name, output_ir.num_operations(),
             output_ir.to_string());
  std::fflush(nullptr);
#endif

  // Generate syntax tree:
  std::vector<ast::variant> body = ast::create_ast(output_ir, signature);

  const std::string code = generator.generate_code(signature, body);
  fmt::format_to(std::back_inserter(output), "{}\n\n", code);

  const auto end = std::chrono::steady_clock::now();
  const auto elapsed = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
  fmt::print("Generated {}: {:.4} seconds\n", name, static_cast<double>(elapsed) / 1.0e6);
}

}  // namespace wf
