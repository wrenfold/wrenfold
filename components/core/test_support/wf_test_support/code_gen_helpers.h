// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/code_generation/ast_conversion.h"
#include "wf/code_generation/control_flow_graph.h"
#include "wf/code_generation/function_evaluator.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

template <typename CodeGenerator, typename Func, typename... Args>
void generate_func(CodeGenerator&& generator, std::string& output, Func&& func,
                   const bool convert_ternaries, const std::string_view name, Args&&... args) {
  const function_description description =
      build_function_description(std::forward<Func>(func), name, std::forward<Args>(args)...);

  const control_flow_graph output_cfg =
      control_flow_graph{description, optimization_params()}.convert_conditionals_to_control_flow(
          convert_ternaries);

  // Generate syntax tree:
  const ast::function_definition definition = ast::create_ast(output_cfg, description);

  // Convert to output code:
  const std::string code = generator(definition);
  fmt::format_to(std::back_inserter(output), "{}\n\n", code);
}

template <typename CodeGenerator, typename Func, typename... Args>
void generate_func(CodeGenerator&& generator, std::string& output, Func&& func,
                   const std::string_view name, Args&&... args) {
  generate_func(std::forward<CodeGenerator>(generator), output, std::forward<Func>(func), true,
                name, std::forward<Args>(args)...);
}

}  // namespace wf
