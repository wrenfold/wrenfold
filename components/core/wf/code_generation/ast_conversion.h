// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/ast.h"
#include "wf/code_generation/function_description.h"

namespace wf {
// Forwar declare.
class control_flow_graph;
}  // namespace wf

namespace wf::ast {

// Create function_definition from the intermediate representation:
function_definition create_ast(const wf::control_flow_graph& ir,
                               const function_description& description);

}  // namespace wf::ast
