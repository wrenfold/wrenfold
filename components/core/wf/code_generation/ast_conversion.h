// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/ast.h"
#include "wf/code_generation/function_definition.h"

namespace wf::ast {

// Create function_definition from the intermediate representation:
function_definition create_ast(const output_ir& ir, const function_signature& signature);

}  // namespace wf::ast
