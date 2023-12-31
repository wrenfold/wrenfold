// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/ast.h"

namespace wf {
class flat_ir;
class output_ir;
}  // namespace wf

namespace wf::ast {

// Create function_definition from the intermediate representation:
function_definition create_ast(const output_ir& ir, const function_description& description);

}  // namespace wf::ast
