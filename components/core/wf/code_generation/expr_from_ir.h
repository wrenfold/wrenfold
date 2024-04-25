// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <string>
#include <unordered_map>

#include "wf/code_generation/expression_group.h"
#include "wf/code_generation/ir_block.h"
#include "wf/expression.h"

namespace wf {

// Create `scalar_expr` tree from the IR representation. For use in round-trip unit tests.
std::unordered_map<output_key, std::vector<scalar_expr>, hash_struct<output_key>>
create_output_expression_map(ir::block_ptr starting_block,
                             std::unordered_map<std::string, bool> output_arg_exists);

}  // namespace wf
