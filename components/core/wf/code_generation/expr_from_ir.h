// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <string>
#include <unordered_map>

#include "wf/any_expression.h"
#include "wf/code_generation/expression_group.h"
#include "wf/code_generation/ir_block.h"
#include "wf/expression.h"

namespace wf {

struct rebuilt_expressions {
  // Map from function output to a vector of scalar expressions.
  std::unordered_map<output_key, any_expression, hash_struct<output_key>> output_expressions{};

  // Intermediate values used in the computation of the output.
  // Stores tuples of (variable name, expression).
  std::vector<std::tuple<scalar_expr, scalar_expr>> intermediate_values{};
};

// Reconstitute the symbolic expression tree from the IR representation.
rebuilt_expressions rebuild_expression_tree(ir::const_block_ptr starting_block,
                                            std::unordered_map<std::string, bool> output_arg_exists,
                                            bool use_intermediate_values = false);

}  // namespace wf
