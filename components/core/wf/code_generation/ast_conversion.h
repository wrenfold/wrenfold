// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "wf/code_generation/ast.h"
#include "wf/code_generation/function_description.h"
#include "wf/code_generation/ir_block.h"

namespace wf {
// Forward declare.
class control_flow_graph;
class operation_counts;
}  // namespace wf

namespace wf::ast {

// Visitor that traverses the intermediate representation and converts it to the ast types.
// We recursively covert the control flow graph into a syntax tree that can emitted in different
// target languages.
class ast_form_visitor {
 public:
  ast_form_visitor(const std::size_t value_width, const function_description& description)
      : value_width_(value_width),
        signature_{description.name(), description.return_value_type(), description.arguments()} {}

  // Convert a function, starting from `block`.
  ast::function_definition convert_function(ir::const_block_ptr block,
                                            const operation_counts& counts);

 private:
  // Sort and move all the provided assignments into `operations_`.
  void push_back_conditional_assignments(std::vector<ast::assign_temporary>&& assignments);

  // Traverse the block and identify all the `ir::save` operations. Create ast objets that represent
  // these, either by making return values or by writing to output arguments.
  void push_back_output_operations(ir::const_block_ptr block);

  // Insert declarations for temporary values used as the outputs of conditional branches.
  void push_back_conditional_output_declarations(ir::const_block_ptr block);

  // Process all the operations in the given block, appending their equivalent ast types to
  // `operations_`. Recurses into any blocks that are jumped-to from this one.
  void process_block(ir::const_block_ptr block);

  // Stash the current set of operations, and process a child block.
  // We return the nested block's operations (and pop our stash before returning).
  std::vector<ast::ast_element> process_nested_block(ir::const_block_ptr block);

  // At the end of `block`, check if we need to inject an if-else statement to handle conditional
  // control flow.
  void handle_control_flow(ir::const_block_ptr block);

  // Convert the value integer to the formatted variable name that will appear in code.
  std::string format_variable_name(const ir::const_value_ptr val) const {
    return fmt::format("v{:0>{}}", val->name(), value_width_);
  }

  // Visit the operation type on `value`, and delegate to the appropriate method on self.
  ast::ast_element visit_value(const ir::value& value);
  ast::ast_element visit_value(const ir::const_value_ptr value) { return visit_value(*value); }

  // Create a `variable_ref` object with the name of the variable used to store `value`.
  ast::variable_ref make_variable_ref(const ir::const_value_ptr value) const {
    WF_ASSERT(declared_values_.contains(value), "value = {}", value);
    return ast::variable_ref{format_variable_name(value)};
  }

  // Check if the provided value is going to be placed in-line. If so, we just directly return
  // the converted `value`. If not, we assume a variable will be declared elsewhere and instead
  // return a reference to that.
  ast::ast_element visit_operation_argument(ir::const_value_ptr value,
                                            std::optional<precedence> parent_precedence);

  // Convert all operands to `val` into equivalent ast types.
  std::vector<ast::ast_element> transform_operands(const ir::value& val,
                                                   std::optional<precedence> parent_precedence);

  // Push back operation of type `T` into `operations_`.
  template <typename T, typename... Args>
  void emplace_operation(Args&&... args) {
    operations_.emplace_back(std::in_place_type_t<T>{}, std::forward<Args>(args)...);
  }

  ast::ast_element operator()(const ir::value& val, const ir::add&);
  ast::ast_element operator()(const ir::value& val, const ir::call_external_function& call);
  ast::ast_element operator()(const ir::value& val, const ir::call_std_function& func);
  ast::ast_element operator()(const ir::value& val, const ir::cast& cast);
  ast::ast_element operator()(const ir::value& val, const ir::compare& compare);
  ast::ast_element operator()(const ir::value& val, const ir::construct& construct);
  ast::ast_element operator()(const ir::value& val, const ir::cond&);
  ast::ast_element operator()(const ir::value& val, const ir::copy&);
  ast::ast_element operator()(const ir::value& val, const ir::div&);
  ast::ast_element operator()(const ir::value& val, const ir::get& get);
  ast::ast_element operator()(const ir::value& val, const ir::load& load);
  ast::ast_element operator()(const ir::value& val, const ir::mul&);
  ast::ast_element operator()(const ir::value& val, const ir::neg&);
  ast::ast_element operator()(const scalar_type&, const argument& arg,
                              std::size_t element_index) const;
  ast::ast_element operator()(const matrix_type& m, const argument& arg,
                              std::size_t element_index) const;
  ast::ast_element operator()(const custom_type& c, const argument& arg,
                              std::size_t element_index) const;

  static ast::ast_element make_field_access_sequence(ast::ast_element prev, const custom_type& c,
                                                     std::size_t element_index);

  std::size_t value_width_;
  function_signature signature_;

  // Operations accrued in the current block.
  std::vector<ast::ast_element> operations_;

  // Blocks we can't process yet (pending processing of all their ancestors).
  std::unordered_set<ir::const_block_ptr> non_traversable_blocks_;

  // Values for which we will make explicit declarations in the output code.
  std::unordered_set<ir::const_value_ptr> declared_values_;
};

// Create function_definition from the intermediate representation:
function_definition create_ast(const wf::control_flow_graph& ir,
                               const function_description& description);

}  // namespace wf::ast
