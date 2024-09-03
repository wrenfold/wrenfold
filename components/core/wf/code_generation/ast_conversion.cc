// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/ast_conversion.h"

#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/control_flow_graph.h"
#include "wf/code_generation/ir_block.h"
#include "wf/code_generation/operation_counts.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"
#include "wf/expressions/variable.h"
#include "wf/utility/overloaded_visit.h"
#include "wf/utility/scoped_trace.h"

namespace wf::ast {

// Given a starting value `v`, find any downstream conditionals values that equal this value.
inline void find_conditional_output_values(const ir::const_value_ptr v,
                                           std::vector<ir::const_value_ptr>& outputs) {
  bool all_phi_consumers = true;
  for (const ir::const_value_ptr consumer : v->ordered_consumers()) {
    if (consumer->is_phi()) {
      // A phi function might just be an input to another phi function, so we need to recurse here.
      find_conditional_output_values(consumer, outputs);
    } else {
      all_phi_consumers = false;
    }
  }
  // On first invocation, the passed value will not be a phi.
  if (!all_phi_consumers && v->is_phi()) {
    outputs.push_back(v);
  }
}

static ast::ast_element format_operation_count_comment(const operation_counts& counts) {
  std::string comment{"Operation counts:\n"};
  for (const auto& [label, count] : counts.labels_and_counts()) {
    if (count > 0) {
      fmt::format_to(std::back_inserter(comment), "{}: {}\n",
                     string_from_operation_count_label(label), count);
    }
  }
  fmt::format_to(std::back_inserter(comment), "total: {}", counts.total());
  return ast::ast_element{std::in_place_type_t<ast::comment>{}, std::move(comment)};
}

ast::function_definition ast_form_visitor::convert_function(const ir::const_block_ptr block,
                                                            const operation_counts& counts) {
  // First place a comment describing max operation counts:
  operations_.push_back(format_operation_count_comment(counts));
  process_block(block);
  return ast::function_definition(std::move(signature_), std::move(operations_));
}

void ast_form_visitor::push_back_conditional_assignments(
    std::vector<ast::assign_temporary>&& assignments) {
  std::sort(assignments.begin(), assignments.end(),
            [](const auto& a, const auto& b) { return a.left < b.left; });
  std::transform(std::make_move_iterator(assignments.begin()),
                 std::make_move_iterator(assignments.end()), std::back_inserter(operations_),
                 [](ast::assign_temporary temp) { return ast::ast_element{std::move(temp)}; });
  assignments.clear();
}

void ast_form_visitor::push_back_output_operations(const ir::const_block_ptr block) {
  for (const ir::const_value_ptr value : block->operations()) {
    if (!value->is_op<ir::save>()) {
      continue;
    }
    WF_ASSERT_EQ(1, value->num_operands());

    const ir::save& save = value->as_op<ir::save>();
    const output_key& key = save.key();
    auto converted_value = visit_operation_argument(value->first_operand(), std::nullopt);

    if (key.usage == expression_usage::return_value) {
      WF_ASSERT(block->has_no_descendants(), "Must be the final block");
      emplace_operation<ast::return_object>(std::move(converted_value));
    } else {
      auto arg = signature_.argument_by_name(key.name);
      WF_ASSERT(arg.has_value(), "Argument missing from signature: {}", key.name);

      overloaded_visit(
          arg->type(),
          [&](const scalar_type) {
            emplace_operation<ast::assign_output_scalar>(*arg, std::move(converted_value));
          },
          [&](const matrix_type&) {
            emplace_operation<ast::assign_output_matrix>(
                *arg, *get_if<construct_matrix>(converted_value));
          },
          [&](const custom_type&) {
            emplace_operation<ast::assign_output_struct>(
                *arg, *get_if<construct_custom_type>(converted_value));
          });
    }
  }
}

// Iterate over values in the specified block. Every time we hit the result of a phi function,
// check if it is consumed by anything _other_ than a phi function. If not, it is just a nested
// conditional, and we don't need an inner declaration.
//
// For example:
//
//  float v00;
//  if (...) {
//    // We could declare v01 here, and then assign v00 = v01, but it would be pointless and
//    // verbose.
//    if (...) {
//      v00 = sin(x);
//    }
//  }
void ast_form_visitor::push_back_conditional_output_declarations(const ir::const_block_ptr block) {
  for (const ir::const_value_ptr value : block->operations()) {
    if (value->is_phi()) {
      if (const bool no_declaration =
              value->all_consumers_satisfy([](const ir::const_value_ptr v) { return v->is_phi(); });
          no_declaration) {
        continue;
      }
      // We should declare this variable prior to entering the branch.
      // We need to cast the type here to handle the void type.
      emplace_operation<ast::declaration>(format_variable_name(value), value->non_void_type());
      declared_values_.insert(value);
    }
  }
}

// Return true if the specified value should be written in-line instead of declared as a variable.
// At present, only constants and casts of constants do not receive variable declarations.
inline bool should_inline_constant(const ir::const_value_ptr val) {
  return overloaded_visit(
      val->value_op(),
      [](const ir::load& load) {
        return load
            .is_type<integer_constant, float_constant, symbolic_constant, custom_type_argument>();
      },
      [&](const ir::cast&) { return should_inline_constant(val->first_operand()); },
      [](auto&&) constexpr { return false; });
}

// Return true if this operation consists of copying from an input argument.
inline bool is_argument_read_operation(const ir::const_value_ptr val) {
  if (const ir::load* l = std::get_if<ir::load>(&val->value_op()); l != nullptr) {
    return l->is_type<variable>();
  }
  return false;
}

void ast_form_visitor::process_block(const ir::const_block_ptr block) {
  if (non_traversable_blocks_.count(block)) {
    // Don't recurse too far - we are waiting on one of the ancestors of this block to get
    // processed.
    return;
  }
  WF_ASSERT(block->has_no_descendants() || !block->is_empty(),
            "Only the terminal block may be empty. block->name(): {}", block->name());

  operations_.reserve(operations_.capacity() + block->size());

  std::vector<ast::assign_temporary> phi_assignments{};
  phi_assignments.reserve(block->size());

  for (const ir::const_value_ptr value : block->operations()) {
    if (value->is_op<ir::save>()) {
      // Defer output values to the end of the block.
    } else if (value->is_phi()) {
      // Phi is not a real operation, we just use it to determine when branches should write to
      // variables declared before the if-else.
    } else if (value->is_op<ir::jump_condition>() || value->is_op<ir::output_required>()) {
      // These are placeholders and have no representation in the output code.
    } else {
      // Find any downstream phi values that are equal to this value:
      std::vector<ir::const_value_ptr> phi_consumers{};
      find_conditional_output_values(value, phi_consumers);

      // Does this value have any consumers that are not the outputs of conditional branches?
      const std::size_t num_non_phi_consumers =
          std::count_if(value->consumers().begin(), value->consumers().end(),
                        [](const ir::const_value_ptr c) { return !c->is_phi(); });

      // We declare a variable if:
      // - This is a copy of an input argument.
      // - The value is _not_ a numerical constant, and it has either:
      //    - More than one non-phi consumer. (Used in more than one computation).
      //    - Or, multiple phi consumers. (Multiple conditionals accept this value as the output).
      const bool needs_declaration = is_argument_read_operation(value) ||
                                     (!should_inline_constant(value) &&
                                      (num_non_phi_consumers > 1 || phi_consumers.size() > 1));
      if (needs_declaration) {
        declared_values_.insert(value);
      }

      ast::ast_element rhs{visit_value(value)};
      if (needs_declaration) {
        // We are going to declare a temporary for this value:
        emplace_operation<ast::declaration>(format_variable_name(value), value->non_void_type(),
                                            std::move(rhs));
        rhs = ast::ast_element{std::in_place_type_t<ast::variable_ref>{},
                               format_variable_name(value)};
      }

      // Here we write assignments to every conditional output that contains this value:
      for (const ir::const_value_ptr consumer : phi_consumers) {
        phi_assignments.emplace_back(format_variable_name(consumer), rhs);
      }
    }
  }

  // Before the end of the block, push back outputs from this if-else branch:
  push_back_conditional_assignments(std::move(phi_assignments));

  // The last thing in the block is writing to output variables:
  push_back_output_operations(block);

  // Recurse into the blocks that follow this one:
  handle_control_flow(block);
}

std::vector<ast::ast_element> ast_form_visitor::process_nested_block(
    const ir::const_block_ptr block) {
  // Move aside operations of the current block temporarily:
  std::vector<ast::ast_element> operations_stashed = std::move(operations_);
  operations_.clear();

  // Process the block, writing to operations_ in the process.
  process_block(block);

  // Take the accrued operations and put them in `operations_stashed`.
  // In the process, we reset operations for the calling block.
  std::swap(operations_, operations_stashed);
  return operations_stashed;
}

// Determine if the provided block terminates in conditional control flow. If it does, we need to
// branch both left and right to compute the contents of the if-else statement.
void ast_form_visitor::handle_control_flow(const ir::const_block_ptr block) {
  const auto& descendants = block->descendants();
  if (descendants.empty()) {
    // This is the terminal block - nothing to do.
    return;
  }

  if (const ir::const_value_ptr last_op = block->last_operation();
      !last_op->is_op<ir::jump_condition>()) {
    // just keep appending:
    WF_ASSERT_EQ(1, descendants.size());
    process_block(descendants.front());
  } else {
    WF_ASSERT_EQ(2, descendants.size());

    // Figure out where this if-else statement will terminate:
    const ir::const_block_ptr merge_point =
        find_merge_point(descendants[0], descendants[1], ir::search_direction::downwards);
    non_traversable_blocks_.insert(merge_point);

    // Declare any variables that will be written in both the if and else blocks:
    push_back_conditional_output_declarations(merge_point);

    // Descend into both branches:
    std::vector<ast::ast_element> operations_true = process_nested_block(descendants[0]);

    // We have two kinds of branches. One for optionally-computed outputs, which only has
    // an if-branch. The other is for conditional logic in computations (where both if and
    // else branches are required).
    if (const ir::const_value_ptr condition = last_op->first_operand();
        condition->is_op<ir::output_required>()) {
      const ir::output_required& oreq = condition->as_op<ir::output_required>();

      // Create an optional-output assignment block
      auto arg_optional = signature_.argument_by_name(oreq.name());
      WF_ASSERT(arg_optional, "Missing argument: {}", oreq.name());
      emplace_operation<ast::optional_output_branch>(*std::move(arg_optional),
                                                     std::move(operations_true));
    } else {
      // Fill out operations in the else branch:
      std::vector<ast::ast_element> operations_false = process_nested_block(descendants[1]);

      // Create a conditional
      ast::ast_element condition_var =
          visit_operation_argument(last_op->first_operand(), std::nullopt);
      emplace_operation<ast::branch>(std::move(condition_var), std::move(operations_true),
                                     std::move(operations_false));
    }

    non_traversable_blocks_.erase(merge_point);
    process_block(merge_point);
  }
}

ast::ast_element ast_form_visitor::visit_value(const ir::value& value) {
  return std::visit(
      [this, &value](const auto& op) -> ast::ast_element {
        // These types are placeholders, and don't directly appear in the ast output:
        using T = std::decay_t<decltype(op)>;
        using excluded_types =
            type_list<ir::jump_condition, ir::save, ir::cond, ir::phi, ir::output_required>;
        if constexpr (type_list_contains_v<T, excluded_types>) {
          throw type_error("Type cannot be converted to AST: {}", typeid(T).name());
        } else {
          return operator()(value, op);
        }
      },
      value.value_op());
}

ast::ast_element ast_form_visitor::visit_operation_argument(
    const ir::const_value_ptr value, const std::optional<precedence> parent_precedence) {
  if (!declared_values_.count(value)) {
    if (parent_precedence.has_value() && value->operation_precedence() <= *parent_precedence) {
      return ast::ast_element{std::in_place_type_t<parenthetical>{}, visit_value(value)};
    } else {
      return visit_value(value);
    }
  } else {
    return ast::ast_element{make_variable_ref(value)};
  }
}

std::vector<ast::ast_element> ast_form_visitor::transform_operands(
    const ir::value& val, const std::optional<precedence> parent_precedence) {
  return transform_map<std::vector>(val.operands(), [&](const ir::const_value_ptr arg) {
    return visit_operation_argument(arg, parent_precedence);
  });
}

ast::ast_element ast_form_visitor::operator()(const ir::value& val, const ir::add&) {
  auto args =
      transform_map<ast::add::container_type>(val.operands(), [this](const ir::const_value_ptr v) {
        return visit_operation_argument(v, precedence::addition);
      });
  return ast::ast_element{std::in_place_type_t<ast::add>{}, std::move(args)};
}

ast::ast_element ast_form_visitor::operator()(const ir::value& val,
                                              const ir::call_external_function& call) {
  WF_ASSERT_EQ(val.num_operands(), call.function().num_arguments());
  return ast::ast_element{std::in_place_type_t<ast::call_external_function>{}, call.function(),
                          transform_operands(val, std::nullopt)};
}

ast::ast_element ast_form_visitor::operator()(const ir::value& val,
                                              const ir::call_std_function& func) {
  return ast::ast_element{std::in_place_type_t<ast::call_std_function>{}, func.name(),
                          transform_operands(val, std::nullopt)};
}

ast::ast_element ast_form_visitor::operator()(const ir::value& val, const ir::cast& cast) {
  return ast::ast_element{std::in_place_type_t<ast::cast>{}, cast.destination_type(),
                          val[0]->numeric_type(), visit_operation_argument(val[0], std::nullopt)};
}

ast::ast_element ast_form_visitor::operator()(const ir::value& val, const ir::compare& compare) {
  return ast::ast_element{std::in_place_type_t<ast::compare>{}, compare.operation(),
                          visit_operation_argument(val[0], std::nullopt),
                          visit_operation_argument(val[1], std::nullopt)};
}

ast::ast_element ast_form_visitor::operator()(const ir::value& val,
                                              const ir::construct& construct) {
  auto operands = transform_operands(val, std::nullopt);
  return overloaded_visit(
      construct.type(),
      [&](const matrix_type& mat) {
        return ast::ast_element{std::in_place_type_t<ast::construct_matrix>(), mat,
                                std::move(operands)};
      },
      [&](const custom_type& type) {
        WF_ASSERT_EQ(operands.size(), type.size());
        auto named_fields = transform_enumerate_map<std::vector>(
            type.fields(), [&operands](const std::size_t index, const struct_field& field) {
              return std::make_tuple(field.name(), std::move(operands[index]));
            });
        return ast::ast_element{std::in_place_type_t<ast::construct_custom_type>(), type,
                                std::move(named_fields)};
      });
}

ast::ast_element ast_form_visitor::operator()(const ir::value& val, const ir::copy&) {
  return visit_operation_argument(val.first_operand(), std::nullopt);
}

ast::ast_element ast_form_visitor::operator()(const ir::value& val, const ir::div&) {
  return ast::ast_element{std::in_place_type_t<ast::divide>{},
                          visit_operation_argument(val[0], precedence::multiplication),
                          visit_operation_argument(val[1], precedence::multiplication)};
}

ast::ast_element ast_form_visitor::operator()(const ir::value& val, const ir::get& get) {
  return overloaded_visit(
      val[0]->type(),
      [&](ir::void_type) -> ast::ast_element {
        WF_ASSERT_ALWAYS("Object cannot be void type. value = {}, index = {}", val.name(),
                         get.index());
      },
      [&](const scalar_type) -> ast::ast_element {
        WF_ASSERT_EQ(0, get.index());
        return visit_value(val[0]);
      },
      [&](const matrix_type& mat) -> ast::ast_element {
        WF_ASSERT_LT(get.index(), mat.size());
        const auto [row, col] = mat.compute_indices(get.index());
        return ast::ast_element{std::in_place_type_t<ast::get_matrix_element>{},
                                visit_operation_argument(val[0], std::nullopt), row, col};
      },
      [&](const custom_type& custom) -> ast::ast_element {
        return make_field_access_sequence(visit_operation_argument(val[0], std::nullopt), custom,
                                          get.index());
      });
}

ast::ast_element ast_form_visitor::operator()(const ir::value&, const ir::load& load) {
  return visit(
      [this](const auto& inner) -> ast::ast_element {
        using T = std::decay_t<decltype(inner)>;
        if constexpr (std::is_same_v<T, symbolic_constant>) {
          return ast::ast_element{std::in_place_type_t<ast::special_constant>{}, inner.name()};
        } else if constexpr (std::is_same_v<T, integer_constant>) {
          return ast::ast_element{std::in_place_type_t<ast::integer_literal>{},
                                  static_cast<std::int64_t>(inner.value())};
        } else if constexpr (std::is_same_v<T, float_constant>) {
          return ast::ast_element{std::in_place_type_t<ast::float_literal>{},
                                  static_cast<float_constant>(inner).value()};
        } else if constexpr (std::is_same_v<T, rational_constant>) {
          return ast::ast_element{std::in_place_type_t<ast::float_literal>{},
                                  static_cast<float_constant>(inner).value()};
        } else if constexpr (std::is_same_v<T, boolean_constant>) {
          return ast::ast_element{std::in_place_type_t<ast::boolean_literal>{}, inner.value()};
        } else if constexpr (std::is_same_v<T, variable>) {
          // inspect inner type of the variable
          return std::visit([this](const auto& var_type) { return this->operator()(var_type); },
                            inner.identifier());
        } else if constexpr (std::is_same_v<T, custom_type_argument>) {
          return ast::ast_element{std::in_place_type_t<ast::get_argument>{},
                                  signature_.argument_by_index(inner.arg_index())};
        }
      },
      load.variant());
}

ast::ast_element ast_form_visitor::operator()(const ir::value& val, const ir::mul&) {
  WF_ASSERT_GE(val.num_operands(), 2);
  auto args =
      transform_map<ast::add::container_type>(val.operands(), [this](const ir::const_value_ptr v) {
        return visit_operation_argument(v, precedence::multiplication);
      });
  return ast::ast_element{std::in_place_type_t<ast::multiply>{}, std::move(args)};
}

ast::ast_element ast_form_visitor::operator()(const ir::value& val, const ir::neg&) {
  return ast::ast_element{std::in_place_type_t<ast::negate>{},
                          visit_operation_argument(val[0], precedence::multiplication)};
}

ast::ast_element ast_form_visitor::operator()(const scalar_type&, const argument& arg,
                                              std::size_t element_index) const {
  WF_ASSERT_EQ(0, element_index);
  return ast::ast_element{std::in_place_type_t<ast::get_argument>{}, arg};
}

ast::ast_element ast_form_visitor::operator()(const matrix_type& m, const argument& arg,
                                              const std::size_t element_index) const {
  const auto [row, col] = m.compute_indices(element_index);
  return ast::ast_element{std::in_place_type_t<ast::get_matrix_element>{},
                          ast::ast_element{std::in_place_type_t<ast::get_argument>{}, arg}, row,
                          col};
}

ast::ast_element ast_form_visitor::operator()(const custom_type& c, const argument& arg,
                                              const std::size_t element_index) const {
  ast::get_argument get_arg{arg};
  return make_field_access_sequence(
      ast::ast_element{std::in_place_type_t<ast::get_argument>{}, arg}, c, element_index);
}

ast::ast_element ast_form_visitor::operator()(const named_variable& v) const {
  return ast::ast_element{std::in_place_type_t<ast::variable_ref>{}, v.name()};
}

ast::ast_element ast_form_visitor::operator()(const function_argument_variable& a) const {
  const argument& arg = signature_.argument_by_index(a.arg_index());
  return std::visit([&](const auto& type) { return operator()(type, arg, a.element_index()); },
                    arg.type());
}

ast::ast_element ast_form_visitor::operator()(const unique_variable& u) const {
  throw type_error("Cannot convert unique_variable to ast: {}", u.index());
}

ast::ast_element ast_form_visitor::make_field_access_sequence(ast::ast_element prev,
                                                              const custom_type& c,
                                                              const std::size_t element_index) {
  const auto access_sequence = determine_access_sequence(c, element_index);
  for (const access_variant& access : access_sequence) {
    overloaded_visit(
        access,
        [&](const matrix_access& m) {
          prev = ast::ast_element{std::in_place_type_t<ast::get_matrix_element>{}, std::move(prev),
                                  m.row(), m.col()};
        },
        [&](const field_access& f) {
          prev = ast::ast_element{std::in_place_type_t<ast::get_field>{}, std::move(prev), f.type(),
                                  f.field_name()};
        });
  }
  return prev;
}

function_definition create_ast(const wf::control_flow_graph& ir,
                               const function_description& description) {
  WF_FUNCTION_TRACE();
  ast_form_visitor converter{ir.value_print_width(), description};
  return converter.convert_function(ir.first_block(), ir.compute_operation_counts());
}

}  // namespace wf::ast
