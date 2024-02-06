// Copyright 2023 Gareth Cross
#include "wf/code_generation/ast_conversion.h"

#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/control_flow_graph.h"
#include "wf/code_generation/ir_block.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"
#include "wf/expressions/variable.h"
#include "wf/template_utils.h"

namespace wf::ast {

// Given a starting value `v`, find any downstream conditionals values that equal this value.
inline void find_conditional_output_values(const ir::value_ptr v, const bool top_level_invocation,
                                           std::vector<ir::value_ptr>& outputs) {
  bool all_phi_consumers = true;
  for (const ir::value_ptr consumer : v->consumers()) {
    if (consumer->is_phi()) {
      // A phi function might just be an input to another phi function, so we need to recurse here.
      find_conditional_output_values(consumer, false, outputs);
    } else {
      all_phi_consumers = false;
    }
  }
  if (!all_phi_consumers && !top_level_invocation) {
    outputs.push_back(v);
  }
}

// Given all the values that are required to build a given output, make the syntax to construct
// that object. For structs, we need to recurse and build each member.
struct type_constructor {
  explicit type_constructor(std::vector<ast::variant> contents) noexcept
      : contents_(std::move(contents)), index_(0) {}

  // Scalar doesn't have a constructor, just return the input directly and step forward by one
  // value.
  ast::variant operator()(const scalar_type&) {
    WF_ASSERT_LESS(index_, contents_.size());
    ast::variant result = std::move(contents_[index_]);
    ++index_;
    return result;
  }

  // Matrices we just group the next `row * col` elements into `construct_matrix` element.
  ast::construct_matrix operator()(const matrix_type& mat) {
    WF_ASSERT_LESS_OR_EQ(index_ + mat.size(), contents_.size());

    std::vector<ast::variant> matrix_args{};
    matrix_args.reserve(mat.size());
    std::copy_n(std::make_move_iterator(contents_.begin()) + index_, mat.size(),
                std::back_inserter(matrix_args));
    return ast::construct_matrix{mat, std::move(matrix_args)};
  }

  // For custom types, recurse and consume however many values are required for each individual
  // field.
  ast::construct_custom_type operator()(const custom_type& type) {
    // Recursively convert every field in the custom type.
    std::vector<std::tuple<std::string, ast::variant>> fields_out{};
    fields_out.reserve(type.size());
    for (const struct_field& field : type.fields()) {
      ast::variant field_var = std::visit(
          [this](const auto& t) -> ast::variant { return this->operator()(t); }, field.type());
      fields_out.emplace_back(field.name(), std::move(field_var));
    }
    return ast::construct_custom_type{type, std::move(fields_out)};
  }

 private:
  std::vector<ast::variant> contents_;
  std::size_t index_;
};

ast::function_definition ast_form_visitor::convert_function(const ir::block_ptr block) {
  process_block(block);

  std::vector<ast::variant> result;
  result.reserve(operations_.size() + 1);
  result.push_back(format_operation_count_comment());
  std::copy(std::make_move_iterator(operations_.begin()),
            std::make_move_iterator(operations_.end()), std::back_inserter(result));
  operations_.clear();
  return ast::function_definition(std::move(signature_), std::move(result));
}

void ast_form_visitor::push_back_conditional_assignments(
    std::vector<ast::assign_temporary>&& assignments) {
  std::sort(assignments.begin(), assignments.end(),
            [](const auto& a, const auto& b) { return a.left < b.left; });
  std::copy(std::make_move_iterator(assignments.begin()),
            std::make_move_iterator(assignments.end()), std::back_inserter(operations_));
  assignments.clear();
}

void ast_form_visitor::push_back_output_operations(const ir::block_ptr block) {
  for (const ir::value_ptr value : block->operations()) {
    if (!value->is_op<ir::save>()) {
      continue;
    }
    const ir::save& save = value->as_op<ir::save>();
    const output_key& key = save.key();
    type_constructor constructor{transform_operands(*value)};

    if (key.usage == expression_usage::return_value) {
      WF_ASSERT(block->has_no_descendents(), "Must be the final block");
      WF_ASSERT(signature_.return_type().has_value(), "Return type must be specified");
      ast::variant var = std::visit([&](const auto& x) -> ast::variant { return constructor(x); },
                                    *signature_.return_type());
      emplace_operation<ast::return_object>(std::make_shared<ast::variant>(std::move(var)));
    } else {
      auto arg = signature_.argument_by_name(key.name);
      WF_ASSERT(arg.has_value(), "Argument missing from signature: {}", key.name);

      overloaded_visit(
          arg->type(),
          [&](const scalar_type scalar) {
            // TODO: Make variant_ptr a proper type so we can put the pointer allocation
            // internally.
            emplace_operation<ast::assign_output_scalar>(
                *arg, std::make_shared<ast::variant>(constructor(scalar)));
          },
          [&](const matrix_type& mat) {
            emplace_operation<ast::assign_output_matrix>(*arg, constructor(mat));
          },
          [&](const custom_type& custom) {
            emplace_operation<ast::assign_output_struct>(*arg, constructor(custom));
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
void ast_form_visitor::push_back_conditional_output_declarations(const ir::block_ptr block) {
  for (const ir::value_ptr value : block->operations()) {
    if (value->is_phi()) {
      if (const bool no_declaration =
              value->all_consumers_satisfy([](const ir::value_ptr v) { return v->is_phi(); });
          no_declaration) {
        continue;
      }
      // We should declare this variable prior to entering the branch.
      // We need to cast the type here to handle the void type.
      emplace_operation<ast::declaration>(format_variable_name(value), value->non_void_type());
    }
  }
}

// Return true if the specified value should be written in-line instead of declared as a variable.
// At present, only constants and casts of constants do not receive variable declarations.
static bool should_inline_constant(const ir::value_ptr val) {
  return overloaded_visit(
      val->value_op(),
      [](const ir::load& load) {
        return load
            .is_type<integer_constant, float_constant, symbolic_constant, custom_type_argument>();
      },
      [&](const ir::cast&) { return should_inline_constant(val->first_operand()); },
      [](auto&&) constexpr { return false; });
}

void ast_form_visitor::process_block(const ir::block_ptr block) {
  if (non_traversable_blocks_.count(block)) {
    // Don't recurse too far - we are waiting on one of the ancestors of this block to get
    // processed.
    return;
  }
  WF_ASSERT(block->has_no_descendents() || !block->is_empty(),
            "Only the terminal block may be empty. block->name(): {}", block->name());

  operations_.reserve(operations_.capacity() + block->size());

  std::vector<ast::assign_temporary> phi_assignments{};
  phi_assignments.reserve(block->size());

  for (const ir::value_ptr value : block->operations()) {
    if (value->is_op<ir::save>()) {
      // Defer output values to the end of the block.
    } else if (value->is_phi()) {
      // Phi is not a real operation, we just use it to determine when branches should write to
      // variables declared before the if-else.
    } else if (value->is_op<ir::jump_condition>() || value->is_op<ir::output_required>()) {
      // These are placeholders and have no representation in the output code.
    } else {
      // Create the computation of the value:
      const auto computed_value = std::make_shared<const ast::variant>(visit_value(value));

      // Find any downstream phi values that are equal to this value:
      std::vector<ir::value_ptr> phi_consumers{};
      find_conditional_output_values(value, true, phi_consumers);

      // Does this value have any consumers that are not the outputs of conditional branches?
      const bool any_none_phi_consumers =
          !value->all_consumers_satisfy([](ir::value_ptr c) { return c->is_phi(); });

      // If we have a single non-phi consumer, or more than one phi consumer, we declare a
      // variable.
      const bool needs_declaration =
          !should_inline_constant(value) && (any_none_phi_consumers || phi_consumers.size() > 1);

      ast::variant_ptr rhs = computed_value;
      if (needs_declaration) {
        // We are going to declare a temporary for this value:
        emplace_operation<ast::declaration>(format_variable_name(value), value->non_void_type(),
                                            computed_value);
        rhs = std::make_shared<const ast::variant>(make_variable_ref(value));
      }

      // Here we write assignments to every conditional output that contains this value:
      for (const ir::value_ptr consumer : phi_consumers) {
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

std::vector<ast::variant> ast_form_visitor::process_nested_block(const ir::block_ptr block) {
  // Move aside operations of the current block temporarily:
  std::vector<ast::variant> operations_stashed = std::move(operations_);
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
void ast_form_visitor::handle_control_flow(const ir::block_ptr block) {
  const auto& descendants = block->descendants();
  if (descendants.empty()) {
    // This is the terminal block - nothing to do.
    return;
  }

  if (const ir::value_ptr last_op = block->last_operation();
      !last_op->is_op<ir::jump_condition>()) {
    // just keep appending:
    WF_ASSERT_EQUAL(1, descendants.size());
    process_block(descendants.front());
  } else {
    WF_ASSERT_EQUAL(2, descendants.size());

    // This over-counts a bit, since nested branches don't all run. We are just counting
    // if-statements, basically.
    operation_counts_[operation_count_label::branch]++;

    // Figure out where this if-else statement will terminate:
    const ir::block_ptr merge_point =
        find_merge_point(descendants[0], descendants[1], ir::search_direction::downwards);
    non_traversable_blocks_.insert(merge_point);

    // Declare any variables that will be written in both the if and else blocks:
    push_back_conditional_output_declarations(merge_point);

    // Descend into both branches:
    std::vector<ast::variant> operations_true = process_nested_block(descendants[0]);

    // We have two kinds of branches. One for optionally-computed outputs, which only has
    // an if-branch. The other is for conditional logic in computations (where both if and
    // else branches are required).
    if (const ir::value_ptr condition = last_op->first_operand();
        condition->is_op<ir::output_required>()) {
      const ir::output_required& oreq = condition->as_op<ir::output_required>();

      // Create an optional-output assignment block
      auto arg_optional = signature_.argument_by_name(oreq.name());
      WF_ASSERT(arg_optional, "Missing argument: {}", oreq.name());
      emplace_operation<ast::optional_output_branch>(*std::move(arg_optional),
                                                     std::move(operations_true));
    } else {
      // Fill out operations in the else branch:
      std::vector<ast::variant> operations_false = process_nested_block(descendants[1]);

      // Create a conditional
      auto condition_var =
          std::make_shared<ast::variant>(make_variable_ref(last_op->first_operand()));
      emplace_operation<ast::branch>(std::move(condition_var), std::move(operations_true),
                                     std::move(operations_false));
    }

    non_traversable_blocks_.erase(merge_point);
    process_block(merge_point);
  }
}

ast::variant ast_form_visitor::visit_value(const ir::value& value) {
  return std::visit(
      [this, &value](const auto& op) -> ast::variant {
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

ast::variant ast_form_visitor::visit_operation_argument(const ir::value_ptr value) {
  if (should_inline_constant(value)) {
    return visit_value(value);
  }
  return make_variable_ref(value);
}

ast::variant_ptr ast_form_visitor::visit_operation_argument_ptr(const ir::value_ptr val) {
  return std::make_shared<const ast::variant>(visit_operation_argument(val));
}

std::vector<ast::variant> ast_form_visitor::transform_operands(const ir::value& val) {
  std::vector<ast::variant> transformed_args{};
  transformed_args.reserve(val.num_operands());
  for (const ir::value_ptr arg : val.operands()) {
    transformed_args.push_back(visit_operation_argument(arg));
  }
  return transformed_args;
}

ast::variant ast_form_visitor::operator()(const ir::value& val, const ir::add&) {
  operation_counts_[operation_count_label::add]++;
  return ast::add{visit_operation_argument_ptr(val[0]), visit_operation_argument_ptr(val[1])};
}

ast::variant ast_form_visitor::operator()(const ir::value& val,
                                          const ir::call_external_function& call) {
  WF_ASSERT_EQUAL(val.num_operands(), call.function().num_arguments());
  operation_counts_[operation_count_label::call]++;
  return ast::call_external_function{call.function(), transform_operands(val)};
}

ast::variant ast_form_visitor::operator()(const ir::value& val, const ir::call_std_function& func) {
  operation_counts_[operation_count_label::call]++;
  return ast::call_std_function{func.name(), transform_operands(val)};
}

ast::variant ast_form_visitor::operator()(const ir::value& val, const ir::cast& cast) {
  return ast::cast{cast.destination_type(), val[0]->numeric_type(),
                   visit_operation_argument_ptr(val[0])};
}

ast::variant ast_form_visitor::operator()(const ir::value& val, const ir::compare& compare) {
  operation_counts_[operation_count_label::compare]++;
  return ast::compare{compare.operation(), visit_operation_argument_ptr(val[0]),
                      visit_operation_argument_ptr(val[1])};
}

ast::variant ast_form_visitor::operator()(const ir::value& val, const ir::construct& construct) {
  type_constructor constructor{transform_operands(val)};
  return std::visit([&](const auto& x) -> ast::variant { return constructor(x); },
                    construct.type());
}

ast::variant ast_form_visitor::operator()(const ir::value& val, const ir::copy&) {
  return visit_operation_argument(val.first_operand());
}

ast::variant ast_form_visitor::operator()(const ir::value& val, const ir::div&) {
  operation_counts_[operation_count_label::divide]++;
  return ast::divide{visit_operation_argument_ptr(val[0]), visit_operation_argument_ptr(val[1])};
}

ast::variant ast_form_visitor::operator()(const ir::value& val, const ir::get& get) {
  return overloaded_visit(
      val[0]->type(),
      [&](ir::void_type) -> ast::variant {
        WF_ASSERT_ALWAYS("Object cannot be void type. value = {}, index = {}", val.name(),
                         get.index());
      },
      [&](const scalar_type) -> ast::variant {
        WF_ASSERT_EQUAL(0, get.index());
        return visit_value(val[0]);
      },
      [&](const matrix_type& mat) -> ast::variant {
        WF_ASSERT_LESS(get.index(), mat.size());
        const auto [row, col] = mat.compute_indices(get.index());
        return ast::get_matrix_element{visit_operation_argument_ptr(val[0]), row, col};
      },
      [&](const custom_type& custom) -> ast::variant {
        return make_field_access_sequence(visit_operation_argument(val[0]), custom, get.index());
      });
}

ast::variant ast_form_visitor::operator()(const ir::value&, const ir::load& load) {
  return visit(
      [this](const auto& inner) -> ast::variant {
        using T = std::decay_t<decltype(inner)>;
        if constexpr (std::is_same_v<T, symbolic_constant>) {
          return ast::special_constant{inner.name()};
        } else if constexpr (std::is_same_v<T, integer_constant>) {
          return ast::integer_literal{inner.get_value()};
        } else if constexpr (std::is_same_v<T, float_constant>) {
          return ast::float_literal{static_cast<float_constant>(inner).get_value()};
        } else if constexpr (std::is_same_v<T, rational_constant>) {
          return ast::float_literal{static_cast<float_constant>(inner).get_value()};
        } else if constexpr (std::is_same_v<T, variable>) {
          // inspect inner type of the variable
          return std::visit([this](const auto& var_type) { return operator()(var_type); },
                            inner.identifier());
        } else if constexpr (std::is_same_v<T, custom_type_argument>) {
          return ast::get_argument{signature_.argument_by_index(inner.arg_index())};
        }
      },
      load.variant());
}

ast::variant ast_form_visitor::operator()(const ir::value& val, const ir::mul&) {
  operation_counts_[operation_count_label::multiply]++;
  return ast::multiply{visit_operation_argument_ptr(val[0]), visit_operation_argument_ptr(val[1])};
}

ast::variant ast_form_visitor::operator()(const ir::value& val, const ir::neg&) {
  operation_counts_[operation_count_label::negate]++;
  return ast::negate{visit_operation_argument_ptr(val[0])};
}

ast::variant ast_form_visitor::operator()(const scalar_type&, const argument& arg,
                                          std::size_t element_index) const {
  WF_ASSERT_EQUAL(0, element_index);
  return ast::get_argument{arg};
}

ast::variant ast_form_visitor::operator()(const matrix_type& m, const argument& arg,
                                          const std::size_t element_index) const {
  const auto [row, col] = m.compute_indices(element_index);
  return ast::get_matrix_element{ast::make_shared_variant<ast::get_argument>(arg), row, col};
}

ast::variant ast_form_visitor::operator()(const custom_type& c, const argument& arg,
                                          const std::size_t element_index) const {
  ast::get_argument get_arg{arg};
  return make_field_access_sequence(ast::get_argument{arg}, c, element_index);
}

ast::variant ast_form_visitor::operator()(const named_variable& v) const {
  return ast::variable_ref{v.name()};
}

ast::variant ast_form_visitor::operator()(const function_argument_variable& a) const {
  const argument& arg = signature_.argument_by_index(a.arg_index());
  return std::visit([&](const auto& type) { return operator()(type, arg, a.element_index()); },
                    arg.type());
}

ast::variant ast_form_visitor::operator()(const unique_variable& u) const {
  throw type_error("Cannot convert unique_variable to ast: {}", u.index());
}

ast::variant ast_form_visitor::make_field_access_sequence(ast::variant prev, const custom_type& c,
                                                          const std::size_t element_index) {
  const auto access_sequence = determine_access_sequence(c, element_index);
  for (const access_variant& access : access_sequence) {
    overloaded_visit(
        access,
        [&](const matrix_access& m) {
          prev = ast::get_matrix_element{std::make_shared<ast::variant>(std::move(prev)), m.row(),
                                         m.col()};
        },
        [&](const field_access& f) {
          prev = ast::get_field{std::make_shared<ast::variant>(std::move(prev)), f.type(),
                                f.field_name()};
        });
  }
  return prev;
}

static constexpr std::string_view string_from_operation_count_label(
    const operation_count_label label) noexcept {
  switch (label) {
    case operation_count_label::add:
      return "add";
    case operation_count_label::branch:
      return "branch";
    case operation_count_label::call:
      return "call";
    case operation_count_label::compare:
      return "compare";
    case operation_count_label::divide:
      return "divide";
    case operation_count_label::multiply:
      return "multiply";
    case operation_count_label::negate:
      return "negate";
  }
  return "<NOT A VALID ENUM VALUE>";
}

ast::variant ast_form_visitor::format_operation_count_comment() const {
  // Sort counts into order of the label itself:
  std::vector<std::pair<operation_count_label, std::size_t>> counts{operation_counts_.begin(),
                                                                    operation_counts_.end()};
  std::sort(counts.begin(), counts.end(),
            [](const auto& a, const auto& b) { return a.first < b.first; });

  std::string comment{"Operation counts:\n"};
  std::size_t total = 0;
  for (const auto& [label, count] : counts) {
    fmt::format_to(std::back_inserter(comment), "{}: {}\n",
                   string_from_operation_count_label(label), count);
    total += count;
  }
  fmt::format_to(std::back_inserter(comment), "total: {}", total);
  return ast::comment{std::move(comment)};
}

function_definition create_ast(const wf::control_flow_graph& ir,
                               const function_description& description) {
  ast_form_visitor converter{ir.value_print_width(), description};
  return converter.convert_function(ir.first_block());
}

}  // namespace wf::ast
