// Copyright 2023 Gareth Cross
#include "wf/code_generation/ast_conversion.h"

#include <unordered_set>

#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/ir_builder.h"
#include "wf/code_generation/ir_types.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"
#include "wf/expressions/variable.h"

namespace wf {

// Different categories of operations we count in order to generate summaries for the user.
enum class operation_count_label {
  add,
  branch,
  call,
  compare,
  divide,
  multiply,
  negate,
};

template <>
struct hash_struct<operation_count_label> {
  constexpr std::size_t operator()(operation_count_label enum_value) const noexcept {
    return static_cast<std::size_t>(enum_value);
  }
};
}  // namespace wf

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
template <typename Iterator>
struct create_custom_type_constructor {
  create_custom_type_constructor(Iterator begin, Iterator end) : input_it(begin), end(end) {}

  // Scalar doesn't have a constructor, just return the input directly and step forward by one
  // value.
  ast::variant operator()(const scalar_type&) {
    WF_ASSERT(input_it != end);
    ast::variant result = *input_it;
    ++input_it;
    return result;
  }

  // Matrices we just group the next `row * col` elements into `construct_matrix` element.
  ast::construct_matrix operator()(const matrix_type& mat) {
    const auto mat_size = static_cast<std::ptrdiff_t>(mat.size());
    WF_ASSERT_GREATER_OR_EQ(std::distance(input_it, end), mat_size);

    std::vector<ast::variant> matrix_args{};
    matrix_args.reserve(mat.size());
    const auto start = input_it;
    std::advance(input_it, mat_size);
    std::copy(start, input_it, std::back_inserter(matrix_args));
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
  Iterator input_it;
  const Iterator end;
};

static auto make_custom_type_constructor(std::vector<ast::variant>& args) {
  return create_custom_type_constructor{std::make_move_iterator(args.begin()),
                                        std::make_move_iterator(args.end())};
}

// Object that iterates over operations in IR blocks, and visits each operation. We recursively
// convert our basic control-flow-graph to a limited syntax tree that can be emitted in different
// languages.
struct ast_from_ir {
  ast_from_ir(const std::size_t value_width, const function_description& description)
      : value_width_(value_width),
        signature_{description.name(), description.return_value_type(), description.arguments()} {}

  ast::function_definition convert_function(const ir::block_ptr block) {
    process_block(block);

    std::vector<ast::variant> result;
    result.reserve(operations_.size() + 1);
    result.push_back(format_operation_count_comment());
    std::copy(std::make_move_iterator(operations_.begin()),
              std::make_move_iterator(operations_.end()), std::back_inserter(result));
    operations_.clear();
    return ast::function_definition(std::move(signature_), std::move(result));
  }

  // Sort and move all the provided assignments into `operations_`.
  void push_back_conditional_assignments(std::vector<ast::assign_temporary>&& assignments) {
    std::sort(assignments.begin(), assignments.end(),
              [](const auto& a, const auto& b) { return a.left < b.left; });
    std::copy(std::make_move_iterator(assignments.begin()),
              std::make_move_iterator(assignments.end()), std::back_inserter(operations_));
    assignments.clear();
  }

  // Given all the `ir::save` operations for a block, create the AST objects that represent
  // either return values, or writing to output arguments (and add them to operations_).
  void push_back_outputs(const ir::block_ptr block) {
    for (const ir::value_ptr value : block->operations) {
      if (!value->is_type<ir::save>()) {
        continue;
      }
      const ir::save& save = value->as_type<ir::save>();
      const output_key& key = save.key();

      std::vector<ast::variant> args{};
      args.reserve(value->num_operands());
      for (const ir::value_ptr v : value->operands()) {
        args.emplace_back(make_operation_argument(v));
      }

      if (key.usage == expression_usage::return_value) {
        WF_ASSERT(block->descendants.empty(), "Must be the final block");
        WF_ASSERT(signature_.return_type().has_value(), "Return type must be specified");
        ast::variant var = std::visit(
            [&](const auto& t) -> ast::variant { return make_custom_type_constructor(args)(t); },
            *signature_.return_type());
        emplace_operation<ast::return_object>(std::make_shared<ast::variant>(std::move(var)));
      } else {
        auto arg = signature_.argument_by_name(key.name);
        WF_ASSERT(arg.has_value(), "Argument missing from signature: {}", key.name);

        overloaded_visit(
            arg->type(),
            [&](scalar_type) {
              // TODO: Make variant_ptr a proper type so we can put the pointer allocation
              // internally.
              WF_ASSERT_EQUAL(1, args.size(), "");
              emplace_operation<ast::assign_output_scalar>(
                  *arg, std::make_shared<ast::variant>(std::move(args[0])));
            },
            [&](const matrix_type& mat) {
              emplace_operation<ast::assign_output_matrix>(*arg,
                                                           construct_matrix{mat, std::move(args)});
            },
            [&](const custom_type& custom) {
              construct_custom_type ctor = make_custom_type_constructor(args)(custom);
              emplace_operation<ast::assign_output_struct>(*arg, std::move(ctor));
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
  void push_back_conditional_output_declarations(const ir::block_ptr block) {
    for (const ir::value_ptr value : block->operations) {
      if (value->is_phi()) {
        const bool no_declaration =
            value->all_consumers_satisfy([](ir::value_ptr v) { return v->is_phi(); });
        if (no_declaration) {
          continue;
        }
        // We should declare this variable prior to entering the branch:
        emplace_operation<ast::declaration>(format_variable_name(value), value->numeric_type());
      }
    }
  }

  void process_block(const ir::block_ptr block) {
    if (non_traversable_blocks_.count(block)) {
      // Don't recurse too far - we are waiting on one of the ancestors of this block to get
      // processed.
      return;
    }
    WF_ASSERT(block->has_no_descendents() || !block->is_empty(),
              "Only the terminal block may be empty. block->name: {}", block->name);

    operations_.reserve(operations_.capacity() + block->operations.size());

    std::vector<ast::assign_temporary> phi_assignments{};
    phi_assignments.reserve(block->operations.size());

    for (const ir::value_ptr value : block->operations) {
      if (value->is_type<ir::save>()) {
        // Defer output values to the end of the block.
      } else if (value->is_phi()) {
        // Phi is not a real operation, we just use it to determine when branches should write to
        // variables declared before the if-else.
      } else if (value->is_type<ir::jump_condition>() || value->is_type<ir::output_required>()) {
        // These are placeholders and have no representation in the output code.
      } else {
        // Create the computation of the value:
        const ast::variant_ptr computed_value =
            std::make_shared<const ast::variant>(visit_value(value));

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
          emplace_operation<ast::declaration>(format_variable_name(value), value->numeric_type(),
                                              computed_value);
          rhs = std::make_shared<const ast::variant>(make_variable_ref(value));
        }

        // Here we write assignments to every conditional output that contains this value:
        for (ir::value_ptr consumer : phi_consumers) {
          phi_assignments.emplace_back(format_variable_name(consumer), rhs);
        }
      }
    }

    // Before the end of the block, push back outputs from this if-else branch:
    push_back_conditional_assignments(std::move(phi_assignments));

    // The last thing in the block is writing to output variables:
    push_back_outputs(block);

    // Recurse into the blocks that follow this one:
    handle_control_flow(block);
  }

  // Stash the current set of operations, and process a child block.
  // We return the nested block's operations (and pop our stash before returning).
  std::vector<ast::variant> process_nested_block(const ir::block_ptr block) {
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
  void handle_control_flow(ir::block_ptr block) {
    if (block->descendants.empty()) {
      // This is the terminal block - nothing to do.
      return;
    }

    const ir::value_ptr last_op = block->operations.back();
    if (!last_op->is_type<ir::jump_condition>()) {
      // just keep appending:
      WF_ASSERT_EQUAL(1, block->descendants.size());
      process_block(block->descendants.front());
    } else {
      WF_ASSERT(last_op->is_type<ir::jump_condition>());
      WF_ASSERT_EQUAL(2, block->descendants.size());

      // This over-counts a bit, since nested branches don't all run. We are just counting
      // if-statements, basically.
      operation_counts_[operation_count_label::branch]++;

      // Figure out where this if-else statement will terminate:
      const ir::block_ptr merge_point = find_merge_point(
          block->descendants[0], block->descendants[1], search_direction::downwards);
      non_traversable_blocks_.insert(merge_point);

      // Declare any variables that will be written in both the if and else blocks:
      push_back_conditional_output_declarations(merge_point);

      // Descend into both branches:
      std::vector<ast::variant> operations_true = process_nested_block(block->descendants[0]);

      // We have two kinds of branches. One for optionally-computed outputs, which only has
      // an if-branch. The other is for conditional logic in computations (where both if and
      // else branches are required).
      const ir::value_ptr condition = last_op->first_operand();
      if (condition->is_type<ir::output_required>()) {
        const ir::output_required& oreq = condition->as_type<ir::output_required>();

        // Create an optional-output assignment block
        auto arg_optional = signature_.argument_by_name(oreq.name());
        WF_ASSERT(arg_optional, "Missing argument: {}", oreq.name());
        emplace_operation<ast::optional_output_branch>(std::move(*arg_optional),
                                                       std::move(operations_true));
      } else {
        // Fill out operations in the else branch:
        std::vector<ast::variant> operations_false = process_nested_block(block->descendants[1]);

        // Create a conditional
        emplace_operation<ast::branch>(make_variable_ref(last_op->first_operand()),
                                       std::move(operations_true), std::move(operations_false));
      }

      non_traversable_blocks_.erase(merge_point);
      process_block(merge_point);
    }
  }

  // Convert the value integer to the formatted variable name that will appear in code.
  std::string format_variable_name(const ir::value_ptr val) const {
    return fmt::format("v{:0>{}}", val->name(), value_width_);
  }

  // Visit the operation type on `value`, and delegate to the appropriate method on this.
  ast::variant visit_value(ir::value_ptr value) {
    return std::visit(
        [this, value](const auto& op) -> ast::variant {
          // These types are placeholders, and don't directly appear in the ast output:
          using T = std::decay_t<decltype(op)>;
          using excluded_types =
              type_list<ir::jump_condition, ir::save, ir::cond, ir::phi, ir::output_required>;
          if constexpr (type_list_contains_type_v<T, excluded_types>) {
            throw type_error("Type cannot be converted to AST: {}", typeid(T).name());
          } else {
            return operator()(*value, op);
          }
        },
        value->value_op());
  }

  // Return true if the specified value should be written in-line instead of declared as a variable.
  // At present, only constants and casts of constants do not receive variable declarations.
  bool should_inline_constant(const ir::value_ptr val) const {
    return overloaded_visit(
        val->value_op(),
        [](const ir::load& load) {
          return load.is_type<integer_constant, float_constant, symbolic_constant>();
        },
        [&](const ir::cast&) { return should_inline_constant(val->first_operand()); },
        [](auto&&) constexpr { return false; });
  }

  // Create a `VariableRef` object with the name of the variable used to store `value`.
  ast::variable_ref make_variable_ref(const ir::value_ptr value) const {
    return ast::variable_ref{format_variable_name(value)};
  }

  // Check if the provided value is going to be placed in-line. If so, we just directly return
  // the converted `value`. If not, we assume a variable will be declared elsewhere and instead
  // return a reference to that.
  ast::variant make_operation_argument(const ir::value_ptr value) {
    if (should_inline_constant(value)) {
      return visit_value(value);
    }
    return make_variable_ref(value);
  }

  // Version of `make_operation_argument` that wraps the result in a shared ptr.
  ast::variant_ptr make_operation_argument_ptr(const ir::value_ptr val) {
    return std::make_shared<const ast::variant>(make_operation_argument(val));
  }

  // Push back operation of type `T` into `operations_`.
  template <typename T, typename... Args>
  void emplace_operation(Args&&... args) {
    operations_.emplace_back(T{std::forward<Args>(args)...});
  }

  ast::variant operator()(const ir::value& val, const ir::add&) {
    operation_counts_[operation_count_label::add]++;
    return ast::add{make_operation_argument_ptr(val[0]), make_operation_argument_ptr(val[1])};
  }

  ast::variant operator()(const ir::value& val, const ir::call_std_function& func) {
    operation_counts_[operation_count_label::call]++;

    std::vector<ast::variant> transformed_args{};
    transformed_args.reserve(val.num_operands());
    for (ir::value_ptr arg : val.operands()) {
      transformed_args.push_back(make_operation_argument(arg));
    }
    return ast::call{func.name(), std::move(transformed_args)};
  }

  ast::variant operator()(const ir::value& val, const ir::cast& cast) {
    return ast::cast{cast.destination_type(), val[0]->numeric_type(),
                     make_operation_argument_ptr(val[0])};
  }

  ast::variant operator()(const ir::value& val, const ir::compare& compare) {
    operation_counts_[operation_count_label::compare]++;
    return ast::compare{compare.operation(), make_operation_argument_ptr(val[0]),
                        make_operation_argument_ptr(val[1])};
  }

  ast::variant operator()(const ir::value& val, const ir::copy&) {
    return make_operation_argument(val.first_operand());
  }

  ast::variant operator()(const ir::value& val, const ir::div&) {
    operation_counts_[operation_count_label::divide]++;
    return ast::divide{make_operation_argument_ptr(val[0]), make_operation_argument_ptr(val[1])};
  }

  ast::variant operator()(const ir::value& val, const ir::mul&) {
    operation_counts_[operation_count_label::multiply]++;
    return ast::multiply{make_operation_argument_ptr(val[0]), make_operation_argument_ptr(val[1])};
  }

  ast::variant operator()(const ir::value& val, const ir::neg&) {
    operation_counts_[operation_count_label::negate]++;
    return ast::negate{make_operation_argument_ptr(val[0])};
  }

  ast::variant operator()(const named_variable& v) const { return ast::variable_ref{v.name()}; }

  ast::variant operator()(const scalar_type&, const argument& arg, std::size_t) const {
    return ast::get_argument{arg};
  }

  ast::variant operator()(const matrix_type& m, const argument& arg,
                          const std::size_t element_index) const {
    const auto [row, col] = m.compute_indices(element_index);
    return ast::get_matrix_element{std::make_shared<ast::variant>(ast::get_argument{arg}), row,
                                   col};
  }

  ast::variant operator()(const custom_type& c, const argument& arg,
                          const std::size_t element_index) const {
    const auto access_sequence = determine_access_sequence(c, element_index);

    ast::get_argument get_arg{arg};
    if (access_sequence.empty()) {
      return get_arg;
    }

    ast::variant prev = std::move(get_arg);
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

  ast::variant operator()(const function_argument_variable& a) const {
    const argument& arg = signature_.argument_by_index(a.arg_index());
    return std::visit([&](const auto& type) { return operator()(type, arg, a.element_index()); },
                      arg.type());
  }

  ast::variant operator()(const unique_variable& u) const {
    throw type_error("Cannot convert unique_variable to ast: {}", u.index());
  }

  ast::variant operator()(const ir::value&, const ir::load& load) {
    return std::visit(
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
          } else {  // std::is_same_v<T, variable>
            // inspect inner type of the variable
            return std::visit(*this, inner.identifier());
          }
        },
        load.variant());
  }

  static constexpr std::string_view string_from_operation_count_label(
      operation_count_label label) noexcept {
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

  ast::variant format_operation_count_comment() const {
    // Sort counts into order of the label itself:
    std::vector<std::pair<operation_count_label, std::size_t>> counts{operation_counts_.begin(),
                                                                      operation_counts_.end()};
    std::sort(counts.begin(), counts.end(),
              [](const auto& a, const auto& b) { return a.first < b.first; });

    std::string comment{"Operation counts:\n"};
    std::size_t total = 0;
    for (const auto& pair : counts) {
      fmt::format_to(std::back_inserter(comment), "{}: {}\n",
                     string_from_operation_count_label(pair.first), pair.second);
      total += pair.second;
    }
    fmt::format_to(std::back_inserter(comment), "total: {}", total);
    return ast::comment{std::move(comment)};
  }

 private:
  std::size_t value_width_;
  function_signature signature_;

  // Operations accrued in the current block.
  std::vector<ast::variant> operations_;

  // Blocks we can't process yet (pending processing of all their ancestors).
  std::unordered_set<ir::block_ptr> non_traversable_blocks_;

  // Operation counts
  std::unordered_map<operation_count_label, std::size_t, hash_struct<operation_count_label>>
      operation_counts_{};
};

function_definition create_ast(const wf::output_ir& ir, const function_description& description) {
  ast_from_ir converter{ir.value_print_width(), description};
  return converter.convert_function(ir.first_block());
}

}  // namespace wf::ast
