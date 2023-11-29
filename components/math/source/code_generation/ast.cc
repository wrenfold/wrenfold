// Copyright 2023 Gareth Cross
#include "code_generation/ast.h"

#include <unordered_set>

#include "code_generation/ir_builder.h"
#include "code_generation/ir_types.h"
#include "expressions/all_expressions.h"
#include "visitor_impl.h"

namespace math {

// Given a starting value `v`, find any downstream conditionals values that equal this value.
inline void find_conditional_output_values(const ir::ValuePtr v, const bool top_level_invocation,
                                           std::vector<ir::ValuePtr>& outputs) {
  bool all_phi_consumers = true;
  for (ir::ValuePtr consumer : v->consumers()) {
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

// Object that iterates over operations in IR blocks, and visits each operation. We recursively
// convert our basic control-flow-graph to a limited syntax tree that can be emitted in different
// languages.
struct AstBuilder {
  AstBuilder(std::size_t value_width, const ast::FunctionSignature& signature)
      : value_width_(value_width), signature_(signature) {}

  std::vector<ast::Variant> create_function(ir::BlockPtr block) {
    process_block(block);
    std::vector<ast::Variant> result = std::move(operations_);
    operations_.clear();
    return result;
  }

  // Sort and move all the provided assignments into `operations_`.
  void push_back_conditional_assignments(std::vector<ast::AssignTemporary>&& assignments) {
    std::sort(assignments.begin(), assignments.end(),
              [](const auto& a, const auto& b) { return a.left < b.left; });
    std::copy(std::make_move_iterator(assignments.begin()),
              std::make_move_iterator(assignments.end()), std::back_inserter(operations_));
    assignments.clear();
  }

  // Given all the `ir::Save` operations for a block, create the AST objects that represent
  // either return values, or writing to output arguments (and add them to operations_).
  void push_back_outputs(const ir::BlockPtr block) {
    for (const ir::ValuePtr value : block->operations) {
      if (!value->is_type<ir::Save>()) {
        continue;
      }
      const ir::Save& save = value->as_type<ir::Save>();
      const OutputKey& key = save.key;

      std::vector<ast::Variant> args{};
      args.reserve(value->num_operands());
      for (const ir::ValuePtr v : value->operands()) {
        args.emplace_back(make_operation_argument(v));
      }

      if (key.usage == ExpressionUsage::ReturnValue) {
        ZEN_ASSERT(block->descendants.empty(), "Must be the final block");
        emplace_operation<ast::ConstructReturnValue>(signature_.return_value.value(),
                                                     std::move(args));
      } else {
        emplace_operation<ast::AssignOutputArgument>(signature_.get_argument(key.name),
                                                     std::move(args));
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
  void push_back_conditional_output_declarations(ir::BlockPtr block) {
    for (ir::ValuePtr value : block->operations) {
      if (value->is_phi()) {
        const bool no_declaration =
            value->all_consumers_satisfy([](ir::ValuePtr v) { return v->is_phi(); });
        if (no_declaration) {
          continue;
        }
        // We should declare this variable prior to entering the branch:
        emplace_operation<ast::Declaration>(format_variable_name(value), value->numeric_type());
      }
    }
  }

  void process_block(ir::BlockPtr block) {
    ZEN_ASSERT(!block->is_empty());
    if (non_traversable_blocks_.count(block)) {
      // Don't recurse too far - we are waiting on one of the ancestors of this block to get
      // processed.
      return;
    }
    operations_.reserve(operations_.capacity() + block->operations.size());

    std::vector<ast::AssignTemporary> phi_assignments{};
    phi_assignments.reserve(block->operations.size());

    for (const ir::ValuePtr value : block->operations) {
      if (value->is_type<ir::Save>()) {
        // Defer output values to the end of the block.
      } else if (value->is_phi()) {
        // Phi is not a real operation, we just use it to determine when branches should write to
        // variables declared before the if-else.
      } else if (value->is_type<ir::JumpCondition>() || value->is_type<ir::OutputRequired>()) {
        // These are placeholders and have no representation in the output code.
      } else {
        // Create the computation of the value:
        const ast::VariantPtr computed_value =
            std::make_shared<const ast::Variant>(visit_value(value));

        // Find any downstream phi values that are equal to this value:
        std::vector<ir::ValuePtr> phi_consumers{};
        find_conditional_output_values(value, true, phi_consumers);

        // Does this value have any consumers that are not the outputs of conditional branches?
        const bool any_none_phi_consumers =
            !value->all_consumers_satisfy([](ir::ValuePtr c) { return c->is_phi(); });

        // If we have a single non-phi consumer, or more than one phi consumer, we declare a
        // variable.
        const bool needs_declaration =
            !should_inline_constant(value) && (any_none_phi_consumers || phi_consumers.size() > 1);

        ast::VariantPtr rhs = computed_value;
        if (needs_declaration) {
          // We are going to declare a temporary for this value:
          emplace_operation<ast::Declaration>(format_variable_name(value), value->numeric_type(),
                                              computed_value);
          rhs = std::make_shared<const ast::Variant>(make_variable_ref(value));
        }

        // Here we write assignments to every conditional output that contains this value:
        for (ir::ValuePtr consumer : phi_consumers) {
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
  std::vector<ast::Variant> process_nested_block(const ir::BlockPtr block) {
    // Move aside operations of the current block temporarily:
    std::vector<ast::Variant> operations_stashed = std::move(operations_);
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
  void handle_control_flow(ir::BlockPtr block) {
    if (block->descendants.empty()) {
      // This is the terminal block - nothing to do.
      return;
    }

    const ir::ValuePtr last_op = block->operations.back();
    if (!last_op->is_type<ir::JumpCondition>()) {
      // just keep appending:
      ZEN_ASSERT_EQUAL(1, block->descendants.size());
      process_block(block->descendants.front());
    } else {
      ZEN_ASSERT(last_op->is_type<ir::JumpCondition>());
      ZEN_ASSERT_EQUAL(2, block->descendants.size());

      // Figure out where this if-else statement will terminate:
      const ir::BlockPtr merge_point = find_merge_point(
          block->descendants[0], block->descendants[1], SearchDirection::Downwards);
      non_traversable_blocks_.insert(merge_point);

      // Declare any variables that will be written in both the if and else blocks:
      push_back_conditional_output_declarations(merge_point);

      // Descend into both branches:
      std::vector<ast::Variant> operations_true = process_nested_block(block->descendants[0]);

      // We have two kinds of branches. One for optionally-computed outputs, which only has
      // an if-branch. The other is for conditional logic in computations (where both if and
      // else branches are required).
      const ir::ValuePtr condition = last_op->first_operand();
      if (condition->is_type<ir::OutputRequired>()) {
        const ir::OutputRequired& oreq = condition->as_type<ir::OutputRequired>();

        // Create an optional-output assignment block
        emplace_operation<ast::OptionalOutputBranch>(signature_.get_argument(oreq.name),
                                                     std::move(operations_true));
      } else {
        // Fill out operations in the else branch:
        std::vector<ast::Variant> operations_false = process_nested_block(block->descendants[1]);

        // Create a conditional
        emplace_operation<ast::Branch>(make_variable_ref(last_op->first_operand()),
                                       std::move(operations_true), std::move(operations_false));
      }

      non_traversable_blocks_.erase(merge_point);
      process_block(merge_point);
    }
  }

  // Convert the value integer to the formatted variable name that will appear in code.
  std::string format_variable_name(const ir::ValuePtr val) const {
    return fmt::format("v{:0>{}}", val->name(), value_width_);
  }

  // Visit the operation type on `value`, and delegate to the appropriate method on this.
  ast::Variant visit_value(ir::ValuePtr value) {
    return std::visit(
        [this, value](const auto& op) -> ast::Variant {
          // These types are placeholders, and don't directly appear in the ast output:
          using T = std::decay_t<decltype(op)>;
          using excluded_types =
              type_list<ir::JumpCondition, ir::Save, ir::Cond, ir::Phi, ir::OutputRequired>;
          if constexpr (type_list_contains_type_v<T, excluded_types>) {
            throw TypeError("Type cannot be converted to AST: {}", typeid(T).name());
          } else {
            return operator()(*value, op);
          }
        },
        value->value_op());
  }

  // Return true if the specified value should be written in-line instead of declared as a variable.
  // At present, only constants and casts of constants do not receive variable declarations.
  bool should_inline_constant(const ir::ValuePtr val) const {
    return overloaded_visit(
        val->value_op(),
        [](const ir::Load& load) { return load.is_type<Integer, Float, Constant>(); },
        [&](const ir::Cast&) { return should_inline_constant(val->first_operand()); },
        [](auto&&) constexpr { return false; });
  }

  // Create a `VariableRef` object with the name of the variable used to store `value`.
  ast::VariableRef make_variable_ref(const ir::ValuePtr value) const {
    return ast::VariableRef{format_variable_name(value)};
  }

  // Check if the provided value is going to be placed in-line. If so, we just directly return
  // the converted `value`. If not, we assume a variable will be declared elsewhere and instead
  // return a reference to that.
  ast::Variant make_operation_argument(const ir::ValuePtr value) {
    if (should_inline_constant(value)) {
      return visit_value(value);
    }
    return make_variable_ref(value);
  }

  // Version of `make_operation_argument` that wraps the result in a shared ptr.
  ast::VariantPtr make_operation_argument_ptr(const ir::ValuePtr val) {
    return std::make_shared<const ast::Variant>(make_operation_argument(val));
  }

  // Push back operation of type `T` into `operations_`.
  template <typename T, typename... Args>
  void emplace_operation(Args&&... args) {
    operations_.emplace_back(T{std::forward<Args>(args)...});
  }

  ast::Variant operator()(const ir::Value& val, const ir::Add&) {
    return ast::Add{make_operation_argument_ptr(val[0]), make_operation_argument_ptr(val[1])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::CallStdFunction& func) {
    std::vector<ast::Variant> transformed_args{};
    transformed_args.reserve(val.num_operands());
    for (ir::ValuePtr arg : val.operands()) {
      transformed_args.push_back(make_operation_argument(arg));
    }
    return ast::Call{func.name, std::move(transformed_args)};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Cast& cast) {
    return ast::Cast{cast.destination_type, val.numeric_type(),
                     make_operation_argument_ptr(val[0])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Compare& compare) {
    return ast::Compare{compare.operation, make_operation_argument_ptr(val[0]),
                        make_operation_argument_ptr(val[1])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Copy&) {
    return make_operation_argument(val.first_operand());
  }

  ast::Variant operator()(const ir::Value& val, const ir::Div&) {
    return ast::Divide{make_operation_argument_ptr(val[0]), make_operation_argument_ptr(val[1])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Mul&) {
    return ast::Multiply{make_operation_argument_ptr(val[0]), make_operation_argument_ptr(val[1])};
  }

  ast::Variant operator()(const NamedVariable& v) const { return ast::VariableRef{v.name()}; }

  ast::Variant operator()(const FuncArgVariable& a) const {
    const auto element_index = static_cast<index_t>(a.element_index());
    return ast::InputValue{signature_.arguments.at(a.arg_index()), element_index};
  }

  ast::Variant operator()(const UniqueVariable& u) const {
    throw TypeError("Cannot convert UniqueVariable to ast: {}", u.index());
  }

  ast::Variant operator()(const ir::Value&, const ir::Load& load) {
    return std::visit(
        [this](const auto& inner) -> ast::Variant {
          using T = std::decay_t<decltype(inner)>;
          if constexpr (std::is_same_v<T, Constant>) {
            return ast::SpecialConstant{inner.name()};
          } else if constexpr (std::is_same_v<T, Integer>) {
            return ast::IntegerConstant{inner.get_value()};
          } else if constexpr (std::is_same_v<T, Float>) {
            return ast::FloatConstant{static_cast<Float>(inner).get_value()};
          } else if constexpr (std::is_same_v<T, Rational>) {
            return ast::FloatConstant{static_cast<Float>(inner).get_value()};
          } else if constexpr (std::is_same_v<T, Variable>) {
            // inspect inner type of the variable
            return std::visit(*this, inner.identifier());
          }
        },
        load.variant);
  }

 private:
  std::size_t value_width_;
  const ast::FunctionSignature& signature_;

  // Operations accrued in the current block.
  std::vector<ast::Variant> operations_;

  // Blocks we can't process yet (pending processing of all their ancestors).
  std::unordered_set<ir::BlockPtr> non_traversable_blocks_;
};

namespace ast {
std::vector<ast::Variant> create_ast(const math::OutputIr& ir, const FunctionSignature& signature) {
  AstBuilder builder(ir.value_print_width(), signature);
  return builder.create_function(ir.first_block());
}
}  // namespace ast
}  // namespace math
