// Copyright 2023 Gareth Cross
#include "code_generation/ast.h"

#include <unordered_set>

#include "code_generation/ir_builder.h"
#include "code_generation/ir_types.h"
#include "expressions/all_expressions.h"
#include "visitor_impl.h"

namespace math {

inline void GetConditionalOutputs(const ir::ValuePtr v, std::vector<ir::ValuePtr>& outputs) {
  bool all_phi_consumers = true;
  for (ir::ValuePtr consumer : v->consumers()) {
    if (consumer->is_phi()) {
      GetConditionalOutputs(consumer, outputs);
    } else {
      all_phi_consumers = false;
    }
  }
  if (!all_phi_consumers) {
    // There are no consumers of this phi function that not phi functions, so we need to declare
    // a variable for this value.
    outputs.push_back(v);
  }
}

struct AstBuilder {
  AstBuilder(std::size_t value_width, const ast::FunctionSignature& signature)
      : value_width_(value_width), signature_(signature) {}

  std::vector<ast::Variant> create_function(ir::BlockPtr block) {
    process_block(block);
    std::vector<ast::Variant> result = std::move(operations_);
    operations_.clear();
    return result;
  }

  void process_block(ir::BlockPtr block) {
    ASSERT(!block->is_empty());
    if (stop_set_.count(block)) {
      return;
    }
    operations_.reserve(operations_.capacity() + block->operations.size());

    // We want to place any `ir::Save` operations at the end before conditional logic, so separate
    // those:
    std::vector<ir::ValuePtr> save_values{};
    save_values.reserve(block->operations.size());

    std::vector<ast::AssignTemporary> phi_assignments{};
    phi_assignments.reserve(block->operations.size());

    for (const ir::ValuePtr value : block->operations) {
      if (value->is_type<ir::Save>()) {
        save_values.push_back(value);
      } else if (value->is_phi()) {
        // Do nothing.
      } else if (value->is_type<ir::JumpCondition>() || value->is_type<ir::OutputRequired>()) {
        // Do nothing.
      } else {
        std::vector<ir::ValuePtr> conditional_outputs{};
        bool all_phi_consumers = true;
        for (ir::ValuePtr consumer : value->consumers()) {
          if (consumer->is_phi()) {
            GetConditionalOutputs(consumer, conditional_outputs);
          } else {
            all_phi_consumers = false;
          }
        }

        ast::VariantPtr rhs;
        if (!all_phi_consumers || conditional_outputs.size() > 1) {
          if (should_inline_constant(value)) {
            rhs = make_argument_ptr(value);
          } else {
            emplace_operation<ast::Declaration>(format_temporary(value), value->determine_type(),
                                                visit_and_make_ptr(value));
            rhs = make_variable_ref_ptr(value);
          }
        } else {
          rhs = visit_and_make_ptr(value);
        }

        for (ir::ValuePtr consumer : conditional_outputs) {
          phi_assignments.emplace_back(format_temporary(consumer), rhs);
        }
      }
    }

    std::sort(phi_assignments.begin(), phi_assignments.end(),
              [](const auto& a, const auto& b) { return a.left < b.left; });
    std::copy(std::make_move_iterator(phi_assignments.begin()),
              std::make_move_iterator(phi_assignments.end()), std::back_inserter(operations_));
    phi_assignments.clear();

    // insert outputs
    for (const ir::ValuePtr save_value : save_values) {
      const ir::Save& save = save_value->as_type<ir::Save>();
      const OutputKey& key = save.key;

      std::vector<ast::Variant> args{};
      args.reserve(save_value->num_operands());
      for (const ir::ValuePtr v : save_value->operands()) {
        args.emplace_back(make_variable_ref(v));
      }

      if (key.usage == ExpressionUsage::ReturnValue) {
        ASSERT(block->descendants.empty());  //  This must be the final block.
        emplace_operation<ast::ConstructReturnValue>(signature_.return_value.value(),
                                                     std::move(args));
      } else {
        emplace_operation<ast::AssignOutputArgument>(signature_.get_argument(key.name),
                                                     std::move(args));
      }
    }

    if (block->descendants.empty()) {
      return;
    }

    ir::ValuePtr last_op = block->operations.back();
    if (!last_op->is_type<ir::JumpCondition>()) {
      // just keep appending:
      ASSERT_EQUAL(1, block->descendants.size());
      process_block(block->descendants.front());
    } else {
      ASSERT(last_op->is_type<ir::JumpCondition>());
      ASSERT_EQUAL(2, block->descendants.size());

      // Find phi functions:
      const ir::BlockPtr merge_point = find_merge_point(
          block->descendants[0], block->descendants[1], SearchDirection::Downwards);
      stop_set_.insert(merge_point);

      for (ir::ValuePtr maybe_phi : merge_point->operations) {
        if (maybe_phi->is_phi()) {
          const bool no_declaration =
              maybe_phi->all_consumers_satisfy([](ir::ValuePtr v) { return v->is_phi(); });
          if (no_declaration) {
            continue;
          }
          // We should declare this variable prior to entering the branch:
          emplace_operation<ast::Declaration>(format_temporary(maybe_phi),
                                              maybe_phi->determine_type());
        }
      }

      // move aside the contents of this block, as we are about to descend the branches:
      std::vector<ast::Variant> operations_true = process_nested_block(block->descendants[0]);
      std::vector<ast::Variant> operations_false = process_nested_block(block->descendants[1]);

      ir::ValuePtr condition = last_op->first_operand();
      if (condition->is_type<ir::OutputRequired>()) {
        ASSERT(operations_false.empty());
        const ir::OutputRequired& oreq = condition->as_type<ir::OutputRequired>();
        // Create an optional-output assignment block
        emplace_operation<ast::OptionalOutputBranch>(signature_.get_argument(oreq.name),
                                                     std::move(operations_true));
      } else {
        // Create a conditional
        emplace_operation<ast::Branch>(ast::VariableRef{format_temporary(last_op->first_operand())},
                                       std::move(operations_true), std::move(operations_false));
      }

      stop_set_.erase(merge_point);
      process_block(merge_point);
    }
  }

  // Stash the current set of operations, and process a child block.
  // We return the nested block's operations (and pop our stash before returning).
  std::vector<ast::Variant> process_nested_block(const ir::BlockPtr b) {
    // Move aside operations of the current block temporarily:
    std::vector<ast::Variant> operations_stashed = std::move(operations_);
    operations_.clear();

    // Process the block, writing to operations_ in the process.
    process_block(b);

    // Take the accrued operations and put them in `operations_stashed`.
    // In the process, we reset operations for the calling block.
    std::swap(operations_, operations_stashed);
    return operations_stashed;
  }

  std::string format_temporary(const ir::Value& val) const {
    return fmt::format("v{:0>{}}", val.name(), value_width_);
  }

  std::string format_temporary(const ir::ValuePtr val) const { return format_temporary(*val); }

  ast::Variant visit_value(ir::ValuePtr val) {
    return std::visit(
        [this, &val](const auto& op) -> ast::Variant {
          // These types should never get converted to AST:
          using T = std::decay_t<decltype(op)>;
          using ExcludedTypes =
              type_list<ir::JumpCondition, ir::Save, ir::Cond, ir::Phi, ir::OutputRequired>;
          if constexpr (list_contains_type_v<T, ExcludedTypes>) {
            throw TypeError("Type cannot be converted to AST: {}", typeid(T).name());
          } else {
            return this->operator()(*val, op);
          }
        },
        val->value_op());
  }

  ast::VariantPtr visit_and_make_ptr(ir::ValuePtr val) {
    return std::make_shared<const ast::Variant>(visit_value(val));
  }

  // Return true if the specified value should be inlined instead of declared as a variable.
  bool should_inline_constant(const ir::ValuePtr val) const {
    return overloaded_visit(
        val->value_op(),
        [](const ir::Load& load) { return load.expr.is_type<Integer, Float, Constant>(); },
        [&](const ir::Cast&) { return should_inline_constant(val->first_operand()); },
        [](auto&&) constexpr { return false; });
  }

  ast::Variant make_variable_ref(const ir::ValuePtr val) const {
    return ast::VariableRef{format_temporary(val)};
  }

  ast::VariantPtr make_variable_ref_ptr(const ir::ValuePtr val) const {
    return std::make_shared<const ast::Variant>(make_variable_ref(val));
  }

  ast::Variant make_argument(const ir::ValuePtr val) {
    if (should_inline_constant(val)) {
      return visit_value(val);
    }
    return make_variable_ref(val);
  }

  ast::VariantPtr make_argument_ptr(const ir::ValuePtr val) {
    return std::make_shared<const ast::Variant>(make_argument(val));
  }

  template <typename T, typename... Args>
  void emplace_operation(Args&&... args) {
    operations_.emplace_back(T{std::forward<Args>(args)...});
  }

  ast::Variant operator()(const ir::Value& val, const ir::Add&) {
    return ast::Add{make_argument_ptr(val[0]), make_argument_ptr(val[1])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::CallBuiltInFunction& func) {
    std::vector<ast::Variant> transformed_args{};
    transformed_args.reserve(val.num_operands());
    for (ir::ValuePtr arg : val.operands()) {
      transformed_args.push_back(make_argument(arg));
    }
    return ast::Call{func.name, std::move(transformed_args)};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Cast& cast) {
    return ast::Cast{cast.destination_type, val.determine_type(), make_argument_ptr(val[0])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Compare& compare) {
    return ast::Compare{compare.operation, make_argument_ptr(val[0]), make_argument_ptr(val[1])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Copy&) {
    return make_argument(val.first_operand());
  }

  ast::Variant operator()(const ir::Value& val, const ir::Mul&) {
    return ast::Multiply{make_argument_ptr(val[0]), make_argument_ptr(val[1])};
  }

  ast::Variant operator()(const ir::Value&, const ir::Load& load) {
    return Visit(load.expr, [this](const auto& inner) -> ast::Variant {
      using T = std::decay_t<decltype(inner)>;
      if constexpr (std::is_same_v<T, Integer>) {
        return ast::IntegerConstant{inner.get_value()};
      } else if constexpr (std::is_same_v<T, Float>) {
        return ast::FloatConstant{static_cast<Float>(inner).get_value()};
      } else if constexpr (std::is_same_v<T, Rational>) {
        return ast::FloatConstant{static_cast<Float>(inner).get_value()};
      } else if constexpr (std::is_same_v<T, Variable>) {
        return ast::VariableRef{static_cast<const Variable&>(inner).name()};
      } else if constexpr (std::is_same_v<T, FunctionArgument>) {
        const auto element_index = static_cast<index_t>(inner.element_index());
        return ast::InputValue{signature_.arguments[inner.arg_index()], element_index};
      } else {
        throw TypeError("Invalid type in code generation expression: {}", T::NameStr);
      }
    });
  }

  ast::Variant operator()(const ir::Value& val, const ir::Pow&) {
    return ast::Call{BuiltInFunctionName::Pow, make_argument(val[0]), make_argument(val[1])};
  }

 private:
  std::size_t value_width_;
  const ast::FunctionSignature& signature_;

  std::vector<ast::Variant> operations_;
  std::unordered_set<ir::BlockPtr> stop_set_;
};

namespace ast {
std::vector<ast::Variant> create_ast(const math::OutputIr& ir, const FunctionSignature& signature) {
  AstBuilder builder(ir.value_print_width(), signature);
  return builder.create_function(ir.first_block());
}
}  // namespace ast
}  // namespace math
