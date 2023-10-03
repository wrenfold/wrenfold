// Copyright 2023 Gareth Cross
#include "code_generation/ir_builder.h"

#include <deque>
#include <unordered_set>

#include "expressions/all_expressions.h"

namespace math {

struct ExprFromIrVisitor {
  explicit ExprFromIrVisitor(const std::unordered_map<ir::ValuePtr, Expr>& value_to_expression,
                             const std::unordered_map<std::string, bool>* output_arg_exists)
      : value_to_expression_(value_to_expression), output_arg_exists_(output_arg_exists) {}

  Expr operator()(const ir::Add&, const std::vector<ir::ValuePtr>& args) const {
    return MapValue(args[0]) + MapValue(args[1]);
  }

  Expr operator()(const ir::Mul&, const std::vector<ir::ValuePtr>& args) const {
    return MapValue(args[0]) * MapValue(args[1]);
  }

  Expr operator()(const ir::OutputRequired& output, const std::vector<ir::ValuePtr>&) const {
    ASSERT(output_arg_exists_, "Must have an output arg map to process `OutputRequired`");
    return output_arg_exists_->at(output.name) ? Constants::True : Constants::False;
  }

  Expr operator()(const ir::Pow&, const std::vector<ir::ValuePtr>& args) const {
    return Power::create(MapValue(args[0]), MapValue(args[1]));
  }

  Expr operator()(const ir::CallBuiltInFunction& func,
                  const std::vector<ir::ValuePtr>& args) const {
    Function::ContainerType container{};
    std::transform(args.begin(), args.end(), std::back_inserter(container),
                   [this](ir::ValuePtr v) { return MapValue(v); });
    return Function::create(func.name, std::move(container));
  }

  Expr operator()(const ir::Cast&, const std::vector<ir::ValuePtr>& args) const {
    return MapValue(args[0]);
  }

  Expr operator()(const ir::Cond&, const std::vector<ir::ValuePtr>& args) const {
    return where(MapValue(args[0]), MapValue(args[1]), MapValue(args[2]));
  }

  Expr operator()(const ir::Phi&, const std::vector<ir::ValuePtr>& args) const {
    ASSERT_EQUAL(2, args.size());

    // We find to find the condition for this jump:
    const ir::BlockPtr jump_block =
        find_merge_point(args.front()->parent(), args.back()->parent(), SearchDirection::Upwards);

    // Determine the condition:
    ASSERT(!jump_block->is_empty());

    const ir::ValuePtr jump_val = jump_block->operations.back();
    ASSERT(jump_val->is_type<ir::JumpCondition>());

    return where(MapValue(jump_val->first_operand()), MapValue(args[0]), MapValue(args[1]));
  }

  Expr operator()(const ir::Compare& cmp, const std::vector<ir::ValuePtr>& args) const {
    return Relational::create(cmp.operation, MapValue(args[0]), MapValue(args[1]));
  }

  Expr operator()(const ir::Copy&, const std::vector<ir::ValuePtr>& args) const {
    return MapValue(args[0]);
  }

  Expr operator()(const ir::Load& load, const std::vector<ir::ValuePtr>&) const {
    return load.expr;
  }

  Expr MapValue(const ir::ValuePtr& val) const {
    const auto arg_it = value_to_expression_.find(val);
    ASSERT(arg_it != value_to_expression_.end(), "Missing value: {}", val->name());
    return arg_it->second;
  }

  const std::unordered_map<ir::ValuePtr, Expr>& value_to_expression_;
  const std::unordered_map<std::string, bool>* output_arg_exists_;
};

std::unordered_map<OutputKey, std::vector<Expr>, hash_struct<OutputKey>>
create_output_expression_map(ir::BlockPtr starting_block,
                             const std::unordered_map<std::string, bool>* output_arg_exists) {
  std::unordered_map<ir::ValuePtr, Expr> value_to_expression{};
  value_to_expression.reserve(200);

  // Set of all visited blocks:
  std::unordered_set<ir::BlockPtr> completed;

  // Queue of pending blocks
  std::deque<ir::BlockPtr> queue;
  queue.emplace_back(starting_block);

  // Map from key to ordered output expressions:
  std::unordered_map<OutputKey, std::vector<Expr>, hash_struct<OutputKey>> output_map{};
  output_map.reserve(5);

  while (!queue.empty()) {
    // de-queue the next block
    const ir::BlockPtr block = queue.front();
    queue.pop_front();

    if (completed.count(block)) {
      continue;
    }
    completed.insert(block);

    const ExprFromIrVisitor visitor{value_to_expression, output_arg_exists};
    for (const ir::ValuePtr& code : block->operations) {
      // Visit the operation, and convert it to an expression.
      // We don't do anything w/ jumps, which don't actually translate to an output value directly.
      overloaded_visit(
          code->value_op(), [](const ir::JumpCondition&) {},
          [&](const ir::Save& save) {
            // Get all the output expressions for this output:
            std::vector<Expr> output_expressions{};
            output_expressions.reserve(code->num_operands());
            for (const ir::ValuePtr operand : code->operands()) {
              auto it = value_to_expression.find(operand);
              ASSERT(it != value_to_expression.end(), "Missing value: {}", operand->name());
              output_expressions.push_back(it->second);
            }
            output_map.emplace(save.key, std::move(output_expressions));
          },
          [&](const auto& op) {
            Expr expr = visitor(op, code->operands());
            value_to_expression.emplace(code, std::move(expr));
          });
    }

    // If all the ancestors of a block are done, we wire it jump.
    for (const ir::BlockPtr b : block->descendants) {
      const bool valid = std::all_of(b->ancestors.begin(), b->ancestors.end(),
                                     [&](auto blk) { return completed.count(blk) > 0; });
      if (valid) {
        queue.push_back(b);
      }
    }
  }
  return output_map;
}

}  // namespace math
