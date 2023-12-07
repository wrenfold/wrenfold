// Copyright 2023 Gareth Cross
#include "wf/code_generation/ir_builder.h"
#include "wf/code_generation/ir_types.h"
#include "wf/expressions/all_expressions.h"

#include <deque>
#include <unordered_set>

namespace wf {

// Convert the IR operations back to expressions.
// This is supported so we can do round-trip tests.
struct expression_from_ir_visitor {
  explicit expression_from_ir_visitor(
      const std::unordered_map<ir::value_ptr, Expr>& value_to_expression,
      std::unordered_map<std::string, bool>&& output_arg_exists)
      : value_to_expression_(value_to_expression),
        output_arg_exists_(std::move(output_arg_exists)) {}

  Expr operator()(const ir::add&, const std::vector<ir::value_ptr>& args) const {
    return map_value(args[0]) + map_value(args[1]);
  }

  Expr operator()(const ir::mul&, const std::vector<ir::value_ptr>& args) const {
    return map_value(args[0]) * map_value(args[1]);
  }

  Expr operator()(const ir::output_required& output, const std::vector<ir::value_ptr>&) const {
    return output_arg_exists_.at(output.name()) ? constants::boolean_true
                                                : constants::boolean_false;
  }

  static constexpr built_in_function built_in_function_from_standard_library_function(
      std_math_function func) {
    switch (func) {
      case std_math_function::cos:
        return built_in_function::cos;
      case std_math_function::sin:
        return built_in_function::sin;
      case std_math_function::tan:
        return built_in_function::tan;
      case std_math_function::acos:
        return built_in_function::arccos;
      case std_math_function::asin:
        return built_in_function::arcsin;
      case std_math_function::atan:
        return built_in_function::arctan;
      case std_math_function::log:
        return built_in_function::ln;
      case std_math_function::abs:
        return built_in_function::abs;
      case std_math_function::signum:
        return built_in_function::signum;
      case std_math_function::atan2:
        return built_in_function::arctan2;
      default:
        // Other cases handled by the assertion below.
        break;
    }
    throw assertion_error("Invalid enum value: {}", string_from_standard_library_function(func));
  }

  Expr operator()(const ir::call_std_function& func, const std::vector<ir::value_ptr>& args) const {
    function::container_type container{};
    std::transform(args.begin(), args.end(), std::back_inserter(container),
                   [this](ir::value_ptr v) { return map_value(v); });

    if (func.name() == std_math_function::powi || func.name() == std_math_function::powf) {
      return pow(container[0], container[1]);
    } else if (func.name() == std_math_function::sqrt) {
      static const Expr one_half = constants::one / 2;
      return pow(container[0], one_half);
    } else {
      return function::create(built_in_function_from_standard_library_function(func.name()),
                              std::move(container));
    }
  }

  Expr operator()(const ir::cast&, const std::vector<ir::value_ptr>& args) const {
    WF_ASSERT(!args.empty());
    return map_value(args[0]);
  }

  Expr operator()(const ir::cond&, const std::vector<ir::value_ptr>& args) const {
    return where(map_value(args[0]), map_value(args[1]), map_value(args[2]));
  }

  Expr operator()(const ir::compare& cmp, const std::vector<ir::value_ptr>& args) const {
    return relational::create(cmp.operation(), map_value(args[0]), map_value(args[1]));
  }

  Expr operator()(const ir::copy&, const std::vector<ir::value_ptr>& args) const {
    return map_value(args[0]);
  }

  Expr operator()(const ir::div&, const std::vector<ir::value_ptr>& args) const {
    return map_value(args[0]) / map_value(args[1]);
  }

  Expr operator()(const ir::load& load, const std::vector<ir::value_ptr>&) const {
    return std::visit(
        [](const auto& expression) {
          using T = std::decay_t<decltype(expression)>;
          return make_expr<T>(expression);
        },
        load.variant());
  }

  Expr operator()(const ir::neg&, const std::vector<ir::value_ptr>& args) const {
    return -map_value(args.front());
  }

  Expr operator()(const ir::phi&, const std::vector<ir::value_ptr>& args) const {
    WF_ASSERT_EQUAL(2, args.size());

    // We find to find the condition for this jump:
    const ir::block_ptr jump_block =
        find_merge_point(args.front()->parent(), args.back()->parent(), search_direction::upwards);

    // Determine the condition:
    WF_ASSERT(!jump_block->is_empty());

    const ir::value_ptr jump_val = jump_block->operations.back();
    WF_ASSERT(jump_val->is_type<ir::jump_condition>());

    return where(map_value(jump_val->first_operand()), map_value(args[0]), map_value(args[1]));
  }

  Expr map_value(ir::value_ptr value) const {
    const auto arg_it = value_to_expression_.find(value);
    WF_ASSERT(arg_it != value_to_expression_.end(), "Missing value: {}", value->name());
    return arg_it->second;
  }

  const std::unordered_map<ir::value_ptr, Expr>& value_to_expression_;
  const std::unordered_map<std::string, bool> output_arg_exists_;
};

std::unordered_map<output_key, std::vector<Expr>, hash_struct<output_key>>
create_output_expression_map(ir::block_ptr starting_block,
                             std::unordered_map<std::string, bool>&& output_arg_exists) {
  std::unordered_map<ir::value_ptr, Expr> value_to_expression{};
  value_to_expression.reserve(200);

  // Set of all visited blocks:
  std::unordered_set<ir::block_ptr> completed;

  // Queue of pending blocks
  std::deque<ir::block_ptr> queue;
  queue.emplace_back(starting_block);

  // Map from key to ordered output expressions:
  std::unordered_map<output_key, std::vector<Expr>, hash_struct<output_key>> output_map{};
  output_map.reserve(5);

  const expression_from_ir_visitor visitor{value_to_expression, std::move(output_arg_exists)};
  while (!queue.empty()) {
    // de-queue the next block
    const ir::block_ptr block = queue.front();
    queue.pop_front();

    if (completed.count(block)) {
      continue;
    }
    completed.insert(block);

    for (const ir::value_ptr& code : block->operations) {
      // Visit the operation, and convert it to an expression.
      // We don't do anything w/ jumps - they do not actually translate to an output value directly.
      overloaded_visit(
          code->value_op(), [](const ir::jump_condition&) constexpr {},
          [&](const ir::save& save) {
            // Get all the output expressions for this output:
            std::vector<Expr> output_expressions{};
            output_expressions.reserve(code->num_operands());
            for (const ir::value_ptr operand : code->operands()) {
              auto it = value_to_expression.find(operand);
              WF_ASSERT(it != value_to_expression.end(), "Missing value: {}", operand->name());
              output_expressions.push_back(it->second);
            }
            output_map.emplace(save.key(), std::move(output_expressions));
          },
          [&](const auto& op) {
            Expr expr = visitor(op, code->operands());
            value_to_expression.emplace(code, std::move(expr));
          });
    }

    // If all the ancestors of a block are done, we can queue it:
    for (const ir::block_ptr b : block->descendants) {
      const bool valid = std::all_of(b->ancestors.begin(), b->ancestors.end(),
                                     [&](auto blk) { return completed.count(blk) > 0; });
      if (valid) {
        queue.push_back(b);
      }
    }
  }
  return output_map;
}

}  // namespace wf
