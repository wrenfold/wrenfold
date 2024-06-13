// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/expr_from_ir.h"

#include <deque>
#include <unordered_set>

#include "wf/code_generation/control_flow_graph.h"
#include "wf/code_generation/ir_types.h"
#include "wf/expressions/all_expressions.h"
#include "wf/utility/overloaded_visit.h"
#include "wf/utility/scoped_trace.h"

namespace wf {

// Convert the IR operations back to expressions.
// This is supported so we can do round-trip tests.
class expression_from_ir_visitor {
 public:
  explicit expression_from_ir_visitor(std::unordered_map<std::string, bool>&& output_arg_exists,
                                      const bool use_intermediate_values)
      : output_arg_exists_(std::move(output_arg_exists)),
        use_intermediate_values_(use_intermediate_values) {
    value_to_expression_.reserve(200);
    if (use_intermediate_values_) {
      scalar_variables_.reserve(50);
      scalar_variable_order_.reserve(50);
    }
  }

  scalar_expr operator()(const ir::add&, const ir::value::operands_container& args) const {
    const auto args_transformed = transform_map<std::vector>(
        args, [&](const ir::const_value_ptr v) { return map_value<scalar_expr>(v); });
    return addition::from_operands(args_transformed);
  }

  scalar_expr operator()(const ir::mul&, const ir::value::operands_container& args) const {
    const auto args_transformed = transform_map<std::vector>(
        args, [&](const ir::const_value_ptr v) { return map_value<scalar_expr>(v); });
    return multiplication::from_operands(args_transformed);
  }

  boolean_expr operator()(const ir::output_required& output,
                          const ir::value::operands_container&) const {
    return output_arg_exists_.at(output.name()) ? constants::boolean_true
                                                : constants::boolean_false;
  }

  static constexpr built_in_function built_in_function_from_standard_library_function(
      const std_math_function func) {
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
        return built_in_function::log;
      case std_math_function::abs:
        return built_in_function::abs;
      case std_math_function::signum:
        return built_in_function::signum;
      case std_math_function::floor:
        return built_in_function::floor;
      case std_math_function::atan2:
        return built_in_function::arctan2;
      case std_math_function::cosh:
        return built_in_function::cosh;
      case std_math_function::sinh:
        return built_in_function::sinh;
      case std_math_function::tanh:
        return built_in_function::tanh;
      case std_math_function::acosh:
        return built_in_function::arccosh;
      case std_math_function::asinh:
        return built_in_function::arcsinh;
      case std_math_function::atanh:
        return built_in_function::arctanh;
      case std_math_function::sqrt:
      case std_math_function::powi:
      case std_math_function::powf:
      default:
        // Other cases handled by the assertion below.
        break;
    }
    WF_ASSERT_ALWAYS("Invalid enum value: {}", string_from_standard_library_function(func));
  }

  any_expression operator()(const ir::call_external_function& func,
                            const ir::value::operands_container& args) const {
    auto args_converted = transform_map<external_function_invocation::container_type>(
        args, [this](const ir::const_value_ptr val) { return map_value_to_variant(val); });

    compound_expr invocation{std::in_place_type_t<external_function_invocation>{}, func.function(),
                             std::move(args_converted)};

    if (std::holds_alternative<scalar_type>(func.return_type())) {
      return compound_expression_element::create(std::move(invocation), 0);
    } else {
      // There will be a specific ir value that does the element access.
      return invocation;
    }
  }

  scalar_expr operator()(const ir::call_std_function& func,
                         const ir::value::operands_container& args) const {
    auto container = transform_map<function::container_type>(
        args, [this](const ir::const_value_ptr v) { return map_value<scalar_expr>(v); });

    if (func.name() == std_math_function::powi || func.name() == std_math_function::powf) {
      return pow(container[0], container[1]);
    } else if (func.name() == std_math_function::sqrt) {
      static const scalar_expr one_half = constants::one / 2;
      return pow(container[0], one_half);
    } else {
      return function::create(built_in_function_from_standard_library_function(func.name()),
                              std::move(container));
    }
  }

  scalar_expr operator()(const ir::cast&, const ir::value::operands_container& args) const {
    WF_ASSERT(!args.empty());
    if (args[0]->numeric_type() == code_numeric_type::boolean) {
      return iverson(map_value<boolean_expr>(args[0]));
    } else {
      return map_value<scalar_expr>(args[0]);
    }
  }

  boolean_expr operator()(const ir::compare& cmp, const ir::value::operands_container& args) const {
    return relational::create(cmp.operation(), map_value<scalar_expr>(args[0]),
                              map_value<scalar_expr>(args[1]));
  }

  any_expression operator()(const ir::construct& construct,
                            const ir::value::operands_container& args) const {
    std::vector<scalar_expr> args_converted = transform_map<std::vector>(
        args, [this](const ir::const_value_ptr v) { return map_value<scalar_expr>(v); });
    return overloaded_visit(
        construct.type(),
        [&](const matrix_type& mat) -> any_expression {
          return matrix_expr::create(mat.rows(), mat.cols(), std::move(args_converted));
        },
        [&](const custom_type& custom) -> any_expression {
          return custom_type_construction::create(custom, std::move(args_converted));
        });
  }

  scalar_expr operator()(const ir::cond&, const ir::value::operands_container& args) const {
    return where(map_value<boolean_expr>(args[0]), map_value<scalar_expr>(args[1]),
                 map_value<scalar_expr>(args[2]));
  }

  scalar_expr operator()(const ir::copy&, const ir::value::operands_container& args) const {
    return map_value<scalar_expr>(args[0]);
  }

  scalar_expr operator()(const ir::div&, const ir::value::operands_container& args) const {
    return map_value<scalar_expr>(args[0]) / map_value<scalar_expr>(args[1]);
  }

  scalar_expr operator()(const ir::get& get, const ir::value::operands_container& args) const {
    return scalar_expr{std::in_place_type_t<compound_expression_element>{},
                       map_value<compound_expr>(args.front()), get.index()};
  }

  any_expression operator()(const ir::load& load, const ir::value::operands_container&) const {
    return std::visit(
        [](const auto& expression) -> any_expression {
          using T = std::decay_t<decltype(expression)>;
          if constexpr (type_list_contains_v<T, scalar_expr::types>) {
            return scalar_expr{expression};
          } else if constexpr (type_list_contains_v<T, boolean_expr::types>) {
            return boolean_expr{expression};
          } else {
            return compound_expr{expression};
          }
        },
        load.variant());
  }

  scalar_expr operator()(const ir::neg&, const ir::value::operands_container& args) const {
    return -map_value<scalar_expr>(args.front());
  }

  scalar_expr operator()(const ir::phi&, const ir::value::operands_container& args) const {
    WF_ASSERT_EQ(2, args.size());

    // We find to find the condition for this jump:
    const ir::const_block_ptr jump_block = find_merge_point(
        args.front()->parent(), args.back()->parent(), ir::search_direction::upwards);

    // Determine the condition:
    const ir::const_value_ptr jump_val = jump_block->last_operation();
    WF_ASSERT(jump_val->is_op<ir::jump_condition>());

    return where(map_value<boolean_expr>(jump_val->first_operand()),
                 map_value<scalar_expr>(args[0]), map_value<scalar_expr>(args[1]));
  }

  const any_expression& map_value_to_variant(const ir::const_value_ptr value) const {
    const auto arg_it = value_to_expression_.find(value);
    WF_ASSERT(arg_it != value_to_expression_.end(), "Missing value: {}", value->name());
    return arg_it->second;
  }

  template <typename T>
  const T& map_value(const ir::const_value_ptr value) const {
    if constexpr (std::is_same_v<T, scalar_expr>) {
      // Don't insert loads, since these
      if (use_intermediate_values_) {
        if (const auto var_it = scalar_variables_.find(value); var_it != scalar_variables_.end()) {
          return var_it->second;
        }
      }
    }
    const auto& var = map_value_to_variant(value);
    const T* result = std::get_if<T>(&var);
    WF_ASSERT(result, "Variant does not contain type `{}`", typeid(T).name());
    return *result;
  }

  std::vector<std::tuple<scalar_expr, scalar_expr>> get_intermediate_values() const {
    return transform_map<std::vector>(
        scalar_variable_order_, [this](const ir::const_value_ptr val) {
          const auto variable_it = scalar_variables_.find(val);
          WF_ASSERT(variable_it != scalar_variables_.end());

          const auto variable_value_it = value_to_expression_.find(val);
          WF_ASSERT(variable_value_it != value_to_expression_.end());

          const scalar_expr* const as_scalar = std::get_if<scalar_expr>(&variable_value_it->second);
          WF_ASSERT(as_scalar != nullptr);

          return std::make_tuple(variable_it->second, *as_scalar);
        });
  }

  template <typename T>
  void process(const ir::const_value_ptr key, const T& op,
               const ir::value::operands_container& args) {
    const auto [it, was_inserted] = value_to_expression_.emplace(key, operator()(op, args));
    if (use_intermediate_values_ && was_inserted &&
        std::holds_alternative<scalar_expr>(it->second)) {
      // Make a variable expression for this as well:
      scalar_variables_.emplace(
          key, make_expr<variable>(fmt::format("v{}", key->name()), number_set::unknown));
      scalar_variable_order_.push_back(key);
    }
  }

 private:
  // Map from value to rebuilt expression:
  std::unordered_map<ir::const_value_ptr, any_expression> value_to_expression_;

  // If `use_intermediate_values_` is true, we leave immediately children of expressions as
  // variables. This map stores the variable expressions for each name.
  std::unordered_map<ir::const_value_ptr, scalar_expr> scalar_variables_;

  // Store the order in which `scalar_variables_` was filled.
  std::vector<ir::const_value_ptr> scalar_variable_order_;

  // Map that indicates whether a given optional output should be computed.
  const std::unordered_map<std::string, bool> output_arg_exists_;

  // If false, we recursively substitute expressions into each other as we reconstitute
  // the expression tree - effectively rebuilding the original input tree completely.
  // If true, we capture each intermediate expression and produce sub-expressions that
  // reference each other via variables (effectively making a CSE-style output).
  bool use_intermediate_values_;
};

rebuilt_expressions rebuild_expression_tree(const ir::const_block_ptr starting_block,
                                            std::unordered_map<std::string, bool> output_arg_exists,
                                            const bool use_intermediate_values) {
  WF_FUNCTION_TRACE();

  // Set of all visited blocks:
  std::unordered_set<ir::const_block_ptr> completed;

  // Queue of pending blocks
  std::deque<ir::const_block_ptr> queue;
  queue.emplace_back(starting_block);

  // Map from key to ordered output expressions:
  rebuilt_expressions result{};
  result.output_expressions.reserve(5);

  expression_from_ir_visitor visitor{std::move(output_arg_exists), use_intermediate_values};
  while (!queue.empty()) {
    // de-queue the next block
    const ir::const_block_ptr block = queue.front();
    queue.pop_front();

    if (completed.count(block)) {
      continue;
    }
    completed.insert(block);

    for (const ir::const_value_ptr code : block->operations()) {
      // Visit the operation, and convert it to an expression.
      // We don't do anything w/ jumps - they do not actually translate to an output value directly.
      overloaded_visit(
          code->value_op(), [](const ir::jump_condition&) constexpr {},
          [&](const ir::save& save) {
            // Get all the output expressions for this output:
            std::vector<scalar_expr> output_expressions = transform_map<std::vector>(
                code->operands(),
                [&visitor](const ir::value_ptr v) { return visitor.map_value<scalar_expr>(v); });

            result.output_expressions.emplace(save.key(), std::move(output_expressions));
          },
          [&](const auto& op) { visitor.process(code, op, code->operands()); });
    }

    // If all the ancestors of a block are done, we can queue it:
    for (const ir::block_ptr b : block->descendants()) {
      const auto& b_ancestors = b->ancestors();
      if (const bool valid = all_of(
              b_ancestors, [&](const ir::const_block_ptr blk) { return completed.count(blk) > 0; });
          valid) {
        queue.push_back(b);
      }
    }
  }

  if (use_intermediate_values) {
    result.intermediate_values = visitor.get_intermediate_values();
  }
  return result;
}

}  // namespace wf
