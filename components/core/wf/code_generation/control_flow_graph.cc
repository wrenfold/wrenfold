// Copyright 2023 Gareth Cross
#include "wf/code_generation/control_flow_graph.h"

#include <algorithm>
#include <deque>
#include <unordered_set>
#include <vector>

#include "wf/code_generation/ir_control_flow_converter.h"
#include "wf/code_generation/ir_form_visitor.h"
#include "wf/common_visitors.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"
#include "wf/hashing.h"
#include "wf/scoped_trace.h"

namespace wf {
namespace ir {

template <typename Container>
static void format_operands(std::string& output, const Container& operands,
                            const std::size_t width) {
  for (auto it = operands.begin(); it != operands.end(); ++it) {
    const ir::value_ptr& val = *it;
    fmt::format_to(std::back_inserter(output), "v{:0>{}}", val->name(), width);
    if (std::next(it) != operands.end()) {
      output += ", ";
    }
  }
}

template <typename T>
struct format_op_args_struct {
  template <typename Container>
  void operator()(std::string& output, const T&, const Container& operands,
                  const std::size_t width) {
    format_operands(output, operands, width);
  }
};

template <>
struct format_op_args_struct<ir::call_external_function> {
  template <typename Container>
  void operator()(std::string& output, const ir::call_external_function& func,
                  const Container& operands, const std::size_t width) {
    output += func.function().name();
    if (!operands.empty()) {
      output += ", ";
      format_operands(output, operands, width);
    }
  }
};

template <>
struct format_op_args_struct<ir::construct> {
  void operator()(std::string& output, const matrix_type& m) const {
    fmt::format_to(std::back_inserter(output), "matrix<{}, {}>", m.rows(), m.cols());
  }

  void operator()(std::string& output, const custom_type& c) const { output += c.name(); }

  template <typename Container>
  void operator()(std::string& output, const ir::construct& construct, const Container& operands,
                  const std::size_t width) {
    std::visit([&](const auto& type) { operator()(output, type); }, construct.type());
    if (!operands.empty()) {
      output += ", ";
      format_operands(output, operands, width);
    }
  }
};

template <>
struct format_op_args_struct<ir::get> {
  template <typename Container>
  void operator()(std::string& output, const ir::get& get, const Container& operands,
                  const std::size_t width) {
    format_operands(output, operands, width);
    fmt::format_to(std::back_inserter(output), ", {}", get.index());
  }
};

template <>
struct format_op_args_struct<ir::load> {
  template <typename Container>
  void operator()(std::string& output, const ir::load& load, const Container&, const std::size_t) {
    plain_formatter formatter{};
    std::visit(formatter, load.variant());
    output += formatter.take_output();
  }
};

template <>
struct format_op_args_struct<ir::output_required> {
  template <typename Container>
  void operator()(std::string& output, const ir::output_required& oreq, const Container&,
                  const std::size_t) {
    fmt::format_to(std::back_inserter(output), "{}", oreq.name());
  }
};

template <typename Container>
void format_op_args(std::string& output, const ir::operation& op, const Container& operands,
                    const std::size_t width) {
  std::visit(
      [&](const auto& op) {
        using T = std::decay_t<decltype(op)>;
        format_op_args_struct<T>{}(output, op, operands, width);
      },
      op);
}

}  // namespace ir

// Test two values for equality. The operation must be the same type, and the operands must be
// identical. This does not recursively test the operands for equality - the pointer themselves
// must match.
struct value_equality_struct {
  bool operator()(const ir::value_ptr& a, const ir::value_ptr& b) const {
    if (const bool ops_match = are_identical(a->value_op(), b->value_op()); !ops_match) {
      return false;
    }
    return a->operands_match(b);
  }
};

// A hash-set of values:
using value_table =
    std::unordered_set<ir::value_ptr, hash_struct<ir::value_ptr>, value_equality_struct>;

// Eliminate duplicates in `block`, using existing values stored in `table`.
inline void local_value_numbering(const ir::block_ptr block, value_table& table) {
  WF_FUNCTION_TRACE();
  for (const ir::value_ptr val : block->operations()) {
    // Then see if this operation already exists in the map:
    if (auto [it, was_inserted] = table.insert(val); !was_inserted) {
      // Propagate the copy:
      if (val->is_consumed_by_phi()) {
        // If this value feeds a phi function, we need to keep it, but turn it into a copy:
        val->set_operation(ir::copy{}, (*it)->type(), *it);
      } else {
        val->replace_with(*it);
      }
    }
  }
}

// Visitor that recursively sorts additions and multiplications into lexicographical order.
class sort_expression_order_visitor {
 public:
  template <typename T, typename X>
  X operator()(const T& concrete, const X& abstract) {
    if constexpr (std::is_same_v<T, multiplication> || std::is_same_v<T, addition>) {
      auto args = transform_map<typename T::container_type>(concrete, *this);
      std::sort(args.begin(), args.end(), expression_order_struct{});
      // Pass `no_sort` to indicate that args should be left in this sorted order.
      return make_expr<T>(typename T::no_sort{}, std::move(args));
    } else if constexpr (!T::is_leaf_node) {
      return concrete.map_children(*this);
    }
    return abstract;
  }

  template <typename X,
            typename = enable_if_contains_type_t<X, scalar_expr, boolean_expr, matrix_expr>>
  X operator()(const X& expr) {
    return cache_.get_or_insert(expr, [this](const X& x) { return visit(x, *this); });
  }

  compound_expr operator()(const compound_expr& expr) {
    return cache_.get_or_insert(
        expr, [this](const compound_expr& x) { return map_compound_expressions(x, *this); });
  }

 private:
  expression_cache<> cache_;
};

// We pass `groups` by copy so we can replace the expressions with sorted versions below:
control_flow_graph::control_flow_graph(std::vector<expression_group> groups) {
  WF_FUNCTION_TRACE();
  blocks_.push_back(std::make_unique<ir::block>(0));

  // First pass where we sort things into a canonical order, and count operations.
  sort_expression_order_visitor order_visitor{};
  mul_add_count_visitor count_visitor{};
  for (expression_group& group : groups) {
    std::transform(group.expressions.begin(), group.expressions.end(), group.expressions.begin(),
                   [&](const scalar_expr& x) { return order_visitor(x); });
    count_visitor.count_group_expressions(group);
  }

  ir_form_visitor visitor{*this, std::move(count_visitor).take_counts()};
  for (const expression_group& group : groups) {
    // Transform expressions into Values
    auto group_values = transform_map<ir::value::operands_container>(
        group.expressions, [&](const scalar_expr& expr) {
          // TODO: Allow returning other types - derive numeric type from the group.
          return visitor.apply_output_value(expr, code_numeric_type::floating_point);
        });

    // Then create a sink to consume these values, the `save` operation is the sink:
    create_operation(values_, first_block(), ir::save{group.key}, ir::void_type{},
                     std::move(group_values));
  }

  eliminate_duplicates();
}

control_flow_graph control_flow_graph::convert_conditionals_to_control_flow() && {
  WF_FUNCTION_TRACE();
  return ir_control_flow_converter{std::move(*this)}.convert();
}

// ReSharper disable once CppMemberFunctionMayBeConst
void control_flow_graph::eliminate_duplicates() {
  WF_FUNCTION_TRACE();
  value_table table{};
  table.reserve(values_.size());
  local_value_numbering(first_block(), table);

  // Remove anything that does not have a consumer.
  // We do this in reverse order so that eliminating one useless value can eliminate its inputs.
  first_block()->reverse_remove_operation_if([&](const ir::value_ptr v) {
    if (v->is_unused()) {
      v->remove();
      return true;
    }
    return false;
  });
}

std::string control_flow_graph::to_string() const {
  const std::size_t width = value_print_width();
  std::string output{};

  // size of the left column, so we can align things
  const std::size_t left_column_width = fmt::formatted_size("  v{:0>{}} <- ", 0, width);

  for (const std::unique_ptr<ir::block>& block : blocks_) {
    fmt::format_to(std::back_inserter(output), "{}:", ir::block_ptr{block.get()});
    output += "\n";

    for (const ir::value_ptr& code : block->operations()) {
      // Print the value name:
      if (code->num_consumers() > 0) {
        fmt::format_to(std::back_inserter(output), "  v{:0>{}} <- ", code->name(), width);
      } else {
        output.append(left_column_width, ' ');
      }

      // Print the instruction name:
      constexpr int operation_width = 4;
      fmt::format_to(std::back_inserter(output), "{:>{}} ",
                     std::visit([](const auto& op) { return op.to_string(); }, code->value_op()),
                     operation_width);
      format_op_args(output, code->value_op(), code->operands(), width);
      output += "\n";
    }

    if (!block->has_no_descendents()) {
      output.append(left_column_width, ' ');
      fmt::format_to(std::back_inserter(output), "jump {}\n",
                     fmt::join(block->descendants(), ", "));
    }
  }
  if (!output.empty()) {
    output.pop_back();
  }
  return output;
}

constexpr std::size_t compute_print_width(std::size_t num_assignments) noexcept {
  std::size_t width = 1;
  for (; num_assignments > 0; ++width) {
    num_assignments /= 10;
  }
  return width;
}

std::size_t control_flow_graph::value_print_width() const {
  const uint32_t highest_value_name = values_.empty() ? 0 : values_.back()->name();
  return compute_print_width(highest_value_name);
}

std::size_t control_flow_graph::num_operations() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](const std::size_t total, const ir::block::unique_ptr& b) {
                           return total + b->count_operation([](const ir::value_ptr v) {
                             return !v->is_op<ir::jump_condition, ir::save, ir::load, ir::copy>();
                           });
                         });
}

std::size_t control_flow_graph::num_conditionals() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](const std::size_t total, const ir::block::unique_ptr& b) {
                           return total + b->count_operation([](const ir::value_ptr v) {
                             return v->is_op<ir::jump_condition, ir::cond>();
                           });
                         });
}

ir::block_ptr control_flow_graph::first_block() const {
  const auto it =
      std::find_if(blocks_.begin(), blocks_.end(),
                   [](const ir::block::unique_ptr& block) { return block->has_no_ancestors(); });
  WF_ASSERT(it != blocks_.end(), "There must be an entry block");
  return ir::block_ptr{it->get()};
}

std::size_t control_flow_graph::count_function(const std_math_function enum_value) const noexcept {
  return count_operation(
      [&](const ir::call_std_function func) { return func.name() == enum_value; });
}

}  // namespace wf
