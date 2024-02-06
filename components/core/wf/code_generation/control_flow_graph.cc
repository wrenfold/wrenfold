// Copyright 2023 Gareth Cross
#include "wf/code_generation/control_flow_graph.h"

#include <algorithm>
#include <deque>
#include <unordered_set>
#include <vector>

#include "wf/code_generation/ir_control_flow_converter.h"
#include "wf/code_generation/ir_form_visitor.h"
#include "wf/common_visitors.h"
#include "wf/expressions/all_expressions.h"
#include "wf/hashing.h"
#include "wf/visit.h"

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
  for (const ir::value_ptr& code : block->operations) {
    // Then see if this operation already exists in the map:
    if (auto [it, was_inserted] = table.insert(code); !was_inserted) {
      // Propagate the copy:
      if (code->is_consumed_by_phi()) {
        // If this value feeds a phi function, we need to keep it, but turn it into a copy:
        code->set_value_op(ir::copy{}, (*it)->type(), *it);
      } else {
        code->replace_with(*it);
      }
    }
  }
}

control_flow_graph::control_flow_graph(const std::vector<expression_group>& groups) {
  blocks_.push_back(std::make_unique<ir::block>(0));

  // First pass where we count occurrences of some sub-expressions:
  mul_add_count_visitor count_visitor{};
  for (const expression_group& group : groups) {
    count_visitor.count_group_expressions(group);
  }

  ir_form_visitor visitor{*this, std::move(count_visitor).take_counts()};
  for (const expression_group& group : groups) {
    // Transform expressions into Values
    ir::value::operands_container group_values{};
    group_values.reserve(group.expressions.size());
    std::transform(group.expressions.begin(), group.expressions.end(),
                   std::back_inserter(group_values), [&](const scalar_expr& expr) {
                     // TODO: Allow returning other types - derive the numeric type from the
                     // group.
                     const ir::value_ptr output =
                         visitor.apply_output_value(expr, code_numeric_type::floating_point);
                     return output;
                   });

    // Then create a sink to consume these values, the `save` operation is the sink:
    create_operation(values_, first_block(), ir::save{group.key}, ir::void_type{},
                     std::move(group_values));
  }

  eliminate_duplicates();
}

control_flow_graph control_flow_graph::convert_conditionals_to_control_flow() && {
  return ir_control_flow_converter{std::move(*this)}.convert();
}

void control_flow_graph::eliminate_duplicates() {
  value_table table{};
  table.reserve(values_.size());
  local_value_numbering(first_block(), table);
  strip_unused_values();
}

// ReSharper disable once CppMemberFunctionMayBeConst
void control_flow_graph::strip_unused_values() {
  // Somewhat lazy: Reverse the operations so that we can use forward iterator, then reverse back.
  const ir::block_ptr block = first_block();
  std::reverse(block->operations.begin(), block->operations.end());
  const auto new_end = std::remove_if(block->operations.begin(), block->operations.end(),
                                      [&](const ir::value_ptr v) {
                                        if (v->is_unused()) {
                                          v->remove();
                                          return true;
                                        }
                                        return false;
                                      });
  block->operations.erase(new_end, block->operations.end());
  std::reverse(block->operations.begin(), block->operations.end());
}

std::string control_flow_graph::to_string() const {
  const std::size_t width = value_print_width();
  std::string output{};

  // size of the left column, so we can align things
  const std::size_t left_column_width = fmt::formatted_size("  v{:0>{}} <- ", 0, width);

  for (const std::unique_ptr<ir::block>& block : blocks_) {
    fmt::format_to(std::back_inserter(output), "{}:", ir::block_ptr{block.get()});
    output += "\n";

    for (const ir::value_ptr& code : block->operations) {
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

    if (!block->descendants.empty()) {
      output.append(left_column_width, ' ');
      fmt::format_to(std::back_inserter(output), "jump {}\n", fmt::join(block->descendants, ", "));
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
  return std::accumulate(
      blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
      [](const std::size_t total, const ir::block::unique_ptr& b) {
        return total +
               std::count_if(b->operations.begin(), b->operations.end(), [](const ir::value_ptr v) {
                 return !v->is_type<ir::jump_condition>() && !v->is_type<ir::save>() &&
                        !v->is_type<ir::load>() && !v->is_type<ir::copy>();
               });
      });
}

std::size_t control_flow_graph::num_conditionals() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](const std::size_t total, const ir::block::unique_ptr& b) {
                           return total + std::count_if(b->operations.begin(), b->operations.end(),
                                                        [](const ir::value_ptr v) {
                                                          return v->is_type<ir::jump_condition>() ||
                                                                 v->is_type<ir::cond>();
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
