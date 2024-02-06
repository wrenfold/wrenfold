// Copyright 2023 Gareth Cross
#include "wf/code_generation/flat_ir.h"

#include <algorithm>
#include <deque>
#include <unordered_map>
#include <unordered_set>
#include <vector>

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

constexpr std::size_t compute_print_width(std::size_t num_assignments) {
  std::size_t width = 1;
  for (; num_assignments > 0; ++width) {
    num_assignments /= 10;
  }
  return width;
}

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

inline bool is_countable_operation(ir::value_ptr v) {
  return !v->is_type<ir::jump_condition>() && !v->is_type<ir::save>() && !v->is_type<ir::load>() &&
         !v->is_type<ir::copy>();
}

inline bool is_conditional(ir::value_ptr v) {
  return v->is_type<ir::jump_condition>() || v->is_type<ir::cond>();
}

template <typename Container, typename OutputIterator>
static void reverse_copy_save_value_ptrs(const Container& container, expression_usage usage,
                                         OutputIterator output_iterator) {
  for (auto it = container.rbegin(); it != container.rend(); ++it) {
    const ir::value_ptr val{it->get()};
    if (const ir::save* save = std::get_if<ir::save>(&val->value_op()); save != nullptr) {
      if (save->key().usage == usage) {
        *output_iterator = val;
      }
    }
  }
}

// Extract all `save` operations (placeholders for the final output values) from `flat_values`
// in reverse order. Return values and require outputs are placed into a queue, and optional
// outputs are returned as a separate vector.
static auto get_reverse_ordered_output_values(
    const std::vector<ir::value::unique_ptr>& flat_values) {
  // We need to build up an initial queue of all the outputs that are required - these
  // will be processed first. Optional outputs are generated after.
  std::deque<ir::value_ptr> required_outputs_queue{};
  std::vector<ir::value_ptr> optional_outputs{};

  reverse_copy_save_value_ptrs(flat_values, expression_usage::return_value,
                               std::back_inserter(required_outputs_queue));

  reverse_copy_save_value_ptrs(flat_values, expression_usage::output_argument,
                               std::back_inserter(required_outputs_queue));

  reverse_copy_save_value_ptrs(flat_values, expression_usage::optional_output_argument,
                               std::back_inserter(optional_outputs));

  return std::make_tuple(std::move(required_outputs_queue), std::move(optional_outputs));
}

class ir_converter {
 public:
  control_flow_graph convert() && {
    // Conversion will modify `output.values_`, so shallow-copy the outputs first:
    auto [required_outputs_queue, optional_outputs] = get_reverse_ordered_output_values(values_);

    // Insert computations for required output values:
    std::vector<ir::value_ptr> deferred_values{};
    ir::block_ptr next_block =
        process(std::move(required_outputs_queue), create_block(), deferred_values);

    // Should be nothing deferred yet:
    WF_ASSERT(deferred_values.empty(), "deferred_values = [{}]", fmt::join(deferred_values, ", "));

    // Traverse optional outputs:
    for (ir::value_ptr v : optional_outputs) {
      const ir::save& save = v->as_type<ir::save>();
      const output_key& key = save.key();
      WF_ASSERT(key.usage == expression_usage::optional_output_argument, "Usage: {}",
                string_from_expression_usage(key.usage));

      const ir::block_ptr left_block_exit = create_block();
      left_block_exit->add_descendant(next_block);

      // Insert block to evaluate if this output is required:
      const ir::block_ptr jump_block = create_block();

      // Operation that evaluates whether this argument is required:
      const ir::value_ptr jump_condition =
          create_operation(values_, jump_block, ir::output_required{key.name}, ir::void_type{});

      // Either we go into `left_block` and compute the arg outputs, or we skip to `next_block`:
      create_operation(values_, jump_block, ir::jump_condition{}, ir::void_type{}, jump_condition);
      jump_block->add_descendant(left_block_exit);
      jump_block->add_descendant(next_block);

      std::deque<ir::value_ptr> queued_left = {v};
      process(std::move(queued_left), left_block_exit, deferred_values);

      std::deque<ir::value_ptr> queue{deferred_values.begin(), deferred_values.end()};
      deferred_values.clear();
      next_block = process(std::move(queue), jump_block, deferred_values);
      WF_ASSERT(deferred_values.empty(), "deferred_values = [{}]",
                fmt::join(deferred_values, ", "));
    }

    std::deque<ir::value_ptr> queue;
    std::copy(deferred_values.begin(), deferred_values.end(), std::back_inserter(queue));
    deferred_values.clear();

    process(std::move(queue), next_block, deferred_values);
    WF_ASSERT(deferred_values.empty(), "deferred_values = [{}]", fmt::join(deferred_values, ", "));

    // There should only be one start block:
    WF_ASSERT_EQUAL(
        1,
        std::count_if(blocks_.begin(), blocks_.end(),
                      [](const ir::block::unique_ptr& block) { return block->has_no_ancestors(); }),
        "Must be only one entry block");

    // The process above sometimes introduces pointless copies, which we remove now:
    eliminate_useless_copies();

    // Some of the input values may no longer be required, so nuke those.
    discard_unused_input_values();

    return control_flow_graph{std::move(blocks_), std::move(values_)};
  }

  // True if node `v` has been visited.
  bool is_visited(ir::value_ptr v) const { return visited_.count(v) > 0; }

  // True if all downstream consumers of `v` have been visited.
  bool all_consumers_visited(ir::value_ptr val) const {
    return val->all_consumers_satisfy([this](ir::value_ptr v) { return is_visited(v); });
  }

  // Queue any operands of `v` whose consumers have all been visited.
  void queue_operands(std::deque<ir::value_ptr>& queue, const ir::value_ptr v) const {
    for (ir::value_ptr val : v->operands()) {
      if (!is_visited(val) && all_consumers_visited(val)) {
        queue.push_back(val);
      }
    }
  }

  // Return true if `parent_block` is executed on all paths through `test_block`.
  static bool parent_is_on_all_paths_through_block(ir::block_ptr test_block,
                                                   ir::block_ptr parent_block) noexcept {
    if (test_block == parent_block) {
      return true;
    }
    return !test_block->ancestors.empty() &&
           std::all_of(test_block->ancestors.begin(), test_block->ancestors.end(),
                       [&](ir::block_ptr b) {
                         return parent_is_on_all_paths_through_block(b, parent_block);
                       });
  }

  auto process_non_conditionals(std::deque<ir::value_ptr>& queue,
                                std::vector<ir::value_ptr>& deferred,
                                const ir::block_ptr output_block) {
    // This will contain any conditionals we queue (conditionals whose consumers have all been
    // processed). They are processed outside of this method.
    std::vector<ir::value_ptr> queued_conditionals{};
    queued_conditionals.reserve(10);

    // This will contain all the operations that can be processed without hitting
    // a blocking conditional. Because we traverse the graph backwards, these are in reverse order.
    // We flip things back at the end of this function.
    std::vector<ir::value_ptr> output_reversed{};
    output_reversed.reserve(queue.size());

    while (!queue.empty()) {
      ir::value_ptr top = queue.front();
      queue.pop_front();
      if (is_visited(top)) {
        continue;
      }
      WF_ASSERT(all_consumers_visited(top), "Not all consumers have been visited: {}", top);

      // Check if this block is a valid place to insert this value. This will be the case
      // if `output_block` is on all paths through the downstream consumer blocks. This check
      // ensures we don't write the computation of a value into a scope that isn't visible by all
      // other scopes that need this value.
      const bool is_valid_to_insert = top->all_consumers_satisfy([&](ir::value_ptr consumer) {
        return parent_is_on_all_paths_through_block(consumer->parent(), output_block);
      });
      if (!is_valid_to_insert) {
        deferred.push_back(top);
        continue;
      }

      if (top->is_type<ir::cond>()) {
        // Defer conditionals to be processed together later:
        queued_conditionals.push_back(top);
        continue;
      }

      // Put it into the output, then queue its operands.
      top->set_parent(output_block);
      output_reversed.push_back(top);
      visited_.insert(top);
      queue_operands(queue, top);
    }

    // Flip things back when inserting into the actual block operations:
    output_block->operations.insert(output_block->operations.begin(), output_reversed.rbegin(),
                                    output_reversed.rend());
    return queued_conditionals;
  }

  ir::block_ptr process(std::deque<ir::value_ptr> queue, const ir::block_ptr output_block,
                        std::vector<ir::value_ptr>& deferred) {
    std::vector<ir::value_ptr> queued_conditionals =
        process_non_conditionals(queue, deferred, output_block);
    if (queued_conditionals.empty()) {
      return output_block;
    }

    const auto [condition, grouped_conditionals] =
        select_conditionals_to_group(queued_conditionals);

    WF_ASSERT(
        std::is_sorted(grouped_conditionals.begin(), grouped_conditionals.end(),
                       [](ir::value_ptr a, ir::value_ptr b) { return a->name() > b->name(); }),
        "Should be sorted: {}", fmt::join(grouped_conditionals, ","));

    const ir::block_ptr left_block_tail = create_block();
    const ir::block_ptr right_block_tail = create_block();

    std::deque<ir::value_ptr> queue_left{};
    std::deque<ir::value_ptr> queue_right{};
    for (ir::value_ptr v : grouped_conditionals) {
      // Turn conditionals into phi functions. We insert copies in the left and right blocks so that
      // arguments to the phi functions are computed on the branched code path, even if the
      // computation is just a copy. These copies can be eliminated later, in some cases.
      const ir::value_ptr copy_left = create_operation(values_, left_block_tail, ir::copy{},
                                                       v->operator[](1)->type(), v->operator[](1));
      const ir::value_ptr copy_right = create_operation(values_, right_block_tail, ir::copy{},
                                                        v->operator[](2)->type(), v->operator[](2));

      v->set_value_op(ir::phi{}, copy_left->type(), copy_left, copy_right);
      v->set_parent(output_block);
      visited_.insert(v);
      visited_.insert(copy_left);
      visited_.insert(copy_right);
      queue_operands(queue_left, copy_left);
      queue_operands(queue_right, copy_right);
    }

    // Insert the phi functions at the top of the block:
    output_block->operations.insert(output_block->operations.begin(), grouped_conditionals.rbegin(),
                                    grouped_conditionals.rend());

    // Save the ancestor vector before modifying it by creating jumps:
    const std::vector<ir::block_ptr> previous_ancestors = output_block->ancestors;

    left_block_tail->add_descendant(output_block);
    right_block_tail->add_descendant(output_block);

    const ir::block_ptr jump_block = create_block();
    const ir::value_ptr jump_condition =
        create_operation(values_, jump_block, ir::jump_condition{}, ir::void_type{}, condition);
    visited_.insert(jump_condition);

    jump_block->add_descendant(left_block_tail);
    jump_block->add_descendant(right_block_tail);

    // Any blocks that jumped to `output_block` should now jump to `jump_block` instead:
    for (const ir::block_ptr ancestor : previous_ancestors) {
      // If this block is our ancestor, it must contain a jump at the end:
      WF_ASSERT(!ancestor->operations.empty() && !ancestor->descendants.empty(),
                "block cannot be empty, must contain a jump");
      ancestor->replace_descendant(output_block, jump_block);
    }

    // Process left and right sides:
    std::vector<ir::value_ptr> process_later;
    process(std::move(queue_left), left_block_tail, process_later);
    process(std::move(queue_right), right_block_tail, process_later);

    // Queue the nodes we deferred
    queue.clear();
    if (all_consumers_visited(condition)) {
      queue.push_back(condition);
    }
    queue.insert(queue.end(), process_later.begin(), process_later.end());
    queue.insert(queue.end(), queued_conditionals.begin(), queued_conditionals.end());

    return process(std::move(queue), jump_block, deferred);
  }

  // In the process of converting, we inserted copies to satisfy the phi functions.
  // In some cases, these copies are just duplicating values computed in the same scope.
  void eliminate_useless_copies() {
    for (const auto& block : blocks_) {
      const auto new_end =
          std::remove_if(block->operations.begin(), block->operations.end(), [&](ir::value_ptr v) {
            // A copy is useless if we are duplicating a value in our current block:
            const bool should_eliminate =
                v->is_type<ir::copy>() && v->first_operand()->parent() == v->parent();
            if (should_eliminate) {
              v->replace_with(v->first_operand());
              v->remove();
              return true;
            }
            return false;
          });
      block->operations.erase(new_end, block->operations.end());
    }
  }

  // Given the current pending queue of conditionals, identify the most common condition.
  // Remove all conditionals that share that condition from `queue_conditionals`, and return them.
  static std::tuple<ir::value_ptr, std::vector<ir::value_ptr>> select_conditionals_to_group(
      std::vector<ir::value_ptr>& queue_conditionals) {
    // Sort in descending order of variable name:
    std::sort(queue_conditionals.begin(), queue_conditionals.end(),
              [](ir::value_ptr a, ir::value_ptr b) { return a->name() > b->name(); });

    // These may not be unique, because deferred conditionals may end up on the top of the queue
    // more than once. We don't mark them visited until we make the conditional block.
    queue_conditionals.erase(std::unique(queue_conditionals.begin(), queue_conditionals.end()),
                             queue_conditionals.end());

    // Count occurrences of the first operand, which is the condition evaluated before
    // the if-else branch.
    std::unordered_map<ir::value_ptr, int> counts{};
    counts.reserve(queue_conditionals.size());
    for (ir::value_ptr cond : queue_conditionals) {
      counts[cond->first_operand()] += 1;
    }

    // Find the most frequently occurring one:
    const auto max_it =
        std::max_element(counts.begin(), counts.end(), [](const auto& a, const auto& b) {
          return std::make_pair(a.second, a.first->name()) <
                 std::make_pair(b.second, b.first->name());
        });
    const ir::value_ptr condition = max_it->first;

    // Group together any conditionals that use this same condition:
    const auto matches_condition = [&](ir::value_ptr v) { return v->first_operand() == condition; };

    // We'll copy and return them.
    std::vector<ir::value_ptr> grouped_conditionals{};
    grouped_conditionals.reserve(queue_conditionals.size());
    std::copy_if(queue_conditionals.begin(), queue_conditionals.end(),
                 std::back_inserter(grouped_conditionals), matches_condition);

    // Erase the conditionals from the queue:
    queue_conditionals.erase(
        std::remove_if(queue_conditionals.begin(), queue_conditionals.end(), matches_condition),
        queue_conditionals.end());
    return std::make_tuple(condition, std::move(grouped_conditionals));
  }

  explicit ir_converter(control_flow_graph&& input) : values_(std::move(input.values_)) {
    visited_.reserve(values_.size());

    WF_ASSERT_EQUAL(1, input.blocks_.size(),
                    "Unconverted control flow graph should have only one block.");
    input_block_ = std::move(input.blocks_.front());
    input.blocks_.clear();

    // Discard all references the input block has to values, since we are going to reassign them.
    input_block_->operations.clear();
  }

 private:
  // Clean up anything that is not referenced in the output:
  void discard_unused_input_values() {
    values_.erase(std::remove_if(values_.begin(), values_.end(),
                                 [this](const ir::value::unique_ptr& v) {
                                   if (v->parent().get() == input_block_.get()) {
                                     WF_ASSERT_EQUAL(0, v->num_consumers(), "v = {}", v->name());
                                     return true;
                                   }
                                   return false;
                                 }),
                  values_.end());
  }

  // Allocate a new block.
  ir::block_ptr create_block() {
    auto block = std::make_unique<ir::block>(blocks_.size());
    blocks_.push_back(std::move(block));
    return ir::block_ptr(blocks_.back().get());
  }

  std::vector<ir::value::unique_ptr> values_;
  std::vector<ir::block::unique_ptr> blocks_{};
  std::unordered_set<ir::value_ptr> visited_{};
  ir::block::unique_ptr input_block_;
};

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
  return ir_converter{std::move(*this)}.convert();
}

void control_flow_graph::eliminate_duplicates() {
  value_table table{};
  table.reserve(values_.size());
  local_value_numbering(first_block(), table);
  strip_unused_values();
}

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

std::size_t control_flow_graph::value_print_width() const {
  const uint32_t highest_value_name = values_.empty() ? 0 : values_.back()->name();
  return compute_print_width(highest_value_name);
}

std::size_t control_flow_graph::num_operations() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](const std::size_t total, const ir::block::unique_ptr& b) {
                           return total + std::count_if(b->operations.begin(), b->operations.end(),
                                                        &is_countable_operation);
                         });
}

std::size_t control_flow_graph::num_conditionals() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](const std::size_t total, const ir::block::unique_ptr& b) {
                           return total + std::count_if(b->operations.begin(), b->operations.end(),
                                                        &is_conditional);
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
