// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/ir_control_flow_converter.h"

namespace wf {

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

ir_control_flow_converter::ir_control_flow_converter(control_flow_graph&& input)
    : values_(std::move(input.values_)) {
  visited_.reserve(values_.size());

  WF_ASSERT_EQ(1, input.blocks_.size(),
               "Unconverted control flow graph should have only one block.");
  input_block_ = std::move(input.blocks_.front());
  input.blocks_.clear();

  // input_block_ will be destroyed on destruction of this object, which occurs after convert().
}

control_flow_graph ir_control_flow_converter::convert() && {
  // Conversion will modify `output.values_`, so shallow-copy the outputs first:
  auto [required_outputs_queue, optional_outputs] = get_reverse_ordered_output_values(values_);

  // Insert computations for required output values:
  std::vector<ir::value_ptr> deferred_values{};
  ir::block_ptr next_block =
      process(std::move(required_outputs_queue), create_block(), deferred_values);

  // Should be nothing deferred yet:
  WF_ASSERT(deferred_values.empty(), "deferred_values = [{}]", fmt::join(deferred_values, ", "));

  // Traverse optional outputs:
  for (const ir::value_ptr v : optional_outputs) {
    const ir::save& save = v->as_op<ir::save>();
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

    process({v}, left_block_exit, deferred_values);

    std::deque<ir::value_ptr> queue{deferred_values.begin(), deferred_values.end()};
    deferred_values.clear();
    next_block = process(std::move(queue), jump_block, deferred_values);
    WF_ASSERT(deferred_values.empty(), "deferred_values = [{}]", fmt::join(deferred_values, ", "));
  }

  std::deque<ir::value_ptr> queue;
  std::copy(deferred_values.begin(), deferred_values.end(), std::back_inserter(queue));
  deferred_values.clear();

  process(std::move(queue), next_block, deferred_values);
  WF_ASSERT(deferred_values.empty(), "deferred_values = [{}]", fmt::join(deferred_values, ", "));

  // There should only be one start block:
  WF_ASSERT_EQ(
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

bool ir_control_flow_converter::is_visited(const ir::value_ptr v) const {
  return visited_.count(v) > 0;
}

bool ir_control_flow_converter::all_consumers_visited(const ir::value_ptr val) const {
  return val->all_consumers_satisfy([this](const ir::value_ptr v) { return is_visited(v); });
}

void ir_control_flow_converter::queue_operands(std::deque<ir::value_ptr>& queue,
                                               const ir::value_ptr v) const {
  for (const ir::value_ptr val : v->operands()) {
    if (!is_visited(val) && all_consumers_visited(val)) {
      queue.push_back(val);
    }
  }
}

// Return true if `parent_block` is executed on all paths through `test_block`.
static bool parent_is_on_all_paths_through_block(const ir::block_ptr test_block,
                                                 const ir::block_ptr parent_block) noexcept {
  if (test_block == parent_block) {
    return true;
  }
  const auto& ancestors = test_block->ancestors();
  return !ancestors.empty() &&
         std::all_of(ancestors.begin(), ancestors.end(), [&](const ir::block_ptr b) {
           return parent_is_on_all_paths_through_block(b, parent_block);
         });
}

std::vector<ir::value_ptr> ir_control_flow_converter::process_non_conditionals(
    std::deque<ir::value_ptr>& queue, std::vector<ir::value_ptr>& deferred,
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
    if (const bool is_valid_to_insert =
            top->all_consumers_satisfy([&](const ir::value_ptr consumer) {
              return parent_is_on_all_paths_through_block(consumer->parent(), output_block);
            });
        !is_valid_to_insert) {
      deferred.push_back(top);
      continue;
    }

    if (top->is_op<ir::cond>()) {
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
  output_block->insert_front(output_reversed.rbegin(), output_reversed.rend());
  return queued_conditionals;
}

// Given the current pending queue of conditionals, identify the most common condition.
// Remove all conditionals that share that condition from `queue_conditionals`, and return them.
static std::tuple<ir::value_ptr, std::vector<ir::value_ptr>> select_conditionals_to_group(
    std::vector<ir::value_ptr>& queue_conditionals) {
  // Sort in descending order of variable name:
  std::sort(queue_conditionals.begin(), queue_conditionals.end(),
            [](const ir::value_ptr a, const ir::value_ptr b) { return a->name() > b->name(); });

  // These may not be unique, because deferred conditionals may end up on the top of the queue
  // more than once. We don't mark them visited until we make the conditional block.
  queue_conditionals.erase(std::unique(queue_conditionals.begin(), queue_conditionals.end()),
                           queue_conditionals.end());

  // Count occurrences of the first operand, which is the condition evaluated before the if-else
  // branch.
  std::unordered_map<ir::value_ptr, int> counts{};
  counts.reserve(queue_conditionals.size());
  for (const ir::value_ptr cond : queue_conditionals) {
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
  const auto matches_condition = [&](const ir::value_ptr v) {
    return v->first_operand().get() == condition.get();
  };

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

ir::block_ptr ir_control_flow_converter::process(std::deque<ir::value_ptr> queue,
                                                 const ir::block_ptr output_block,
                                                 std::vector<ir::value_ptr>& deferred) {
  std::vector<ir::value_ptr> queued_conditionals =
      process_non_conditionals(queue, deferred, output_block);
  if (queued_conditionals.empty()) {
    return output_block;
  }

  const auto [condition, grouped_conditionals] = select_conditionals_to_group(queued_conditionals);

  WF_ASSERT(std::is_sorted(grouped_conditionals.begin(), grouped_conditionals.end(),
                           [](ir::value_ptr a, ir::value_ptr b) { return a->name() > b->name(); }),
            "Should be sorted: {}", fmt::join(grouped_conditionals, ","));

  const ir::block_ptr left_block_tail = create_block();
  const ir::block_ptr right_block_tail = create_block();

  std::deque<ir::value_ptr> queue_left{};
  std::deque<ir::value_ptr> queue_right{};
  for (const ir::value_ptr v : grouped_conditionals) {
    // Turn conditionals into phi functions. We insert copies in the left and right blocks so that
    // arguments to the phi functions are computed on the branched code path, even if the
    // computation is just a copy. These copies can be eliminated later, in some cases.
    const ir::value_ptr copy_left = create_operation(values_, left_block_tail, ir::copy{},
                                                     v->operator[](1)->type(), v->operator[](1));
    const ir::value_ptr copy_right = create_operation(values_, right_block_tail, ir::copy{},
                                                      v->operator[](2)->type(), v->operator[](2));

    v->set_operation(ir::phi{}, copy_left->type(), copy_left, copy_right);
    v->set_parent(output_block);
    visited_.insert(v);
    visited_.insert(copy_left);
    visited_.insert(copy_right);
    queue_operands(queue_left, copy_left);
    queue_operands(queue_right, copy_right);
  }

  // Insert the phi functions at the top of the block:
  output_block->insert_front(grouped_conditionals.rbegin(), grouped_conditionals.rend());

  // Save the ancestor vector before modifying it by creating jumps:
  const std::vector<ir::block_ptr> previous_ancestors = output_block->ancestors();

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
    WF_ASSERT(!ancestor->is_empty() && !ancestor->has_no_descendents(),
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
// ReSharper disable once CppMemberFunctionMayBeConst
void ir_control_flow_converter::eliminate_useless_copies() {
  for (const auto& block : blocks_) {
    auto operations = block->operations();
    remove_if(operations, [&](const ir::value_ptr v) {
      // A copy is useless if we are duplicating a value in our current block:
      if (v->is_op<ir::copy>() && v->first_operand()->parent() == v->parent()) {
        v->replace_with(v->first_operand());
        v->remove();
        return true;
      }
      return false;
    });
    block->set_operations(std::move(operations));
  }
}

void ir_control_flow_converter::discard_unused_input_values() {
  values_.erase(std::remove_if(values_.begin(), values_.end(),
                               [this](const ir::value::unique_ptr& v) {
                                 if (v->parent().get() == input_block_.get()) {
                                   WF_ASSERT_EQ(0, v->num_consumers(), "value: {}", v->name());
                                   return true;
                                 }
                                 return false;
                               }),
                values_.end());
}

ir::block_ptr ir_control_flow_converter::create_block() {
  auto block = std::make_unique<ir::block>(blocks_.size());
  blocks_.push_back(std::move(block));
  return ir::block_ptr(blocks_.back().get());
}

}  // namespace wf
