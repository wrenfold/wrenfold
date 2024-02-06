// Copyright 2023 Gareth Cross
#pragma once
#include <vector>

#include "wf/code_generation/expression_group.h"
#include "wf/code_generation/ir_block.h"
#include "wf/code_generation/ir_types.h"
#include "wf/enumerations.h"

namespace wf {

// Object for creating and manipulating our simple intermediate representation.
class flat_ir {
 public:
  // Construct from a set of output expressions.
  explicit flat_ir(const std::vector<expression_group>& expressions);

  // Format IR for every value.
  std::string to_string() const;

  // Size of value numbers when printed (# digits).
  std::size_t value_print_width() const;

  // Eliminate duplicate operations. Most are eliminated during the conversion from expressions
  // to the IR.
  void eliminate_duplicates();

  // Number of operations:
  std::size_t num_operations() const;

  // Number of conditional statements.
  std::size_t num_conditionals() const;

  // Count instances of operations matching predicate `Func`.
  template <typename Func>
  std::size_t count_operation(Func&& func) const {
    return block_->count_operation(std::forward<Func>(func));
  }

  // Count instances of operations of type `T`.
  template <typename T>
  std::size_t count_operation() const {
    return count_operation([](const T&) constexpr { return true; });
  }

  // Count invocations of the specified function.
  std::size_t count_function(const std_math_function enum_value) const noexcept {
    return count_operation(
        [&](const ir::call_std_function& func) { return func.name() == enum_value; });
  }

  // Get the single block of operations.
  ir::block_ptr get_block() const { return ir::block_ptr{block_.get()}; }

 protected:
  // Remove any values without consumers (that are not endpoints like Save).
  void strip_unused_values();

  // Single flat block:
  ir::block::unique_ptr block_;

  // Owns all the instructions.
  std::vector<ir::value::unique_ptr> values_;

  friend class ir_form_visitor;
  friend class output_ir;
};

class output_ir {
 public:
  // Construct from `flat_ir` (which is cleared in the process).
  explicit output_ir(flat_ir&& input);

  // Format IR for every value.
  std::string to_string() const;

  // Size of value numbers when printed (# digits).
  std::size_t value_print_width() const;

  // Number of operations in the IR.
  std::size_t num_operations() const;

  // Number of operations:
  std::size_t num_conditionals() const;

  // NUmber of blocks.
  std::size_t num_blocks() const { return blocks_.size(); }

  // Get the first block (start of execution).
  ir::block_ptr first_block() const {
    const auto it =
        std::find_if(blocks_.begin(), blocks_.end(),
                     [](const ir::block::unique_ptr& block) { return block->has_no_ancestors(); });
    WF_ASSERT(it != blocks_.end(), "Must be an entry block");
    return ir::block_ptr{it->get()};
  }

  // Count instances of operations matching predicate `Func`.
  template <typename Func>
  std::size_t count_operation(Func&& func) const {
    return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                           [&func](std::size_t total, const ir::block::unique_ptr& blk) {
                             return total + blk->count_operation(func);
                           });
  }

  // Count instances of operations of type `T`.
  template <typename T>
  std::size_t count_operation() const {
    return count_operation([](const T&) constexpr { return true; });
  }

  // Count instances of a function call.
  std::size_t count_function(std_math_function enum_value) const noexcept {
    return count_operation([&](ir::call_std_function func) { return func.name() == enum_value; });
  }

 private:
  // Allocate a new block
  ir::block_ptr create_block();

  // Owns all the blocks.
  std::vector<ir::block::unique_ptr> blocks_;

  // Owns all the instructions
  std::vector<ir::value::unique_ptr> values_;

  friend class ir_converter;
};

}  // namespace wf
