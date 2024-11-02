// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/ir_value.h"

#include "wf/utility/overloaded_visit.h"

namespace wf::ir {

void value::set_parent(const ir::block_ptr b) {
  WF_ASSERT(!std::holds_alternative<ir::jump_condition>(op_));
  parent_ = b;
}

bool value::is_consumed_by_phi() const noexcept {
  return std::any_of(consumers_.begin(), consumers_.end(),
                     [](const value_ptr v) { return v->is_phi(); });
}

void value::replace_operand(const ir::value* old, const value_ptr replacement) {
  // Traverse operands, and replace any that point to `old` with `replacement`.
  // In the process, `replacement` has consumers added so the graph is intact.
  for (operand_ptr& operand : operands_) {
    if (operand.get() == old) {
      // Note we don't remove the consumer from `operand`, that happens in replace_with(...)
      operand = replacement->add_consumer(this);
    }
  }
  maybe_sort_operands();
}

precedence value::operation_precedence() const {
  return overloaded_visit(
      op_, [](const ir::compare&) constexpr { return precedence::relational; },
      [](const ir::add&) constexpr { return precedence::addition; },
      [](const ir::mul&) constexpr { return precedence::multiplication; },
      [](const ir::load& load) constexpr {
        return std::visit(
            [](const auto& contents) constexpr {
              using T = std::decay_t<decltype(contents)>;
              if constexpr (type_list_contains_v<T, type_list<integer_constant, float_constant,
                                                              rational_constant>>) {
                return contents.is_negative() ? precedence::multiplication : precedence::none;
              } else {
                return precedence::none;
              }
            },
            load.variant());
      },
      [](const ir::div&) constexpr { return precedence::multiplication; },
      [](const ir::cond&) constexpr { return precedence::ternary; },
      [](const auto&) constexpr { return precedence::none; });
}

operand_ptr value::add_consumer(ir::value* v) {
  return operand_ptr{ir::value_ptr{this}, consumers_.insert(v)};
}

void value::remove_consumer(const operand_ptr v) { consumers_.remove(v.consumer_index()); }

std::size_t value::replace_operand_pair(const ir::const_value_ptr arg0,
                                        const ir::const_value_ptr arg1,
                                        const ir::value_ptr replacement) {
  // TODO: Dumb that we have to re-count this here. The binarization step should be tracking this
  //  count and reasoning about it.
  // If we are replacing the pair (vX, vX) we should not remove twice by mistake.
  const std::size_t num_valid_replacements =
      arg0 == arg1 ? std::count(operands_.begin(), operands_.end(), arg0) / 2
                   : std::min(std::count(operands_.begin(), operands_.end(), arg0),
                              std::count(operands_.begin(), operands_.end(), arg1));

  // Predicate we use to remove `val` up to `count` times.
  struct remove_specific_number_of_times {
    std::size_t count;
    ir::const_value_ptr val;

    bool operator()(const operand_ptr& operand) {
      if (operand == val && count > 0) {
        --count;
        operand.remove_operand();
        return true;
      }
      return false;
    }
  };

  remove_if(operands_, remove_specific_number_of_times{num_valid_replacements, arg0});
  remove_if(operands_, remove_specific_number_of_times{num_valid_replacements, arg1});

  for (std::size_t i = 0; i < num_valid_replacements; ++i) {
    operands_.push_back(replacement->add_consumer(this));
  }

  maybe_sort_operands();
  return num_valid_replacements;
}

absl::InlinedVector<ir::value_ptr, 8> value::ordered_consumers() const {
  absl::InlinedVector<ir::value_ptr, 8> result{consumers_.begin(), consumers_.end()};
  std::sort(result.begin(), result.end(), [](auto a, auto b) { return a->name() < b->name(); });
  return result;
}

bool value::operands_match(const value_ptr other) const noexcept {
  if (operands_.size() != other->operands_.size()) {
    return false;
  }
  return std::equal(operands_.begin(), operands_.end(), other->operands_.begin());
}

void value::replace_with(const value_ptr other) {
  WF_ASSERT(this != other.get());
  for (const value_ptr consumer : consumers_) {
    consumer->replace_operand(this, other);
  }
  consumers_.clear();
}

void value::remove() {
  WF_ASSERT(consumers_.empty(), "Attempting to remove a value `{}` that has {} consumers.", name_,
            consumers_.size());
  // Notify our operands we no longer consume them.
  for (const operand_ptr& operand : operands_) {
    operand.remove_operand();
  }
  // This value is dead, so clear the operand vector so we don't accidentally access it.
  operands_.clear();
}

type_variant value::non_void_type() const {
  return std::visit(
      [&](const auto& t) -> type_variant {
        using T = std::decay_t<decltype(t)>;
        if constexpr (std::is_same_v<T, ir::void_type>) {
          WF_ASSERT_ALWAYS("Attempted to coerce void-typed value: {}, op index = {}", name(),
                           op_.index());
        } else {
          return t;
        }
      },
      type());
}

void value::maybe_sort_operands() {
  if (const bool commutative = std::visit([](const auto& op) { return op.is_commutative(); }, op_);
      commutative) {
    sort_operands();
  }
}

}  // namespace wf::ir
