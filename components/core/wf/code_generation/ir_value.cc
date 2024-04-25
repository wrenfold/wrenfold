// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/ir_value.h"

namespace wf::ir {

void value::set_parent(const ir::block_ptr b) {
  WF_ASSERT(!std::holds_alternative<ir::jump_condition>(op_));
  parent_ = b;
}

bool value::is_consumed_by_phi() const noexcept {
  return std::any_of(consumers_.begin(), consumers_.end(),
                     [](const value_ptr& v) { return v->is_phi(); });
}

void value::replace_operand(const value_ptr old, const value_ptr replacement) {
  const value_ptr self{this};
  for (ir::value_ptr& operand : operands_) {
    if (operand == old) {
      // Note we don't remove the consumer from `operand`, that happens in replace_with(...)
      operand = replacement;
      replacement->add_consumer(self);
    }
  }
  maybe_sort_operands();
}

void value::add_consumer(const ir::value_ptr v) { consumers_.insert(v); }

void value::remove_consumer(const value_ptr v) { consumers_.erase(v); }

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
  const value_ptr self{this};
  WF_ASSERT_NOT_EQUAL(self, other);
  for (const value_ptr& consumer : consumers_) {
    consumer->replace_operand(self, other);
  }
  consumers_.clear();
}

void value::remove() {
  WF_ASSERT(consumers_.empty(), "Attempting to remove a value `{}` that is consumed by: [{}]",
            name_, fmt::join(consumers_, ","));
  // Notify our operands we no longer consume them.
  for (const value_ptr& operand : operands_) {
    operand->remove_consumer(this);
  }
  // This value is dead, so clear the operand vector
  operands_.clear();
}

type_variant value::non_void_type() const {
  return std::visit(
      [&](const auto& t) -> type_variant {
        using T = std::decay_t<decltype(t)>;
        if constexpr (std::is_same_v<T, ir::void_type>) {
          WF_ASSERT_ALWAYS("Attempted to coerce void-typed value: {}, op index = {}, operands = {}",
                           name(), op_.index(), fmt::join(operands_, ", "));
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

void value::notify_operands() {
  const value_ptr self{this};
  for (const value_ptr& operand : operands_) {
    operand->add_consumer(self);
  }
}

}  // namespace wf::ir
