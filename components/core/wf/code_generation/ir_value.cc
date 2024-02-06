// Copyright 2024 Gareth Cross
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

void value::add_consumer(const ir::value_ptr v) {
  // The value might be consumed twice by the same expression, for instance: pow(x, x)
  if (const auto it = std::find(consumers_.begin(), consumers_.end(), v); it == consumers_.end()) {
    consumers_.push_back(v);
  }
}

void value::remove_consumer(ir::value* const v) {
  if (const auto it = std::find_if(consumers_.begin(), consumers_.end(),
                                   [&](const auto& c) { return c.get() == v; });
      it != consumers_.end()) {
    // Might have already been removed, if we were an operand twice.
    consumers_.erase(it);
  }
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
