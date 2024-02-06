// Copyright 2024 Gareth Cross
#pragma once
#include <vector>

#include "wf/checked_pointers.h"
#include "wf/code_generation/ir_types.h"
#include "wf/code_generation/types.h"

namespace wf::ir {

class block;
class value;
using block_ptr = non_null<ir::block*>;
using value_ptr = non_null<ir::value*>;

// Values are the result of any instruction we store in the IR.
// All values have a name (an integer), and an operation that computed them.
// Values may be used as operands to other operations.
class value {
 public:
  using unique_ptr = std::unique_ptr<value>;
  using operands_container = std::vector<ir::value_ptr>;
  using types = std::variant<void_type, scalar_type, matrix_type, custom_type>;

  // Construct:
  template <typename OpType, typename... Operands>
  value(const uint32_t name, const ir::block_ptr parent, OpType&& operation, types type,
        Operands&&... operands)
      : name_(name),
        parent_(parent),
        op_(std::forward<OpType>(operation)),
        operands_{std::forward<Operands>(operands)...},
        type_(std::move(type)) {
    post_init_steps<OpType>();
  }

  // Access underlying integer.
  constexpr uint32_t name() const noexcept { return name_; }

  // Get the parent block.
  ir::block_ptr parent() const noexcept { return parent_; }

  // Set the parent pointer.
  void set_parent(ir::block_ptr b);

  // Access underlying operation
  constexpr const operation& value_op() const noexcept { return op_; }

  // True if the underlying operation is one of `Ts`.
  template <typename... Ts>
  constexpr bool is_op() const noexcept {
    return (std::holds_alternative<Ts>(op_) || ...);
  }

  // Cast the operation to the specified type.
  template <typename T>
  constexpr const T& as_op() const {
    return std::get<T>(op_);
  }

  // True if this is a phi function.
  constexpr bool is_phi() const noexcept { return is_op<ir::phi>(); }

  // True if any values that consume this one are phi functions.
  bool is_consumed_by_phi() const noexcept;

  // Replace an operand to this instruction with another.
  void replace_operand(value_ptr old, value_ptr replacement);

  // Change the underlying operation that computes this value, as well as the arguments to that
  // operation.
  template <typename OpType, typename... Args>
  void set_operation(OpType&& op, types type, Args&&... args) {
    // Disconnect from the current vector of operands:
    for (const value_ptr& operand : operands_) {
      operand->remove_consumer(this);
    }
    operands_ = {std::forward<Args>(args)...};
    type_ = std::move(type);
    op_ = std::forward<OpType>(op);
    post_init_steps<OpType>();
  }

  // Add `v` to the list of consumers of this value.
  void add_consumer(value_ptr v);

  // Remove `v` from the list of consumers of this value.
  void remove_consumer(ir::value* v);

  // Access instruction operands.
  constexpr const operands_container& operands() const noexcept { return operands_; }

  // Number of operands.
  std::size_t num_operands() const noexcept { return operands_.size(); }

  // Get the first operand.
  const value_ptr& first_operand() const {
    WF_ASSERT(!operands_.empty());
    return operands_.front();
  }

  // Access i'th operand:
  value_ptr operator[](std::size_t i) const {
    WF_ASSERT_LESS(i, operands_.size());
    return operands_[i];
  }

  // Access all consumers of this value.
  constexpr const std::vector<value_ptr>& consumers() const noexcept { return consumers_; }

  // Number of values that directly consume this one.
  std::size_t num_consumers() const { return consumers_.size(); }

  // True if operands of `this` and `other` match (have the same names).
  bool operands_match(value_ptr other) const noexcept;

  // True if all the consumers of this value satisfy the given predicate.
  template <typename Predicate>
  bool all_consumers_satisfy(Predicate&& predicate) const {
    return std::all_of(consumers_.begin(), consumers_.end(), std::forward<Predicate>(predicate));
  }

  // Replace this value w/ the argument.
  // All downstream consumers have their arguments swapped, and the consumer list is cleared.
  void replace_with(value_ptr other);

  // Nuke this value. Only valid if it is not consumed.
  void remove();

  // True if there are no consumers of this value.
  bool is_unused() const noexcept {
    return consumers_.empty() && !is_op<ir::save>() && !is_op<ir::jump_condition>();
  }

  // Access the type variant.
  constexpr const auto& type() const noexcept { return type_; }

  // True if the type of the expression is scalar.
  bool is_scalar_type() const noexcept { return std::holds_alternative<scalar_type>(type_); }

  // Get the concrete type of the value if it matches `T`, otherwise nullptr.
  template <typename T>
  maybe_null<const T*> get_type_if() const noexcept {
    return std::get_if<T>(&type_);
  }

  // Access the scalar numeric type. Invalid if `type_` is not a scalar_type.
  code_numeric_type numeric_type() const {
    const scalar_type* scalar = std::get_if<scalar_type>(&type_);
    WF_ASSERT(scalar != nullptr, "Value is not scalar-valued: {}, index = {}", name(),
              type_.index());
    return scalar->numeric_type();
  }

  // Get the type as `type_variant`. Throws if the type is void.
  type_variant non_void_type() const;

 protected:
  template <typename OpType>
  void post_init_steps() {
    notify_operands();
    if constexpr (OpType::is_commutative()) {
      // Sort operands for commutative operations so everything is a canonical order.
      sort_operands();
    }
    if constexpr (constexpr int expected_num_args = OpType::num_value_operands();
                  expected_num_args >= 0) {
      WF_ASSERT_EQUAL(static_cast<std::size_t>(expected_num_args), operands_.size());
    }
  }

  // If the underlying operation is commutative, sort the operands by name.
  void maybe_sort_operands();

  // Sort operands in-place by increasing value of name.
  void sort_operands() {
    std::sort(operands_.begin(), operands_.end(),
              [](const value_ptr& a, const value_ptr& b) { return a->name() < b->name(); });
  }

  // Add `this` as a consumer of its own operands.
  void notify_operands();

  // Unique name for this value (used for formatting variable names).
  uint32_t name_;

  // Parent block
  ir::block_ptr parent_;

  // The operation that computes this value.
  operation op_;

  // Operands to the operation. May be empty for some operations.
  // TODO: An inlined vector doesn't make any meaningful difference here, but that maybe merits
  //  a bit more thorough investigation.
  operands_container operands_;

  // Downstream values that consume this one:
  std::vector<value_ptr> consumers_;

  // The cached numeric type of this operation.
  types type_;
};

}  // namespace wf::ir

// Hashes the operation and all the arguments of a value.
// This deliberately ignores the name of the value. Two different values w/ identical operations
// should produce the same hash.
template <>
struct wf::hash_struct<wf::ir::value_ptr> {
  std::size_t operator()(const ir::value_ptr& val) const noexcept {
    // First hash the operation, then all all the operands.
    std::size_t seed = wf::hash(val->value_op());
    for (const ir::value_ptr& operand : val->operands()) {
      seed = hash_combine(seed, operand->name());
    }
    return seed;
  }
};

// Formatter for `value`.
template <>
struct fmt::formatter<wf::ir::value, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::ir::value& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.name());
  }
};

// Formatter for pointer to `value`.
template <>
struct fmt::formatter<wf::ir::value_ptr, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::ir::value_ptr x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x->name());
  }
};

// Formatter for `value::types`:
template <>
struct fmt::formatter<wf::ir::value::types, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::ir::value::types& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return std::visit([&](const auto& y) { return fmt::format_to(ctx.out(), "{}", y); }, x);
  }
};
