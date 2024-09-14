// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/code_generation/ir_consumer_vector.h"
#include "wf/code_generation/ir_types.h"
#include "wf/code_generation/types.h"
#include "wf/utility/checked_pointers.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/flat_hash_set.h>
#include <absl/container/inlined_vector.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf::ir {

template <typename... Args>
constexpr bool are_non_const_value_ptrs =
    std::conjunction_v<std::is_same<std::decay_t<Args>, ir::value_ptr>...>;

// Pair a value_ptr with a corresponding index. Each operand has an index into the consumer vector
// that it points to. This is to allow O(1) deletion of consumers from a `consumer_vector`.
class operand_ptr {
 public:
  constexpr operand_ptr(const ir::value_ptr operand, const std::size_t consumer_index) noexcept
      : operand_(operand), consumer_index_(consumer_index) {}

  // Get underlying ir::value*
  constexpr auto get() const noexcept { return operand_.get(); }

  // De-reference operators.
  constexpr decltype(auto) operator->() const noexcept { return get(); }
  constexpr decltype(auto) operator*() const noexcept { return *get(); }

  // Allow implicit conversion to value_ptr.
  constexpr operator ir::value_ptr() const noexcept { return operand_; }
  constexpr operator ir::const_value_ptr() const noexcept { return operand_; }

  // Remove self from the `operand_` consumer vector.
  void remove_operand() const;

  // Index in the `consumers_` vector of `operand_`.
  constexpr std::size_t consumer_index() const noexcept { return consumer_index_; }

  // Consumer index does not need to match for two operands to be identical.
  constexpr bool operator==(const operand_ptr& other) const noexcept {
    return operand_ == other.operand_;
  }

  // Allow direct comparison to ir::value_ptr.
  constexpr bool operator==(const ir::const_value_ptr& other) const noexcept {
    return operand_ == other;
  }

 private:
  ir::value_ptr operand_;
  std::size_t consumer_index_;
};

// Values are the result of any instruction we store in the IR.
// All values have a name (an integer), and an operation that computed them.
// Values may be used as operands to other operations.
class value {
 public:
  using unique_ptr = std::unique_ptr<value>;
  using operands_container = absl::InlinedVector<operand_ptr, 4>;
  using types = std::variant<void_type, scalar_type, matrix_type, custom_type>;

  // Construct:
  template <typename OpType, typename... Operands>
  value(const uint32_t name, const ir::block_ptr parent, OpType&& operation, types type,
        Operands&&... operands)
      : name_(name),
        parent_(parent),
        op_(std::forward<OpType>(operation)),
        operands_(create_operands(operands...)),
        type_(std::move(type)) {
    post_init_steps<std::decay_t<OpType>>();
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

  // Get the name of the operation.
  constexpr std::string_view op_name() const noexcept {
    return std::visit([](const auto& op) -> std::string_view { return op.to_string(); }, op_);
  }

  // True if this is a phi function.
  constexpr bool is_phi() const noexcept { return is_op<ir::phi>(); }

  // True if any values that consume this one are phi functions.
  bool is_consumed_by_phi() const noexcept;

  // Change the underlying operation that computes this value, as well as the arguments to that
  // operation.
  template <typename OpType, typename... Args>
  void set_operation(OpType&& op, types type, const Args&... args) {
    // Disconnect from the current vector of operands:
    for (const operand_ptr& operand : operands_) {
      operand.remove_operand();
    }
    operands_ = create_operands(args...);
    type_ = std::move(type);
    op_ = std::forward<OpType>(op);
    post_init_steps<std::decay_t<OpType>>();
  }

  // Determine the precedence of this operation.
  precedence operation_precedence() const;

  // Add `v` to the list of consumers of this value.
  operand_ptr add_consumer(ir::value* v);

  // Remove `v` from the list of consumers of this value.
  void remove_consumer(operand_ptr v);

  // Get the unordered set of consumers.
  constexpr const consumer_vector& consumers() const noexcept { return consumers_; }

  // Access instruction operands.
  constexpr const operands_container& operands() const noexcept { return operands_; }

  // Number of operands.
  std::size_t num_operands() const noexcept { return operands_.size(); }

  // True if this value consumes operand `v`.
  bool has_operand(const ir::const_value_ptr v) const noexcept {
    return any_of(operands_, [v](const auto& operand) { return operand.get() == v.get(); });
  }

  // Replace a pair of operands with a single operand.
  std::size_t replace_operand_pair(ir::const_value_ptr arg0, ir::const_value_ptr arg1,
                                   ir::value_ptr replacement);

  // Get the first operand.
  ir::value_ptr first_operand() const {
    WF_ASSERT(!operands_.empty());
    return operands_.front();
  }

  // Access i'th operand:
  ir::value_ptr operator[](std::size_t i) const {
    WF_ASSERT_LT(i, operands_.size());
    return operands_[i];
  }

  // Access all consumers of this value, ordered by their ID.
  absl::InlinedVector<ir::value_ptr, 8> ordered_consumers() const;

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
    return consumers_.empty() && !is_op<ir::save, ir::jump_condition>();
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
  numeric_primitive_type numeric_type() const {
    const scalar_type* scalar = std::get_if<scalar_type>(&type_);
    WF_ASSERT(scalar != nullptr, "Value is not scalar-valued: {}, index = {}", name(),
              type_.index());
    return scalar->numeric_type();
  }

  // Get the type as `type_variant`. Throws if the type is void.
  type_variant non_void_type() const;

 protected:
  // Replace an operand to this instruction with another.
  void replace_operand(const ir::value* old, value_ptr replacement);

  template <typename... Args>
  auto create_operands(const Args&... args)
      -> std::enable_if_t<are_non_const_value_ptrs<Args...>, operands_container> {
    return operands_container{args->add_consumer(this)...};
  }

  template <typename Container, typename = std::enable_if_t<is_iterable_v<Container>>>
  operands_container create_operands(const Container& container) {
    return transform_map<operands_container>(
        container, [this](const ir::value_ptr v) { return v->add_consumer(this); });
  }

  template <typename OpType>
  void post_init_steps() {
    if constexpr (OpType::is_commutative()) {
      // Sort operands for commutative operations so everything is a canonical order.
      sort_operands();
    }
    if constexpr (constexpr int expected_num_args = OpType::num_value_operands();
                  expected_num_args >= 0) {
      WF_ASSERT_EQ(static_cast<std::size_t>(expected_num_args), operands_.size());
    }
  }

  // If the underlying operation is commutative, sort the operands by name.
  void maybe_sort_operands();

  // Sort operands in-place by increasing value of name.
  void sort_operands() {
    std::sort(operands_.begin(), operands_.end(),
              [](const operand_ptr& a, const operand_ptr& b) { return a->name() < b->name(); });
  }

  // Unique name for this value (used for formatting variable names).
  uint32_t name_;

  // Parent block where this operation appears.
  ir::block_ptr parent_;

  // The operation that computes this value.
  operation op_;

  // Operands to the operation. May be empty for some operations.
  operands_container operands_;

  // Downstream values that consume this one.
  consumer_vector consumers_;

  // The cached numeric type of this operation.
  types type_;
};

inline void operand_ptr::remove_operand() const { operand_->remove_consumer(*this); }

}  // namespace wf::ir

// Hashes the operation and all the arguments of a value.
// This deliberately ignores the name of the value. Two different values w/ identical operations
// should produce the same hash.
template <>
struct wf::hash_struct<wf::ir::value_ptr> {
  std::size_t operator()(const ir::value_ptr& val) const noexcept {
    // First hash the operation, then all all the operands.
    std::size_t seed = wf::hash(val->value_op());
    for (const ir::operand_ptr& operand : val->operands()) {
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
template <typename T>
struct fmt::formatter<T,
                      std::enable_if_t<std::is_constructible_v<wf::ir::const_value_ptr, T>, char>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::ir::const_value_ptr x, FormatContext& ctx) const -> decltype(ctx.out()) {
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
