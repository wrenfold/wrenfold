// Copyright 2023 Gareth Cross
#pragma once
#include <array>
#include <variant>
#include <vector>

#include "hashing.h"
#include "non_null_ptr.h"
#include "template_utils.h"

// Define types for a very simple "intermediate representation" we can use to simplify
// and reduce the user's expressions.
namespace math::ir {

class Value;
class Block;
using ValuePtr = non_null_ptr<ir::Value>;
using BlockPtr = non_null_ptr<ir::Block>;

// A block of operations:
class Block {
 public:
  using unique_ptr = std::unique_ptr<Block>;

  // Construct w/ counter.
  explicit Block(std::size_t name) : name(name) {}

  // Unique number to refer to the block (a counter).
  std::size_t name;

  // All the operations in the block, in order.
  // TODO: There is an argument to be made that this should be an intrusive double-linked-list.
  // For now I'm going with vec of pointers, since this is simpler to get started, even though it
  // means we'll have some O(N) operations (for moderately small N).
  std::vector<ValuePtr> operations;

  // Ancestor blocks (blocks that preceded this one).
  std::vector<BlockPtr> ancestors;

  // Descendants of this block (blocks we jump to from this one).
  std::vector<BlockPtr> descendants{};

  // True if the block has no operations.
  bool is_empty() const { return operations.empty(); }

  // True if this block has no ancestors (typically only true for the starting block).
  bool has_no_ancestors() const { return ancestors.empty(); }

  // Replace descendant `target` w/ `replacement`.
  void replace_descendant(ir::BlockPtr target, ir::BlockPtr replacement);

  // Insert block into `ancestors` vector.
  void add_ancestor(BlockPtr b);

  // Remove block from `ancestors` vector.
  void remove_ancestor(BlockPtr b);

  // Add `b` as a descendant, and add `this` as an ancestor of b.
  void add_descendant(ir::BlockPtr b);
};

// Determine the underlying numeric type of the provided value.
NumericType determine_value_type(ValuePtr v);

// Add together two operands.
struct Add {
  constexpr static bool is_commutative() { return true; }
  constexpr static int num_value_operands() { return 2; }
  constexpr std::string_view to_string() const { return "add"; }
  constexpr std::size_t hash_seed() const { return 0; }
  constexpr bool is_same(const Add&) const { return true; }

  NumericType determine_type(ValuePtr a, ValuePtr b) const {
    return std::max(std::max(determine_value_type(a), determine_value_type(b)),
                    NumericType::Integer);
  }
};

// Copy the operand.
struct Copy {
  constexpr static bool is_commutative() { return false; }
  constexpr static int num_value_operands() { return 1; }
  constexpr std::string_view to_string() const { return "copy"; }
  constexpr std::size_t hash_seed() const { return 0; }
  constexpr bool is_same(const Copy&) const { return true; }

  NumericType determine_type(ValuePtr arg) const { return determine_value_type(arg); }
};

// Cast the operand to the specified destination type.
struct Cast {
  constexpr static bool is_commutative() { return false; }
  constexpr static int num_value_operands() { return 1; }
  constexpr std::string_view to_string() const { return "cast"; }
  constexpr std::size_t hash_seed() const { return static_cast<std::size_t>(destination_type); }

  constexpr bool is_same(const Cast& other) const {
    return destination_type == other.destination_type;
  }

  constexpr NumericType determine_type(const ValuePtr&) const { return destination_type; }

  // Construct w/ destination type.
  constexpr explicit Cast(NumericType destination) : destination_type(destination) {}

  NumericType destination_type;
};

// A call to a built-in mathematical function like sin, cos, log, etc.
struct CallBuiltInFunction {
  constexpr static bool is_commutative() { return false; }
  constexpr static int num_value_operands() { return -1; }
  constexpr std::string_view to_string() const { return math::to_string(name); }
  constexpr std::size_t hash_seed() const { return static_cast<std::size_t>(name); }
  constexpr bool is_same(const CallBuiltInFunction& other) const { return name == other.name; }

  // TODO: This should not be hardcoded to `real`.
  constexpr NumericType determine_type(const std::vector<ValuePtr>&) const {
    return NumericType::Real;
  }

  CallBuiltInFunction(BuiltInFunctionName name) : name(name) {}

  BuiltInFunctionName name;
};

// Compare two operands (equality or inequality).
struct Compare {
  constexpr static int num_value_operands() { return 2; }
  constexpr static bool is_commutative() { return false; }

  constexpr std::string_view to_string() const {
    switch (operation) {
      case RelationalOperation::LessThan:
        return "lt";
      case RelationalOperation::LessThanOrEqual:
        return "lte";
      case RelationalOperation::Equal:
        return "eq";
    }
    return "<NOT A VALID ENUM VALUE>";
  }

  constexpr std::size_t hash_seed() const { return static_cast<std::size_t>(operation); }
  constexpr bool is_same(const Compare& comp) const { return operation == comp.operation; }
  constexpr NumericType determine_type(const ValuePtr&, const ValuePtr&) const {
    return NumericType::Bool;
  }

  explicit Compare(const RelationalOperation operation) : operation(operation) {}

  RelationalOperation operation;
};

// A trinary we use prior to insertion of conditional logic:
// cond(a, b, c) = a ? b : c;
// This expression is used to simplify duplicate elimination. It is then replaced by conditional
// logic and phi functions.
struct Cond {
  constexpr static int num_value_operands() { return 3; }
  constexpr static bool is_commutative() { return false; }
  constexpr std::string_view to_string() const { return "cond"; }
  constexpr std::size_t hash_seed() const { return 0; }
  constexpr bool is_same(const Cond&) const { return true; }

  NumericType determine_type(const ValuePtr&, const ValuePtr& true_val,
                             const ValuePtr& false_val) const {
    return std::max(determine_value_type(true_val), determine_value_type(false_val));
  }
};

// Divide first operand by the second one.
struct Div {
  constexpr static bool is_commutative() { return true; }
  constexpr static int num_value_operands() { return 2; }
  constexpr std::string_view to_string() const { return "div"; }
  constexpr std::size_t hash_seed() const { return 0; }
  constexpr bool is_same(const Div&) const { return true; }

  NumericType determine_type(ValuePtr a, ValuePtr b) const {
    return std::max(determine_value_type(a), determine_value_type(b));
  }
};

// A source to insert input values (either constants or function arguments) into the IR. Has no
// value arguments, but has an expression.
struct Load {
  constexpr static bool is_commutative() { return false; }
  constexpr static int num_value_operands() { return 0; }
  constexpr std::string_view to_string() const { return "load"; }
  std::size_t hash_seed() const { return expr.get_hash(); }
  bool is_same(const Load& other) const { return expr.is_identical_to(other.expr); }

  // Defined in cc file.
  NumericType determine_type() const;

  explicit Load(Expr expr) : expr(std::move(expr)) {}

  Expr expr;
};

// Multiply together two operands.
struct Mul {
  constexpr static bool is_commutative() { return true; }
  constexpr static int num_value_operands() { return 2; }
  constexpr std::string_view to_string() const { return "mul"; }
  constexpr std::size_t hash_seed() const { return 0; }
  constexpr bool is_same(const Mul&) const { return true; }

  NumericType determine_type(ValuePtr a, ValuePtr b) const {
    return std::max(std::max(determine_value_type(a), determine_value_type(b)),
                    NumericType::Integer);
  }
};

// Evaluates to true if the specified output index is required.
struct OutputRequired {
  constexpr static bool is_commutative() { return false; }
  constexpr static int num_value_operands() { return 0; }
  constexpr std::string_view to_string() const { return "oreq"; }
  std::size_t hash_seed() const { return hash_string_fnv(name); }
  bool is_same(const OutputRequired& other) const { return name == other.name; }

  constexpr NumericType determine_type() const { return NumericType::Bool; }

  explicit OutputRequired(std::string name) : name(name) {}

  std::string name;
};

// Phi function. The output is equal to whichever operand was generated on the evaluated code-path.
struct Phi {
  constexpr static int num_value_operands() { return 2; }
  constexpr static bool is_commutative() { return false; }
  constexpr std::string_view to_string() const { return "phi"; }
  constexpr std::size_t hash_seed() const { return 0; }
  constexpr bool is_same(const Phi&) const { return true; }

  NumericType determine_type(const ValuePtr& a, const ValuePtr&) const {
    // Both values should be the same:
    return determine_value_type(a);
  }
};

// Take thw power of the first operand to the second.
struct Pow {
  constexpr static bool is_commutative() { return false; }
  constexpr static int num_value_operands() { return 2; }
  constexpr std::string_view to_string() const { return "pow"; }
  constexpr std::size_t hash_seed() const { return 0; }
  constexpr bool is_same(const Pow&) const { return true; }

  NumericType determine_type(ValuePtr a, ValuePtr b) const {
    return std::max(std::max(determine_value_type(a), determine_value_type(b)),
                    NumericType::Integer);
  }
};

// A sink used to indicate that a value is consumed by the output (for example in a return type or
// an output argument). Operands to `Save` are never eliminated.
struct Save {
  constexpr static bool is_commutative() { return false; }
  constexpr static int num_value_operands() {
    return -1;  //  Dynamic
  }
  constexpr std::string_view to_string() const { return "save"; }

  std::size_t hash_seed() const { return hash_struct<OutputKey>{}(key); }

  constexpr bool is_same(const Save& other) const {
    return key == other.key;  // && output_index == other.output_index;
  }

  constexpr bool IsReturnValue() const { return key.usage == ExpressionUsage::ReturnValue; }

  OutputKey key;
};

struct JumpCondition {
  constexpr static bool is_commutative() { return false; }
  constexpr static int num_value_operands() { return 1; }
  constexpr std::string_view to_string() const { return "jcnd"; }
  constexpr std::size_t hash_seed() const { return 0; }
  constexpr bool is_same(const JumpCondition&) const { return true; }
};

// Different operations are represented by a variant.
using Operation = std::variant<Add, CallBuiltInFunction, Cast, Compare, Cond, Copy, Load, Mul,
                               OutputRequired, Pow, Phi, Save, JumpCondition>;

// Values are the result of any instruction we store in the IR.
// All values have a name (an integer), and an operation that computed them.
// Values may be used as operands to other operations.
class Value {
 public:
  using unique_ptr = std::unique_ptr<Value>;

  // Construct:
  template <typename OpType>
  explicit Value(uint32_t name, ir::BlockPtr parent, OpType&& operation,
                 std::vector<ValuePtr>&& operands)
      : name_(name),
        parent_(parent),
        op_(std::forward<OpType>(operation)),
        operands_(std::move(operands)) {
    notify_operands();
    if constexpr (OpType::is_commutative()) {
      // Sort operands for commutative operations so everything is a canonical order.
      sort_operands();
    }
    check_num_operands<OpType>();
  }

  // Construct with input values specified as variadic arg list.
  template <typename OpType, typename... Args,
            typename = std::enable_if_t<std::conjunction_v<std::is_convertible<Args, ValuePtr>...>>>
  explicit Value(uint32_t name, ir::BlockPtr parent, OpType&& operation, Args... args)
      : Value(name, parent, std::forward<OpType>(operation), make_value_vector(args...)) {}

  // Access underlying integer.
  constexpr uint32_t name() const { return name_; }

  // Get the parent block.
  ir::BlockPtr parent() const { return parent_; }

  // Set the parent pointer.
  void set_parent(ir::BlockPtr b) {
    ZEN_ASSERT(!std::holds_alternative<ir::JumpCondition>(op_));
    parent_ = b;
  }

  // Access underlying operation
  const Operation& value_op() const { return op_; }

  // True if the underlying operation is `T`.
  template <typename T>
  bool is_type() const {
    return std::holds_alternative<T>(op_);
  }

  // Cast the operation to the specified type.
  template <typename T>
  const T& as_type() const {
    return std::get<T>(op_);
  }

  // True if this is a phi function.
  bool is_phi() const { return is_type<ir::Phi>(); }

  // True if any values that consume this one are phi functions.
  bool is_consumed_by_phi() const {
    return std::any_of(consumers_.begin(), consumers_.end(),
                       [](const ValuePtr& v) { return v->is_phi(); });
  }

  // Replace an operand to this instruction with another.
  void replace_operand(const ValuePtr old, const ValuePtr replacement) {
    const ValuePtr self{this};
    for (ir::ValuePtr& operand : operands_) {
      if (operand == old) {
        // Note we don't remove the consumer from `operand`, that happens in replace_with(...)
        operand = replacement;
        replacement->add_consumer(self);
      }
    }
    maybe_sort_operands();
  }

  // Change the underlying operation that computes this value.
  template <typename OpType, typename... Args>
  void set_value_op(OpType&& op, Args... args) {
    // Notify existing operands we no longer reference them
    for (const ValuePtr& operand : operands_) {
      operand->remove_consumer(this);
    }
    // Record new operands:
    operands_.clear();
    (operands_.push_back(args), ...);
    op_ = std::forward<OpType>(op);
    notify_operands();
    if constexpr (OpType::is_commutative()) {
      sort_operands();
    }
    check_num_operands<OpType>();
  }

  void add_consumer(const ir::ValuePtr v) {
    // The value might be consumed twice by the same expression, for instance: pow(x, x)
    auto it = std::find(consumers_.begin(), consumers_.end(), v);
    if (it == consumers_.end()) {
      consumers_.push_back(v);
    }
  }

  void remove_consumer(ir::Value* v) {
    auto it = std::find(consumers_.begin(), consumers_.end(), v);
    if (it != consumers_.end()) {
      // Might have already been removed, if we were an operand twice.
      consumers_.erase(it);
    }
  }

  // Access instruction operands.
  const std::vector<ValuePtr>& operands() const { return operands_; }

  // Number of operands.
  std::size_t num_operands() const { return operands_.size(); }

  // Get the first operand.
  const ValuePtr& first_operand() const {
    ZEN_ASSERT(!operands_.empty());
    return operands_.front();
  }

  // Access i'th operand:
  ValuePtr operator[](std::size_t i) const { return operands_[i]; }

  // Access all consumers of this value.
  const std::vector<ValuePtr>& consumers() const { return consumers_; }

  // Number of values that directly consume this one.
  std::size_t num_consumers() const { return consumers_.size(); }

  // True if operands to values match.
  bool operands_match(const ValuePtr& other) const {
    if (operands_.size() != other->operands_.size()) {
      return false;
    }
    return std::equal(operands_.begin(), operands_.end(), other->operands_.begin());
  }

  // True if all the consumers of this value satisfy the given predicate.
  template <typename Predicate>
  bool all_consumers_satisfy(Predicate&& predicate) const {
    return std::all_of(consumers_.begin(), consumers_.end(), std::forward<Predicate>(predicate));
  }

  // Replace this value w/ the argument.
  // All downstream consumers have their arguments swapped, and the consumer list is cleared.
  void replace_with(ValuePtr other);

  // Nuke this value. Only valid if it is not consumed.
  void remove();

  // True if there are no consumers of this value.
  bool is_unused() const {
    return consumers_.empty() && !is_type<ir::Save>() && !is_type<ir::JumpCondition>();
  }

  // Determine the numeric type of the resulting expression.
  // TODO: This should be determined during construction and not require recursion.
  NumericType determine_type() const {
    return std::visit(
        [&](const auto& op) -> NumericType {
          using T = std::decay_t<decltype(op)>;
          using OmittedTypes = type_list<Save, JumpCondition>;
          if constexpr (list_contains_type_v<T, OmittedTypes>) {
            throw TypeError("Operation does not have a numeric type");
          } else if constexpr (T::num_value_operands() == 0) {
            return op.determine_type();
          } else if constexpr (T::num_value_operands() == 1) {
            return op.determine_type(operands_[0]);
          } else if constexpr (T::num_value_operands() == 2) {
            return op.determine_type(operands_[0], operands_[1]);
          } else if constexpr (T::num_value_operands() == 3) {
            return op.determine_type(operands_[0], operands_[1], operands_[2]);
          } else {
            return op.determine_type(operands_);
          }
        },
        op_);
  }

 protected:
  void maybe_sort_operands() {
    const bool commutative = std::visit([](const auto& op) { return op.is_commutative(); }, op_);
    if (commutative) {
      sort_operands();
    }
  }

  void sort_operands() {
    std::sort(operands_.begin(), operands_.end(),
              [](const ValuePtr& a, const ValuePtr& b) { return a->name() < b->name(); });
  }

  template <typename OpType>
  void check_num_operands() {
    constexpr int expected_num_args = OpType::num_value_operands();
    if constexpr (expected_num_args >= 0) {
      ZEN_ASSERT_EQUAL(static_cast<std::size_t>(expected_num_args), operands_.size());
    }
  }

  void notify_operands() {
    const ValuePtr self{this};
    for (const ValuePtr& operand : operands_) {
      operand->add_consumer(self);
    }
  }

  template <typename... Args>
  std::vector<ValuePtr> make_value_vector(Args&&... args) {
    std::vector<ValuePtr> vec{};
    vec.reserve(sizeof...(Args));
    (vec.emplace_back(std::forward<Args>(args)), ...);
    return vec;
  }

  // Unique name for this value (used for formatting variable names).
  uint32_t name_;

  // Parent block
  ir::BlockPtr parent_;

  // The operation that computes this value.
  Operation op_;

  // Operands to the operation. Maybe be empty for some operations.
  // TODO: Small vector of size ~4:
  std::vector<ValuePtr> operands_;

  // Downstream values that consume this one:
  std::vector<ValuePtr> consumers_;
};

// Inline method definition:
inline NumericType determine_value_type(ValuePtr v) { return v->determine_type(); }

// Hashes the operation and all the arguments of a value.
// This deliberately ignores the name of the value. Two different values w/ identical operations
// should produce the same hash.
struct ValueHasher {
  std::size_t operator()(const ir::ValuePtr& val) const {
    // Seed the hash w/ the index in the variant, which accounts for the type of the op.
    std::size_t seed = val->value_op().index();
    // Then some operations w/ members need to reason about the hash of those members:
    seed = hash_combine(
        seed, std::visit([&](const auto& op) { return op.hash_seed(); }, val->value_op()));
    for (const ir::ValuePtr& operand : val->operands()) {
      const uint32_t val_name = operand->name();
      seed = hash_combine(seed, static_cast<std::size_t>(val_name));
    }
    return seed;
  }
};

// Test two values for equality. The operation must be the same type, and the operands must be
// identical. This does not recursively test the operands for equality - the pointer themselves
// must match.
struct ValueEquality {
  bool operator()(const ir::ValuePtr& a, const ir::ValuePtr& b) const {
    const bool ops_match = std::visit(
        [&](const auto& a, const auto& b) -> bool {
          using A = std::decay_t<decltype(a)>;
          using B = std::decay_t<decltype(b)>;
          if constexpr (std::is_same_v<A, B>) {
            return a.is_same(b);
          } else {
            return false;
          }
        },
        a->value_op(), b->value_op());
    if (!ops_match) {
      return false;
    }
    return a->operands_match(b);
  }
};

}  // namespace math::ir

// Formatter for Value
template <>
struct fmt::formatter<math::ir::Value, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::ir::Value& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.name());
  }
};

// Formatter for pointer to Value
template <>
struct fmt::formatter<math::ir::ValuePtr, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::ir::ValuePtr x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x->name());
  }
};
