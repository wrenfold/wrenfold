// Copyright 2023 Gareth Cross
#pragma once
#include <array>
#include <variant>
#include <vector>

#include "expressions/numeric_expressions.h"
#include "expressions/special_constants.h"
#include "expressions/variable.h"
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
  explicit Block(std::size_t name) noexcept : name(name) {}

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
  bool is_empty() const noexcept { return operations.empty(); }

  // True if this block has no ancestors (typically only true for the starting block).
  bool has_no_ancestors() const noexcept { return ancestors.empty(); }

  // Replace descendant `target` w/ `replacement`.
  void replace_descendant(ir::BlockPtr target, ir::BlockPtr replacement);

  // Insert block into `ancestors` vector.
  void add_ancestor(BlockPtr b);

  // Remove block from `ancestors` vector.
  void remove_ancestor(BlockPtr b);

  // Add `b` as a descendant, and add `this` as an ancestor of b.
  void add_descendant(ir::BlockPtr b);

  // Count instances of operation of type `T`.
  template <typename Func>
  std::size_t count_operation(Func&& func) const;
};

// Determine the underlying numeric type of the provided value.
// Defined here so that we can use this prior to defining ValuePtr.
inline NumericType get_value_type(const ValuePtr& v);

// Add together two operands.
struct Add {
  constexpr static bool is_commutative() noexcept { return true; }
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr std::string_view to_string() const noexcept { return "add"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const Add&) const noexcept { return true; }

  NumericType determine_type(ValuePtr a, ValuePtr b) const {
    return std::max(std::max(get_value_type(a), get_value_type(b)), NumericType::Integer);
  }
};

// Cast the operand to the specified destination type.
struct Cast {
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 1; }
  constexpr std::string_view to_string() const noexcept { return "cast"; }
  constexpr std::size_t hash_seed() const noexcept {
    return static_cast<std::size_t>(destination_type);
  }

  constexpr bool is_same(const Cast& other) const noexcept {
    return destination_type == other.destination_type;
  }

  constexpr NumericType determine_type(const ValuePtr&) const noexcept { return destination_type; }

  // Construct w/ destination type.
  constexpr explicit Cast(NumericType destination) noexcept : destination_type(destination) {}

  NumericType destination_type;
};

// A call to a built-in mathematical function like sin, cos, log, etc.
struct CallStdFunction {
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return -1; }

  constexpr std::string_view to_string() const noexcept {
    return string_from_standard_library_function(name);
  }

  constexpr std::size_t hash_seed() const noexcept { return static_cast<std::size_t>(name); }

  constexpr bool is_same(const CallStdFunction& other) const noexcept { return name == other.name; }

  // TODO: This should not be hardcoded to `real`.
  template <typename Container>
  constexpr NumericType determine_type(const Container&) const {
    return NumericType::Real;
  }

  explicit constexpr CallStdFunction(StdMathFunction name) noexcept : name(name) {}

  StdMathFunction name;
};

// Compare two operands (equality or inequality).
struct Compare {
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr static bool is_commutative() noexcept { return false; }

  constexpr std::string_view to_string() const noexcept {
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

  constexpr std::size_t hash_seed() const noexcept { return static_cast<std::size_t>(operation); }
  constexpr bool is_same(const Compare& comp) const noexcept { return operation == comp.operation; }
  constexpr NumericType determine_type(const ValuePtr&, const ValuePtr&) const noexcept {
    return NumericType::Bool;
  }

  explicit constexpr Compare(const RelationalOperation operation) noexcept : operation(operation) {}

  RelationalOperation operation;
};

// A trinary we use prior to insertion of conditional logic:
// cond(a, b, c) = a ? b : c;
// This expression is used to simplify duplicate elimination. It is then replaced by conditional
// logic and phi functions.
struct Cond {
  constexpr static int num_value_operands() noexcept { return 3; }
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr std::string_view to_string() const noexcept { return "cond"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const Cond&) const noexcept { return true; }

  NumericType determine_type(const ValuePtr&, const ValuePtr& true_val,
                             const ValuePtr& false_val) const {
    return std::max(get_value_type(true_val), get_value_type(false_val));
  }
};

// Copy the operand.
struct Copy {
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 1; }
  constexpr std::string_view to_string() const noexcept { return "copy"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const Copy&) const noexcept { return true; }

  NumericType determine_type(ValuePtr arg) const { return get_value_type(arg); }
};

// Divide first operand by the second one.
struct Div {
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr std::string_view to_string() const noexcept { return "div"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const Div&) const noexcept { return true; }

  static NumericType determine_type(ValuePtr a, ValuePtr b) {
    return std::max(get_value_type(a), get_value_type(b));
  }
};

// This objects acts as a sink to indicate that a value will be used in the output code
// to adjust control flow. The single operand is a boolean value that will ultimately
// determine which path an if-else statement takes.
struct JumpCondition {
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 1; }
  constexpr std::string_view to_string() const noexcept { return "jcnd"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const JumpCondition&) const noexcept { return true; }
};

// A source to insert input values (either constants or function arguments) into the IR. Has no
// value arguments, but has an expression.
struct Load {
  using storage_type = std::variant<Constant, Integer, Float, Rational, Variable>;

  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 0; }
  constexpr std::string_view to_string() const noexcept { return "load"; }

  std::size_t hash_seed() const {
    return std::visit([](const auto& contents) { return hash(contents); }, variant);
  }

  //  Check if the underlying variants contain identical expressions.
  bool is_same(const Load& other) const {
    return std::visit(
        [](const auto& a, const auto& b) {
          if constexpr (std::is_same_v<decltype(a), decltype(b)>) {
            return a.is_identical_to(b);
          } else {
            return false;
          }
        },
        variant, other.variant);
  }

  // Returns true if variant contains one of the types `Ts...`
  template <typename... Ts>
  bool is_type() const noexcept {
    return ((std::holds_alternative<Ts>(variant)) || ...);
  }

  // Defined in cc file.
  NumericType determine_type() const;

  // Construct with one of the types in `storage_type`.
  template <typename T, typename = std::enable_if_t<std::is_constructible_v<storage_type, T>>>
  explicit Load(T&& contents) : variant{std::forward<T>(contents)} {}

  // Variant of different leaf expression types.
  storage_type variant;
};

// Multiply together two operands.
struct Mul {
  constexpr static bool is_commutative() noexcept { return true; }
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr std::string_view to_string() const noexcept { return "mul"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const Mul&) const noexcept { return true; }

  NumericType determine_type(ValuePtr a, ValuePtr b) const {
    return std::max(std::max(get_value_type(a), get_value_type(b)), NumericType::Integer);
  }
};

// Evaluates to true if the specified output index is required.
struct OutputRequired {
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 0; }
  constexpr std::string_view to_string() const noexcept { return "oreq"; }
  std::size_t hash_seed() const noexcept { return hash_string_fnv(name); }
  bool is_same(const OutputRequired& other) const noexcept { return name == other.name; }

  constexpr NumericType determine_type() const noexcept { return NumericType::Bool; }

  explicit OutputRequired(std::string name) : name(name) {}

  std::string name;
};

// Phi function. The output is equal to whichever operand was generated on the evaluated code-path.
struct Phi {
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr std::string_view to_string() const noexcept { return "phi"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const Phi&) const noexcept { return true; }

  NumericType determine_type(const ValuePtr& a, const ValuePtr&) const {
    // Both values should be the same:
    return get_value_type(a);
  }
};

// A sink used to indicate that a value is consumed by the output (for example in a return type or
// an output argument). Operands to `Save` are never eliminated.
struct Save {
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept {
    return -1;  //  Dynamic
  }
  constexpr std::string_view to_string() const noexcept { return "save"; }

  std::size_t hash_seed() const { return hash_struct<OutputKey>{}(key); }

  constexpr bool is_same(const Save& other) const noexcept { return key == other.key; }

  OutputKey key;
};

// Different operations are represented by a variant.
using Operation = std::variant<Add, CallStdFunction, Cast, Compare, Cond, Copy, Div, JumpCondition,
                               Load, Mul, OutputRequired, Phi, Save>;

// Values are the result of any instruction we store in the IR.
// All values have a name (an integer), and an operation that computed them.
// Values may be used as operands to other operations.
class Value {
 public:
  using unique_ptr = std::unique_ptr<Value>;
  using operands_container = std::vector<ir::ValuePtr>;

  // Construct:
  template <typename OpType>
  explicit Value(uint32_t name, ir::BlockPtr parent, OpType&& operation,
                 operands_container&& operands)
      : name_(name),
        parent_(parent),
        op_(std::forward<OpType>(operation)),
        operands_(std::move(operands)),
        numeric_type_(determine_type(op_, operands_)) {
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
  constexpr uint32_t name() const noexcept { return name_; }

  // Get the parent block.
  ir::BlockPtr parent() const noexcept { return parent_; }

  // Set the parent pointer.
  void set_parent(ir::BlockPtr b);

  // Access underlying operation
  constexpr const Operation& value_op() const noexcept { return op_; }

  // True if the underlying operation is `T`.
  template <typename T>
  constexpr bool is_type() const noexcept {
    return std::holds_alternative<T>(op_);
  }

  // Cast the operation to the specified type.
  template <typename T>
  const T& as_type() const {
    return std::get<T>(op_);
  }

  // True if this is a phi function.
  constexpr bool is_phi() const noexcept { return is_type<ir::Phi>(); }

  // True if any values that consume this one are phi functions.
  bool is_consumed_by_phi() const noexcept;

  // Replace an operand to this instruction with another.
  void replace_operand(ValuePtr old, ValuePtr replacement);

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
    numeric_type_ = determine_type(op, operands_);
    op_ = std::forward<OpType>(op);
    notify_operands();
    if constexpr (OpType::is_commutative()) {
      sort_operands();
    }
    check_num_operands<OpType>();
  }

  // Add `v` to the list of consumers of this value.
  void add_consumer(ValuePtr v);

  // Remove `v` from the list of consumers of this value.
  void remove_consumer(ir::Value* v);

  // Access instruction operands.
  constexpr const operands_container& operands() const noexcept { return operands_; }

  // Number of operands.
  std::size_t num_operands() const noexcept { return operands_.size(); }

  // Get the first operand.
  const ValuePtr& first_operand() const {
    ZEN_ASSERT(!operands_.empty());
    return operands_.front();
  }

  // Access i'th operand:
  ValuePtr operator[](std::size_t i) const {
    ZEN_ASSERT_LESS(i, operands_.size());
    return operands_[i];
  }

  // Access all consumers of this value.
  constexpr const std::vector<ValuePtr>& consumers() const noexcept { return consumers_; }

  // Number of values that directly consume this one.
  std::size_t num_consumers() const { return consumers_.size(); }

  // True if operands of `this` and `other` match (have the same names).
  bool operands_match(ValuePtr other) const noexcept;

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
  bool is_unused() const noexcept {
    return consumers_.empty() && !is_type<ir::Save>() && !is_type<ir::JumpCondition>();
  }

  // Access the underlying numeric type.
  NumericType numeric_type() const {
    ZEN_ASSERT(numeric_type_.has_value());
    return *numeric_type_;
  }

 protected:
  // If the underlying operation is commutative, sort the operands by name.
  void maybe_sort_operands();

  // Sort operands in-place by increasing value of name.
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

  // Determine the numeric type of the resulting expression.
  template <typename T, typename Container>
  static std::optional<NumericType> determine_type(const T& op, const Container& operands) {
    if constexpr (type_list_contains_type_v<T, Save, JumpCondition>) {
      return std::nullopt;
    } else if constexpr (T::num_value_operands() == 0) {
      return op.determine_type();
    } else if constexpr (T::num_value_operands() == 1) {
      return op.determine_type(operands[0]);
    } else if constexpr (T::num_value_operands() == 2) {
      return op.determine_type(operands[0], operands[1]);
    } else if constexpr (T::num_value_operands() == 3) {
      return op.determine_type(operands[0], operands[1], operands[2]);
    } else {
      return op.determine_type(operands);
    }
  }

  // Version of determine_type that accepts the operation directly.
  template <typename Container>
  static auto determine_type(const Operation& op, const Container& operands) {
    return std::visit([&operands](const auto& op) { return determine_type(op, operands); }, op);
  }

  template <typename... Args>
  operands_container make_value_vector(Args&&... args) {
    operands_container vec{};
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

  // Operands to the operation. May be empty for some operations.
  // TODO: An inlined vector doesn't make any meaningful difference here, but that maybe merits
  //  a bit more thorough investigation.
  operands_container operands_;

  // Downstream values that consume this one:
  std::vector<ValuePtr> consumers_;

  // The cached numeric type of this operation. Optional because some operations lack a type.
  std::optional<NumericType> numeric_type_;
};

// Inline method definition:
inline NumericType get_value_type(const ValuePtr& v) { return v->numeric_type(); }

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

// Count instances of operation of type `T`.
// Defined here so that we can use methods on `Value`.
template <typename Func>
std::size_t Block::count_operation(Func&& func) const {
  return std::count_if(operations.begin(), operations.end(), [&func](ir::ValuePtr v) -> bool {
    return std::visit(
        [&func](const auto& op) -> bool {
          using argument_type = std::decay_t<decltype(op)>;
          if constexpr (has_call_operator_v<Func, argument_type>) {
            return func(op);
          } else {
            return false;
          }
        },
        v->value_op());
  });
}

}  // namespace math::ir

namespace math {

// Hashes the operation and all the arguments of a value.
// This deliberately ignores the name of the value. Two different values w/ identical operations
// should produce the same hash.
template <>
struct hash_struct<ir::ValuePtr> {
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

}  // namespace math

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
