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
using ValuePtr = NonNullPtr<ir::Value>;
using BlockPtr = NonNullPtr<ir::Block>;

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
  bool IsEmpty() const { return operations.empty(); }

  // True if this block has no ancestors (typically only true for the starting block).
  bool HasNoAncestors() const { return ancestors.empty(); }

  // Replace descendant `target` w/ `replacement`.
  void ReplaceDescendant(ir::BlockPtr target, ir::BlockPtr replacement);

  // Insert block into `ancestors` vector.
  void AddAncestor(BlockPtr b);

  // Remove block from `ancestors` vector.
  void RemoveAncestor(BlockPtr b);

  // Add `b` as a descendant, and add `this` as an ancestor of b.
  void AddDescendant(ir::BlockPtr b);
};

// Determine the underlying numeric type of the provided value.
NumericType DetermineValueType(ValuePtr v);

// Add together two operands.
struct Add {
  constexpr static bool IsCommutative() { return true; }
  constexpr static int NumValueOperands() { return 2; }
  constexpr std::string_view ToString() const { return "add"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Add&) const { return true; }

  NumericType DetermineType(ValuePtr a, ValuePtr b) const {
    return std::max(std::max(DetermineValueType(a), DetermineValueType(b)), NumericType::Integer);
  }
};

// Copy the operand.
struct Copy {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 1; }
  constexpr std::string_view ToString() const { return "copy"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Copy&) const { return true; }

  NumericType DetermineType(ValuePtr arg) const { return DetermineValueType(arg); }
};

// Cast the operand to the specified destination type.
struct Cast {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 1; }
  constexpr std::string_view ToString() const { return "cast"; }
  constexpr std::size_t Hash() const { return static_cast<std::size_t>(destination_type); }

  constexpr bool IsSame(const Cast& other) const {
    return destination_type == other.destination_type;
  }

  constexpr NumericType DetermineType(const ValuePtr&) const { return destination_type; }

  // Construct w/ destination type.
  constexpr explicit Cast(NumericType destination) : destination_type(destination) {}

  NumericType destination_type;
};

// TODO: Should probably just support n-ary functions w/ one object.
struct CallUnaryFunc {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 1; }
  constexpr std::string_view ToString() const { return math::ToString(name); }
  constexpr std::size_t Hash() const { return static_cast<std::size_t>(name); }
  constexpr bool IsSame(const CallUnaryFunc& other) const { return name == other.name; }
  constexpr NumericType DetermineType(const ValuePtr&) const { return NumericType::Real; }

  CallUnaryFunc(BuiltInFunctionName name) : name(name) {}

  BuiltInFunctionName name;
};

// Compare two operands (equality or inequality).
struct Compare {
  constexpr static int NumValueOperands() { return 2; }
  constexpr static bool IsCommutative() { return false; }

  constexpr std::string_view ToString() const {
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

  constexpr std::size_t Hash() const { return static_cast<std::size_t>(operation); }
  constexpr bool IsSame(const Compare& comp) const { return operation == comp.operation; }
  constexpr NumericType DetermineType(const ValuePtr&, const ValuePtr&) const {
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
  constexpr static int NumValueOperands() { return 3; }
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return "cond"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Cond&) const { return true; }

  NumericType DetermineType(const ValuePtr&, const ValuePtr& true_val,
                            const ValuePtr& false_val) const {
    return std::max(DetermineValueType(true_val), DetermineValueType(false_val));
  }
};

// Divide first operand by the second one.
struct Div {
  constexpr static bool IsCommutative() { return true; }
  constexpr static int NumValueOperands() { return 2; }
  constexpr std::string_view ToString() const { return "div"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Div&) const { return true; }

  NumericType DetermineType(ValuePtr a, ValuePtr b) const {
    return std::max(DetermineValueType(a), DetermineValueType(b));
  }
};

// A source to insert input values (either constants or function arguments) into the IR. Has no
// value arguments, but has an expression.
struct Load {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 0; }
  constexpr std::string_view ToString() const { return "load"; }
  std::size_t Hash() const { return expr.Hash(); }
  bool IsSame(const Load& other) const { return expr.IsIdenticalTo(other.expr); }

  // Defined in cc file.
  NumericType DetermineType() const;

  explicit Load(Expr expr) : expr(std::move(expr)) {}

  Expr expr;
};

// Multiply together two operands.
struct Mul {
  constexpr static bool IsCommutative() { return true; }
  constexpr static int NumValueOperands() { return 2; }
  constexpr std::string_view ToString() const { return "mul"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Mul&) const { return true; }

  NumericType DetermineType(ValuePtr a, ValuePtr b) const {
    return std::max(std::max(DetermineValueType(a), DetermineValueType(b)), NumericType::Integer);
  }
};

// Evaluates to true if the specified output index is required.
struct OutputRequired {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 0; }
  constexpr std::string_view ToString() const { return "oreq"; }
  constexpr std::size_t Hash() const { return HashString(name); }
  bool IsSame(const OutputRequired& other) const { return name == other.name; }

  constexpr NumericType DetermineType() const { return NumericType::Bool; }

  explicit OutputRequired(std::string name) : name(name) {}

  std::string name;
};

// Phi function. The output is equal to whichever operand was generated on the evaluated code-path.
struct Phi {
  constexpr static int NumValueOperands() { return 2; }
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return "phi"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Phi&) const { return true; }

  NumericType DetermineType(const ValuePtr& a, const ValuePtr&) const {
    // Both values should be the same:
    return DetermineValueType(a);
  }
};

// Take thw power of the first operand to the second.
struct Pow {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 2; }
  constexpr std::string_view ToString() const { return "pow"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Pow&) const { return true; }

  NumericType DetermineType(ValuePtr a, ValuePtr b) const {
    return std::max(std::max(DetermineValueType(a), DetermineValueType(b)), NumericType::Integer);
  }
};

// A sink used to indicate that a value is consumed by the output (for example in a return type or
// an output argument). Operands to `Save` are never eliminated.
struct Save {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() {
    return -1;  //  Dynamic
  }
  constexpr std::string_view ToString() const { return "save"; }

  constexpr std::size_t Hash() const { return OutputKeyHasher{}(key); }

  constexpr bool IsSame(const Save& other) const {
    return key == other.key;  // && output_index == other.output_index;
  }

  constexpr bool IsReturnValue() const { return key.usage == ExpressionUsage::ReturnValue; }

  OutputKey key;
};

struct JumpCondition {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 1; }
  constexpr std::string_view ToString() const { return "jcnd"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const JumpCondition&) const { return true; }
};

// Different operations are represented by a variant.
using Operation = std::variant<Add, CallUnaryFunc, Cast, Compare, Cond, Copy, Load, Mul,
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
    NotifyOperands();
    if constexpr (OpType::IsCommutative()) {
      // Sort operands for commutative operations so everything is a canonical order.
      SortOperands();
    }
    CheckNumOperands<OpType>();
  }

  // Construct with input values specified as variadic arg list.
  template <typename OpType, typename... Args,
            typename = std::enable_if_t<std::conjunction_v<std::is_convertible<Args, ValuePtr>...>>>
  explicit Value(uint32_t name, ir::BlockPtr parent, OpType&& operation, Args... args)
      : Value(name, parent, std::forward<OpType>(operation), MakeValueVector(args...)) {}

  // Access underlying integer.
  constexpr uint32_t Name() const { return name_; }

  // Get the parent block.
  ir::BlockPtr Parent() const { return parent_; }

  // Set the parent pointer.
  void SetParent(ir::BlockPtr b) {
    ASSERT(!std::holds_alternative<ir::JumpCondition>(op_));
    parent_ = b;
  }

  // Access underlying operation
  const Operation& Op() const { return op_; }

  // True if the underlying operation is `T`.
  template <typename T>
  bool Is() const {
    return std::holds_alternative<T>(op_);
  }

  // Cast the operation to the specified type.
  template <typename T>
  const T& As() const {
    return std::get<T>(op_);
  }

  // True if this is a phi function.
  bool IsPhi() const { return Is<ir::Phi>(); }

  // True if any values that consume this one are phi functions.
  bool IsConsumedByPhi() const {
    return std::any_of(consumers_.begin(), consumers_.end(),
                       [](const ValuePtr& v) { return v->IsPhi(); });
  }

  // Replace an operand to this instruction with another.
  void ReplaceOperand(const ValuePtr old, const ValuePtr replacement) {
    const ValuePtr self{this};
    for (ir::ValuePtr& operand : operands_) {
      if (operand == old) {
        // Note we don't remove the consumer from `operand`, that happens in ReplaceWith(...)
        operand = replacement;
        replacement->AddConsumer(self);
      }
    }
    MaybeSortOperands();
  }

  // Change the underlying operation that computes this value.
  template <typename OpType, typename... Args>
  void SetOp(OpType&& op, Args... args) {
    // Notify existing operands we no longer reference them
    for (const ValuePtr& operand : operands_) {
      operand->RemoveConsumer(this);
    }
    // Record new operands:
    operands_.clear();
    (operands_.push_back(args), ...);
    op_ = std::forward<OpType>(op);
    NotifyOperands();
    if constexpr (OpType::IsCommutative()) {
      SortOperands();
    }
    CheckNumOperands<OpType>();
  }

  void AddConsumer(const ir::ValuePtr v) {
    // The value might be consumed twice by the same expression, for instance: pow(x, x)
    auto it = std::find(consumers_.begin(), consumers_.end(), v);
    if (it == consumers_.end()) {
      consumers_.push_back(v);
    }
  }

  void RemoveConsumer(ir::Value* v) {
    auto it = std::find(consumers_.begin(), consumers_.end(), v);
    if (it != consumers_.end()) {
      // Might have already been removed, if we were an operand twice.
      consumers_.erase(it);
    }
  }

  // Access instruction operands.
  const std::vector<ValuePtr>& Operands() const { return operands_; }

  // Number of operands.
  std::size_t NumOperands() const { return operands_.size(); }

  // Get the first operand.
  const ValuePtr& Front() const {
    ASSERT(!operands_.empty());
    return operands_.front();
  }

  // Access i'th operand:
  ValuePtr operator[](std::size_t i) const { return operands_[i]; }

  // Access all consumers of this value.
  const std::vector<ValuePtr>& Consumers() const { return consumers_; }

  // Number of values that directly consume this one.
  std::size_t NumConsumers() const { return consumers_.size(); }

  // True if operands to values match.
  bool OperandsMatch(const ValuePtr& other) const {
    if (operands_.size() != other->operands_.size()) {
      return false;
    }
    return std::equal(operands_.begin(), operands_.end(), other->operands_.begin());
  }

  // True if all the consumers of this value satisfy the given predicate.
  template <typename Predicate>
  bool AllConsumersSatisfy(Predicate&& predicate) const {
    return std::all_of(consumers_.begin(), consumers_.end(), std::forward<Predicate>(predicate));
  }

  // Replace this value w/ the argument.
  // All downstream consumers have their arguments swapped, and the consumer list is cleared.
  void ReplaceWith(ValuePtr other);

  // Nuke this value. Only valid if it is not consumed.
  void Remove();

  // True if there are no consumers of this value.
  bool IsUnused() const {
    return consumers_.empty() && !Is<ir::Save>() && !Is<ir::JumpCondition>();
  }

  // Determine the numeric type of the resulting expression.
  // TODO: This should be determined during construction and not require recursion.
  NumericType DetermineType() const {
    return std::visit(
        [&](const auto& op) -> NumericType {
          using T = std::decay_t<decltype(op)>;
          using OmittedTypes = TypeList<Save, JumpCondition>;
          if constexpr (ContainsType<T, OmittedTypes>) {
            throw TypeError("Operation does not have a numeric type");
          } else if constexpr (T::NumValueOperands() == 0) {
            return op.DetermineType();
          } else if constexpr (T::NumValueOperands() == 1) {
            return op.DetermineType(operands_[0]);
          } else if constexpr (T::NumValueOperands() == 2) {
            return op.DetermineType(operands_[0], operands_[1]);
          } else if constexpr (T::NumValueOperands() == 3) {
            return op.DetermineType(operands_[0], operands_[1], operands_[2]);
          } else {
            return op.DetermineType(operands_);
          }
        },
        op_);
  }

 protected:
  void MaybeSortOperands() {
    const bool commutative = std::visit([](const auto& op) { return op.IsCommutative(); }, op_);
    if (commutative) {
      SortOperands();
    }
  }

  void SortOperands() {
    std::sort(operands_.begin(), operands_.end(),
              [](const ValuePtr& a, const ValuePtr& b) { return a->Name() < b->Name(); });
  }

  template <typename OpType>
  void CheckNumOperands() {
    constexpr int expected_num_args = OpType::NumValueOperands();
    if constexpr (expected_num_args >= 0) {
      ASSERT_EQUAL(static_cast<std::size_t>(expected_num_args), operands_.size());
    }
  }

  void NotifyOperands() {
    const ValuePtr self{this};
    for (const ValuePtr& operand : operands_) {
      operand->AddConsumer(self);
    }
  }

  template <typename... Args>
  std::vector<ValuePtr> MakeValueVector(Args&&... args) {
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
inline NumericType DetermineValueType(ValuePtr v) { return v->DetermineType(); }

// Hashes the operation and all the arguments of a value.
// This deliberately ignores the name of the value. Two different values w/ identical operations
// should produce the same hash.
struct ValueHasher {
  std::size_t operator()(const ir::ValuePtr& val) const {
    // Seed the hash w/ the index in the variant, which accounts for the type of the op.
    std::size_t seed = val->Op().index();
    // Then some operations w/ members need to reason about the hash of those members:
    seed = HashCombine(seed, std::visit([&](const auto& op) { return op.Hash(); }, val->Op()));
    for (const ir::ValuePtr& operand : val->Operands()) {
      const uint32_t val_name = operand->Name();
      seed = HashCombine(seed, static_cast<std::size_t>(val_name));
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
            return a.IsSame(b);
          } else {
            return false;
          }
        },
        a->Op(), b->Op());
    if (!ops_match) {
      return false;
    }
    return a->OperandsMatch(b);
  }
};

}  // namespace math::ir

// Formatter for Value
template <>
struct fmt::formatter<math::ir::Value, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::ir::Value& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.Name());
  }
};

// Formatter for pointer to Value
template <>
struct fmt::formatter<math::ir::ValuePtr, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::ir::ValuePtr x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x->Name());
  }
};
