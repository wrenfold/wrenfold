// Copyright 2023 Gareth Cross
#pragma once
#include <array>
#include <variant>
#include <vector>

#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"
#include "wf/expressions/variable.h"
#include "wf/hashing.h"
#include "wf/non_null_ptr.h"
#include "wf/template_utils.h"

// Define types for a very simple "intermediate representation" we can use to simplify
// and reduce the tree of mathematical operations.
namespace math::ir {

class value;
class block;
using value_ptr = non_null_ptr<ir::value>;
using block_ptr = non_null_ptr<ir::block>;

// Add together two operands.
class add {
 public:
  constexpr static bool is_commutative() noexcept { return true; }
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr std::string_view to_string() const noexcept { return "add"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const add&) const noexcept { return true; }
};

// Cast the operand to the specified destination type.
class cast {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 1; }
  constexpr std::string_view to_string() const noexcept { return "cast"; }
  constexpr std::size_t hash_seed() const noexcept {
    return static_cast<std::size_t>(destination_type_);
  }
  constexpr bool is_same(const cast& other) const noexcept {
    return destination_type_ == other.destination_type_;
  }

  // Construct w/ destination type.
  constexpr explicit cast(code_numeric_type destination) noexcept
      : destination_type_(destination) {}

  // Access the target type we are casting to.
  constexpr code_numeric_type destination_type() const noexcept { return destination_type_; }

 private:
  code_numeric_type destination_type_;
};

// A call to a built-in mathematical function like sin, cos, log, etc.
class call_std_function {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return -1; }
  constexpr std::string_view to_string() const noexcept {
    return string_from_standard_library_function(name_);
  }
  constexpr std::size_t hash_seed() const noexcept { return static_cast<std::size_t>(name_); }
  constexpr bool is_same(const call_std_function& other) const noexcept {
    return name_ == other.name_;
  }

  explicit constexpr call_std_function(std_math_function name) noexcept : name_(name) {}

  constexpr std_math_function name() const noexcept { return name_; }

 private:
  std_math_function name_;
};

// Compare two operands (equality or inequality).
class compare {
 public:
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr static bool is_commutative() noexcept { return false; }

  constexpr std::string_view to_string() const noexcept {
    switch (operation_) {
      case relational_operation::less_than:
        return "lt";
      case relational_operation::less_than_or_equal:
        return "lte";
      case relational_operation::equal:
        return "eq";
    }
    return "<NOT A VALID ENUM VALUE>";
  }

  constexpr std::size_t hash_seed() const noexcept { return static_cast<std::size_t>(operation_); }
  constexpr bool is_same(const compare& comp) const noexcept {
    return operation_ == comp.operation_;
  }

  explicit constexpr compare(relational_operation operation) noexcept : operation_(operation) {}

  // Access operation enum.
  constexpr relational_operation operation() const noexcept { return operation_; }

 private:
  relational_operation operation_;
};

// A trinary we use prior to insertion of conditional logic:
// cond(a, b, c) = a ? b : c;
// This expression is used to simplify duplicate elimination. It is then replaced by conditional
// logic and phi functions.
class cond {
 public:
  constexpr static int num_value_operands() noexcept { return 3; }
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr std::string_view to_string() const noexcept { return "cond"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const cond&) const noexcept { return true; }
};

// Copy the operand.
class copy {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 1; }
  constexpr std::string_view to_string() const noexcept { return "copy"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const copy&) const noexcept { return true; }
};

// Divide first operand by the second one.
class div {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr std::string_view to_string() const noexcept { return "div"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const div&) const noexcept { return true; }
};

// This objects acts as a sink to indicate that a value will be used in the output code
// to adjust control flow. The single operand is a boolean value that will ultimately
// determine which path an if-else statement takes.
class jump_condition {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 1; }
  constexpr std::string_view to_string() const noexcept { return "jcnd"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const jump_condition&) const noexcept { return true; }
};

// A source to insert input values (either constants or function arguments) into the IR. Has no
// value arguments, but has an expression.
class load {
 public:
  using storage_type = std::variant<symbolic_constant, integer_constant, float_constant,
                                    rational_constant, variable>;

  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 0; }
  constexpr std::string_view to_string() const noexcept { return "load"; }

  std::size_t hash_seed() const {
    return std::visit([](const auto& contents) { return hash(contents); }, variant_);
  }

  //  Check if the underlying variants contain identical expressions.
  bool is_same(const load& other) const {
    return std::visit(
        [](const auto& a, const auto& b) {
          if constexpr (std::is_same_v<decltype(a), decltype(b)>) {
            return a.is_identical_to(b);
          } else {
            return false;
          }
        },
        variant_, other.variant_);
  }

  // Returns true if variant contains one of the types `Ts...`
  template <typename... Ts>
  bool is_type() const noexcept {
    return ((std::holds_alternative<Ts>(variant_)) || ...);
  }

  // Construct with one of the types in `storage_type`.
  explicit load(storage_type contents) : variant_{std::move(contents)} {}

  // Access the underlying variant of input expressions.
  constexpr const storage_type& variant() const noexcept { return variant_; }

 private:
  // Variant of different leaf expression types.
  storage_type variant_;
};

// Multiply together two operands.
class mul {
 public:
  constexpr static bool is_commutative() noexcept { return true; }
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr std::string_view to_string() const noexcept { return "mul"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const mul&) const noexcept { return true; }
};

// Evaluates to true if the specified output index is required.
class output_required {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 0; }
  constexpr std::string_view to_string() const noexcept { return "oreq"; }
  std::size_t hash_seed() const noexcept { return hash_string_fnv(name_); }
  bool is_same(const output_required& other) const noexcept { return name_ == other.name_; }

  // Construct with string name of the relevant optional output argument.
  explicit output_required(std::string name) : name_(std::move(name)) {}

  // Name of the output argument.
  constexpr const std::string& name() const noexcept { return name_; }

 private:
  std::string name_;
};

// Phi function. The output is equal to whichever operand was generated on the evaluated code-path.
class phi {
 public:
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr std::string_view to_string() const noexcept { return "phi"; }
  constexpr std::size_t hash_seed() const noexcept { return 0; }
  constexpr bool is_same(const phi&) const noexcept { return true; }
};

// A sink used to indicate that a value is consumed by the output (for example in a return type or
// an output argument). Operands to `save` are never eliminated.
class save {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept {
    return -1;  //  Dynamic
  }
  constexpr std::string_view to_string() const noexcept { return "save"; }
  std::size_t hash_seed() const { return hash_struct<output_key>{}(key_); }
  bool is_same(const save& other) const noexcept { return key_ == other.key_; }

  // Construct with key.
  explicit save(output_key key) noexcept : key_(std::move(key)) {}

  // Get output key.
  constexpr const output_key& key() const noexcept { return key_; }

 private:
  output_key key_;
};

// Different operations are represented by a variant.
using operation = std::variant<add, call_std_function, cast, compare, cond, copy, div,
                               jump_condition, load, mul, output_required, phi, save>;

// A block of operations:
class block {
 public:
  using unique_ptr = std::unique_ptr<block>;

  // Construct w/ counter.
  explicit block(std::size_t name) noexcept : name(name) {}

  // Unique number to refer to the block (a counter).
  std::size_t name;

  // All the operations in the block, in order.
  // TODO: There is an argument to be made that this should be an intrusive double-linked-list.
  // For now I'm going with vec of pointers, since this is simpler to get started, even though it
  // means we'll have some O(N) operations (for moderately small N).
  std::vector<value_ptr> operations{};

  // Ancestor blocks (blocks that preceded this one).
  std::vector<block_ptr> ancestors{};

  // Descendants of this block (blocks we jump to from this one).
  std::vector<block_ptr> descendants{};

  // True if the block has no operations.
  bool is_empty() const noexcept { return operations.empty(); }

  // True if this block has no ancestors (typically only true for the starting block).
  bool has_no_ancestors() const noexcept { return ancestors.empty(); }

  // Replace descendant `target` w/ `replacement`.
  void replace_descendant(ir::block_ptr target, ir::block_ptr replacement);

  // Insert block into `ancestors` vector.
  void add_ancestor(block_ptr b);

  // Remove block from `ancestors` vector.
  void remove_ancestor(block_ptr b);

  // Add `b` as a descendant, and add `this` as an ancestor of b.
  void add_descendant(ir::block_ptr b);

  // Count instances of operation of type `T`.
  template <typename Func>
  std::size_t count_operation(Func&& func) const;
};

// Values are the result of any instruction we store in the IR.
// All values have a name (an integer), and an operation that computed them.
// Values may be used as operands to other operations.
class value {
 public:
  using unique_ptr = std::unique_ptr<value>;
  using operands_container = std::vector<ir::value_ptr>;

  // Construct:
  template <typename OpType>
  value(uint32_t name, ir::block_ptr parent, OpType&& operation,
        std::optional<code_numeric_type> numeric_type, operands_container&& operands)
      : name_(name),
        parent_(parent),
        op_(std::forward<OpType>(operation)),
        operands_(std::move(operands)),
        numeric_type_(numeric_type) {
    notify_operands();
    if constexpr (OpType::is_commutative()) {
      // Sort operands for commutative operations so everything is a canonical order.
      sort_operands();
    }
    check_num_operands<OpType>();
  }

  // Enable if `Args` are all types that are convertible to value_ptr.
  template <typename... Args>
  using enable_if_convertible_to_value_ptr =
      std::enable_if_t<std::conjunction_v<std::is_convertible<Args, value_ptr>...>>;

  // Construct with input values specified as variadic arg list.
  template <typename Op, typename... Args, typename = enable_if_convertible_to_value_ptr<Args...>>
  value(uint32_t name, ir::block_ptr parent, Op&& operation,
        std::optional<code_numeric_type> numeric_type, Args... args)
      : value(name, parent, std::forward<Op>(operation), numeric_type,
              operands_container{args...}) {}

  // Access underlying integer.
  constexpr uint32_t name() const noexcept { return name_; }

  // Get the parent block.
  ir::block_ptr parent() const noexcept { return parent_; }

  // Set the parent pointer.
  void set_parent(ir::block_ptr b);

  // Access underlying operation
  constexpr const operation& value_op() const noexcept { return op_; }

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
  constexpr bool is_phi() const noexcept { return is_type<ir::phi>(); }

  // True if any values that consume this one are phi functions.
  bool is_consumed_by_phi() const noexcept;

  // Replace an operand to this instruction with another.
  void replace_operand(value_ptr old, value_ptr replacement);

  // Change the underlying operation that computes this value.
  template <typename OpType, typename... Args>
  void set_value_op(OpType&& op, std::optional<code_numeric_type> numeric_type, Args... args) {
    // Notify existing operands we no longer reference them
    for (const value_ptr& operand : operands_) {
      operand->remove_consumer(this);
    }
    // Record new operands:
    operands_.clear();
    (operands_.push_back(args), ...);
    numeric_type_ = numeric_type;
    op_ = std::forward<OpType>(op);
    notify_operands();
    if constexpr (OpType::is_commutative()) {
      sort_operands();
    }
    check_num_operands<OpType>();
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
    return consumers_.empty() && !is_type<ir::save>() && !is_type<ir::jump_condition>();
  }

  // Access the underlying numeric type.
  code_numeric_type numeric_type() const {
    WF_ASSERT(numeric_type_.has_value());
    return *numeric_type_;
  }

 protected:
  // If the underlying operation is commutative, sort the operands by name.
  void maybe_sort_operands();

  // Sort operands in-place by increasing value of name.
  void sort_operands() {
    std::sort(operands_.begin(), operands_.end(),
              [](const value_ptr& a, const value_ptr& b) { return a->name() < b->name(); });
  }

  template <typename OpType>
  void check_num_operands() {
    constexpr int expected_num_args = OpType::num_value_operands();
    if constexpr (expected_num_args >= 0) {
      WF_ASSERT_EQUAL(static_cast<std::size_t>(expected_num_args), operands_.size());
    }
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

  // The cached numeric type of this operation. Optional because some operations lack a type.
  std::optional<code_numeric_type> numeric_type_;
};

// Count instances of operation of type `T`.
// Defined here so that we can use methods on `value`.
template <typename Func>
std::size_t block::count_operation(Func&& func) const {
  return std::count_if(operations.begin(), operations.end(), [&func](ir::value_ptr v) -> bool {
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
struct hash_struct<ir::value_ptr> {
  std::size_t operator()(const ir::value_ptr& val) const {
    // Seed the hash w/ the index in the variant, which accounts for the type of the op.
    std::size_t seed = val->value_op().index();
    // Then some operations w/ members need to reason about the hash of those members:
    seed = hash_combine(
        seed, std::visit([&](const auto& op) { return op.hash_seed(); }, val->value_op()));
    for (const ir::value_ptr& operand : val->operands()) {
      const uint32_t val_name = operand->name();
      seed = hash_combine(seed, static_cast<std::size_t>(val_name));
    }
    return seed;
  }
};

}  // namespace math

// Formatter for value
template <>
struct fmt::formatter<math::ir::value, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::ir::value& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.name());
  }
};

// Formatter for pointer to value
template <>
struct fmt::formatter<math::ir::value_ptr, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::ir::value_ptr x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x->name());
  }
};
