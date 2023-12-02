// Copyright 2023 Gareth Cross
#include "wf/code_generation/ir_builder.h"

#include <algorithm>
#include <deque>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "wf/common_visitors.h"
#include "wf/expressions/all_expressions.h"
#include "wf/hashing.h"
#include "wf/visitor_impl.h"

namespace math {
namespace ir {

void Block::replace_descendant(ir::BlockPtr target, ir::BlockPtr replacement) {
  WF_ASSERT_NOT_EQUAL(target, replacement);

  if (!operations.empty()) {
    const ir::ValuePtr jump_val = operations.back();
    if (jump_val->is_type<ir::JumpCondition>()) {
      WF_ASSERT_EQUAL(2, descendants.size());
    } else {
      WF_ASSERT_GREATER_OR_EQ(1, descendants.size());
    }
  }

  const ir::BlockPtr self{this};
  target->remove_ancestor(self);
  replacement->add_ancestor(self);

  auto it = std::find(descendants.begin(), descendants.end(), target);
  WF_ASSERT(it != descendants.end());
  *it = replacement;
}

void Block::add_ancestor(BlockPtr b) {
  WF_ASSERT(std::find(ancestors.begin(), ancestors.end(), b) == ancestors.end(),
            "Attempted to insert duplicate into ancestor list: {}", b->name);
  ancestors.push_back(b);
}

void Block::remove_ancestor(BlockPtr b) {
  auto it = std::find(ancestors.begin(), ancestors.end(), b);
  WF_ASSERT(it != ancestors.end(), "Block {} is not an ancestor of {}", b->name, name);
  ancestors.erase(it);
}

void Block::add_descendant(BlockPtr b) {
  WF_ASSERT(std::find(descendants.begin(), descendants.end(), b) == descendants.end(),
            "Block {} already exists in descendants list: {},", b, fmt::join(descendants, ", "));
  descendants.push_back(b);
  b->add_ancestor(ir::BlockPtr{this});
}

void Value::set_parent(ir::BlockPtr b) {
  WF_ASSERT(!std::holds_alternative<ir::JumpCondition>(op_));
  parent_ = b;
}

bool Value::is_consumed_by_phi() const noexcept {
  return std::any_of(consumers_.begin(), consumers_.end(),
                     [](const ValuePtr& v) { return v->is_phi(); });
}

void Value::replace_operand(const ValuePtr old, const ValuePtr replacement) {
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

void Value::add_consumer(const ir::ValuePtr v) {
  // The value might be consumed twice by the same expression, for instance: pow(x, x)
  auto it = std::find(consumers_.begin(), consumers_.end(), v);
  if (it == consumers_.end()) {
    consumers_.push_back(v);
  }
}

void Value::remove_consumer(ir::Value* const v) {
  auto it = std::find(consumers_.begin(), consumers_.end(), v);
  if (it != consumers_.end()) {
    // Might have already been removed, if we were an operand twice.
    consumers_.erase(it);
  }
}

bool Value::operands_match(const ValuePtr other) const noexcept {
  if (operands_.size() != other->operands_.size()) {
    return false;
  }
  return std::equal(operands_.begin(), operands_.end(), other->operands_.begin());
}

void Value::replace_with(const ValuePtr other) {
  const ValuePtr self{this};
  WF_ASSERT_NOT_EQUAL(self, other);
  for (const ValuePtr& consumer : consumers_) {
    consumer->replace_operand(self, other);
  }
  consumers_.clear();
}

void Value::remove() {
  WF_ASSERT(consumers_.empty(), "Attempting to remove a value `{}` that is consumed by: [{}]",
            name_, fmt::join(consumers_, ","));
  // Notify our operands we no longer consume them.
  for (const ValuePtr& operand : operands_) {
    operand->remove_consumer(this);
  }
  // This value is dead, so clear the operand vector
  operands_.clear();
}

void Value::maybe_sort_operands() {
  const bool commutative = std::visit([](const auto& op) { return op.is_commutative(); }, op_);
  if (commutative) {
    sort_operands();
  }
}

void Value::notify_operands() {
  const ValuePtr self{this};
  for (const ValuePtr& operand : operands_) {
    operand->add_consumer(self);
  }
}

template <typename T>
struct format_op_args_struct {
  template <typename Container>
  void operator()(std::string& output, const T&, const Container& operands,
                  const std::size_t width) {
    for (auto it = operands.begin(); it != operands.end(); ++it) {
      const ir::ValuePtr& val = *it;
      fmt::format_to(std::back_inserter(output), "v{:0>{}}", val->name(), width);
      if (std::next(it) != operands.end()) {
        output += ", ";
      }
    }
  }
};

template <>
struct format_op_args_struct<ir::Load> {
  template <typename Container>
  void operator()(std::string& output, const ir::Load& load, const Container&, const std::size_t) {
    PlainFormatter formatter{};
    std::visit(formatter, load.variant());
    output += formatter.take_output();
  }
};

template <>
struct format_op_args_struct<ir::OutputRequired> {
  template <typename Container>
  void operator()(std::string& output, const ir::OutputRequired& oreq, const Container&,
                  const std::size_t) {
    fmt::format_to(std::back_inserter(output), "{}", oreq.name());
  }
};

template <typename Container>
void format_op_args(std::string& output, const ir::Operation& op, const Container& operands,
                    const std::size_t width) {
  std::visit(
      [&](const auto& op) {
        using T = std::decay_t<decltype(op)>;
        format_op_args_struct<T>{}(output, op, operands, width);
      },
      op);
}

}  // namespace ir

template <typename OpType, typename... Args>
static ir::ValuePtr create_operation(std::vector<ir::Value::unique_ptr>& values, ir::BlockPtr block,
                                     OpType&& op, std::optional<NumericType> numeric_type,
                                     Args&&... args) {
  // Create a new value:
  const uint32_t name = values.empty() ? 0 : (values.back()->name() + 1);
  std::unique_ptr<ir::Value> value = std::make_unique<ir::Value>(
      name, block, std::forward<OpType>(op), numeric_type, std::forward<Args>(args)...);
  // Insert int the provided block:
  block->operations.emplace_back(value.get());
  // This is owned by the `values` vector:
  values.push_back(std::move(value));
  return block->operations.back();
}

struct operation_term_counts {
  // The key in this table is a sub-expression (a term) in a multiplication or addition.
  // The value is the # of times that term appears in a unique addition or multiplication sequence.
  using count_container =
      std::unordered_map<Expr, std::size_t, hash_struct<Expr>, is_identical_struct<Expr>>;

  count_container muls;
  count_container adds;
};

// Count incidences of unique multiplications and additions.
struct mul_add_count_visitor {
  // A map of all expressions that appear as terms in an addition or multiplication.
  // The key is the term, and the value is a count of inbound edges.
  operation_term_counts::count_container adds{};
  operation_term_counts::count_container muls{};

  // Track which nodes we have already visited, so that we do not double count repeated operations.
  std::unordered_set<Expr, hash_struct<Expr>, is_identical_struct<Expr>> visited;

  mul_add_count_visitor() {
    adds.reserve(100);
    muls.reserve(100);
    visited.reserve(100);
  }

  // Visit every expression in the provided expression group.
  void count_group_expressions(const ExpressionGroup& group) {
    for (const Expr& expr : group.expressions) {
      visit(expr, *this);
    }
  }

  template <typename T>
  void operator()(const T& concrete) {
    if constexpr (!T::IsLeafNode) {
      // If this is an add or a mul, record the incoming edges to children.
      if constexpr (std::is_same_v<T, Addition>) {
        for (const Expr& child : concrete) {
          adds[child]++;
        }
      } else if constexpr (std::is_same_v<T, Multiplication>) {
        for (const Expr& child : concrete) {
          muls[child]++;
        }
      }

      // Recurse:
      for (const Expr& child : concrete) {
        if (!visited.count(child)) {
          visited.insert(child);
          visit(child, *this);
        }
      }
    }
  }

  // Return final counts of how many times each term appears in a unique addition
  // or multiplication.
  operation_term_counts take_counts() {
    operation_term_counts counts{};
    counts.muls = std::move(muls);
    counts.adds = std::move(adds);
    return counts;
  }
};

// Visitor for converting an expression tree into static-single-assignment form.
struct IRFormVisitor {
  explicit IRFormVisitor(FlatIr& builder, operation_term_counts&& counts)
      : builder_(builder), counts_(counts) {}

  ir::ValuePtr maybe_cast(ir::ValuePtr input, NumericType output_type) {
    if (input->numeric_type() != output_type) {
      return push_operation(ir::Cast{output_type}, output_type, input);
    } else {
      return input;
    }
  }

  template <typename T>
  constexpr const operation_term_counts::count_container& get_count_table() const noexcept {
    if constexpr (std::is_same_v<T, Multiplication>) {
      return counts_.muls;
    } else {
      return counts_.adds;
    }
  }

  // Handler for additions and multiplications:
  template <typename T>
  ir::ValuePtr convert_addition_or_multiplication(const T& op) {
    // Sort first by frequency of occurrence, then in ambiguous cases by expression order:
    Multiplication::ContainerType expressions{op.begin(), op.end()};
    std::sort(expressions.begin(), expressions.end(), [&](const Expr& a, const Expr& b) {
      const operation_term_counts::count_container& count_table = get_count_table<T>();
      const std::size_t count_a = count_table.at(a);
      const std::size_t count_b = count_table.at(b);
      if (count_a > count_b) {
        return true;
      } else if (count_a < count_b) {
        return false;
      } else {
        return expression_order_struct{}(a, b);
      }
    });

    // first recursively transform all the inputs
    std::vector<ir::ValuePtr> args;
    args.reserve(op.arity());
    std::transform(expressions.begin(), expressions.end(), std::back_inserter(args),
                   [this](const Expr& expr) { return apply(expr); });

    NumericType promoted_type = NumericType::Integer;
    for (ir::ValuePtr v : args) {
      promoted_type = std::max(promoted_type, v->numeric_type());
    }

    // then create multiplications or adds for this expression:
    ir::ValuePtr prev_result = maybe_cast(args[0], promoted_type);
    for (std::size_t i = 1; i < args.size(); ++i) {
      if constexpr (std::is_same_v<T, Multiplication>) {
        prev_result = push_operation(ir::Mul{}, promoted_type, prev_result,
                                     maybe_cast(args[i], promoted_type));
      } else {
        prev_result = push_operation(ir::Add{}, promoted_type, prev_result,
                                     maybe_cast(args[i], promoted_type));
      }
    }
    return prev_result;
  }

  ir::ValuePtr operator()(const Addition& add, const Expr& add_abstract) {
    // For additions, first check if the negated version has already been cached:
    const Expr negative_add = -add_abstract;
    if (auto it = computed_values_.find(negative_add); it != computed_values_.end()) {
      const auto promoted_type = std::max(it->second->numeric_type(), NumericType::Integer);
      const ir::ValuePtr negative_one = maybe_cast(apply(Constants::NegativeOne), promoted_type);
      return push_operation(ir::Mul{}, promoted_type, it->second, negative_one);
    }
    return convert_addition_or_multiplication(add);
  }

  ir::ValuePtr operator()(const CastBool& cast) {
    const ir::ValuePtr arg = apply(cast.arg());
    return push_operation(ir::Cast{NumericType::Integer}, NumericType::Integer, arg);
  }

  ir::ValuePtr operator()(const Conditional& cond) {
    const ir::ValuePtr condition = apply(cond.condition());
    const ir::ValuePtr if_branch = apply(cond.if_branch());
    const ir::ValuePtr else_branch = apply(cond.else_branch());

    const NumericType promoted_type =
        std::max(if_branch->numeric_type(), else_branch->numeric_type());
    return push_operation(ir::Cond{}, promoted_type, condition,
                          maybe_cast(if_branch, promoted_type),
                          maybe_cast(else_branch, promoted_type));
  }

  static NumericType numeric_type_from_constant(const Constant& c) {
    switch (c.name()) {
      case SymbolicConstants::Euler:
      case SymbolicConstants::Pi:
        return NumericType::Real;
      case SymbolicConstants::True:
      case SymbolicConstants::False:
        return NumericType::Bool;
    }
    throw TypeError("Unhandled symbolic constant: {}", string_from_symbolic_constant(c.name()));
  }

  ir::ValuePtr operator()(const Constant& c) {
    return push_operation(ir::Load{c}, numeric_type_from_constant(c));
  }

  ir::ValuePtr operator()(const Derivative&) {
    throw TypeError("Cannot generate code for expressions containing `{}`.", Derivative::NameStr);
  }

  static constexpr StdMathFunction std_math_function_from_built_in(BuiltInFunction name) {
    switch (name) {
      case BuiltInFunction::Cos:
        return StdMathFunction::Cos;
      case BuiltInFunction::Sin:
        return StdMathFunction::Sin;
      case BuiltInFunction::Tan:
        return StdMathFunction::Tan;
      case BuiltInFunction::ArcCos:
        return StdMathFunction::ArcCos;
      case BuiltInFunction::ArcSin:
        return StdMathFunction::ArcSin;
      case BuiltInFunction::ArcTan:
        return StdMathFunction::ArcTan;
      case BuiltInFunction::Log:
        return StdMathFunction::Log;
      case BuiltInFunction::Abs:
        return StdMathFunction::Abs;
      case BuiltInFunction::Signum:
        return StdMathFunction::Signum;
      case BuiltInFunction::Arctan2:
        return StdMathFunction::Arctan2;
    }
    throw AssertionError("Invalid enum value: {}", string_from_built_in_function(name));
  }

  ir::ValuePtr operator()(const Function& func) {
    ir::Value::operands_container args;
    args.reserve(func.arity());
    std::transform(func.begin(), func.end(), std::back_inserter(args),
                   [this](const Expr& expr) { return apply(expr); });
    const StdMathFunction enum_value = std_math_function_from_built_in(func.enum_value());
    // TODO: Special case for `abs` and `signum` here.
    return push_operation(ir::CallStdFunction{enum_value}, NumericType::Real, std::move(args));
  }

  ir::ValuePtr operator()(const Infinity&) const {
    throw TypeError("Cannot generate code for complex infinity.");
  }

  ir::ValuePtr operator()(const Integer& i) {
    return push_operation(ir::Load{i}, NumericType::Integer);
  }

  ir::ValuePtr operator()(const Float& f) { return push_operation(ir::Load{f}, NumericType::Real); }

  // Apply exponentiation by squaring to implement a power of an integer.
  ir::ValuePtr exponentiate_by_squaring(ir::ValuePtr base, uint64_t exponent) {
    if (exponent == 0) {
      return apply(Constants::One);
    }
    // TODO: Somewhat lazy way of handling the first iteration - use an empty optional.
    std::optional<ir::ValuePtr> result{};
    for (;;) {
      if (exponent & 1) {
        result = result.has_value() ? push_operation(ir::Mul{}, base->numeric_type(), *result, base)
                                    : base;
      }
      exponent /= 2;
      if (exponent == 0) {
        break;
      }
      base = push_operation(ir::Mul{}, base->numeric_type(), base, base);
    }
    return result.value();
  }

  ir::ValuePtr operator()(const Multiplication& mul) {
    // Extract constants here? Empirically it appears to introduce more expressions, not fewer.
    return convert_addition_or_multiplication(mul);
  }

  ir::ValuePtr operator()(const Power& power) {
    const ir::ValuePtr base = maybe_cast(apply(power.base()), NumericType::Real);

    // Check if this exponent has a negative coefficient on it:
    const auto [exp_coefficient, exp_mul] = as_coeff_and_mul(power.exponent());
    if (is_negative_number(exp_coefficient)) {
      // Construct the reciprocal version of this power.
      const Expr reciprocal = pow(power.base(), -power.exponent());
      const ir::ValuePtr reciprocal_value = apply(reciprocal);

      // Write the power as: 1 / pow(base, -exponent)
      const ir::ValuePtr one = apply(Constants::One);
      constexpr NumericType promoted_type = NumericType::Real;
      return push_operation(ir::Div{}, promoted_type, maybe_cast(one, promoted_type),
                            maybe_cast(reciprocal_value, promoted_type));
    }

    constexpr int max_integer_mul_exponent = 16;
    if (const Integer* exp_int = cast_ptr<Integer>(power.exponent()); exp_int != nullptr) {
      WF_ASSERT_GREATER_OR_EQ(exp_int->get_value(), 0, "Negative exponents were handled above");
      // Maximum exponent below which we rewrite `pow` as a series of multiplications.
      // Have not experimented with this cutoff much, but on GCC94 and Clang17, using a series of
      // multiplications is still faster even past x^32.
      if (exp_int->get_value() <= max_integer_mul_exponent) {
        return exponentiate_by_squaring(base, static_cast<uint64_t>(exp_int->get_value()));
      } else {
        // Just call power variant with integer exponent:
        return push_operation(ir::CallStdFunction{StdMathFunction::Powi}, NumericType::Real, base,
                              apply(power.exponent()));
      }
    } else if (const Rational* exp_rational = cast_ptr<Rational>(power.exponent());
               exp_rational != nullptr) {
      WF_ASSERT_GREATER_OR_EQ(exp_rational->numerator(), 0, "rational = {}", *exp_rational);
      // If the denominator is 1/2 and the exponent is small, it is faster to do power
      // exponentiation followed by sqrt. This is not the case for cbrt, where pow() is the same
      // approximate performance.
      if (exp_rational->denominator() == 2 &&
          exp_rational->numerator() <= max_integer_mul_exponent) {
        const ir::ValuePtr sqrt =
            push_operation(ir::CallStdFunction{StdMathFunction::Sqrt}, NumericType::Real, base);
        return exponentiate_by_squaring(sqrt, static_cast<uint64_t>(exp_rational->numerator()));
      }
    }

    // TODO: Support (int ** int) powers?
    const ir::ValuePtr exponent = apply(power.exponent());
    return push_operation(ir::CallStdFunction{StdMathFunction::Powf}, NumericType::Real, base,
                          maybe_cast(exponent, NumericType::Real));
  }

  ir::ValuePtr operator()(const Rational& r) {
    return push_operation(ir::Load{r}, NumericType::Real);
  }

  ir::ValuePtr operator()(const Relational& relational) {
    ir::ValuePtr left = apply(relational.left());
    ir::ValuePtr right = apply(relational.right());
    NumericType promoted_type = std::max(left->numeric_type(), right->numeric_type());
    return push_operation(ir::Compare{relational.operation()}, NumericType::Bool,
                          maybe_cast(left, promoted_type), maybe_cast(right, promoted_type));
  }

  ir::ValuePtr operator()(const Undefined&) const {
    throw TypeError("Cannot generate code with expressions containing {}", Undefined::NameStr);
  }

  ir::ValuePtr operator()(const Variable& var) {
    return push_operation(ir::Load{var}, NumericType::Real);
  }

  template <typename OpType, typename... Args>
  ir::ValuePtr push_operation(OpType&& op, std::optional<NumericType> numeric_type,
                              Args&&... args) {
    return create_operation(builder_.values_, builder_.get_block(), std::forward<OpType>(op),
                            numeric_type, std::forward<Args>(args)...);
  }

  // Check if a value has been computed. If not, convert it and return the result.
  ir::ValuePtr apply(const Expr& expr) {
    if (auto it = computed_values_.find(expr); it != computed_values_.end()) {
      return it->second;
    }
    ir::ValuePtr val = visit_with_expr(expr, *this);
    computed_values_.emplace(expr, val);
    return val;
  }

 private:
  FlatIr& builder_;

  std::unordered_map<Expr, ir::ValuePtr, hash_struct<Expr>, is_identical_struct<Expr>>
      computed_values_;

  const operation_term_counts counts_;
};

FlatIr::FlatIr(const std::vector<ExpressionGroup>& groups)
    : block_(std::make_unique<ir::Block>(0)) {
  // First pass where we count occurrences of some sub-expressions:
  mul_add_count_visitor count_visitor{};
  for (const ExpressionGroup& group : groups) {
    count_visitor.count_group_expressions(group);
  }

  IRFormVisitor visitor{*this, count_visitor.take_counts()};
  for (const ExpressionGroup& group : groups) {
    // Transform expressions into Values
    ir::Value::operands_container group_values{};
    group_values.reserve(group.expressions.size());
    std::transform(group.expressions.begin(), group.expressions.end(),
                   std::back_inserter(group_values), [&](const Expr& expr) {
                     ir::ValuePtr output = visitor.apply(expr);
                     if (output->numeric_type() != NumericType::Real) {
                       // TODO: Allow returning other types - derive the numeric type from the
                       // group.
                       output = create_operation(values_, get_block(), ir::Cast{NumericType::Real},
                                                 NumericType::Real, output);
                     }
                     return output;
                   });

    // Then create a sink to consume these values, the `Save` operation is the sink:
    create_operation(values_, get_block(), ir::Save{group.key}, std::nullopt,
                     std::move(group_values));
  }
}

inline constexpr std::size_t compute_print_width(std::size_t num_assignments) {
  std::size_t width = 1;
  for (; num_assignments > 0; ++width) {
    num_assignments /= 10;
  }
  return width;
}

std::string FlatIr::to_string() const {
  const std::size_t width = value_print_width();
  std::string output{};

  for (const ir::ValuePtr& code : block_->operations) {
    // Print the value name:
    fmt::format_to(std::back_inserter(output), "  v{:0>{}} <- ", code->name(), width);

    // Print the instruction name:
    constexpr int operation_width = 4;
    fmt::format_to(std::back_inserter(output), "{:>{}} ",
                   std::visit([](const auto& op) { return op.to_string(); }, code->value_op()),
                   operation_width);
    format_op_args(output, code->value_op(), code->operands(), width);
    output += "\n";
  }

  if (!output.empty()) {
    output.pop_back();
  }
  return output;
}

std::size_t FlatIr::value_print_width() const {
  const uint32_t highest_value_name = values_.empty() ? 0 : values_.back()->name();
  return compute_print_width(highest_value_name);
}

// Test two values for equality. The operation must be the same type, and the operands must be
// identical. This does not recursively test the operands for equality - the pointer themselves
// must match.
struct value_equality_struct {
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

// A hash-set of values:
using ValueTable =
    std::unordered_set<ir::ValuePtr, hash_struct<ir::ValuePtr>, value_equality_struct>;

// Eliminate duplicates in `block`, using existing values stored in `table`.
inline void local_value_numbering(ir::BlockPtr block, ValueTable& table) {
  for (const ir::ValuePtr& code : block->operations) {
    // Then see if this operation already exists in the map:
    auto [it, was_inserted] = table.insert(code);
    if (!was_inserted) {
      // Propagate the copy:
      if (code->is_consumed_by_phi()) {
        // If this value feeds a phi function, we need to keep it, but turn it into a copy:
        code->set_value_op(ir::Copy{}, (*it)->numeric_type(), *it);
      } else {
        code->replace_with(*it);
      }
    }
  }
}

void FlatIr::eliminate_duplicates() {
  ValueTable table{};
  table.reserve(values_.size());
  local_value_numbering(get_block(), table);
  strip_unused_values();
}

void FlatIr::strip_unused_values() {
  // Somewhat lazy: Reverse the operations so that we can use forward iterator, then reverse back.
  ir::BlockPtr block{block_};
  std::reverse(block->operations.begin(), block->operations.end());
  const auto new_end =
      std::remove_if(block->operations.begin(), block->operations.end(), [&](ir::ValuePtr v) {
        if (v->is_unused()) {
          v->remove();
          return true;
        }
        return false;
      });
  block->operations.erase(new_end, block->operations.end());
  std::reverse(block->operations.begin(), block->operations.end());
}

inline bool is_countable_operation(ir::ValuePtr v) {
  return !v->is_type<ir::JumpCondition>() && !v->is_type<ir::Save>() && !v->is_type<ir::Load>() &&
         !v->is_type<ir::Copy>();
}

inline bool is_conditional(ir::ValuePtr v) {
  return v->is_type<ir::JumpCondition>() || v->is_type<ir::Cond>();
}

std::size_t FlatIr::num_operations() const {
  return std::count_if(block_->operations.begin(), block_->operations.end(),
                       &is_countable_operation);
}

std::size_t FlatIr::num_conditionals() const {
  return std::count_if(block_->operations.begin(), block_->operations.end(), &is_conditional);
}

template <typename Container, typename OutputIterator>
static void reverse_copy_save_value_ptrs(const Container& container, ExpressionUsage usage,
                                         OutputIterator output_iterator) {
  for (auto it = container.rbegin(); it != container.rend(); ++it) {
    const ir::ValuePtr val{*it};
    if (const ir::Save* save = std::get_if<ir::Save>(&val->value_op()); save != nullptr) {
      if (save->key().usage == usage) {
        *output_iterator = val;
      }
    }
  }
}

// Extract all `Save` operations (placeholders for the final output values) from `flat_values`
// in reverse order. Return values and require outputs are placed into a queue, and optional
// outputs are returned as a separate vector.
static auto get_reverse_ordered_output_values(
    const std::vector<ir::Value::unique_ptr>& flat_values) {
  // We need to build up an initial queue of all the outputs that are required - these
  // will be processed first. Optional outputs are generated after.
  std::deque<ir::ValuePtr> required_outputs_queue{};
  std::vector<ir::ValuePtr> optional_outputs{};

  reverse_copy_save_value_ptrs(flat_values, ExpressionUsage::ReturnValue,
                               std::back_inserter(required_outputs_queue));

  reverse_copy_save_value_ptrs(flat_values, ExpressionUsage::OutputArgument,
                               std::back_inserter(required_outputs_queue));

  reverse_copy_save_value_ptrs(flat_values, ExpressionUsage::OptionalOutputArgument,
                               std::back_inserter(optional_outputs));

  return std::make_tuple(std::move(required_outputs_queue), std::move(optional_outputs));
}

struct IrConverter {
  void convert() {
    // Conversion will modify `output.values_`, so shallow-copy the outputs first:
    auto [required_outputs_queue, optional_outputs] =
        get_reverse_ordered_output_values(output.values_);

    // Insert computations for required output values:
    std::vector<ir::ValuePtr> deferred_values{};
    ir::BlockPtr next_block =
        process(std::move(required_outputs_queue), output.create_block(), deferred_values);

    // Should be nothing deferred yet:
    WF_ASSERT(deferred_values.empty(), "deferred_values = [{}]", fmt::join(deferred_values, ", "));

    // Traverse optional outputs:
    for (ir::ValuePtr v : optional_outputs) {
      const ir::Save& save = v->as_type<ir::Save>();
      const OutputKey& key = save.key();
      WF_ASSERT(key.usage == ExpressionUsage::OptionalOutputArgument, "Usage: {}",
                string_from_expression_usage(key.usage));

      const ir::BlockPtr left_block_exit = output.create_block();
      left_block_exit->add_descendant(next_block);

      // Insert block to evaluate if this output is required:
      const ir::BlockPtr jump_block = output.create_block();

      // Operation that evaluates whether this argument is required:
      const ir::ValuePtr jump_condition =
          create_operation(output.values_, jump_block, ir::OutputRequired{key.name}, std::nullopt);

      // Either we go into `left_block` and compute the arg outputs, or we skip to `next_block`:
      create_operation(output.values_, jump_block, ir::JumpCondition{}, std::nullopt,
                       jump_condition);
      jump_block->add_descendant(left_block_exit);
      jump_block->add_descendant(next_block);

      std::deque<ir::ValuePtr> queued_left = {v};
      process(std::move(queued_left), left_block_exit, deferred_values);

      std::deque<ir::ValuePtr> queue{deferred_values.begin(), deferred_values.end()};
      deferred_values.clear();
      next_block = process(std::move(queue), jump_block, deferred_values);
      WF_ASSERT(deferred_values.empty(), "deferred_values = [{}]",
                fmt::join(deferred_values, ", "));
    }

    std::deque<ir::ValuePtr> queue;
    std::copy(deferred_values.begin(), deferred_values.end(), std::back_inserter(queue));
    deferred_values.clear();

    process(std::move(queue), next_block, deferred_values);
    WF_ASSERT(deferred_values.empty(), "deferred_values = [{}]", fmt::join(deferred_values, ", "));

    // There should only be one start block:
    WF_ASSERT_EQUAL(
        1,
        std::count_if(output.blocks_.begin(), output.blocks_.end(),
                      [](const ir::Block::unique_ptr& block) { return block->has_no_ancestors(); }),
        "Must be only one entry block");

    // The process above sometimes introduces pointless copies, which we remove now:
    eliminate_useless_copies();
  }

  // True if node `v` has been visited.
  bool is_visited(ir::ValuePtr v) const { return visited.count(v) > 0; }

  // True if all downstream consumers of `v` have been visited.
  bool all_consumers_visited(ir::ValuePtr val) const {
    return val->all_consumers_satisfy([this](ir::ValuePtr v) { return is_visited(v); });
  }

  // Queue any operands of `v` whose consumers have all been visited.
  void queue_operands(std::deque<ir::ValuePtr>& queue, ir::ValuePtr v) const {
    for (ir::ValuePtr val : v->operands()) {
      if (!is_visited(val) && all_consumers_visited(val)) {
        queue.push_back(val);
      }
    }
  }

  // Return true if `parent_block` is executed on all paths through `test_block`.
  static bool parent_is_on_all_paths_through_block(ir::BlockPtr test_block,
                                                   ir::BlockPtr parent_block) {
    if (test_block == parent_block) {
      return true;
    }
    return !test_block->ancestors.empty() &&
           std::all_of(test_block->ancestors.begin(), test_block->ancestors.end(),
                       [&](ir::BlockPtr b) {
                         return parent_is_on_all_paths_through_block(b, parent_block);
                       });
  }

  auto process_non_conditionals(std::deque<ir::ValuePtr>& queue,
                                std::vector<ir::ValuePtr>& deferred,
                                const ir::BlockPtr output_block) {
    // This will contain any conditionals we queue (conditionals whose consumers have all been
    // processed). They are processed outside of this method.
    std::vector<ir::ValuePtr> queued_conditionals{};
    queued_conditionals.reserve(10);

    // This will contain all the operations that can be processed without hitting
    // a blocking conditional. Because we traverse the graph backwards, these are in reverse order.
    // We flip things back at the end of this function.
    std::vector<ir::ValuePtr> output_reversed{};
    output_reversed.reserve(queue.size());

    while (!queue.empty()) {
      ir::ValuePtr top = queue.front();
      queue.pop_front();
      if (is_visited(top)) {
        continue;
      }
      WF_ASSERT(all_consumers_visited(top), "Not all consumers have been visited: {}", top);

      // Check if this block is a valid place to insert this value. This will be the case
      // if `output_block` is on all paths through the downstream consumer blocks. This check
      // ensures we don't write the computation of a value into a scope that isn't visible by all
      // other scopes that need this value.
      const bool is_valid_to_insert = top->all_consumers_satisfy([&](ir::ValuePtr consumer) {
        return parent_is_on_all_paths_through_block(consumer->parent(), output_block);
      });
      if (!is_valid_to_insert) {
        deferred.push_back(top);
        continue;
      }

      if (top->is_type<ir::Cond>()) {
        // Defer conditionals to be processed together later:
        queued_conditionals.push_back(top);
        continue;
      }

      // Put it into the output, then queue its operands.
      top->set_parent(output_block);
      output_reversed.push_back(top);
      visited.insert(top);
      queue_operands(queue, top);
    }

    // Flip things back when inserting into the actual block operations:
    output_block->operations.insert(output_block->operations.begin(), output_reversed.rbegin(),
                                    output_reversed.rend());
    return queued_conditionals;
  }

  ir::BlockPtr process(std::deque<ir::ValuePtr> queue, const ir::BlockPtr output_block,
                       std::vector<ir::ValuePtr>& deferred) {
    std::vector<ir::ValuePtr> queued_conditionals =
        process_non_conditionals(queue, deferred, output_block);
    if (queued_conditionals.empty()) {
      return output_block;
    }

    const auto [condition, grouped_conditionals] =
        select_conditionals_to_group(queued_conditionals);

    WF_ASSERT(std::is_sorted(grouped_conditionals.begin(), grouped_conditionals.end(),
                             [](ir::ValuePtr a, ir::ValuePtr b) { return a->name() > b->name(); }),
              "Should be sorted: {}", fmt::join(grouped_conditionals, ","));

    const ir::BlockPtr left_block_tail = output.create_block();
    const ir::BlockPtr right_block_tail = output.create_block();

    std::deque<ir::ValuePtr> queue_left{};
    std::deque<ir::ValuePtr> queue_right{};
    for (ir::ValuePtr v : grouped_conditionals) {
      // Turn conditionals into phi functions. We insert copies in the left and right blocks so that
      // arguments to the phi functions are computed on the branched code path, even if the
      // computation is just a copy. These copies can be eliminated later, in some cases.
      const ir::ValuePtr copy_left =
          create_operation(output.values_, left_block_tail, ir::Copy{},
                           v->operator[](1)->numeric_type(), v->operator[](1));
      const ir::ValuePtr copy_right =
          create_operation(output.values_, right_block_tail, ir::Copy{},
                           v->operator[](2)->numeric_type(), v->operator[](2));

      v->set_value_op(ir::Phi{}, copy_left->numeric_type(), copy_left, copy_right);
      v->set_parent(output_block);
      visited.insert(v);
      visited.insert(copy_left);
      visited.insert(copy_right);
      queue_operands(queue_left, copy_left);
      queue_operands(queue_right, copy_right);
    }

    // Insert the phi functions at the top of the block:
    output_block->operations.insert(output_block->operations.begin(), grouped_conditionals.rbegin(),
                                    grouped_conditionals.rend());

    // Save the ancestor vector before modifying it by creating jumps:
    const std::vector<ir::BlockPtr> previous_ancestors = output_block->ancestors;

    left_block_tail->add_descendant(output_block);
    right_block_tail->add_descendant(output_block);

    const ir::BlockPtr jump_block = output.create_block();
    const ir::ValuePtr jump_condition =
        create_operation(output.values_, jump_block, ir::JumpCondition{}, std::nullopt, condition);
    visited.insert(jump_condition);

    jump_block->add_descendant(left_block_tail);
    jump_block->add_descendant(right_block_tail);

    // Any blocks that jumped to `output_block` should now jump to `jump_block` instead:
    for (ir::BlockPtr ancestor : previous_ancestors) {
      // If this block is our ancestor, it must contain a jump at the end:
      WF_ASSERT(!ancestor->operations.empty() && !ancestor->descendants.empty(),
                "Block cannot be empty, must contain a jump");
      ancestor->replace_descendant(output_block, jump_block);
    }

    // Process left and right sides:
    std::vector<ir::ValuePtr> process_later;
    process(std::move(queue_left), left_block_tail, process_later);
    process(std::move(queue_right), right_block_tail, process_later);

    // Queue the nodes we deferred
    queue.clear();
    if (all_consumers_visited(condition)) {
      queue.push_back(condition);
    }
    queue.insert(queue.end(), process_later.begin(), process_later.end());
    queue.insert(queue.end(), queued_conditionals.begin(), queued_conditionals.end());

    return process(std::move(queue), jump_block, deferred);
  }

  // In the process of converting, we inserted copies to satisfy the phi functions.
  // In some cases, these copies are just duplicating values computed in the same scope.
  void eliminate_useless_copies() {
    for (const auto& block : output.blocks_) {
      auto new_end =
          std::remove_if(block->operations.begin(), block->operations.end(), [&](ir::ValuePtr v) {
            // A copy is useless if we are duplicating a value in our current block:
            const bool should_eliminate =
                v->is_type<ir::Copy>() && v->first_operand()->parent() == v->parent();
            if (should_eliminate) {
              v->replace_with(v->first_operand());
              v->remove();
              return true;
            }
            return false;
          });
      block->operations.erase(new_end, block->operations.end());
    }
  }

  // Given the current pending queue of conditionals, identify the most common condition.
  // Remove all conditionals that share that condition from `queue_conditionals`, and return them.
  static std::tuple<ir::ValuePtr, std::vector<ir::ValuePtr>> select_conditionals_to_group(
      std::vector<ir::ValuePtr>& queue_conditionals) {
    // Sort in descending order of variable name:
    std::sort(queue_conditionals.begin(), queue_conditionals.end(),
              [](ir::ValuePtr a, ir::ValuePtr b) { return a->name() > b->name(); });

    // These may not be unique, because deferred conditionals may end up on the top of the queue
    // more than once. We don't mark them visited until we make the conditional block.
    queue_conditionals.erase(std::unique(queue_conditionals.begin(), queue_conditionals.end()),
                             queue_conditionals.end());

    // Count occurrences of the first operand, which is the condition evaluated before
    // the if-else branch.
    std::unordered_map<ir::ValuePtr, int> counts{};
    counts.reserve(queue_conditionals.size());
    for (ir::ValuePtr cond : queue_conditionals) {
      counts[cond->first_operand()] += 1;
    }

    // Find the most frequently occurring one:
    const auto max_it =
        std::max_element(counts.begin(), counts.end(), [](const auto& a, const auto& b) {
          return std::make_pair(a.second, a.first->name()) <
                 std::make_pair(b.second, b.first->name());
        });
    const ir::ValuePtr condition = max_it->first;

    // Group together any conditionals that use this same condition:
    const auto matches_condition = [&](ir::ValuePtr v) { return v->first_operand() == condition; };

    // We'll copy and return them.
    std::vector<ir::ValuePtr> grouped_conditionals{};
    grouped_conditionals.reserve(queue_conditionals.size());
    std::copy_if(queue_conditionals.begin(), queue_conditionals.end(),
                 std::back_inserter(grouped_conditionals), matches_condition);

    // Erase the conditionals from the queue:
    queue_conditionals.erase(
        std::remove_if(queue_conditionals.begin(), queue_conditionals.end(), matches_condition),
        queue_conditionals.end());
    return std::make_tuple(condition, std::move(grouped_conditionals));
  }

  explicit IrConverter(OutputIr& output) : output(output) {
    visited.reserve(output.values_.size());
  }

  OutputIr& output;
  std::unordered_set<ir::ValuePtr> visited{};
};

OutputIr::OutputIr(math::FlatIr&& input) {
  // Take ownership of the values in `input`.
  values_.assign(std::make_move_iterator(input.values_.begin()),
                 std::make_move_iterator(input.values_.end()));
  input.values_.clear();
  input.block_->operations.clear();

  IrConverter(*this).convert();

  // Clean up anything that is not referenced in the output:
  values_.erase(std::remove_if(values_.begin(), values_.end(),
                               [&input](const ir::Value::unique_ptr& v) {
                                 if (v->parent() == input.get_block()) {
                                   WF_ASSERT_EQUAL(0, v->num_consumers(), "v = {}", v->name());
                                   return true;
                                 }
                                 return false;
                               }),
                values_.end());
}

std::string OutputIr::to_string() const {
  const std::size_t width = value_print_width();
  std::string output{};

  // size of the left column, so we can align things
  const std::size_t left_column_width = fmt::formatted_size("  v{:0>{}} <- ", 0, width);

  for (const std::unique_ptr<ir::Block>& block : blocks_) {
    fmt::format_to(std::back_inserter(output), "{}:", ir::BlockPtr{block});
    output += "\n";

    for (const ir::ValuePtr& code : block->operations) {
      // Print the value name:
      if (code->num_consumers() > 0) {
        fmt::format_to(std::back_inserter(output), "  v{:0>{}} <- ", code->name(), width);
      } else {
        output.append(left_column_width, ' ');
      }

      // Print the instruction name:
      constexpr int operation_width = 4;
      fmt::format_to(std::back_inserter(output), "{:>{}} ",
                     std::visit([](const auto& op) { return op.to_string(); }, code->value_op()),
                     operation_width);
      format_op_args(output, code->value_op(), code->operands(), width);
      output += "\n";
    }

    if (!block->descendants.empty()) {
      output.append(left_column_width, ' ');
      fmt::format_to(std::back_inserter(output), "jump {}\n", fmt::join(block->descendants, ", "));
    }
  }
  if (!output.empty()) {
    output.pop_back();
  }
  return output;
}

std::size_t OutputIr::value_print_width() const {
  const uint32_t highest_value_name = values_.empty() ? 0 : values_.back()->name();
  return compute_print_width(highest_value_name);
}

std::size_t OutputIr::num_operations() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](std::size_t total, const ir::Block::unique_ptr& b) {
                           return total + std::count_if(b->operations.begin(), b->operations.end(),
                                                        &is_countable_operation);
                         });
}

std::size_t OutputIr::num_conditionals() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](std::size_t total, const ir::Block::unique_ptr& b) {
                           return total + std::count_if(b->operations.begin(), b->operations.end(),
                                                        &is_conditional);
                         });
}

ir::BlockPtr OutputIr::create_block() {
  ir::Block::unique_ptr block = std::make_unique<ir::Block>(blocks_.size());
  blocks_.push_back(std::move(block));
  return ir::BlockPtr(blocks_.back());
}

// We traverse either upwards or downwards, recursively coloring nodes until we find a node
// that has already been colored - that is the intersection point. There might be more efficient
// ways to implement this, but we are doing relatively small searches.
ir::BlockPtr find_merge_point(const ir::BlockPtr left, const ir::BlockPtr right,
                              const SearchDirection direction) {
  // queue with [node, color]
  std::deque<std::pair<ir::BlockPtr, bool>> queue;
  queue.emplace_back(left, true);
  queue.emplace_back(right, false);

  std::unordered_map<ir::BlockPtr, bool> visited;
  visited.reserve(20);

  while (!queue.empty()) {
    const auto top = queue.front();
    const auto b = std::get<0>(top);
    const auto color = std::get<1>(top);
    queue.pop_front();

    const auto [it, was_inserted] = visited.emplace(b, color);
    if (!was_inserted && it->second != color) {
      // Already visited by a different color, we found the intersection point:
      return b;
    }
    if (direction == SearchDirection::Downwards) {
      for (ir::BlockPtr child : b->descendants) {
        queue.emplace_back(child, color);
      }
    } else {
      for (ir::BlockPtr ancestor : b->ancestors) {
        queue.emplace_back(ancestor, color);
      }
    }
  }

  throw AssertionError("All branches should have a merge point");
}

}  // namespace math

// Formatter for pointer to Block
template <>
struct fmt::formatter<math::ir::BlockPtr, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::ir::BlockPtr x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "block_{}", x->name);
  }
};
