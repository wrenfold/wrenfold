// Copyright 2023 Gareth Cross
#include "code_generation/ir_builder.h"

#include <algorithm>
#include <deque>
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "expressions/all_expressions.h"
#include "hashing.h"
#include "visitor_impl.h"

#include "code_generation/code_formatter.h"

namespace math {

struct DetermineNumericTypeVisitor {
  using ReturnType = NumericType;

  NumericType operator()(const Addition& add) const {
    // Adding bool to bool yields integer, add any floats - and it becomes real, etc...
    NumericType type = NumericType::Integer;
    for (const auto& expr : add) {
      type = std::max(Visit(expr, DetermineNumericTypeVisitor{}), type);
    }
    return type;
  }

  NumericType operator()(const Conditional& cond) const {
    const NumericType left = Visit(cond.if_branch(), DetermineNumericTypeVisitor{});
    const NumericType right = Visit(cond.else_branch(), DetermineNumericTypeVisitor{});
    return std::max(left, right);
  }

  NumericType operator()(const Constant& c) const {
    switch (c.name()) {
      case SymbolicConstants::Euler:
      case SymbolicConstants::Pi:
        return NumericType::Real;
      case SymbolicConstants::True:
      case SymbolicConstants::False:
        return NumericType::Bool;
    }
    throw TypeError("Unhandled symbolic constant");
  }

  constexpr NumericType operator()(const Derivative&) const {
    // TODO: This should be "unknown", since we don't know the type of the input function.
    return NumericType::Real;
  }

  constexpr NumericType operator()(const Float&) const { return NumericType::Real; }

  constexpr NumericType operator()(const FunctionArgument&) const { return NumericType::Real; }

  constexpr NumericType operator()(const Infinity&) const { return NumericType::Complex; }

  constexpr NumericType operator()(const Integer&) const { return NumericType::Integer; }

  NumericType operator()(const Multiplication& mul) const {
    // Multiplying booleans produces an integer, same as C++.
    NumericType type = NumericType::Integer;
    for (const auto& expr : mul) {
      type = std::max(Visit(expr, DetermineNumericTypeVisitor{}), type);
    }
    return type;
  }

  NumericType operator()(const Power& pow) const {
    const NumericType b = Visit(pow.base(), DetermineNumericTypeVisitor{});
    const NumericType e = Visit(pow.exponent(), DetermineNumericTypeVisitor{});
    return std::max(b, e);
  }

  constexpr NumericType operator()(const Rational&) const { return NumericType::Real; }

  constexpr NumericType operator()(const Relational&) const { return NumericType::Bool; }

  constexpr NumericType operator()(const Function&) const { return NumericType::Real; }

  constexpr NumericType operator()(const Variable&) const { return NumericType::Real; }
};

NumericType DetermineNumericType(const Expr& x) { return Visit(x, DetermineNumericTypeVisitor{}); }

namespace ir {

NumericType Load::determine_type() const { return DetermineNumericType(expr); }

void Block::replace_descendant(ir::BlockPtr target, ir::BlockPtr replacement) {
  ASSERT_NOT_EQUAL(target, replacement);

  if (!operations.empty()) {
    const ir::ValuePtr jump_val = operations.back();
    if (jump_val->is_type<ir::JumpCondition>()) {
      ASSERT_EQUAL(2, descendants.size());
    } else {
      ASSERT_GREATER_OR_EQ(1, descendants.size());
    }
  }

  const ir::BlockPtr self{this};
  target->remove_ancestor(self);
  replacement->add_ancestor(self);

  auto it = std::find(descendants.begin(), descendants.end(), target);
  ASSERT(it != descendants.end());
  *it = replacement;
}

void Block::add_ancestor(BlockPtr b) {
  ASSERT(std::find(ancestors.begin(), ancestors.end(), b) == ancestors.end(),
         "Attempted to insert duplicate into ancestor list: {}", b->name);
  ancestors.push_back(b);
}

void Block::remove_ancestor(BlockPtr b) {
  auto it = std::find(ancestors.begin(), ancestors.end(), b);
  ASSERT(it != ancestors.end(), "Block {} is not an ancestor of {}", b->name, name);
  ancestors.erase(it);
}

void Block::add_descendant(BlockPtr b) {
  ASSERT(std::find(descendants.begin(), descendants.end(), b) == descendants.end(),
         "Block {} already exists in descendants list: {},", b, fmt::join(descendants, ", "));
  descendants.push_back(b);
  b->add_ancestor(ir::BlockPtr{this});
}

// Replace this value w/ the argument.
void Value::replace_with(const ValuePtr other) {
  const ValuePtr self{this};
  ASSERT_NOT_EQUAL(self, other);
  for (const ValuePtr& consumer : consumers_) {
    consumer->replace_operand(self, other);
  }
  consumers_.clear();
}

void Value::remove() {
  ASSERT(consumers_.empty(), "Attempting to remove a value `{}` that is consumed by: [{}]", name_,
         fmt::join(consumers_, ","));
  // Notify our operands we no longer consume them.
  for (const ValuePtr& operand : operands_) {
    operand->remove_consumer(this);
  }
  // This value is dead, so clear the operand vector
  operands_.clear();
}

struct PairCountVisitor {
  using ReturnType = void;

  struct PairHash {
    std::size_t operator()(const std::pair<Expr, Expr>& pair) const {
      // Ignore the order of pairs:
      const std::size_t seed_a = pair.first.get_hash();
      const std::size_t seed_b = pair.second.get_hash();
      if (seed_a < seed_b) {
        return hash_combine(seed_a, seed_b);
      } else {
        return hash_combine(seed_b, seed_a);
      }
    }
  };

  struct PairEquality {
    bool operator()(const std::pair<Expr, Expr>& a, const std::pair<Expr, Expr>& b) const {
      // Order independent:
      return (a.first.is_identical_to(b.first) && a.second.is_identical_to(b.second)) ||
             (a.first.is_identical_to(b.second) && a.second.is_identical_to(b.first));
    }
  };

  // Record counts of single `Expr` children, and every pair-wise combination of children.
  template <typename Operation>
  void RecordCounts(const Operation& operation,
                    std::unordered_map<Expr, std::size_t, hash_struct<Expr>,
                                       IsIdenticalOperator<Expr>>& count_table,
                    std::unordered_map<std::pair<Expr, Expr>, std::size_t, PairHash, PairEquality>&
                        pair_count_table) {
    for (const Expr& operand : operation) {
      count_table[operand]++;
      Visit(operand, *this);
    }
    // generate pairs of expressions:
    for (auto i = operation.begin(); i != operation.end(); ++i) {
      for (auto j = std::next(i); j != operation.end(); ++j) {
        auto [it, was_inserted] = pair_count_table.emplace(std::make_pair(*i, *j), 1);
        if (!was_inserted) {
          it->second += 1;
        }
      }
    }
  }

  void operator()(const Multiplication& mul) {
    RecordCounts(mul, mul_element_counts_, mul_pair_counts_);
  }
  void operator()(const Addition& add) { RecordCounts(add, add_element_counts_, add_pair_counts_); }

  // For every other type, just recurse into the children:
  template <typename T>
  std::enable_if_t<!std::is_same_v<T, Multiplication> && !std::is_same_v<T, Addition>> operator()(
      const T& expr) {
    if constexpr (!T::IsLeafNode) {
      expr.iterate([this](const Expr& expr) { Visit(expr, *this); });
    }
  }

  std::unordered_map<Expr, std::size_t, hash_struct<Expr>, IsIdenticalOperator<Expr>>
      mul_element_counts_;
  std::unordered_map<Expr, std::size_t, hash_struct<Expr>, IsIdenticalOperator<Expr>>
      add_element_counts_;

  std::unordered_map<std::pair<Expr, Expr>, std::size_t, PairHash, PairEquality> mul_pair_counts_;
  std::unordered_map<std::pair<Expr, Expr>, std::size_t, PairHash, PairEquality> add_pair_counts_;
};

template <typename T>
struct FormatOpArgsHelper {
  void operator()(std::string& output, const T&, const std::vector<ir::ValuePtr>& operands,
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
struct FormatOpArgsHelper<ir::Load> {
  void operator()(std::string& output, const ir::Load& load, const std::vector<ir::ValuePtr>&,
                  const std::size_t) {
    output += load.expr.to_string();
  }
};

template <>
struct FormatOpArgsHelper<ir::OutputRequired> {
  void operator()(std::string& output, const ir::OutputRequired& oreq,
                  const std::vector<ir::ValuePtr>&, const std::size_t) {
    fmt::format_to(std::back_inserter(output), "{}", oreq.name);
  }
};

void FormatOpArgs(std::string& output, const ir::Operation& op,
                  const std::vector<ir::ValuePtr>& operands, const std::size_t width) {
  std::visit(
      [&](const auto& op) {
        using T = std::decay_t<decltype(op)>;
        FormatOpArgsHelper<T>{}(output, op, operands, width);
      },
      op);
}

}  // namespace ir

template <typename OpType, typename... Args>
ir::ValuePtr CreateOperation(std::vector<ir::Value::unique_ptr>& values, ir::BlockPtr block,
                             OpType&& op, Args&&... args) {
  // Create a new value:
  const uint32_t name = values.empty() ? 0 : (values.back()->name() + 1);
  std::unique_ptr<ir::Value> value = std::make_unique<ir::Value>(
      name, block, std::forward<OpType>(op), std::forward<Args>(args)...);
  // Insert int the provided block:
  block->operations.emplace_back(value.get());
  // This is owned by the `values` vector:
  values.push_back(std::move(value));
  return block->operations.back();
}

// Visitor for converting an expression tree into static-single-assignment form.
struct IRFormVisitor {
  using ReturnType = ir::ValuePtr;

  explicit IRFormVisitor(FlatIr& builder, const ir::PairCountVisitor& pair_count)
      : builder_(builder), pair_counts(pair_count) {}

  ir::ValuePtr MaybeCast(ir::ValuePtr input, NumericType output_type) {
    if (input->determine_type() != output_type) {
      return PushOperation(ir::Cast{output_type}, input);
    } else {
      return input;
    }
  }

  // Handler for additions and multiplications:
  template <typename T>
  std::enable_if_t<std::is_same_v<T, Multiplication> || std::is_same_v<T, Addition>, ir::ValuePtr>
  operator()(const T& op, const Expr&) {
    // Put the thing w/ the highest count in the first cell:
    std::vector<Expr> expressions{op.begin(), op.end()};

    // Place things into a canonical order so that we get consistent code-generation even if the
    // hash function changes.
    std::sort(expressions.begin(), expressions.end(), ExpressionOrderPredicate{});

    std::nth_element(
        expressions.begin(), expressions.begin(), expressions.end(),
        [&](const Expr& a, const Expr& b) {
          if constexpr (std::is_same_v<T, Multiplication>) {
            return pair_counts.mul_element_counts_.at(a) > pair_counts.mul_element_counts_.at(b);
          } else {
            return pair_counts.add_element_counts_.at(a) > pair_counts.add_element_counts_.at(b);
          }
        });

    // then pick the rest to obtain max pair count
    // TODO: There must be a more efficient way to do this...
    for (auto it = std::next(expressions.begin()); it != expressions.end(); ++it) {
      const auto prev = std::prev(it);
      std::nth_element(it, it, expressions.end(), [&](const Expr& a, const Expr& b) {
        if constexpr (std::is_same_v<T, Multiplication>) {
          return pair_counts.mul_pair_counts_.at(std::make_pair(*prev, a)) >
                 pair_counts.mul_pair_counts_.at(std::make_pair(*prev, b));
        } else {
          return pair_counts.add_pair_counts_.at(std::make_pair(*prev, a)) >
                 pair_counts.add_pair_counts_.at(std::make_pair(*prev, b));
        }
      });
    }

    // first recursively transform all the inputs
    std::vector<ir::ValuePtr> args;
    args.reserve(op.arity());
    std::transform(expressions.begin(), expressions.end(), std::back_inserter(args),
                   [this](const Expr& expr) { return VisitExpr(expr); });
    ASSERT(!args.empty());

    NumericType promoted_type = NumericType::Integer;
    for (ir::ValuePtr v : args) {
      promoted_type = std::max(promoted_type, v->determine_type());
    }

    // then create multiplications or adds for this expression:
    ir::ValuePtr prev_result = MaybeCast(args[0], promoted_type);
    for (std::size_t i = 1; i < args.size(); ++i) {
      if constexpr (std::is_same_v<T, Multiplication>) {
        prev_result = PushOperation(ir::Mul{}, prev_result, MaybeCast(args[i], promoted_type));
      } else {
        prev_result = PushOperation(ir::Add{}, prev_result, MaybeCast(args[i], promoted_type));
      }
    }
    return prev_result;
  }

  ir::ValuePtr operator()(const Conditional& cond, const Expr&) {
    const ir::ValuePtr condition = VisitExpr(cond.condition());
    const ir::ValuePtr if_branch = VisitExpr(cond.if_branch());
    const ir::ValuePtr else_branch = VisitExpr(cond.else_branch());

    const NumericType promoted_type =
        std::max(if_branch->determine_type(), else_branch->determine_type());

    return PushOperation(ir::Cond{}, condition, MaybeCast(if_branch, promoted_type),
                         MaybeCast(else_branch, promoted_type));
  }

  ir::ValuePtr operator()(const Constant&, const Expr& input_expression) {
    return PushOperation(ir::Load{input_expression});
  }

  ir::ValuePtr operator()(const Derivative& derivative) {
    throw TypeError(
        "Cannot generate code for expressions containing `Derivative`. Offending expression is: "
        "Derivative({}, {}, {})",
        derivative.differentiand().to_string(), derivative.argument().to_string(),
        derivative.order());
  }

  ir::ValuePtr operator()(const Function& func, const Expr&) {
    std::vector<ir::ValuePtr> args;
    args.reserve(func.arity());
    std::transform(func.begin(), func.end(), std::back_inserter(args),
                   [this](const Expr& expr) { return VisitExpr(expr); });
    return PushOperation(ir::CallBuiltInFunction{func.enum_value()}, std::move(args));
  }

  ir::ValuePtr operator()(const Infinity&, const Expr&) const {
    throw TypeError("Cannot generate code for complex infinity.");
  }
  ir::ValuePtr operator()(const Integer&, const Expr& input_expression) {
    return PushOperation(ir::Load{input_expression});
  }
  ir::ValuePtr operator()(const Float&, const Expr& input_expression) {
    return PushOperation(ir::Load{input_expression});
  }
  ir::ValuePtr operator()(const FunctionArgument&, const Expr& input_expression) {
    return PushOperation(ir::Load{input_expression});
  }

  ir::ValuePtr operator()(const Power& pow, const Expr&) {
    const ir::ValuePtr b = VisitExpr(pow.base());
    const ir::ValuePtr e = VisitExpr(pow.exponent());
    const NumericType promoted_type =
        std::max(NumericType::Integer, std::max(b->determine_type(), e->determine_type()));
    return PushOperation(ir::Pow{}, MaybeCast(b, promoted_type), MaybeCast(e, promoted_type));
  }

  ir::ValuePtr operator()(const Rational&, const Expr& expr) {
    // We just send Rational directly to the code generator.
    return PushOperation(ir::Load{expr});
  }

  ir::ValuePtr operator()(const Relational& relational, const Expr&) {
    ir::ValuePtr left = VisitExpr(relational.left());
    ir::ValuePtr right = VisitExpr(relational.right());
    NumericType promoted_type = std::max(left->determine_type(), right->determine_type());
    return PushOperation(ir::Compare{relational.operation()}, MaybeCast(left, promoted_type),
                         MaybeCast(right, promoted_type));
  }

  ir::ValuePtr operator()(const Variable&, const Expr& input_expression) {
    return PushOperation(ir::Load{input_expression});
  }

  template <typename OpType, typename... Args>
  ir::ValuePtr PushOperation(OpType&& op, Args&&... args) {
    return CreateOperation(builder_.values_, builder_.get_block(), std::move(op),
                           std::forward<Args>(args)...);
  }

  // Check if a value has been computed. If not, convert it and return the result.
  ir::ValuePtr VisitExpr(const Expr& expr) {
    auto it = computed_values_.find(expr);
    if (it != computed_values_.end()) {
      return it->second;
    }
    ir::ValuePtr val = VisitWithExprArg(expr, *this);
    computed_values_.emplace(expr, val);
    return val;
  }

 private:
  FlatIr& builder_;

  std::unordered_map<Expr, ir::ValuePtr, hash_struct<Expr>, IsIdenticalOperator<Expr>>
      computed_values_;

  const ir::PairCountVisitor& pair_counts;
};

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

FlatIr::FlatIr(const std::vector<ExpressionGroup>& groups)
    : block_(std::make_unique<ir::Block>(0)) {
  // First pass where we count occurrence of some sub-expressions:
  ir::PairCountVisitor pair_visitor{};
  for (const ExpressionGroup& group : groups) {
    for (const Expr& expr : group.expressions) {
      Visit(expr, pair_visitor);
    }
  }

  IRFormVisitor visitor{*this, pair_visitor};

  for (const ExpressionGroup& group : groups) {
    // Transform expressions into Values
    std::vector<ir::ValuePtr> group_values;
    group_values.reserve(group.expressions.size());
    std::transform(group.expressions.begin(), group.expressions.end(),
                   std::back_inserter(group_values), [&](const Expr& expr) {
                     ir::ValuePtr output = visitor.VisitExpr(expr);
                     if (output->determine_type() != NumericType::Real) {
                       // TODO: Allow returning other types - derive the numeric type from the
                       // group.
                       output = CreateOperation(values_, get_block(), ir::Cast{NumericType::Real},
                                                output);
                     }
                     return output;
                   });

    // Then create a sink to consume these values, the `Save` operation is the sink:
    CreateOperation(values_, get_block(), ir::Save{group.key}, std::move(group_values));
  }
}

inline constexpr std::size_t GetPrintWidth(std::size_t num_assignments) {
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
    constexpr int OperationWidth = 4;
    fmt::format_to(std::back_inserter(output), "{:>{}} ",
                   std::visit([](const auto& op) { return op.to_string(); }, code->value_op()),
                   OperationWidth);
    FormatOpArgs(output, code->value_op(), code->operands(), width);
    output += "\n";
  }

  if (!output.empty()) {
    output.pop_back();
  }
  return output;
}

std::size_t FlatIr::value_print_width() const {
  const uint32_t highest_value_name = values_.empty() ? 0 : values_.back()->name();
  return GetPrintWidth(highest_value_name);
}

// A hash-set of values:
using ValueTable = std::unordered_set<ir::ValuePtr, ir::ValueHasher, ir::ValueEquality>;

// Eliminate duplicates in `block`, using existing values stored in `table`.
inline void LocalValueNumbering(ir::BlockPtr block, ValueTable& table) {
  for (const ir::ValuePtr& code : block->operations) {
    // Then see if this operation already exists in the map:
    auto [it, was_inserted] = table.insert(code);
    if (!was_inserted) {
      // Propagate the copy:
      if (code->is_consumed_by_phi()) {
        // If this value feeds a phi function, we need to keep it, but turn it into a copy:
        code->set_value_op(ir::Copy{}, *it);
      } else {
        code->replace_with(*it);
      }
    }
  }
}

void FlatIr::eliminate_duplicates() {
  ValueTable table{};
  table.reserve(values_.size());
  LocalValueNumbering(get_block(), table);
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

inline bool IsCountableOperation(ir::ValuePtr v) {
  return !v->is_type<ir::JumpCondition>() && !v->is_type<ir::Save>() && !v->is_type<ir::Load>() &&
         !v->is_type<ir::Copy>();
}

inline bool IsConditional(ir::ValuePtr v) {
  return v->is_type<ir::JumpCondition>() || v->is_type<ir::Cond>();
}

std::size_t FlatIr::num_operations() const {
  return std::count_if(block_->operations.begin(), block_->operations.end(), &IsCountableOperation);
}

std::size_t FlatIr::num_condtionals() const {
  return std::count_if(block_->operations.begin(), block_->operations.end(), &IsConditional);
}

struct IrConverter {
  std::vector<ir::ValuePtr> GetAllOutputValues() const {
    std::vector<ir::ValuePtr> output_values{};
    output_values.reserve(output.values_.size());
    for (const auto& v : output.values_) {
      const ir::ValuePtr val{v};
      if (val->is_type<ir::Save>()) {
        output_values.emplace_back(val);
      }
    }
    return output_values;
  }

  void Convert() {
    // Conversion will modify `output.values_`, so shallow-copy the outputs first:
    const std::vector<ir::ValuePtr> output_values = GetAllOutputValues();

    // Pull out all the required outputs, these are processed first:
    // Order is: queue the return values, then the required output arguments.
    std::deque<ir::ValuePtr> required_outputs_queue{};
    std::copy_if(output_values.rbegin(), output_values.rend(),
                 std::back_inserter(required_outputs_queue),
                 [&](ir::ValuePtr v) { return v->as_type<ir::Save>().IsReturnValue(); });
    std::copy_if(output_values.rbegin(), output_values.rend(),
                 std::back_inserter(required_outputs_queue), [&](ir::ValuePtr v) {
                   const ir::Save& save = v->as_type<ir::Save>();
                   return save.key.usage == ExpressionUsage::OutputArgument;
                 });

    // Insert computations for required output values:
    std::vector<ir::ValuePtr> deferred_values{};
    ir::BlockPtr next_block =
        Process(std::move(required_outputs_queue), output.create_block(), deferred_values);

    // Should be nothing deferred yet:
    ASSERT(deferred_values.empty(), "deferred_values = [{}]", fmt::join(deferred_values, ", "));

    // Traverse optional outputs:
    for (ir::ValuePtr v : output_values) {
      const ir::Save& save = v->as_type<ir::Save>();
      if (save.key.usage != ExpressionUsage::OptionalOutputArgument) {
        continue;
      }

      const ir::BlockPtr left_block_exit = output.create_block();
      left_block_exit->add_descendant(next_block);

      // Insert block to evaluate if this output is required:
      const ir::BlockPtr jump_block = output.create_block();

      // Operation that evaluates whether this argument is required:
      const ir::ValuePtr jump_condition =
          CreateOperation(output.values_, jump_block, ir::OutputRequired{save.key.name});

      // Either we go into `left_block` and compute the arg outputs, or we skip to `next_block`:
      CreateOperation(output.values_, jump_block, ir::JumpCondition{}, jump_condition);
      jump_block->add_descendant(left_block_exit);
      jump_block->add_descendant(next_block);

      std::deque<ir::ValuePtr> queued_left = {v};
      Process(std::move(queued_left), left_block_exit, deferred_values);

      std::deque<ir::ValuePtr> queue{deferred_values.begin(), deferred_values.end()};
      deferred_values.clear();
      next_block = Process(std::move(queue), jump_block, deferred_values);
      ASSERT(deferred_values.empty(), "deferred_values = [{}]", fmt::join(deferred_values, ", "));
    }

    std::deque<ir::ValuePtr> queue;
    std::copy(deferred_values.begin(), deferred_values.end(), std::back_inserter(queue));
    deferred_values.clear();

    Process(std::move(queue), next_block, deferred_values);
    ASSERT(deferred_values.empty(), "deferred_values = [{}]", fmt::join(deferred_values, ", "));

    // There should only be one start block:
    ASSERT_EQUAL(
        1,
        std::count_if(output.blocks_.begin(), output.blocks_.end(),
                      [](const ir::Block::unique_ptr& block) { return block->has_no_ancestors(); }),
        "Must be only one entry block");

    // The process above sometimes introduces pointless copies, which we remove now:
    EliminateUselessCopies();
  }

  // True if node `v` has been visited.
  bool IsVisited(ir::ValuePtr v) const { return visited.count(v) > 0; }

  // True if all downstream consumers of `v` have been visited.
  bool AllConsumersVisited(ir::ValuePtr val) const {
    return val->all_consumers_satisfy([this](ir::ValuePtr v) { return IsVisited(v); });
  }

  // Queue any operands of `v` whose consumers have all been visited.
  void QueueOperands(std::deque<ir::ValuePtr>& queue, ir::ValuePtr v) const {
    for (ir::ValuePtr val : v->operands()) {
      if (!IsVisited(val) && AllConsumersVisited(val)) {
        queue.push_back(val);
      }
    }
  }

  // Return true if `parent_block` is executed on all paths through `test_block`.
  static bool ParentIsOnAllPathsThroughBlock(ir::BlockPtr test_block, ir::BlockPtr parent_block) {
    if (test_block == parent_block) {
      return true;
    }
    return !test_block->ancestors.empty() &&
           std::all_of(
               test_block->ancestors.begin(), test_block->ancestors.end(),
               [&](ir::BlockPtr b) { return ParentIsOnAllPathsThroughBlock(b, parent_block); });
  }

  ir::BlockPtr Process(std::deque<ir::ValuePtr> queue, const ir::BlockPtr output_block,
                       std::vector<ir::ValuePtr>& deferred) {
    std::vector<ir::ValuePtr> queue_conditionals{};
    queue_conditionals.reserve(10);

    // Process as many non-conditionals as we can.
    std::vector<ir::ValuePtr> output_reversed{};
    output_reversed.reserve(queue.size());

    while (!queue.empty()) {
      ir::ValuePtr top = queue.front();
      queue.pop_front();
      if (IsVisited(top)) {
        continue;
      }
      ASSERT(AllConsumersVisited(top), "Not all consumers have been visited: {}", top);

      // Check if this block is a valid place to insert this value. This will be the case
      // if `output_block` is on all paths through the downstream consumer blocks.
      const bool is_valid_to_insert = top->all_consumers_satisfy([&](ir::ValuePtr consumer) {
        return ParentIsOnAllPathsThroughBlock(consumer->parent(), output_block);
      });
      if (!is_valid_to_insert) {
        deferred.push_back(top);
        continue;
      }

      if (top->is_type<ir::Cond>()) {
        // Defer conditionals to be processed together later:
        queue_conditionals.push_back(top);
        continue;
      }

      // Put it into the output, then queue its operands.
      top->set_parent(output_block);
      output_reversed.push_back(top);
      visited.insert(top);
      QueueOperands(queue, top);
    }

    if (queue_conditionals.empty()) {
      output_block->operations.insert(output_block->operations.begin(), output_reversed.rbegin(),
                                      output_reversed.rend());
      return output_block;
    }

    // Count occurrences of the first operand
    std::unordered_map<ir::ValuePtr, int> counts{};
    for (ir::ValuePtr cond : queue_conditionals) {
      counts[cond->first_operand()] += 1;
    }

    // Find the most frequently occurring one:
    const auto max_it =
        std::max_element(counts.begin(), counts.end(),
                         [](const auto& a, const auto& b) { return a.second < b.second; });
    const ir::ValuePtr condition = max_it->first;

    // Group together any conditionals that use this same condition:
    std::vector<ir::ValuePtr> grouped_conditionals{};
    const auto new_end =
        std::remove_if(queue_conditionals.begin(), queue_conditionals.end(), [&](ir::ValuePtr v) {
          if (v->first_operand() == condition) {
            grouped_conditionals.push_back(v);
          }
          return true;
        });
    queue_conditionals.erase(new_end, queue_conditionals.end());

    // Sort in descending order of variable name:
    std::sort(grouped_conditionals.begin(), grouped_conditionals.end(),
              [](ir::ValuePtr a, ir::ValuePtr b) { return a->name() > b->name(); });

    ir::BlockPtr left_block_tail = output.create_block();
    ir::BlockPtr right_block_tail = output.create_block();

    std::deque<ir::ValuePtr> queue_left{};
    std::deque<ir::ValuePtr> queue_right{};
    for (ir::ValuePtr v : grouped_conditionals) {
      // Turn conditionals into phi functions.
      // We insert copies in the left and right blocks so that arguments to the phi functions are
      // "computed" on the branched code path, even if the computation is just a copy. These copies
      // can be eliminated later, in some cases.
      const ir::ValuePtr copy_left =
          CreateOperation(output.values_, left_block_tail, ir::Copy{}, v->operator[](1));
      const ir::ValuePtr copy_right =
          CreateOperation(output.values_, right_block_tail, ir::Copy{}, v->operator[](2));

      v->set_value_op(ir::Phi{}, copy_left, copy_right);
      v->set_parent(output_block);

      output_reversed.push_back(v);
      visited.insert(v);
      visited.insert(copy_left);
      visited.insert(copy_right);

      QueueOperands(queue_left, copy_left);
      QueueOperands(queue_right, copy_right);
    }

    // Create a block for our ordered operations:
    output_block->operations.insert(output_block->operations.begin(), output_reversed.rbegin(),
                                    output_reversed.rend());

    // Save the ancestor vector before modifying it by creating jumps:
    const std::vector<ir::BlockPtr> previous_ancestors = output_block->ancestors;

    left_block_tail->add_descendant(output_block);
    right_block_tail->add_descendant(output_block);

    const ir::BlockPtr jump_block = output.create_block();
    const ir::ValuePtr jump_condition =
        CreateOperation(output.values_, jump_block, ir::JumpCondition{}, condition);
    visited.insert(jump_condition);

    jump_block->add_descendant(left_block_tail);
    jump_block->add_descendant(right_block_tail);

    // Any blocks that jumped to `output_block` should now jump to `jump_block` instead:
    for (ir::BlockPtr ancestor : previous_ancestors) {
      // If this block is our ancestor, it must contain a jump at the end:
      ASSERT(!ancestor->operations.empty() && !ancestor->descendants.empty(),
             "Block cannot be empty, must contain a jump");
      ancestor->replace_descendant(output_block, jump_block);
    }

    // Process left and right sides:
    std::vector<ir::ValuePtr> process_later;
    Process(std::move(queue_left), left_block_tail, process_later);
    Process(std::move(queue_right), right_block_tail, process_later);

    // Queue the nodes we deferred
    queue.clear();
    if (AllConsumersVisited(condition)) {
      queue.push_back(condition);
    }
    queue.insert(queue.end(), process_later.begin(), process_later.end());
    queue.insert(queue.end(), queue_conditionals.begin(), queue_conditionals.end());

    return Process(std::move(queue), jump_block, deferred);
  }

  // In the process of converting, we inserted copies to satisfy the phi functions.
  // In some cases, these copies are just duplicating values computed in the same scope.
  void EliminateUselessCopies() {
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

  IrConverter(*this).Convert();

  // Clean up anything that is not referenced in the output:
  values_.erase(std::remove_if(values_.begin(), values_.end(),
                               [&input](const ir::Value::unique_ptr& v) {
                                 if (v->parent() == input.get_block()) {
                                   ASSERT_EQUAL(0, v->num_consumers(), "v = {}", v->name());
                                   return true;
                                 }
                                 return false;
                               }),
                values_.end());
}

struct BlockNameFormatter {
  uint32_t name;
};

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
      constexpr int OperationWidth = 4;
      fmt::format_to(std::back_inserter(output), "{:>{}} ",
                     std::visit([](const auto& op) { return op.to_string(); }, code->value_op()),
                     OperationWidth);
      FormatOpArgs(output, code->value_op(), code->operands(), width);
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
  return GetPrintWidth(highest_value_name);
}

std::size_t OutputIr::num_operations() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](std::size_t total, const ir::Block::unique_ptr& b) {
                           return total + std::count_if(b->operations.begin(), b->operations.end(),
                                                        &IsCountableOperation);
                         });
}

std::size_t OutputIr::num_conditionals() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](std::size_t total, const ir::Block::unique_ptr& b) {
                           return total + std::count_if(b->operations.begin(), b->operations.end(),
                                                        &IsConditional);
                         });
}

ir::BlockPtr OutputIr::create_block() {
  ir::Block::unique_ptr block = std::make_unique<ir::Block>(blocks_.size());
  blocks_.push_back(std::move(block));
  return ir::BlockPtr(blocks_.back());
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
