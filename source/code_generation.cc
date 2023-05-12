// Copyright 2023 Gareth Cross
#include "code_generation.h"

#include <algorithm>
#include <deque>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "expressions/all_expressions.h"
#include "hashing.h"
#include "visitor_impl.h"

#include "code_formatter.h"

template <>
struct fmt::formatter<math::ir::Value, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::ir::Value& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.Id());
  }
};

namespace math {

template <class... Ts>
struct Overloaded : Ts... {
  using Ts::operator()...;
};

template <class... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;

template <typename... Funcs, typename Variant>
auto OverloadedVisit(Variant&& var, Funcs&&... funcs) {
  return std::visit(Overloaded{std::forward<Funcs>(funcs)...}, std::forward<Variant>(var));
}

template <typename Handler>
void VisitSuccessors(const std::variant<std::monostate, ir::Jump, ir::ConditionalJump>& jump,
                     Handler&& handler) {
  OverloadedVisit(
      jump, [&](const ir::Jump& jump) { handler(jump.next); },
      [&](const ir::ConditionalJump& jump) {
        handler(jump.next_true);
        handler(jump.next_false);
      },
      [](const std::monostate&) {});
}

namespace ir {

struct PairCountVisitor {
  using Policy = VisitorPolicy::CompileError;
  using ReturnType = void;

  // Keys are either single expressions, or pairs of expressions.
  using MapKey = std::variant<Expr, std::pair<Expr, Expr>>;

  struct KeyHash {
    std::size_t operator()(const Expr& expr) const { return HashExpression(expr); }
    std::size_t operator()(const std::pair<Expr, Expr>& pair) const {
      // Ignore the order of pairs:
      const std::size_t seed_a = HashExpression(pair.first);
      const std::size_t seed_b = HashExpression(pair.second);
      if (seed_a < seed_b) {
        return HashCombine(seed_a, seed_b);
      } else {
        return HashCombine(seed_b, seed_a);
      }
    }
    std::size_t operator()(const MapKey& key) const { return std::visit(*this, key); }
  };

  struct KeyEquality {
    bool operator()(const Expr& a, const Expr& b) const { return a.IsIdenticalTo(b); }
    bool operator()(const std::pair<Expr, Expr>& a, const std::pair<Expr, Expr>& b) const {
      // Order independent:
      return (a.first.IsIdenticalTo(b.first) && a.second.IsIdenticalTo(b.second)) ||
             (a.first.IsIdenticalTo(b.second) && a.second.IsIdenticalTo(b.first));
    }
    bool operator()(const MapKey& a, const MapKey& b) const {
      return std::visit(
          [this](const auto& a, const auto& b) {
            if constexpr (std::is_same_v<decltype(a), decltype(b)>) {
              return this->operator()(a, b);
            } else {
              return false;
            }
          },
          a, b);
    }
  };

  using MapType = std::unordered_map<MapKey, std::size_t, KeyHash, KeyEquality>;

  // Record counts of single `Expr` children, and every pair-wise combination of children.
  template <typename Derived>
  void RecordCounts(const NAryOp<Derived>& operation, MapType& count_table) {
    for (const Expr& operand : operation) {
      count_table[operand]++;
      VisitStruct(operand, *this);
    }
    // generate pairs of expressions:
    for (auto i = operation.begin(); i != operation.end(); ++i) {
      for (auto j = std::next(i); j != operation.end(); ++j) {
        auto [it, was_inserted] = count_table.emplace(std::make_pair(*i, *j), 1);
        if (!was_inserted) {
          it->second += 1;
        }
      }
    }
  }

  void Apply(const Multiplication& mul) { RecordCounts(mul, mul_counts); }
  void Apply(const Addition& add) { RecordCounts(add, add_counts); }

  // For every other type, just recurse into the children:
  template <typename T>
  std::enable_if_t<!std::is_same_v<T, Multiplication> && !std::is_same_v<T, Addition>> Apply(
      const T& expr) {
    if constexpr (!T::IsLeafStatic()) {
      IterateChildren(expr, [this](const Expr& expr) { VisitStruct(expr, *this); });
    }
  }

  MapType mul_counts;
  MapType add_counts;
};

template <typename Derived, std::size_t N>
inline void FormatOpWithArgs(std::string& output, const OperationBase<Derived, N>& op,
                             const std::size_t width) {
  constexpr int OperationWidth = 4;
  fmt::format_to(std::back_inserter(output), "{:>{}} ", static_cast<const Derived&>(op).ToString(),
                 OperationWidth);
  for (auto it = op.args.begin(); it != op.args.end(); ++it) {
    fmt::format_to(std::back_inserter(output), "v{:0>{}}", it->Id(), width);
    if (std::next(it) != op.args.end()) {
      output += ", ";
    }
  }
}

inline void FormatOpWithArgs(std::string& output, const ir::Phi& op, const std::size_t width) {
  constexpr int OperationWidth = 4;
  fmt::format_to(std::back_inserter(output), "{:>{}} ", op.ToString(), OperationWidth);

  ASSERT(std::holds_alternative<ir::ConditionalJump>(op.jump_block->jump));
  const ir::ConditionalJump& jump = std::get<ir::ConditionalJump>(op.jump_block->jump);

  fmt::format_to(std::back_inserter(output), "v{:0>{}} [block_{}], ", op.args[0].Id(), width,
                 jump.next_true->name);
  fmt::format_to(std::back_inserter(output), "v{:0>{}} [block_{}]", op.args[1].Id(), width,
                 jump.next_false->name);
}

inline void FormatOpWithArgs(std::string& output, const ir::Load& load, const std::size_t) {
  constexpr int OperationWidth = 4;
  fmt::format_to(std::back_inserter(output), "{:>{}} ", "load", OperationWidth);
  output += load.expr.ToString();
}

// Visitor for converting an expression tree into static-single-assignment form.
struct IRFormVisitor {
  using Policy = VisitorPolicy::CompileError;
  using ReturnType = ir::Value;

  explicit IRFormVisitor(const PairCountVisitor& pair_count) : pair_counts(pair_count) {}

  // Get non-const reference to blocks.
  std::vector<std::unique_ptr<ir::Block>>& TakeBlocks() { return blocks_; }

  // Handler for additions and multiplications:
  template <typename T>
  std::enable_if_t<std::is_same_v<T, Multiplication> || std::is_same_v<T, Addition>, ir::Value>
  Apply(const T& op) {
    // Put the thing w/ the highest count in the first cell:
    std::vector<Expr> expressions{op.begin(), op.end()};
    std::nth_element(expressions.begin(), expressions.begin(), expressions.end(),
                     [&](const Expr& a, const Expr& b) {
                       if constexpr (std::is_same_v<T, Multiplication>) {
                         return pair_counts.mul_counts.at(a) > pair_counts.mul_counts.at(b);
                       } else {
                         return pair_counts.add_counts.at(a) > pair_counts.add_counts.at(b);
                       }
                     });

    // then pick the rest to obtain max pair count
    for (auto it = std::next(expressions.begin()); it != expressions.end(); ++it) {
      const auto prev = std::prev(it);
      std::nth_element(it, it, expressions.end(), [&](const Expr& a, const Expr& b) {
        if constexpr (std::is_same_v<T, Multiplication>) {
          return pair_counts.mul_counts.at(std::make_pair(*prev, a)) >
                 pair_counts.mul_counts.at(std::make_pair(*prev, b));
        } else {
          return pair_counts.add_counts.at(std::make_pair(*prev, a)) >
                 pair_counts.add_counts.at(std::make_pair(*prev, b));
        }
      });
    }

    // first recursively transform all the inputs
    std::vector<ir::Value> args;
    args.reserve(op.Arity());
    std::transform(expressions.begin(), expressions.end(), std::back_inserter(args),
                   [this](const Expr& expr) { return VisitStruct(expr, *this); });
    ASSERT(!args.empty());

    // then create multiplications or adds for this expression:
    ir::Value prev_result = args[0];
    for (std::size_t i = 1; i < args.size(); ++i) {
      if constexpr (std::is_same_v<T, Multiplication>) {
        prev_result = PushOperation<Mul>(prev_result, args[i]);
      } else {
        prev_result = PushOperation<Add>(prev_result, args[i]);
      }
    }
    return prev_result;
  }

  ir::Value Apply(const Expr& input, const Conditional& cond) {
    fmt::print("processing conditional: {}\n", input.ToString());
    // First compute expressions for the condition itself
    ir::Value condition = VisitStruct(cond.Condition(), *this);

    // The block that will jump into the conditional at the end:
    Block* const current_block = PopStack();

    // Now insert a block for the if-branch:
    Block* const b_true_start = PushStack();
    ir::Value if_branch = VisitStruct(cond.IfBranch(), *this);
    Block* const b_true_end = PopStack();

    // Then the else branch:
    Block* const b_false_start = PushStack();
    ir::Value else_branch = VisitStruct(cond.ElseBranch(), *this);
    Block* const b_false_end = PopStack();

    // Wire up the jump at the end of `current_block`
    current_block->SetConditionalJump(condition, b_true_start, b_false_start);

    Block* const next = PushStack();
    b_true_end->SetJump(next);
    b_false_end->SetJump(next);

    // now we need a phi function - this gets inserted into `next_block`
    return PushOperation<Phi>(if_branch, else_branch, current_block);
  }

  ir::Value Apply(const Expr& input_expression, const Constant&) {
    return PushOperation<Load>(input_expression);
  }

  ir::Value Apply(const Matrix&) const { throw TypeError("Cannot evaluate this on a matrix."); }

  ir::Value Apply(const UnaryFunction& func) {
    ir::Value arg = VisitStruct(func.Arg(), *this);
    return PushOperation<CallUnaryFunc>(func.Func(), arg);
  }

  ir::Value Apply(const Expr&, const Infinity&) const {
    throw TypeError("Cannot generate code for complex infinity.");
  }
  ir::Value Apply(const Expr& input_expression, const Integer&) {
    return PushOperation<Load>(input_expression);
  }
  ir::Value Apply(const Expr& input_expression, const Float&) {
    return PushOperation<Load>(input_expression);
  }
  ir::Value Apply(const Expr& input_expression, const FunctionArgument&) {
    return PushOperation<Load>(input_expression);
  }

  ir::Value Apply(const Power& pow) {
    ir::Value base = VisitStruct(pow.Base(), *this);
    ir::Value exponent = VisitStruct(pow.Exponent(), *this);
    return PushOperation<Pow>(base, exponent);
  }

  ir::Value Apply(const Expr& input_expression, const Rational&) {
    return PushOperation<Load>(input_expression);
  }

  ir::Value Apply(const Relational& relational) {
    ir::Value left = VisitStruct(relational.Left(), *this);
    ir::Value right = VisitStruct(relational.Right(), *this);
    return PushOperation<Compare>(relational.Operation(), left, right);
  }

  ir::Value Apply(const Expr& input_expression, const Variable&) {
    return PushOperation<Load>(input_expression);
  }

  // Push a new block onto the stack.
  Block* PushStack() {
    std::unique_ptr<ir::Block> block = std::make_unique<ir::Block>();
    block->name = blocks_.size();
    stack_.push_back(block.get());
    blocks_.push_back(std::move(block));
    return stack_.back();
  }

  Block* PopStack() {
    ASSERT(!stack_.empty());
    Block* const top = stack_.back();
    stack_.pop_back();
    return top;
  }

  template <typename T, typename... Args>
  ir::Value PushOperation(Args&&... args) {
    ASSERT(!stack_.empty(), "Need at least one block on the stack");
    Block* const current_block = stack_.back();
    // Assign this block the next value:
    const ir::Value value = next_value_;
    T operation{std::forward<Args>(args)...};
    current_block->operations.emplace_back(value, std::move(operation));
    next_value_.Increment();
    return value;
  }

  const ir::Value& NextValue() const { return next_value_; }

 private:
  std::vector<std::unique_ptr<ir::Block>> blocks_;
  std::vector<Block*> stack_;
  ir::Value next_value_{0};

  const PairCountVisitor& pair_counts;
};

template <typename T>
struct TypeListFromVariant;

template <typename... Ts>
struct TypeListFromVariant<std::variant<Ts...>> {
  using List = TypeList<Ts...>;
};

template <typename T>
std::size_t HashOpWithArgs(const T& op) {
  // See the hash with the position of this type in the variant:
  std::size_t seed = IndexOfType<T, TypeListFromVariant<Operation>::List>::Value;
  for (const ir::Value& operand : op.args) {
    seed = HashCombine(seed, operand.Hash());
  }
  return seed;
}

std::size_t HashOp(const Add& addition) { return HashOpWithArgs(addition); }

std::size_t HashOp(const Mul& mul) { return HashOpWithArgs(mul); }

std::size_t HashOp(const Pow& pow) { return HashOpWithArgs(pow); }

std::size_t HashOp(const Copy& copy) { return HashOpWithArgs(copy); }

std::size_t HashOp(const CallUnaryFunc& call) {
  return HashCombine(HashOpWithArgs(call), static_cast<std::size_t>(call.name));
}

std::size_t HashOp(const Cond& cond) { return HashOpWithArgs(cond); }

std::size_t HashOp(const Phi& phi) { return HashOpWithArgs(phi); }

std::size_t HashOp(const Compare& cmp) {
  return HashCombine(HashOpWithArgs(cmp), static_cast<std::size_t>(cmp.operation));
}

std::size_t HashOp(const Load& load) {
  std::size_t seed = IndexOfType<Load, TypeListFromVariant<Operation>::List>::Value;
  return HashCombine(seed, HashExpression(load.expr));
}

struct OperationHasher {
  std::size_t operator()(const Operation* const operation) const {
    return std::visit([](const auto& op) -> std::size_t { return HashOp(op); }, *operation);
  }
};

struct OperationEquality {
  bool operator()(const Operation* const a, const Operation* const b) const {
    return std::visit(
        [&](const auto& a, const auto& b) -> bool {
          using A = std::decay_t<decltype(a)>;
          using B = std::decay_t<decltype(b)>;
          if constexpr (std::is_same_v<A, B>) {
            return a == b;
          } else {
            return false;
          }
        },
        *a, *b);
  }
};

}  // namespace ir

IrBuilder::IrBuilder(const std::vector<Expr>& expressions) {
  //  operations_.reserve(100);
  output_values_.reserve(expressions.size());

  // Count occurrence of some sub-expressions:
  ir::PairCountVisitor pair_visitor{};
  for (const Expr& expr : expressions) {
    VisitStruct(expr, pair_visitor);
  }

  ir::IRFormVisitor visitor{pair_visitor};

  // Now convert every expression to IR and record the output value:
  ir::Block* previous_block{nullptr};
  for (const Expr& expr : expressions) {
    ir::Block* const start_block = visitor.PushStack();
    const ir::Value val = VisitStruct(expr, visitor);
    output_values_.push_back(val);

    // `end_block` is where execution ends for this expression.
    // It may or may not be the same as `start_block`.
    ir::Block* const end_block = visitor.PopStack();
    if (previous_block) {
      previous_block->SetJump(start_block);
    }
    previous_block = end_block;
  }

  blocks_ = std::move(visitor.TakeBlocks());
  insertion_point_ = visitor.NextValue();
}

inline constexpr std::size_t GetPrintWidth(std::size_t num_assignments) {
  std::size_t width = 1;
  for (; num_assignments > 0; ++width) {
    num_assignments /= 10;
  }
  return width;
}

std::string BlockName(ir::Block* const b) {
  if (b) {
    return fmt::format("block_{}", b->name);
  } else {
    return "nowhere";
  }
}

std::string IrBuilder::ToString() const {
  const std::size_t width = ValuePrintWidth();
  std::string output{};

  for (const std::unique_ptr<ir::Block>& block : blocks_) {
    fmt::format_to(std::back_inserter(output), "{}:\n", BlockName(block.get()));
    for (const ir::OpWithTarget& code : block->operations) {
      // Print the operation:
      fmt::format_to(std::back_inserter(output), "  v{:0>{}} <- ", code.target.Id(), width);
      std::visit([&](const auto& op) { FormatOpWithArgs(output, op, width); }, code.op);
      output += "\n";
    }
    std::visit(
        [&](const auto& jump) {
          using T = std::decay_t<decltype(jump)>;
          if constexpr (std::is_same_v<T, ir::Jump>) {
            fmt::format_to(std::back_inserter(output), "  {: >{}} {}\n", "jump", width + 9,
                           BlockName(jump.next));
          } else if constexpr (std::is_same_v<T, ir::ConditionalJump>) {
            fmt::format_to(std::back_inserter(output), "  {: >{}} v{:0>{}}, {}, {}\n", "jump",
                           width + 9, jump.condition.Id(), width, BlockName(jump.next_true),
                           BlockName(jump.next_false));
          } else if constexpr (std::is_same_v<T, std::monostate>) {
            fmt::format_to(std::back_inserter(output), "  {: >{}}\n", "exit", width + 9);
          }
        },
        block->jump);
  }
  if (!output.empty()) {
    output.pop_back();
  }
  return output;
}

std::size_t IrBuilder::ValuePrintWidth() const { return GetPrintWidth(insertion_point_.Id()); }

// Determine where a conditional jump merges back together.
// inline ir::Block* FindMerge(const ir::ConditionalJump& jump) {
//
//}

void MergeBlocksRecursive(ir::Block* b) {
  ASSERT(b);

  // Find the first conditional:
  while (std::holds_alternative<ir::Jump>(b->jump)) {
    b = std::get<ir::Jump>(b->jump).next;
    ASSERT(b);
  }

  // Found a conditional:
  if (!std::holds_alternative<ir::ConditionalJump>(b->jump)) {
    return;
  }

  //  const ir::ConditionalJump& jump = std::get<ir::ConditionalJump>(b->jump);

  // Find first common child
}

void IrBuilder::MergeBlocks() {}

struct ExprFromIrVisitor {
  explicit ExprFromIrVisitor(
      const std::unordered_map<ir::Value, Expr, ir::ValueHash>& value_to_expression)
      : value_to_expression_(value_to_expression) {}

  Expr operator()(const ir::Add& add) const {
    return Addition::FromOperands({MapValue(add.args[0]), MapValue(add.args[1])});
  }

  Expr operator()(const ir::Mul& mul) const {
    return Multiplication::FromOperands({MapValue(mul.args[0]), MapValue(mul.args[1])});
  }

  Expr operator()(const ir::Pow& pow) const {
    return Power::Create(MapValue(pow.args[0]), MapValue(pow.args[1]));
  }

  Expr operator()(const ir::CallUnaryFunc& func) const {
    return CreateUnaryFunction(func.name, MapValue(func.args[0]));
  }

  Expr operator()(const ir::Cond& cond) const {
    return where(MapValue(cond.args[0]), MapValue(cond.args[1]), MapValue(cond.args[2]));
  }

  Expr operator()(const ir::Phi& phi) const {
    ASSERT(phi.jump_block);
    ASSERT(std::holds_alternative<ir::ConditionalJump>(phi.jump_block->jump));
    ir::ConditionalJump jump = std::get<ir::ConditionalJump>(phi.jump_block->jump);
    return where(MapValue(jump.condition), MapValue(phi.args[0]), MapValue(phi.args[1]));
  }

  Expr operator()(const ir::Compare& cmp) const {
    return Relational::Create(cmp.operation, MapValue(cmp.args[0]), MapValue(cmp.args[1]));
  }

  Expr operator()(const ir::Copy& copy) const { return MapValue(copy.args[0]); }

  Expr operator()(const ir::Load& load) const { return load.expr; }

  Expr MapValue(const ir::Value& val) const {
    const auto arg_it = value_to_expression_.find(val);
    ASSERT(arg_it != value_to_expression_.end(), "Missing value: {}", val);
    return arg_it->second;
  }

  const std::unordered_map<ir::Value, Expr, ir::ValueHash>& value_to_expression_;
};

Expr IrBuilder::CreateExpression(const ir::Value& value) const {
  ASSERT(!blocks_.empty(), "No blocks in IrBuilder");

  std::unordered_map<ir::Value, Expr, ir::ValueHash> value_to_expression{};
  value_to_expression.reserve(200);

  std::unordered_set<ir::Block*> completed;
  completed.reserve(blocks_.size());

  std::deque<ir::Block*> queue;
  queue.push_back(blocks_.front().get());

  while (!queue.empty()) {
    // dequeue the next block
    ir::Block* const block = queue.front();
    queue.pop_front();
    ASSERT(block);

    if (completed.count(block)) {
      continue;
    }
    completed.insert(block);

    for (const ir::OpWithTarget& code : block->operations) {
      // Visit the operation, and convert it to an expression:
      Expr expr_result = std::visit(ExprFromIrVisitor{value_to_expression}, code.op);
      value_to_expression.emplace(code.target, std::move(expr_result));
    }

    VisitSuccessors(block->jump, [&](ir::Block* const b) {
      ASSERT(b);
      const bool valid = std::all_of(b->ancestors.begin(), b->ancestors.end(),
                                     [&](auto blk) { return completed.count(blk) > 0; });
      if (valid) {
        queue.push_back(b);
      }
    });
  }

  auto final_it = value_to_expression.find(value);
  ASSERT(final_it != value_to_expression.end());
  return final_it->second;
}

struct ValueTable {
  // Hash map from operation to value:
  std::unordered_map<const ir::Operation*, ir::Value, ir::OperationHasher, ir::OperationEquality>
      value_for_op;

  // Hash map from value to operation:
  std::unordered_map<ir::Value, const ir::Operation*, ir::ValueHash> op_for_value;
};

template <typename OperationType, typename Visitor>
void VisitValueOperations(OperationType&& operation, Visitor&& visitor) {
  static_assert(std::is_same_v<std::decay_t<OperationType>, ir::Operation>);
  std::visit(
      [&](auto&& op) {
        using T = std::decay_t<decltype(op)>;
        if constexpr (!std::is_same_v<T, ir::Load>) {
          visitor(std::forward<decltype(op)>(op));
        }
      },
      std::forward<OperationType>(operation));
}

void PropagateCopiedOperands(
    ir::Operation& operation,
    const std::unordered_map<ir::Value, const ir::Operation*, ir::ValueHash>& op_for_value) {
  VisitValueOperations(operation, [&op_for_value](auto& op) {
    // Iterate over all operands, and extract arguments that are loads.
    for (ir::Value& operand : op.args) {
      auto it = op_for_value.find(operand);
      if (it == op_for_value.end()) {
        continue;
      }
      // This argument is a load, so reference its argument directly.
      if (std::holds_alternative<ir::Copy>(*it->second)) {
        operand = std::get<ir::Copy>(*it->second).args[0];
      }
    }
    // Make sure arguments remain in canonical order
    using OperationType = std::decay_t<decltype(op)>;
    if constexpr (OperationType::IsCommutative()) {
      std::sort(op.args.begin(), op.args.end());
    }
  });
}

void PropagateCopiedOperands(
    ir::ConditionalJump& jump,
    const std::unordered_map<ir::Value, const ir::Operation*, ir::ValueHash>& op_for_value) {
  auto it = op_for_value.find(jump.condition);
  if (it == op_for_value.end()) {
    return;
  }
  // This argument is a copy, so reference its argument directly.
  if (std::holds_alternative<ir::Copy>(*it->second)) {
    jump.condition = std::get<ir::Copy>(*it->second).args[0];
  }
}

void LocalValueNumbering(ir::Block* const block, ValueTable& table) {
  ASSERT(block);
  for (ir::OpWithTarget& code : block->operations) {
    // First inline operands that directly reference a copy:
    PropagateCopiedOperands(code.op, table.op_for_value);

    // Record which operations yield which values. There should be no duplicates.
    const bool no_duplicates = table.op_for_value.emplace(code.target, &code.op).second;
    ASSERT(no_duplicates, "Duplicate value in map: {}", code.target);

    // Then see if this operation already exists in the map:
    const auto [it, was_inserted] = table.value_for_op.emplace(&code.op, code.target);
    if (!was_inserted) {
      // Insert another copy of a previously computed value:
      code.op = ir::Copy(it->second);
    }
  }

  // Make sure the condition on the jump is also propagated:
  if (ir::ConditionalJump* const jump = std::get_if<ir::ConditionalJump>(&block->jump);
      jump != nullptr) {
    PropagateCopiedOperands(*jump, table.op_for_value);
  }
}

void SuperLocalValueNumbering(ir::Block* const block, const ValueTable& table,
                              std::deque<ir::Block*>& queue,
                              std::unordered_set<ir::Block*>& processed) {
  ValueTable local_table = table;  // TODO: Get rid of copy!
  LocalValueNumbering(block, local_table);

  VisitSuccessors(block->jump, [&](ir::Block* const b) {
    ASSERT(b);
    if (b->ancestors.size() == 1) {
      SuperLocalValueNumbering(b, local_table, queue, processed);
    } else if (!processed.count(b)) {
      queue.push_back(b);
    }
  });
}

void IrBuilder::EliminateDuplicates() {
  std::deque<ir::Block*> block_queue;
  block_queue.push_back(FirstBlock());

  std::unordered_set<ir::Block*> processed;
  processed.reserve(blocks_.size());

  while (!block_queue.empty()) {
    ir::Block* const b = block_queue.front();
    block_queue.pop_front();
    SuperLocalValueNumbering(b, ValueTable{}, block_queue, processed);
  }
}

using ConditionSet = std::unordered_map<ir::Value, bool, ir::ValueHash>;

void IrBuilder::ThreadJumps() {
  std::deque<ir::Block*> block_queue;
  block_queue.emplace_back(FirstBlock());

  std::unordered_set<ir::Block*> visited{};
  visited.reserve(blocks_.size());

  std::unordered_map<ir::Block*, ConditionSet> condition_sets{};
  condition_sets.reserve(blocks_.size());
  condition_sets.emplace(FirstBlock(), ConditionSet{});

  std::unordered_set<ir::Block*> eliminated_branches{};
  eliminated_branches.reserve(blocks_.size());

  while (!block_queue.empty()) {
    ir::Block* const head = block_queue.front();
    block_queue.pop_front();
    ConditionSet set = condition_sets.at(head);

    if (std::holds_alternative<ir::ConditionalJump>(head->jump)) {
      const ir::ConditionalJump jump = std::get<ir::ConditionalJump>(head->jump);
      const auto set_it = set.find(jump.condition);
      if (set_it != set.end()) {
        if (set_it->second) {
          head->SetJump(jump.next_true);
          eliminated_branches.insert(jump.next_false);
        } else {
          head->SetJump(jump.next_false);
          eliminated_branches.insert(jump.next_false);
        }
      }
    }

    //    for (ir::OpWithTarget& code : head->operations) {
    //      if (std::holds_alternative<ir::Phi>(code.op)) {
    //        const ir::Phi phi = std::get<ir::Phi>(code.op);
    //        if (phi.jump_block->)
    //      }
    //    }

    OverloadedVisit(
        head->jump,
        [&](const ir::Jump& j) {
          auto [it, was_inserted] = condition_sets.emplace(head, set);
          if (!was_inserted) {
            for (const auto& pair : set) {
              auto set_it = it->second.find(pair.first);
              if (set_it != it->second.end() && set_it->second != pair.second) {
                it->second.erase(set_it);
              } else {
                it->second.emplace(pair.first, pair.second);
              }
            }
          }
          const bool valid = std::all_of(j.next->ancestors.begin(), j.next->ancestors.end(),
                                         [&](ir::Block* b) { return visited.count(b) > 0; });
          if (valid) {
            block_queue.push_back(j.next);
          }
        },
        [&](const ir::ConditionalJump& j) {
          ConditionSet set_true = std::move(set);
          ConditionSet set_false = set_true;
          set_true.emplace(j.condition, true);
          set_false.emplace(j.condition, false);
          condition_sets.emplace(j.next_true, std::move(set_true));
          condition_sets.emplace(j.next_false, std::move(set_false));
          block_queue.push_back(j.next_true);
          block_queue.push_back(j.next_false);
        },
        [](const std::monostate&) {});
  }
}

std::size_t IrBuilder::NumOperations() const {
  return std::accumulate(
      blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
      [](std::size_t total, const auto& b) { return total + b->operations.size(); });
}

std::size_t IrBuilder::NumJumps() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](std::size_t total, const auto& b) {
                           return total + std::holds_alternative<ir::ConditionalJump>(b->jump);
                         });
}

struct AstBuilder {
  explicit AstBuilder(std::size_t value_width,
                      std::vector<std::shared_ptr<const ast::Argument>> arguments)
      : value_width_(value_width), input_args_(std::move(arguments)) {}

  template <typename T>
  ast::VariantPtr VisitMakePtr(const T& arg) const {
    if constexpr (std::is_same_v<std::decay_t<T>, ir::Value>) {
      return std::make_unique<const ast::Variant>(operator()(arg));
    } else {
      return std::make_unique<const ast::Variant>(std::visit(*this, arg));
    }
  }

  ast::Variant operator()(const ir::Value& val) const {
    return ast::VariableRef{fmt::format("v{:0>{}}", val.Id(), value_width_)};
  }

  // temporary, get rid of this:
  ast::Variant operator()(const Expr& expr) const {
    return detail::VisitLambdaWithPolicy<VisitorPolicy::CompileError>(
        expr, [this](const auto& inner) -> ast::Variant {
          using T = std::decay_t<decltype(inner)>;
          if constexpr (std::is_same_v<T, Integer> || std::is_same_v<T, Float> ||
                        std::is_same_v<T, Rational>) {
            return ast::FloatConstant(static_cast<Float>(inner).GetValue());
          } else if constexpr (std::is_same_v<T, Variable>) {
            return ast::VariableRef{inner.GetName()};
          } else if constexpr (std::is_same_v<T, FunctionArgument>) {
            return ast::InputValue{input_args_[inner.ArgIndex()],
                                   static_cast<index_t>(inner.ElementIndex())};
          } else {
            throw TypeError("Invalid type in code generation expression: {}", T::NameStr);
          }
        });
  }

  ast::Variant operator()(const ir::Add& mul) const {
    return ast::Add(VisitMakePtr(mul.args[0]), VisitMakePtr(mul.args[1]));
  }

  ast::Variant operator()(const ir::Mul& mul) const {
    return ast::Multiply(VisitMakePtr(mul.args[0]), VisitMakePtr(mul.args[1]));
  }

  ast::Variant operator()(const ir::Pow& pow) const {
    return ast::Call(BinaryFunctionName::Pow, operator()(pow.args[0]), operator()(pow.args[1]));
  }

  ast::Variant operator()(const ir::Copy& load) const { return operator()(load.args[0]); }

  ast::Variant operator()(const ir::CallUnaryFunc& func) const {
    return ast::Call(func.name, operator()(func.args[0]));
  }

  ast::Variant operator()(const ir::Compare&) const {
    throw TypeError("Cannot make ast from compare");
  }
  ast::Variant operator()(const ir::Cond&) const { throw TypeError("Cannot make ast from cond"); }
  ast::Variant operator()(const ir::Phi&) const { throw TypeError("Cannot make ast from cond"); }

 private:
  std::size_t value_width_;
  std::vector<std::shared_ptr<const ast::Argument>> input_args_;
};

std::vector<ast::Variant> IrBuilder::CreateAST(const ast::FunctionSignature& func) {
  std::vector<ast::Variant> ast{};
  ast.reserve(100);

  const std::size_t value_width = ValuePrintWidth();

  AstBuilder builder{value_width, func.input_args};

  // first put a block w/ all the temporary values:
  //  for (const ir::OpWithTarget& code : operations_) {
  //    ast::VariantPtr rhs = builder.VisitMakePtr(code.op);
  //    std::string name = fmt::format("v{:0>{}}", code.target.Id(), value_width);
  //    ast.emplace_back(ast::Declaration{std::move(name), ast::ScalarType(), std::move(rhs)});
  //  }

  // then create blocks for the outputs:
  std::size_t output_flat_index = 0;
  for (const std::shared_ptr<const ast::Argument>& output_arg : func.output_args) {
    ast::VariantPtr right;
    if (std::holds_alternative<ast::ScalarType>(output_arg->Type())) {
      right = ast::MakeVariantPtr<ast::VariableRef>(
          fmt::format("v{:0>{}}", output_values_[output_flat_index].Id(), value_width));
    } else {
      std::vector<ast::Variant> inputs{};
      inputs.reserve(output_arg->TypeDimension());

      for (std::size_t i = 0; i < output_arg->TypeDimension(); ++i) {
        ASSERT_LESS(i + output_flat_index, output_values_.size());
        inputs.emplace_back(ast::VariableRef{
            fmt::format("v{:0>{}}", output_values_[i + output_flat_index].Id(), value_width)});
      }
      right = ast::MakeVariantPtr<ast::ConstructMatrix>(
          std::get<ast::MatrixType>(output_arg->Type()), std::move(inputs));
    }

    std::vector<ast::Variant> statements{};
    statements.emplace_back(ast::Assignment(output_arg, std::move(right)));

    if (output_arg->IsOptional()) {
      ast.emplace_back(ast::Conditional(ast::MakeVariantPtr<ast::OutputExists>(output_arg),
                                        std::move(statements), {}));
    } else {
      ast.emplace_back(statements.back());  //  TODO: Don't copy.
    }
    output_flat_index += output_arg->TypeDimension();
  }

  // create a block for the return values:
  std::vector<ast::Variant> return_values;
  return_values.reserve(func.return_values.size());

  for (const auto& ret_val_type : func.return_values) {
    const std::size_t dim = std::visit([](const auto& x) { return x.Dimension(); }, ret_val_type);

    if (std::holds_alternative<ast::ScalarType>(ret_val_type)) {
      return_values.emplace_back(ast::VariableRef(
          fmt::format("v{:0>{}}", output_values_[output_flat_index].Id(), value_width)));
    } else {
      std::vector<ast::Variant> inputs{};
      inputs.reserve(dim);

      for (std::size_t i = 0; i < dim; ++i) {
        ASSERT_LESS(i + output_flat_index, output_values_.size());
        inputs.emplace_back(ast::VariableRef{
            fmt::format("v{:0>{}}", output_values_[i + output_flat_index].Id(), value_width)});
      }
      return_values.emplace_back(
          ast::ConstructMatrix(std::get<ast::MatrixType>(ret_val_type), std::move(inputs)));
    }

    output_flat_index += dim;
  }
  ast.emplace_back(ast::ReturnValue(std::move(return_values)));
  return ast;
}

void IrBuilder::StripUnusedValues() {
  std::unordered_set<ir::Value, ir::ValueHash> used_values;
  used_values.reserve(100);

  std::unordered_map<ir::Value, const ir::Operation*, ir::ValueHash> op_for_values{};
  op_for_values.reserve(100);

  for (const std::unique_ptr<ir::Block>& block : blocks_) {
    for (const ir::OpWithTarget& code : block->operations) {
      op_for_values.emplace(code.target, &code.op);
    }
  }

  // Identify any reachable values:
  std::deque<ir::Value> queue;
  queue.insert(queue.end(), output_values_.begin(), output_values_.end());

  while (!queue.empty()) {
    ir::Value val = queue.front();
    queue.pop_front();

    const auto [_, inserted] = used_values.insert(val);
    if (!inserted) {
      continue;
    }

    const auto op_it = op_for_values.find(val);
    ASSERT(op_it != op_for_values.end());

    OverloadedVisit(
        *op_it->second, [](const ir::Load&) {},
        [&](const ir::Phi& phi) {
          queue.insert(queue.end(), phi.args.begin(), phi.args.end());
          const ir::ConditionalJump& jump = std::get<ir::ConditionalJump>(phi.jump_block->jump);
          queue.push_back(jump.condition);
        },
        [&](const auto& op) { queue.insert(queue.end(), op.args.begin(), op.args.end()); });
  }

  // Now nuke anything unused:
  for (const std::unique_ptr<ir::Block>& block : blocks_) {
    const auto new_end = std::remove_if(
        block->operations.begin(), block->operations.end(),
        [&](const ir::OpWithTarget& code) { return !used_values.count(code.target); });
    block->operations.erase(new_end, block->operations.end());
  }
}

}  // namespace math
