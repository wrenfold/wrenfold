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
    return fmt::format_to(ctx.out(), "{}", x.Name());
  }
};

namespace math {

// TODO: Move to template_util
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

namespace ir {

template <typename Callable>
void Block::VisitSuccessors(Callable&& callable) const {
  if (operations.empty()) {
    return;
  }
  OverloadedVisit(
      operations.back()->Op(), [&](const ir::Jump& j) { callable(j.Next()); },
      [&](const ir::ConditionalJump& j) {
        callable(j.NextTrue());
        callable(j.NextFalse());
      },
      [](const auto&) {});
}

void Block::AddAncestor(ir::Block* const b) {
  auto it = std::find(ancestors.begin(), ancestors.end(), b);
  ASSERT(it == ancestors.end(), "Attempted to insert duplicate into ancestor list: {}", b->name);
  ancestors.push_back(b);
}

void Block::RemoveAncestor(ir::Block* const b) {
  auto it = std::find(ancestors.begin(), ancestors.end(), b);
  ASSERT(it != ancestors.end());
  ancestors.erase(it);
}

void Block::PushValue(ir::Value* const v) {
  ASSERT(!v->IsJump());
  ASSERT(v->Parent() != this);
  // If this block has a jump at the end, terminate the search window before it:
  const auto len = (!operations.empty() && operations.back()->IsJump()) ? (operations.size() - 1)
                                                                        : operations.size();
  // Find the insertion point.
  // We need to place this value before any possible consumers of it:
  auto it =
      std::find_if(operations.begin(), operations.begin() + len,
                   [&](ir::Value* possible_consumer) { return possible_consumer->Consumes(v); });

  operations.insert(it, v);
  v->SetParent(this);
}

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

template <typename T>
struct FormatOpArgsHelper {
  void operator()(std::string& output, const T&, const std::vector<ir::Value*>& operands,
                  const std::size_t width) {
    for (auto it = operands.begin(); it != operands.end(); ++it) {
      ir::Value* const val = *it;
      fmt::format_to(std::back_inserter(output), "v{:0>{}}", val->Name(), width);
      if (std::next(it) != operands.end()) {
        output += ", ";
      }
    }
  }
};

template <>
struct FormatOpArgsHelper<ir::Load> {
  void operator()(std::string& output, const ir::Load& load, const std::vector<ir::Value*>&,
                  const std::size_t) {
    output += load.expr.ToString();
  }
};

template <>
struct FormatOpArgsHelper<ir::Jump> {
  void operator()(std::string& output, const ir::Jump& jump, const std::vector<ir::Value*>&,
                  const std::size_t) {
    fmt::format_to(std::back_inserter(output), "block_{}", jump.Next()->name);
  }
};

template <>
struct FormatOpArgsHelper<ir::ConditionalJump> {
  void operator()(std::string& output, const ir::ConditionalJump& jump,
                  const std::vector<ir::Value*>& args, const std::size_t width) {
    fmt::format_to(std::back_inserter(output), "v{:0>{}}, ", args[0]->Name(), width);
    fmt::format_to(std::back_inserter(output), "block_{}, ", jump.NextTrue()->name);
    fmt::format_to(std::back_inserter(output), "block_{}", jump.NextFalse()->name);
  }
};

void FormatOpArgs(std::string& output, const ir::Operation& op,
                  const std::vector<ir::Value*>& operands, const std::size_t width) {
  std::visit(
      [&](const auto& op) {
        using T = std::decay_t<decltype(op)>;
        FormatOpArgsHelper<T>{}(output, op, operands, width);
      },
      op);
}

// Visitor for converting an expression tree into static-single-assignment form.
struct IRFormVisitor {
  using Policy = VisitorPolicy::CompileError;
  using ReturnType = ir::Value*;

  explicit IRFormVisitor(const PairCountVisitor& pair_count) : pair_counts(pair_count) {}

  // Get non-const reference to blocks.
  std::vector<std::unique_ptr<ir::Block>>& TakeBlocks() { return blocks_; }
  std::vector<std::unique_ptr<ir::Value>>& TakeValues() { return values_; }

  uint32_t NextValueName() const { return next_value_name_; }

  // Handler for additions and multiplications:
  template <typename T>
  std::enable_if_t<std::is_same_v<T, Multiplication> || std::is_same_v<T, Addition>, ir::Value*>
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
    std::vector<ir::Value*> args;
    args.reserve(op.Arity());
    std::transform(expressions.begin(), expressions.end(), std::back_inserter(args),
                   [this](const Expr& expr) { return VisitStruct(expr, *this); });
    ASSERT(!args.empty());

    // then create multiplications or adds for this expression:
    ir::Value* prev_result = args[0];
    for (std::size_t i = 1; i < args.size(); ++i) {
      if constexpr (std::is_same_v<T, Multiplication>) {
        prev_result = PushOperation(Mul{}, prev_result, args[i]);
      } else {
        prev_result = PushOperation(Add{}, prev_result, args[i]);
      }
    }
    return prev_result;
  }

  ir::Value* Apply(const Expr& input, const Conditional& cond) {
    fmt::print("processing conditional: {}\n", input.ToString());
    // First compute expressions for the condition itself
    ir::Value* condition = VisitStruct(cond.Condition(), *this);

    // The block that will jump into the conditional at the end:
    Block* const current_block = PopStack();

    // Now insert a block for the if-branch:
    Block* const b_true_start = PushStack();
    ir::Value* if_branch = VisitStruct(cond.IfBranch(), *this);
    Block* const b_true_end = PopStack();

    // Then the else branch:
    Block* const b_false_start = PushStack();
    ir::Value* else_branch = VisitStruct(cond.ElseBranch(), *this);
    Block* const b_false_end = PopStack();

    // Wire up the jump at the end of `current_block`
    CreateOperation(current_block, ir::ConditionalJump{b_true_start, b_false_start}, condition);

    Block* const next = PushStack();
    CreateOperation(b_true_end, Jump{next});
    CreateOperation(b_false_end, Jump{next});

    // now we need a phi function - this gets inserted into `next_block`
    return PushOperation(Phi{}, condition, if_branch, else_branch);
  }

  ir::Value* Apply(const Expr& input_expression, const Constant&) {
    return PushOperation(Load{input_expression});
  }

  ir::Value* Apply(const Matrix&) const { throw TypeError("Cannot evaluate this on a matrix."); }

  ir::Value* Apply(const UnaryFunction& func) {
    ir::Value* arg = VisitStruct(func.Arg(), *this);
    return PushOperation(CallUnaryFunc{func.Func()}, arg);
  }

  ir::Value* Apply(const Expr&, const Infinity&) const {
    throw TypeError("Cannot generate code for complex infinity.");
  }
  ir::Value* Apply(const Expr& input_expression, const Integer&) {
    return PushOperation(Load{input_expression});
  }
  ir::Value* Apply(const Expr& input_expression, const Float&) {
    return PushOperation(Load{input_expression});
  }
  ir::Value* Apply(const Expr& input_expression, const FunctionArgument&) {
    return PushOperation(Load{input_expression});
  }

  ir::Value* Apply(const Power& pow) {
    ir::Value* base = VisitStruct(pow.Base(), *this);
    ir::Value* exponent = VisitStruct(pow.Exponent(), *this);
    return PushOperation(Pow{}, base, exponent);
  }

  ir::Value* Apply(const Expr& input_expression, const Rational&) {
    return PushOperation(Load{input_expression});
  }

  ir::Value* Apply(const Relational& relational) {
    ir::Value* left = VisitStruct(relational.Left(), *this);
    ir::Value* right = VisitStruct(relational.Right(), *this);
    return PushOperation(Compare{relational.Operation()}, left, right);
  }

  ir::Value* Apply(const Expr& input_expression, const Variable&) {
    return PushOperation(Load{input_expression});
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

  template <typename OpType, typename... Args>
  ir::Value* CreateOperation(ir::Block* const block, OpType&& op, Args... args) {
    ASSERT(block);
    if constexpr (std::is_same_v<ir::Jump, OpType>) {
      op.Next()->ancestors.push_back(block);
    } else if constexpr (std::is_same_v<ir::ConditionalJump, OpType>) {
      op.NextTrue()->ancestors.push_back(block);
      op.NextFalse()->ancestors.push_back(block);
    }
    // Create a new value:
    std::unique_ptr<ir::Value> value =
        std::make_unique<ir::Value>(next_value_name_, block, std::move(op), args...);
    ++next_value_name_;
    // Insert int the provided block:
    block->operations.push_back(value.get());
    // This is owned by the values_ vector:
    values_.push_back(std::move(value));
    return values_.back().get();
  }

  template <typename OpType, typename... Args>
  ir::Value* PushOperation(OpType&& op, Args... args) {
    ASSERT(!stack_.empty(), "Need at least one block on the stack");
    // Create a new value:
    return CreateOperation(stack_.back(), std::move(op), args...);
  }

 private:
  std::vector<std::unique_ptr<ir::Block>> blocks_;
  std::vector<std::unique_ptr<ir::Value>> values_;
  std::vector<Block*> stack_;
  uint32_t next_value_name_{0};

  const PairCountVisitor& pair_counts;
};

// Hashes the operation and all the arguments of a value.
// This deliberately ignores the name of the value.
struct ValueHasher {
  std::size_t operator()(const ir::Value* const val) const {
    // Seed the hash w/ the index in the variant, which accounts for the type of the op.
    std::size_t seed = val->Op().index();
    // Then some operations w/ members have to add the hash of those members:
    seed = HashCombine(seed, std::visit([&](const auto& op) { return op.Hash(); }, val->Op()));
    for (const ir::Value* const operand : val->Operands()) {
      const uint32_t val_name = operand->Name();
      seed = HashCombine(seed, static_cast<std::size_t>(val_name));
    }
    return seed;
  }
};

struct ValueEquality {
  bool operator()(const ir::Value* const a, const ir::Value* const b) const {
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

}  // namespace ir

IrBuilder::IrBuilder(const std::vector<Expr>& expressions) {
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
    ir::Value* const val = VisitStruct(expr, visitor);
    output_values_.push_back(val);

    // `end_block` is where execution ends for this expression.
    // It may or may not be the same as `start_block`.
    ir::Block* const end_block = visitor.PopStack();
    if (previous_block) {
      visitor.CreateOperation(previous_block, ir::Jump{start_block});
    }
    previous_block = end_block;
  }

  blocks_ = std::move(visitor.TakeBlocks());
  values_ = std::move(visitor.TakeValues());
  insertion_point_ = visitor.NextValueName();

  for (const auto& blk : blocks_) {
    for (ir::Value* val : blk->operations) {
      for (ir::Value* operand : val->Operands()) {
        auto it = std::find(operand->Consumers().begin(), operand->Consumers().end(), val);
        ASSERT(it != operand->Consumers().end());
      }
    }
  }
}

inline constexpr std::size_t GetPrintWidth(std::size_t num_assignments) {
  std::size_t width = 1;
  for (; num_assignments > 0; ++width) {
    num_assignments /= 10;
  }
  return width;
}

std::string BlockName(const ir::Block* const b) {
  if (b) {
    return fmt::format("block_{}", b->name);
  } else {
    return "nowhere";
  }
}

std::string IrBuilder::ToString(bool print_jump_origins) const {
  const std::size_t width = ValuePrintWidth();
  std::string output{};

  for (const std::unique_ptr<ir::Block>& block : blocks_) {
    fmt::format_to(std::back_inserter(output), "{}:", BlockName(block.get()));
    if (!block->IsUnreachable() && print_jump_origins) {
      output += "  <--  ";
      auto it = block->ancestors.begin();
      output += BlockName(*it);
      for (++it; it != block->ancestors.end(); ++it) {
        fmt::format_to(std::back_inserter(output), ", {}", BlockName(*it));
      }
    }
    output += "\n";

    for (const ir::Value* code : block->operations) {
      // Print the value name:
      fmt::format_to(std::back_inserter(output), "  v{:0>{}} <- ", code->Name(), width);

      // Print the instruction name:
      constexpr int OperationWidth = 4;
      fmt::format_to(std::back_inserter(output), "{:>{}} ",
                     std::visit([](const auto& op) { return op.ToString(); }, code->Op()),
                     OperationWidth);
      FormatOpArgs(output, code->Op(), code->Operands(), width);
      output += "\n";
    }
  }
  if (!output.empty()) {
    output.pop_back();
  }
  return output;
}

std::size_t IrBuilder::ValuePrintWidth() const { return GetPrintWidth(insertion_point_); }

struct ExprFromIrVisitor {
  explicit ExprFromIrVisitor(const std::unordered_map<uint32_t, Expr>& value_to_expression)
      : value_to_expression_(value_to_expression) {}

  Expr operator()(const ir::Add&, const std::vector<ir::Value*>& args) const {
    return Addition::FromOperands({MapValue(args[0]), MapValue(args[1])});
  }

  Expr operator()(const ir::Mul&, const std::vector<ir::Value*>& args) const {
    return Multiplication::FromOperands({MapValue(args[0]), MapValue(args[1])});
  }

  Expr operator()(const ir::Pow&, const std::vector<ir::Value*>& args) const {
    return Power::Create(MapValue(args[0]), MapValue(args[1]));
  }

  Expr operator()(const ir::CallUnaryFunc& func, const std::vector<ir::Value*>& args) const {
    return CreateUnaryFunction(func.name, MapValue(args[0]));
  }

  Expr operator()(const ir::Cond&, const std::vector<ir::Value*>& args) const {
    return where(MapValue(args[0]), MapValue(args[1]), MapValue(args[2]));
  }

  Expr operator()(const ir::Phi&, const std::vector<ir::Value*>& args) const {
    ASSERT_EQUAL(3, args.size());
    return where(MapValue(args[0]), MapValue(args[1]), MapValue(args[2]));
  }

  Expr operator()(const ir::Compare& cmp, const std::vector<ir::Value*>& args) const {
    return Relational::Create(cmp.operation, MapValue(args[0]), MapValue(args[1]));
  }

  Expr operator()(const ir::Copy&, const std::vector<ir::Value*>& args) const {
    return MapValue(args[0]);
  }

  Expr operator()(const ir::Load& load, const std::vector<ir::Value*>&) const { return load.expr; }

  Expr MapValue(const ir::Value* val) const {
    ASSERT(val);
    const auto arg_it = value_to_expression_.find(val->Name());
    ASSERT(arg_it != value_to_expression_.end(), "Missing value: {}", val->Name());
    return arg_it->second;
  }

  const std::unordered_map<uint32_t, Expr>& value_to_expression_;
};

std::vector<Expr> IrBuilder::CreateOutputExpressions() const {
  ASSERT(!blocks_.empty(), "No blocks in IrBuilder");

  std::unordered_map<uint32_t, Expr> value_to_expression{};
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

    const ExprFromIrVisitor visitor{value_to_expression};
    for (const ir::Value* code : block->operations) {
      // Visit the operation, and convert it to an expression.
      // We don't do anything w/ jumps, which don't actually translate to an output value directly.
      OverloadedVisit(
          code->Op(), [](const ir::Jump&) {}, [](const ir::ConditionalJump&) {},
          [&](const auto& op) {
            Expr expr = visitor(op, code->Operands());
            value_to_expression.emplace(code->Name(), std::move(expr));
          });
    }

    // If all the ancestors of a block are done, we wire it jump.
    block->VisitSuccessors([&](ir::Block* const b) {
      ASSERT(b);
      const bool valid = std::all_of(b->ancestors.begin(), b->ancestors.end(),
                                     [&](auto blk) { return completed.count(blk) > 0; });
      if (valid) {
        queue.push_back(b);
      }
    });
  }

  std::vector<Expr> result;
  result.reserve(output_values_.size());
  std::transform(output_values_.begin(), output_values_.end(), std::back_inserter(result),
                 [&value_to_expression](const ir::Value* val) {
                   auto final_it = value_to_expression.find(val->Name());
                   ASSERT(final_it != value_to_expression.end());
                   return final_it->second;
                 });
  return result;
}

using ValueTable = std::unordered_set<ir::Value*, ir::ValueHasher, ir::ValueEquality>;

void LocalValueNumbering(ir::Block* const block, ValueTable& table) {
  ASSERT(block);
  for (ir::Value* code : block->operations) {
    // Then see if this operation already exists in the map:
    auto [it, was_inserted] = table.insert(code);
    if (!was_inserted) {
      // Propagate the copy:
      code->ReplaceWith(*it);
    }
  }
}

void SuperLocalValueNumbering(ir::Block* const block, const ValueTable& table,
                              std::deque<ir::Block*>& queue,
                              std::unordered_set<ir::Block*>& processed) {
  ValueTable local_table = table;  // TODO: Get rid of copy.
  LocalValueNumbering(block, local_table);

  block->VisitSuccessors([&](ir::Block* const b) {
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

void IrBuilder::StripUnusedValues() {
  // Build a set, so that we can query which values are output values:
  const std::unordered_set<ir::Value*> output_values{output_values_.begin(), output_values_.end()};

  for (const std::unique_ptr<ir::Block>& block : blocks_) {
    const auto new_end =
        std::remove_if(block->operations.begin(), block->operations.end(), [&](ir::Value* v) {
          if (v->IsUnused() && !output_values.count(v)) {
            v->Remove();
            return true;
          }
          return false;
        });
    block->operations.erase(new_end, block->operations.end());
  }
}

std::optional<std::pair<ir::Value*, bool>> GetCondition(ir::Block* const block,
                                                        ir::Block* const ancestor) {
  ASSERT(ancestor);
  using ReturnType = std::optional<std::pair<ir::Value*, bool>>;
  ir::Value* const jump = ancestor->operations.back();
  return OverloadedVisit(
      jump->Op(), [](const ir::Jump&) -> ReturnType { return std::nullopt; },
      [block, jump](const ir::ConditionalJump& j) -> ReturnType {
        ASSERT(j.NextTrue() == block || j.NextFalse() == block,
               "Conditional is our ancestor - it must jump to this block");
        if (j.NextTrue() == block) {
          return std::make_pair(jump->Front(), true);
        } else {
          return std::make_pair(jump->Front(), false);
        }
      },
      [](const auto&) -> ReturnType { throw AssertionError("Last operation should be a jump."); });
}

using ConditionSet = std::unordered_map<ir::Value*, bool>;

void IrBuilder::FoldConditionalJumps() {
  std::deque<ir::Block*> block_queue;
  block_queue.emplace_back(FirstBlock());

  std::unordered_set<ir::Block*> visited{};
  visited.reserve(blocks_.size());

  std::unordered_map<ir::Block*, ConditionSet> condition_sets{};
  condition_sets.reserve(blocks_.size());
  condition_sets.emplace(FirstBlock(), ConditionSet{});

  while (!block_queue.empty()) {
    ir::Block* const head = block_queue.front();
    block_queue.pop_front();
    ConditionSet set = condition_sets.at(head);

    // Record that we visited this node:
    visited.insert(head);

    // See if this block is terminated by a conditional jump:
    ASSERT(!head->IsEmpty());
    ir::Value* const jump = head->operations.back();
    const bool is_cond_jump = std::holds_alternative<ir::ConditionalJump>(jump->Op());

    if (is_cond_jump) {
      ir::ConditionalJump cond_jump = std::get<ir::ConditionalJump>(jump->Op());

      // Check if the condition is in the set:
      const auto set_it = set.find(jump->Front());
      if (set_it != set.end()) {
        if (set_it->second) {
          // We need to update blocks:
          cond_jump.NextFalse()->RemoveAncestor(head);
          jump->SetOp(ir::Jump{cond_jump.NextTrue()});
        } else {
          cond_jump.NextTrue()->RemoveAncestor(head);
          jump->SetOp(ir::Jump{cond_jump.NextFalse()});
        }
      }
    }

    OverloadedVisit(
        jump->Op(),
        [&](const ir::Jump& j) {
          auto [it, was_inserted] = condition_sets.emplace(j.Next(), set);
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

          const bool valid = std::all_of(j.Next()->ancestors.begin(), j.Next()->ancestors.end(),
                                         [&](ir::Block* b) { return visited.count(b) > 0; });
          if (valid) {
            block_queue.push_back(j.Next());
          }
        },
        [&](const ir::ConditionalJump& j) {
          ConditionSet set_true = std::move(set);
          ConditionSet set_false = set_true;
          set_true.emplace(jump->Front(), true);
          set_false.emplace(jump->Front(), false);
          condition_sets.emplace(j.NextTrue(), std::move(set_true));
          condition_sets.emplace(j.NextFalse(), std::move(set_false));
          block_queue.push_back(j.NextTrue());
          block_queue.push_back(j.NextFalse());
        },
        [](const auto&) {});
  }
}

void IrBuilder::EliminateUnreachableBlocks() {
  // Skip the first block, which has no ancestors but must run.
  auto new_end = std::remove_if(
      std::next(blocks_.begin()), blocks_.end(), [&](const std::unique_ptr<ir::Block>& block) {
        if (!block->IsUnreachable()) {
          return false;
        }

        // TODO: Remove doesn't actually drop the operation, it just disconnects it
        // from predecessors. We should really have ir::Value store a ptr to the block,
        // and nuke itself from the block.
        // Eliminate in reverse order to respect dependencies.
        for (auto it = block->operations.rbegin(); it != block->operations.rend(); ++it) {
          ir::Value* val = *it;
          val->Remove();
        }

        // Now disconnect the block from its successors:
        block->VisitSuccessors([&](ir::Block* next) { next->RemoveAncestor(block.get()); });
        return true;
      });
  blocks_.erase(new_end, blocks_.end());
}

bool CombineSequentialLinearBlocks(ir::Block* block) {
  if (block->IsEmpty() || block->IsUnreachable()) {
    return false;
  }
  // Look for blocks w/ a single jump at the end:
  ir::Value* const last_value = block->operations.back();
  if (!last_value->Is<ir::Jump>()) {
    return false;
  }

  // Check if this is a jump to a block w/ one ancestor:
  ir::Jump jump = std::get<ir::Jump>(last_value->Op());
  ir::Block* const next_block = jump.Next();
  if (next_block->ancestors.size() == 1) {
    // This block has one ancestor, which is `block`. Combine them:
    // Get rid of our terminating jump:
    block->operations.pop_back();

    // This block is no longer reachable, so notify its successors:
    next_block->VisitSuccessors([&](ir::Block* b) { b->RemoveAncestor(next_block); });

    // Change parent pointer:
    for (ir::Value* v : next_block->operations) {
      v->SetParent(block);
    }

    // Copy all the operations:
    block->operations.insert(block->operations.end(), next_block->operations.begin(),
                             next_block->operations.end());
    next_block->operations.clear();

    // We are no longer an ancestor of this block:
    next_block->RemoveAncestor(block);

    // `block` needs to be hooked up as an ancestor
    block->VisitSuccessors([&](ir::Block* b) { b->AddAncestor(block); });
    return true;
  }
  return false;
}

// DFS to check if `target` is an ancestor of `start`.
bool SearchForAncestor(ir::Block* const start, ir::Block* const target) {
  if (start == target) {
    return true;
  }
  for (ir::Block* a : start->ancestors) {
    if (SearchForAncestor(a, target)) {
      return true;
    }
  }
  return false;
}

inline std::optional<ir::Block*> FindDeepestBlock(const std::vector<ir::Block*>& candidates) {
  if (candidates.empty()) {
    return std::nullopt;
  }
  ir::Block* deepest_block = candidates.front();
  for (ir::Block* candidate : candidates) {
    if (SearchForAncestor(candidate, deepest_block)) {
      // If `candidate` is a descendant of deepest_block, it is the new
      // deepest_block.
      deepest_block = candidate;
    }
  }
  return deepest_block;
}

void IrBuilder::CombineSequentialBlocks() {
  for (const std::unique_ptr<ir::Block>& block : blocks_) {
    ir::Block* const b = block.get();
    while (CombineSequentialLinearBlocks(b)) {
      // There may be multiple combinations we can do
    }
  }
}

void IrBuilder::LiftValues() {
  std::deque<ir::Block*> block_queue;
  block_queue.push_back(FirstBlock());

  std::unordered_set<ir::Block*> processed;
  processed.reserve(blocks_.size());

  std::vector<ir::Block*> ancestors;
  ancestors.reserve(3);

  while (!block_queue.empty()) {
    ir::Block* block = block_queue.front();
    block_queue.pop_front();

    processed.insert(block);

    const auto new_end =
        std::remove_if(block->operations.begin(), block->operations.end(), [&](ir::Value* v) {
          if (v->IsJump() || v->IsPhi() || v->IsConsumedByPhi()) {
            return false;
          }
          // Determine the blocks the operands to this value are defined in:
          ancestors.clear();
          for (ir::Value* operand : v->Operands()) {
            ancestors.push_back(operand->Parent());
          }

          // If the deepest block is this block, we can't move anything.
          ir::Block* const deepest_block = FindDeepestBlock(ancestors).value_or(FirstBlock());
          if (deepest_block == block) {
            return false;
          }

          // Otherwise can move this instruction up to `deepest_block` (and remove it from here):
          deepest_block->PushValue(v);
          return true;
        });
    block->operations.erase(new_end, block->operations.end());

    // Queue successors of this block, if all their ancestors have been executed.
    block->VisitSuccessors([&](ir::Block* b) {
      const bool ready = std::all_of(b->ancestors.begin(), b->ancestors.end(),
                                     [&processed](ir::Block* a) { return processed.count(a) > 0; });
      if (ready) {
        block_queue.push_back(b);
      }
    });
  }
}

ir::Block* FindMergePoint(ir::Block* left, ir::Block* right) {
  std::deque<ir::Block*> queue;
  queue.push_back(left);
  queue.push_back(right);

  std::unordered_set<ir::Block*> visited;
  visited.reserve(20);

  while (!queue.empty()) {
    ir::Block* const b = queue.front();
    queue.pop_front();

    const auto [_, was_inserted] = visited.insert(b);
    if (!was_inserted) {
      // Already visited - we found the intersection point:
      return b;
    }
    b->VisitSuccessors([&](ir::Block* b) { queue.push_back(b); });
  }
  ASSERT(false, "All branches should have a merge point");
  return nullptr;
}

std::optional<ir::Block*> GetNextCommon(ir::Block* const b) {
  ir::Value* const jump_op = b->operations.back();
  if (jump_op->Is<ir::Jump>()) {
    return jump_op->As<ir::Jump>().Next();
  } else if (jump_op->Is<ir::ConditionalJump>()) {
    const ir::ConditionalJump& cond = jump_op->As<ir::ConditionalJump>();
    return FindMergePoint(cond.NextTrue(), cond.NextFalse());
  }
  // this is the last block:
  return std::nullopt;
}

void IrBuilder::DropValues() {
  std::vector<ir::Block*> common_descendants;
  std::vector<ir::Value*> block_scratch;

  for (const std::unique_ptr<ir::Block>& block : blocks_) {
    if (block->IsUnreachable()) {
      continue;
    }

    // Find all the common descendants of this block.
    common_descendants.clear();
    std::optional<ir::Block*> target_block = GetNextCommon(block.get());
    while (target_block) {
      common_descendants.push_back(target_block.value());
      target_block = GetNextCommon(target_block.value());
    }

    // Order from deepest to shallowest
    std::reverse(common_descendants.begin(), common_descendants.end());

    // Iterate backwards over the block:
    // TODO: Just use a double-ended list?
    block_scratch = block->operations;
    std::reverse(block_scratch.begin(), block_scratch.end());

    const auto new_end_rev =
        std::remove_if(block_scratch.begin(), block_scratch.end(), [&](ir::Value* const val) {
          if (val->IsJump() || val->IsUnused() || val->IsConsumedByPhi()) {
            return false;
          }
          auto block_it = std::find_if(
              common_descendants.begin(), common_descendants.end(),
              [val](ir::Block* const descendant_block) {
                return std::all_of(val->Consumers().begin(), val->Consumers().end(),
                                   [&](ir::Value* val) {
                                     return SearchForAncestor(val->Parent(), descendant_block);
                                   });
              });
          if (block_it == common_descendants.end()) {
            return false;
          }
          //  we found a block downstream that we can insert into
          ir::Block* const dest_block = *block_it;
          // now we insert in the block:
          ASSERT(dest_block);
          dest_block->PushValue(val);
          return true;
        });

    // put them back in the block in the correct order:
    block->operations.assign(std::reverse_iterator(new_end_rev), block_scratch.rend());
  }
}

bool EliminateChainedJump(ir::Value* const jump_op) {
  if (jump_op->Is<ir::Jump>()) {
    const ir::Jump jump = jump_op->As<ir::Jump>();

    // Where this jump goes to:
    // `jump_op` is in b1, so b1 --> b2
    ir::Block* const b2 = jump.Next();
    if (b2->operations.size() != 1 || !b2->operations.back()->Is<ir::Jump>()) {
      return false;
    }

    // Is this block empty, other than another jump?
    ir::Value* const b2_jump_operation = b2->operations.back();
    OverloadedVisit(
        b2_jump_operation->Op(),
        [jump_op, b2](const ir::Jump& next_jump) {
          ir::Block* const b3 = next_jump.Next();
          // Skip this jump:
          b2->RemoveAncestor(jump_op->Parent());
          jump_op->SetOp(ir::Jump{b3});
          b3->AddAncestor(jump_op->Parent());  //  b1 --> b3
        },
        [jump_op, b2_jump_operation, b2](const ir::ConditionalJump& next_jump) {
          ir::Block* const b3 = next_jump.NextTrue();
          ir::Block* const b4 = next_jump.NextFalse();
          // go b1 --> [b3, b4]
          b2->RemoveAncestor(jump_op->Parent());
          jump_op->SetOp(ir::ConditionalJump{b3, b4}, b2_jump_operation->Front());
          b3->AddAncestor(jump_op->Parent());  //  b1 --> b3
          b4->AddAncestor(jump_op->Parent());  //  b1 --> b4
        },
        [](const auto&) {});
    return true;
  }
  return false;
}

void IrBuilder::EliminateChainedJumps() {
  for (const std::unique_ptr<ir::Block>& block : blocks_) {
    if (block->IsUnreachable() || block->IsEmpty()) {
      continue;
    }
    ir::Value* const jump_op = block->operations.back();
    while (EliminateChainedJump(jump_op)) {
    }
  }
}

std::size_t IrBuilder::NumOperations() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](std::size_t total, const auto& b) {
                           std::size_t count = b->operations.size();
                           if (count > 0 && b->operations.back()->IsJump()) {
                             count -= 1;  //  Don't count jumps.
                           }
                           return total + count;
                         });
}

std::size_t IrBuilder::NumJumps() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](std::size_t total, const auto& b) {
                           if (b->operations.empty()) {
                             return total;
                           }
                           const ir::Value* last = b->operations.back();
                           return total + static_cast<std::size_t>(last->IsJump());
                         });
}

struct AstBuilder {
  explicit AstBuilder(std::size_t value_width,
                      std::vector<std::shared_ptr<const ast::Argument>> arguments)
      : value_width_(value_width), input_args_(std::move(arguments)) {}

  ast::VariantPtr VariantPtrFromValue(const ir::Value* arg) const {
    return std::make_unique<const ast::Variant>(operator()(arg));

    //    if constexpr (std::is_same_v<std::decay_t<T>, ir::Value>) {
    //    } else {
    //      return std::make_unique<const ast::Variant>(std::visit(*this, arg));
    //    }
  }

  ast::Variant operator()(const ir::Value* val) const {
    return ast::VariableRef{fmt::format("v{:0>{}}", val->Name(), value_width_)};
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

  ast::Variant operator()(const ir::Add&, const std::vector<ir::Value*>& args) const {
    return ast::Add(VariantPtrFromValue(args[0]), VariantPtrFromValue(args[1]));
  }

  ast::Variant operator()(const ir::Mul&, const std::vector<ir::Value*>& args) const {
    return ast::Multiply(VariantPtrFromValue(args[0]), VariantPtrFromValue(args[1]));
  }

  ast::Variant operator()(const ir::Pow&, const std::vector<ir::Value*>& args) const {
    return ast::Call(BinaryFunctionName::Pow, operator()(args[0]), operator()(args[1]));
  }

  ast::Variant operator()(const ir::Copy&, const std::vector<ir::Value*>& args) const {
    return operator()(args[0]);
  }

  ast::Variant operator()(const ir::CallUnaryFunc& func,
                          const std::vector<ir::Value*>& args) const {
    return ast::Call(func.name, operator()(args[0]));
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

std::vector<ast::Variant> IrBuilder::CreateAST(const ast::FunctionSignature&) {
  std::vector<ast::Variant> ast{};
  ast.reserve(100);

  const std::size_t value_width = ValuePrintWidth();

  // traverse the blocks:
  // when we hit a conditional jump, we know this is an `if (...) { ... } else { ... }` statement
  // before jumping, we need to:
  //  - declare any variables that will be assigned
  //       - to do this, find the merger point and inspect phi functions
  //       -

  //  AstBuilder builder{value_width, func.input_args};
  //
  //  // first put a block w/ all the temporary values:
  //  //  for (const ir::OpWithTarget& code : operations_) {
  //  //    ast::VariantPtr rhs = builder.VisitMakePtr(code.op);
  //  //    std::string name = fmt::format("v{:0>{}}", code.target.Id(), value_width);
  //  //    ast.emplace_back(ast::Declaration{std::move(name), ast::ScalarType(), std::move(rhs)});
  //  //  }
  //
  //  // then create blocks for the outputs:
  //  std::size_t output_flat_index = 0;
  //  for (const std::shared_ptr<const ast::Argument>& output_arg : func.output_args) {
  //    ast::VariantPtr right;
  //    if (std::holds_alternative<ast::ScalarType>(output_arg->Type())) {
  //      right = ast::MakeVariantPtr<ast::VariableRef>(
  //          fmt::format("v{:0>{}}", output_values_[output_flat_index].Id(), value_width));
  //    } else {
  //      std::vector<ast::Variant> inputs{};
  //      inputs.reserve(output_arg->TypeDimension());
  //
  //      for (std::size_t i = 0; i < output_arg->TypeDimension(); ++i) {
  //        ASSERT_LESS(i + output_flat_index, output_values_.size());
  //        inputs.emplace_back(ast::VariableRef{
  //            fmt::format("v{:0>{}}", output_values_[i + output_flat_index].Id(), value_width)});
  //      }
  //      right = ast::MakeVariantPtr<ast::ConstructMatrix>(
  //          std::get<ast::MatrixType>(output_arg->Type()), std::move(inputs));
  //    }
  //
  //    std::vector<ast::Variant> statements{};
  //    statements.emplace_back(ast::Assignment(output_arg, std::move(right)));
  //
  //    if (output_arg->IsOptional()) {
  //      ast.emplace_back(ast::Conditional(ast::MakeVariantPtr<ast::OutputExists>(output_arg),
  //                                        std::move(statements), {}));
  //    } else {
  //      ast.emplace_back(statements.back());  //  TODO: Don't copy.
  //    }
  //    output_flat_index += output_arg->TypeDimension();
  //  }
  //
  //  // create a block for the return values:
  //  std::vector<ast::Variant> return_values;
  //  return_values.reserve(func.return_values.size());
  //
  //  for (const auto& ret_val_type : func.return_values) {
  //    const std::size_t dim = std::visit([](const auto& x) { return x.Dimension(); },
  //    ret_val_type);
  //
  //    if (std::holds_alternative<ast::ScalarType>(ret_val_type)) {
  //      return_values.emplace_back(ast::VariableRef(
  //          fmt::format("v{:0>{}}", output_values_[output_flat_index].Id(), value_width)));
  //    } else {
  //      std::vector<ast::Variant> inputs{};
  //      inputs.reserve(dim);
  //
  //      for (std::size_t i = 0; i < dim; ++i) {
  //        ASSERT_LESS(i + output_flat_index, output_values_.size());
  //        inputs.emplace_back(ast::VariableRef{
  //            fmt::format("v{:0>{}}", output_values_[i + output_flat_index].Id(), value_width)});
  //      }
  //      return_values.emplace_back(
  //          ast::ConstructMatrix(std::get<ast::MatrixType>(ret_val_type), std::move(inputs)));
  //    }
  //
  //    output_flat_index += dim;
  //  }
  //  ast.emplace_back(ast::ReturnValue(std::move(return_values)));
  return ast;
}

}  // namespace math
