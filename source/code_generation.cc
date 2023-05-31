// Copyright 2023 Gareth Cross
#include "code_generation.h"

#include <algorithm>
#include <deque>
#include <queue>
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

template <>
struct fmt::formatter<math::ir::ValuePtr, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::ir::ValuePtr x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x->Name());
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

struct DetermineNumericTypeVisitor {
  using Policy = VisitorPolicy::CompileError;
  using ReturnType = NumericType;

  NumericType Apply(const Addition& add) const {
    // Adding bool to bool yields integer, add any floats - and it becomes real, etc...
    NumericType type = NumericType::Integer;
    for (const auto& expr : add) {
      type = std::max(VisitStruct(expr, DetermineNumericTypeVisitor{}), type);
    }
    return type;
  }

  NumericType Apply(const Conditional& cond) const {
    const NumericType left = VisitStruct(cond.IfBranch(), DetermineNumericTypeVisitor{});
    const NumericType right = VisitStruct(cond.ElseBranch(), DetermineNumericTypeVisitor{});
    return std::max(left, right);
  }

  NumericType Apply(const Constant& c) const {
    switch (c.GetName()) {
      case SymbolicConstants::Euler:
      case SymbolicConstants::Pi:
        return NumericType::Real;
      case SymbolicConstants::True:
      case SymbolicConstants::False:
        return NumericType::Bool;
    }
    throw TypeError("Unhandled symbolic constant");
  }

  constexpr NumericType Apply(const Float&) const { return NumericType::Real; }

  constexpr NumericType Apply(const FunctionArgument&) const { return NumericType::Real; }

  constexpr NumericType Apply(const Infinity&) const { return NumericType::Complex; }

  constexpr NumericType Apply(const Integer&) const { return NumericType::Integer; }

  // TODO: For now all matrices are interpreted as real.
  constexpr NumericType Apply(const Matrix&) const { return NumericType::Real; }

  NumericType Apply(const Multiplication& mul) const {
    // Multiplying booleans produces an integer, same as C++.
    NumericType type = NumericType::Integer;
    for (const auto& expr : mul) {
      type = std::max(VisitStruct(expr, DetermineNumericTypeVisitor{}), type);
    }
    return type;
  }

  NumericType Apply(const Power& pow) const {
    const NumericType b = VisitStruct(pow.Base(), DetermineNumericTypeVisitor{});
    const NumericType e = VisitStruct(pow.Exponent(), DetermineNumericTypeVisitor{});
    return std::max(b, e);
  }

  constexpr NumericType Apply(const Rational&) const { return NumericType::Real; }

  constexpr NumericType Apply(const Relational&) const { return NumericType::Bool; }

  constexpr NumericType Apply(const UnaryFunction&) const { return NumericType::Real; }

  constexpr NumericType Apply(const Variable&) const { return NumericType::Real; }
};

NumericType DetermineNumericType(const Expr& x) {
  return VisitStruct(x, DetermineNumericTypeVisitor{});
}

namespace ir {

NumericType Load::DetermineType() const { return DetermineNumericType(expr); }

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

std::optional<ir::ValuePtr> Block::PopJump() {
  if (operations.empty()) {
    return std::nullopt;
  }
  const ir::BlockPtr self{this};
  const bool is_jump = OverloadedVisit(
      operations.back()->Op(),
      [&](const ir::Jump& j) {
        j.Next()->RemoveAncestor(self);
        return true;
      },
      [&](const ir::ConditionalJump& j) {
        j.NextTrue()->RemoveAncestor(self);
        j.NextFalse()->RemoveAncestor(self);
        return true;
      },
      [&](const auto&) constexpr { return false; });
  if (!is_jump) {
    return std::nullopt;
  }
  // Pop and return it:
  ir::ValuePtr jump_val = operations.back();
  operations.pop_back();

  return jump_val;
}

void Block::SetJump(ValuePtr v) {
  ASSERT(v->IsJump());
  ASSERT(operations.empty() || !operations.back()->IsJump(), "Cannot have two jumps");
  BlockPtr self{this};
  OverloadedVisit(
      v->Op(), [&](ir::Jump j) { j.Next()->AddAncestor(self); },
      [&](ir::ConditionalJump j) {
        j.NextTrue()->AddAncestor(self);
        j.NextFalse()->AddAncestor(self);
      },
      [](const auto&) constexpr {});
  operations.push_back(v);
  v->SetParent(self);
}

void Block::AddAncestor(BlockPtr b) {
  auto it = std::find(ancestors.begin(), ancestors.end(), b);
  ASSERT(it == ancestors.end(), "Attempted to insert duplicate into ancestor list: {}", b->name);
  ancestors.push_back(b);
}

void Block::RemoveAncestor(BlockPtr b) {
  auto it = std::find(ancestors.begin(), ancestors.end(), b);
  ASSERT(it != ancestors.end());
  ancestors.erase(it);
}

void Block::PushValue(const ValuePtr& v) {
  ASSERT(!v->IsJump());
  ASSERT(v->Parent() != this);
  // If this block has a jump at the end, terminate the search window before it:
  const auto len = (!operations.empty() && operations.back()->IsJump()) ? (operations.size() - 1)
                                                                        : operations.size();
  // Find the insertion point.
  // We need to place this value before any possible consumers of it:
  auto it = std::find_if(
      operations.begin(), operations.begin() + len,
      [&](const ValuePtr& possible_consumer) { return possible_consumer->Consumes(v); });

  operations.insert(it, v);
  v->SetParent(BlockPtr{this});
}

void Value::RemoveOperand(ir::Value* const v) {
  auto it = std::find(operands_.begin(), operands_.end(), v);
  ASSERT(it != operands_.end(), "Could not find operand {} in {}, which has operands: [{}]",
         ir::ValuePtr(v), ir::ValuePtr(this), fmt::join(operands_, ", "));
  operands_.erase(it);
}

// Replace this value w/ the argument.
void Value::ReplaceWith(const ValuePtr other) {
  ASSERT(this != other);
  for (const ValuePtr& consumer : consumers_) {
    consumer->ReplaceOperand(ValuePtr{this}, other);
  }
  consumers_.clear();
}

void Value::Remove() {
  ASSERT(consumers_.empty(), "Attempting to remove a value `{}` that is consumed by: [{}]", name_,
         fmt::join(consumers_, ","));
  // Notify our operands we no longer consume them.
  for (const ValuePtr& operand : operands_) {
    operand->RemoveConsumer(this);
  }
  // This value is dead, so clear the operand vector
  operands_.clear();
}

void Value::RemoveFromDownstreamPhiFunctions() {
  // Borrow the consumer vector. We do this so if anything tries to touch consumers_, we
  // can catch it:
  std::vector<ir::ValuePtr> consumers = std::move(consumers_);
  auto new_end = std::remove_if(consumers.begin(), consumers.end(), [this](ValuePtr maybe_phi) {
    if (maybe_phi->IsPhi()) {
      // Keep the operand to the phi function that is not `this`:
      const std::vector<ValuePtr>& phi_args = maybe_phi->Operands();
      ASSERT_EQUAL(2, phi_args.size());
      ir::ValuePtr replacement = phi_args.front() == this ? phi_args.back() : phi_args.front();

      //      fmt::print("Replacing phi {} with {}\n", maybe_phi, replacement);
      maybe_phi->ReplaceWith(replacement);
      maybe_phi->RemoveOperand(this);
      return true;
    }
    return false;
  });
  consumers.erase(new_end, consumers.end());
  consumers_ = std::move(consumers);  //  Return ownership.
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
  void operator()(std::string& output, const T&, const std::vector<ir::ValuePtr>& operands,
                  const std::size_t width) {
    for (auto it = operands.begin(); it != operands.end(); ++it) {
      const ir::ValuePtr& val = *it;
      fmt::format_to(std::back_inserter(output), "v{:0>{}}", val->Name(), width);
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
    output += load.expr.ToString();
  }
};

template <>
struct FormatOpArgsHelper<ir::Jump> {
  void operator()(std::string& output, const ir::Jump& jump, const std::vector<ir::ValuePtr>&,
                  const std::size_t) {
    fmt::format_to(std::back_inserter(output), "block_{}", jump.Next()->name);
  }
};

template <>
struct FormatOpArgsHelper<ir::ConditionalJump> {
  void operator()(std::string& output, const ir::ConditionalJump& jump,
                  const std::vector<ir::ValuePtr>& args, const std::size_t width) {
    fmt::format_to(std::back_inserter(output), "v{:0>{}}, ", args[0]->Name(), width);
    fmt::format_to(std::back_inserter(output), "block_{}, ", jump.NextTrue()->name);
    fmt::format_to(std::back_inserter(output), "block_{}", jump.NextFalse()->name);
  }
};

template <>
struct FormatOpArgsHelper<ir::OutputRequired> {
  void operator()(std::string& output, const ir::OutputRequired& oreq,
                  const std::vector<ir::ValuePtr>&, const std::size_t) {
    fmt::format_to(std::back_inserter(output), "{}", oreq.arg_position);
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

// Visitor for converting an expression tree into static-single-assignment form.
struct IRFormVisitor {
  using Policy = VisitorPolicy::CompileError;
  using ReturnType = ValuePtr;

  explicit IRFormVisitor(IrBuilder& builder, const PairCountVisitor& pair_count, ir::BlockPtr block)
      : builder_(builder), pair_counts(pair_count), current_block_(block) {}

  ValuePtr MaybeCast(ValuePtr input, NumericType output_type) {
    if (input->DetermineType() != output_type) {
      return PushOperation(ir::Cast{output_type}, input);
    } else {
      return input;
    }
  }

  // Handler for additions and multiplications:
  template <typename T>
  std::enable_if_t<std::is_same_v<T, Multiplication> || std::is_same_v<T, Addition>, ValuePtr>
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
    std::vector<ValuePtr> args;
    args.reserve(op.Arity());
    std::transform(expressions.begin(), expressions.end(), std::back_inserter(args),
                   [this](const Expr& expr) { return Visit(expr); });
    ASSERT(!args.empty());

    NumericType promoted_type = NumericType::Integer;
    for (ValuePtr v : args) {
      promoted_type = std::max(promoted_type, v->DetermineType());
    }

    // then create multiplications or adds for this expression:
    ValuePtr prev_result = MaybeCast(args[0], promoted_type);
    for (std::size_t i = 1; i < args.size(); ++i) {
      if constexpr (std::is_same_v<T, Multiplication>) {
        prev_result = PushOperation(Mul{}, prev_result, MaybeCast(args[i], promoted_type));
      } else {
        prev_result = PushOperation(Add{}, prev_result, MaybeCast(args[i], promoted_type));
      }
    }
    return prev_result;
  }

  ValuePtr Apply(const Conditional& cond) {
    // Check if the condition is already assumed true in the current scope:
    auto condition_it = condition_set_.find(cond.Condition());
    if (condition_it != condition_set_.end()) {
      if (condition_it->second) {
        // Condition is true in this scope:
        return Visit(cond.IfBranch());
      } else {
        // Condition is false in this scope:
        return Visit(cond.ElseBranch());
      }
    }

    // Compute expression for the condition itself:
    ValuePtr condition = Visit(cond.Condition());

    // Condition is not yet known in this scope.
    // First set it true and process if-branch, then set it false and process else-branch.
    auto [it, _] = condition_set_.emplace(cond.Condition(), true);
    const ValuePtr if_branch = Visit(cond.IfBranch());
    it->second = false;
    const ValuePtr else_branch = Visit(cond.ElseBranch());
    condition_set_.erase(it);  //  Pop it from the set of known conditions.

    const NumericType promoted_type =
        std::max(if_branch->DetermineType(), else_branch->DetermineType());

    return PushOperation(ir::Cond{}, condition, MaybeCast(if_branch, promoted_type),
                         MaybeCast(else_branch, promoted_type));
  }

  ValuePtr Apply(const Expr& input_expression, const Constant&) {
    return PushOperation(Load{input_expression});
  }

  ValuePtr Apply(const Matrix&) const { throw TypeError("Cannot evaluate this on a matrix."); }

  ValuePtr Apply(const UnaryFunction& func) {
    return PushOperation(CallUnaryFunc{func.Func()}, Visit(func.Arg()));
  }

  ValuePtr Apply(const Expr&, const Infinity&) const {
    throw TypeError("Cannot generate code for complex infinity.");
  }
  ValuePtr Apply(const Expr& input_expression, const Integer&) {
    return PushOperation(Load{input_expression});
  }
  ValuePtr Apply(const Expr& input_expression, const Float&) {
    return PushOperation(Load{input_expression});
  }
  ValuePtr Apply(const Expr& input_expression, const FunctionArgument&) {
    return PushOperation(Load{input_expression});
  }

  ValuePtr Apply(const Power& pow) {
    const ValuePtr b = Visit(pow.Base());
    const ValuePtr e = Visit(pow.Exponent());
    const NumericType promoted_type =
        std::max(NumericType::Integer, std::max(b->DetermineType(), e->DetermineType()));

    return PushOperation(Pow{}, MaybeCast(b, promoted_type), MaybeCast(e, promoted_type));
  }

  ValuePtr Apply(const Expr&, const Rational& rational) {
    const auto temp =
        static_cast<double>(rational.Numerator()) / static_cast<double>(rational.Denominator());
    return PushOperation(Load{MakeExpr<Float>(temp)});
  }

  ValuePtr Apply(const Relational& relational) {
    return PushOperation(Compare{relational.Operation()}, Visit(relational.Left()),
                         Visit(relational.Right()));
  }

  ValuePtr Apply(const Expr& input_expression, const Variable&) {
    return PushOperation(Load{input_expression});
  }

  template <typename OpType, typename... Args>
  ValuePtr PushOperation(OpType&& op, Args... args) {
    return builder_.CreateOperation(ir::BlockPtr{current_block_}, std::move(op), args...);
  }

  // Check if a value has been computed. If not, convert it and return the result.
  ValuePtr Visit(const Expr& expr) {
    auto it = computed_values_.find(expr);
    if (it != computed_values_.end()) {
      return it->second;
    }
    ValuePtr val = VisitStruct(expr, *this);
    computed_values_.emplace(expr, val);
    return val;
  }

 private:
  IrBuilder& builder_;
  ir::BlockPtr current_block_;

  std::unordered_map<Expr, ValuePtr, ExprHash, ExprEquality> computed_values_;
  std::unordered_map<Expr, bool, ExprHash, ExprEquality> condition_set_;

  const PairCountVisitor& pair_counts;
};

// Hashes the operation and all the arguments of a value.
// This deliberately ignores the name of the value.
struct ValueHasher {
  std::size_t operator()(const ir::ValuePtr& val) const {
    // Seed the hash w/ the index in the variant, which accounts for the type of the op.
    std::size_t seed = val->Op().index();
    // Then some operations w/ members have to add the hash of those members:
    seed = HashCombine(seed, std::visit([&](const auto& op) { return op.Hash(); }, val->Op()));
    for (const ValuePtr& operand : val->Operands()) {
      const uint32_t val_name = operand->Name();
      seed = HashCombine(seed, static_cast<std::size_t>(val_name));
    }
    return seed;
  }
};

struct ValueEquality {
  bool operator()(const ValuePtr& a, const ValuePtr& b) const {
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

enum class SearchDirection { Downwards, Upwards };

ir::BlockPtr FindMergePoint(const ir::BlockPtr left, const ir::BlockPtr right,
                            const SearchDirection direction) {
  // queue with [node, color]
  std::deque<std::pair<ir::BlockPtr, bool>> queue;
  queue.emplace_back(left, true);
  queue.emplace_back(right, false);

  std::unordered_map<ir::BlockPtr, bool> visited;
  visited.reserve(20);

  while (!queue.empty()) {
    const auto [b, color] = queue.front();
    queue.pop_front();

    const auto [it, was_inserted] = visited.emplace(b, color);
    if (!was_inserted && it->second != color) {
      // Already visited by a different color, we found the intersection point:
      return b;
    }
    if (direction == SearchDirection::Downwards) {
      b->VisitSuccessors([&](ir::BlockPtr child) { queue.emplace_back(child, color); });
    } else {
      for (ir::BlockPtr ancestor : b->ancestors) {
        queue.emplace_back(ancestor, color);
      }
    }
  }

  throw AssertionError("All branches should have a merge point");
}

IrBuilder::IrBuilder(const std::vector<ExpressionGroup>& groups) {
  // First pass where we count occurrence of some sub-expressions:
  ir::PairCountVisitor pair_visitor{};
  std::size_t total_output_size = 0;
  for (const ExpressionGroup& group : groups) {
    for (const Expr& expr : group.expressions) {
      VisitStruct(expr, pair_visitor);
    }
    total_output_size += group.expressions.size();
  }

  // First insert a block for all the computations:
  const ir::BlockPtr start_block = CreateBlock();
  ir::IRFormVisitor visitor{*this, pair_visitor, start_block};

  std::vector<ir::ValuePtr> output_values;
  output_values.reserve(total_output_size);

  std::vector<std::size_t> scatter;
  scatter.reserve(groups.size());

  // Convert everything to ir::Value
  std::size_t group_start = 0;
  for (const ExpressionGroup& group : groups) {
    for (const Expr& expr : group.expressions) {
      output_values.push_back(visitor.Visit(expr));
    }
    scatter.push_back(group_start);
    group_start += group.expressions.size();
  }

  // Then insert blocks to write to the optional outputs:
  ir::BlockPtr last_block = start_block;
  for (std::size_t index = 0; index < groups.size(); ++index) {
    const ExpressionGroup& group = groups[index];
    if (group.key.usage != ExpressionUsage::OptionalOutputArgument) {
      continue;
    }
    const ir::BlockPtr next_block = CreateBlock();

    // Create a bool to check if this output is required?
    const ir::ValuePtr exists =
        CreateOperation(last_block, ir::OutputRequired{group.key.arg_position});

    // Then insert a branch
    const ir::BlockPtr left = CreateBlock();
    CreateOperation(last_block, ir::ConditionalJump{left, next_block}, exists);

    // Insert save operations into the left branch:
    for (std::size_t i = 0; i < group.expressions.size(); ++i) {
      const std::size_t output_index = scatter[index] + i;
      CreateOperation(left, ir::Save{group.key, i}, output_values[output_index]);
    }

    // Jump to the next block after save operations:
    CreateOperation(left, ir::Jump{next_block});
    last_block = next_block;
  }

  // Now insert operations for the return value:
  for (std::size_t index = 0; index < groups.size(); ++index) {
    const ExpressionGroup& group = groups[index];
    if (group.key.usage == ExpressionUsage::OptionalOutputArgument) {
      continue;
    }
    for (std::size_t i = 0; i < group.expressions.size(); ++i) {
      const std::size_t output_index = scatter[index] + i;
      CreateOperation(last_block, ir::Save{group.key, i}, output_values[output_index]);
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

inline std::string BlockName(const ir::Block& b) { return fmt::format("block_{}", b.name); }

std::string IrBuilder::ToString(bool print_jump_origins) const {
  const std::size_t width = ValuePrintWidth();
  std::string output{};

  for (const std::unique_ptr<ir::Block>& block : blocks_) {
    fmt::format_to(std::back_inserter(output), "{}:", BlockName(*block));
    if (!block->IsUnreachable() && print_jump_origins) {
      output += "  <--  ";
      auto it = block->ancestors.begin();
      output += BlockName(**it);
      for (++it; it != block->ancestors.end(); ++it) {
        fmt::format_to(std::back_inserter(output), ", {}", BlockName(**it));
      }
    }
    output += "\n";

    for (const ir::ValuePtr& code : block->operations) {
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
  explicit ExprFromIrVisitor(const std::unordered_map<ir::ValuePtr, Expr>& value_to_expression)
      : value_to_expression_(value_to_expression) {}

  Expr operator()(const ir::Add&, const std::vector<ir::ValuePtr>& args) const {
    return MapValue(args[0]) + MapValue(args[1]);
  }

  Expr operator()(const ir::Mul&, const std::vector<ir::ValuePtr>& args) const {
    return MapValue(args[0]) * MapValue(args[1]);
  }

  Expr operator()(const ir::OutputRequired&, const std::vector<ir::ValuePtr>&) const {
    return Constants::True;
  }

  Expr operator()(const ir::Pow&, const std::vector<ir::ValuePtr>& args) const {
    return Power::Create(MapValue(args[0]), MapValue(args[1]));
  }

  Expr operator()(const ir::CallUnaryFunc& func, const std::vector<ir::ValuePtr>& args) const {
    return CreateUnaryFunction(func.name, MapValue(args[0]));
  }

  Expr operator()(const ir::Cast&, const std::vector<ir::ValuePtr>& args) const {
    return MapValue(args[0]);
  }

  Expr operator()(const ir::Cond&, const std::vector<ir::ValuePtr>& args) const {
    return where(MapValue(args[0]), MapValue(args[1]), MapValue(args[2]));
  }

  Expr operator()(const ir::Phi&, const std::vector<ir::ValuePtr>& args) const {
    ASSERT_EQUAL(2, args.size());

    // We find to find the condition for this jump:
    const ir::BlockPtr jump_block =
        FindMergePoint(args.front()->Parent(), args.back()->Parent(), SearchDirection::Upwards);

    // Determine the condition:
    ASSERT(!jump_block->IsEmpty());

    ir::ValuePtr jump_val = jump_block->operations.back();
    ASSERT(jump_val->Is<ir::ConditionalJump>());

    return where(MapValue(jump_val->Front()), MapValue(args[0]), MapValue(args[1]));
  }

  Expr operator()(const ir::Compare& cmp, const std::vector<ir::ValuePtr>& args) const {
    return Relational::Create(cmp.operation, MapValue(args[0]), MapValue(args[1]));
  }

  Expr operator()(const ir::Copy&, const std::vector<ir::ValuePtr>& args) const {
    return MapValue(args[0]);
  }

  Expr operator()(const ir::Load& load, const std::vector<ir::ValuePtr>&) const {
    return load.expr;
  }

  Expr operator()(const ir::Save&, const std::vector<ir::ValuePtr>& args) const {
    return MapValue(args[0]);
  }

  Expr MapValue(const ir::ValuePtr& val) const {
    const auto arg_it = value_to_expression_.find(val);
    ASSERT(arg_it != value_to_expression_.end(), "Missing value: {}", val->Name());
    return arg_it->second;
  }

  const std::unordered_map<ir::ValuePtr, Expr>& value_to_expression_;
};

// Note: This method is probably somewhat slow/inefficient. It really exists only for unit tests,
// so perhaps not worth improving.
std::unordered_map<OutputKey, std::vector<Expr>, OutputKeyHasher>
IrBuilder::CreateOutputExpressions() const {
  std::unordered_map<ir::ValuePtr, Expr> value_to_expression{};
  value_to_expression.reserve(200);

  std::unordered_set<ir::BlockPtr> completed;
  completed.reserve(blocks_.size());

  std::deque<ir::BlockPtr> queue;
  queue.emplace_back(FirstBlock());

  using OutputTuple = std::tuple<OutputKey, std::size_t, Expr>;
  std::vector<OutputTuple> output_values{};
  output_values.reserve(100);

  while (!queue.empty()) {
    // dequeue the next block
    const ir::BlockPtr block = queue.front();
    queue.pop_front();

    if (completed.count(block)) {
      continue;
    }
    completed.insert(block);

    const ExprFromIrVisitor visitor{value_to_expression};
    for (const ir::ValuePtr& code : block->operations) {
      // Visit the operation, and convert it to an expression.
      // We don't do anything w/ jumps, which don't actually translate to an output value directly.
      OverloadedVisit(
          code->Op(), [](const ir::Jump&) {}, [](const ir::ConditionalJump&) {},
          [&](const auto& op) {
            Expr expr = visitor(op, code->Operands());
            value_to_expression.emplace(code, std::move(expr));
          });

      if (code->Is<ir::Save>()) {
        const ir::Save& save = code->As<ir::Save>();
        output_values.emplace_back(save.key, save.output_index, value_to_expression.at(code));
      }
    }

    // If all the ancestors of a block are done, we wire it jump.
    block->VisitSuccessors([&](ir::BlockPtr b) {
      const bool valid = std::all_of(b->ancestors.begin(), b->ancestors.end(),
                                     [&](auto blk) { return completed.count(blk) > 0; });
      if (valid) {
        queue.push_back(b);
      }
    });
  }

  // Convert all the save operations to expression groups.
  // First put the output values into order:
  const auto key_part_of_tuple = [](const OutputTuple& x) {
    return std::make_pair(std::get<0>(x),
                          std::get<1>(x));  //  Return just the output position and index.
  };
  std::sort(output_values.begin(), output_values.end(),
            [&](const OutputTuple& a, const OutputTuple& b) {
              return key_part_of_tuple(a) < key_part_of_tuple(b);
            });

  // then build the result map:
  std::unordered_map<OutputKey, std::vector<Expr>, OutputKeyHasher> output_map{};
  for (auto it = output_values.begin(); it != output_values.end();) {
    const auto end_it = std::find_if(it, output_values.end(), [&](const OutputTuple& tup) {
      return key_part_of_tuple(*it) != key_part_of_tuple(tup);
    });
    // Insert into output map
    std::vector<Expr> expressions{};
    std::transform(it, end_it, std::back_inserter(expressions),
                   [](const auto& tup) { return std::get<2>(tup); });
    output_map.emplace(std::get<0>(*it), std::move(expressions));
  }
  return output_map;
}

using ValueTable = std::unordered_set<ir::ValuePtr, ir::ValueHasher, ir::ValueEquality>;

inline void LocalValueNumbering(ir::BlockPtr block, ValueTable& table) {
  for (const ir::ValuePtr& code : block->operations) {
    // Then see if this operation already exists in the map:
    auto [it, was_inserted] = table.insert(code);
    if (!was_inserted) {
      // Propagate the copy:
      if (code->IsConsumedByPhi()) {
        // If this value feeds a phi function, we need to keep it, but turn it into a copy:
        code->SetOp(ir::Copy{}, *it);
      } else {
        code->ReplaceWith(*it);
      }
    }
  }
}

inline void SuperLocalValueNumbering(ir::BlockPtr block, const ValueTable& table,
                                     std::deque<ir::BlockPtr>& queue,
                                     std::unordered_set<ir::BlockPtr>& processed) {
  ValueTable local_table = table;  // TODO: Get rid of copy.
  LocalValueNumbering(block, local_table);

  block->VisitSuccessors([&](ir::BlockPtr b) {
    if (b->ancestors.size() == 1) {
      SuperLocalValueNumbering(b, local_table, queue, processed);
    } else if (!processed.count(b)) {
      queue.push_back(b);
    }
  });
}

void IrBuilder::EliminateDuplicates() {
  std::deque<ir::BlockPtr> block_queue;
  block_queue.push_back(FirstBlock());

  std::unordered_set<ir::BlockPtr> processed;
  processed.reserve(blocks_.size());

  while (!block_queue.empty()) {
    ir::BlockPtr b = block_queue.front();
    block_queue.pop_front();
    SuperLocalValueNumbering(b, ValueTable{}, block_queue, processed);
  }
}

void IrBuilder::StripUnusedValues() {
  for (const std::unique_ptr<ir::Block>& block : blocks_) {
    std::reverse(block->operations.begin(), block->operations.end());
    const auto new_end = std::remove_if(block->operations.begin(), block->operations.end(),
                                        [&](const ir::ValuePtr& v) {
                                          if (v->IsUnused() && !v->Is<ir::Save>()) {
                                            v->Remove();
                                            return true;
                                          }
                                          return false;
                                        });
    block->operations.erase(new_end, block->operations.end());
    std::reverse(block->operations.begin(), block->operations.end());
  }
}

inline void ReorderConditionalsInBlock(const ir::BlockPtr block) {
  if (block->IsEmpty()) {
    return;
  }

  std::vector<ir::ValuePtr> ordered{};
  std::unordered_set<ir::ValuePtr> visited{};

  ordered.reserve(block->operations.size());
  visited.reserve(block->operations.size());

  // Check if something is in the visited set:
  const auto is_visited = [&visited](ir::ValuePtr v) { return visited.count(v) > 0; };

  std::deque<ir::ValuePtr> queue{};
  std::vector<ir::ValuePtr> queue_conditionals{};

  // Fill the queue w/ anything that has no dependency:
  for (ir::ValuePtr v : block->operations) {
    if (v->NumOperands() == 0) {
      queue.push_back(v);
    }
  }

  // Helper to queue any non-visited children:
  const auto queue_children = [&](ir::ValuePtr v) {
    for (ir::ValuePtr child : v->Consumers()) {
      if (child->Parent() == block && !visited.count(child) && !child->IsJump() &&
          child->AllOperandsSatisfy(is_visited)) {
        if (child->Is<ir::Cond>()) {
          queue_conditionals.push_back(child);
        } else {
          queue.push_back(child);
        }
      }
    }
  };

  for (;;) {
    // First put any non-conditionals we can:
    while (!queue.empty()) {
      ir::ValuePtr top = queue.front();
      queue.pop_front();
      ordered.push_back(top);
      visited.insert(top);
      queue_children(top);
    }

    if (queue_conditionals.empty()) {
      // no more work to do:
      break;
    }

    // We have run out of non-conditionals to queue, so now we insert conditionals.
    // We know these cannot depend on each other, since otherwise they would not have been
    // queued. So we sort them into order by their condition argument. We move out of
    // `queue_conditionals` because queue_children will touch it.
    std::vector<ir::ValuePtr> conditionals = std::move(queue_conditionals);
    queue_conditionals.clear();
    std::sort(conditionals.begin(), conditionals.end(),
              [](ir::ValuePtr a, ir::ValuePtr b) { return a->Front().get() < b->Front().get(); });

    // Now we can visit them:
    ordered.insert(ordered.end(), conditionals.begin(), conditionals.end());
    visited.insert(conditionals.begin(), conditionals.end());
    std::for_each(conditionals.begin(), conditionals.end(), queue_children);
  }

  if (block->operations.back()->IsJump()) {
    ordered.push_back(block->operations.back());
  }
  ASSERT_EQUAL(ordered.size(), block->operations.size());
  block->operations = std::move(ordered);
}

void IrBuilder::ConvertTernaryConditionalsToJumps(bool b) {
  ir::BlockPtr block = FirstBlock();
  if (b) {
    ReorderConditionalsInBlock(block);
    return;
  }

  // Now we convert conditionals into actual blocks:
  // Iterate until we hit a conditional, which creates a fork.
  // Insert copies in each fork, and a phi function where the branches meet.
  for (;;) {
    const auto start_it = std::find_if(block->operations.begin(), block->operations.end(),
                                       [](ir::ValuePtr v) { return v->Is<ir::Cond>(); });
    if (start_it == block->operations.end()) {
      break;  //  No more conditionals.
    }
    const auto end_it = std::find_if(start_it, block->operations.end(), [&](ir::ValuePtr v) {
      return !v->Is<ir::Cond>() || v->Front() != (*start_it)->Front();
    });

    // Create an if and else branch:
    const ir::BlockPtr left_block = CreateBlock();
    const ir::BlockPtr right_block = CreateBlock();

    // Create the merge point - the left/right blocks will jump here:
    const ir::BlockPtr merge_point = CreateBlock();

    const ir::ValuePtr condition_val = (*start_it)->Operands().front();
    for (auto it = start_it; it != end_it; ++it) {
      const ir::ValuePtr left_val = CreateOperation(left_block, ir::Copy{}, (*it)->Operands()[1]);
      const ir::ValuePtr right_val = CreateOperation(right_block, ir::Copy{}, (*it)->Operands()[2]);
      // Make phi function in the merge block:
      const ir::ValuePtr phi = CreateOperation(merge_point, ir::Phi{}, left_val, right_val);
      // Replace usages of the conditional output w/ uses of the phi function
      (*it)->ReplaceWith(phi);
    }

    CreateOperation(left_block, ir::Jump{merge_point});
    CreateOperation(right_block, ir::Jump{merge_point});

    // Remove all the values we just replaced:
    std::for_each(start_it, end_it, [](ir::ValuePtr val) { val->Remove(); });

    // Take remaining values computed in this block, and move them to the new merge point block:
    std::for_each(end_it, block->operations.end(),
                  [&](ir::ValuePtr val) { val->SetParent(merge_point); });

    merge_point->operations.insert(merge_point->operations.end(), end_it, block->operations.end());

    // Cleanup the previous block
    block->operations.erase(start_it, block->operations.end());

    // And add a conditional jump
    CreateOperation(block, ir::ConditionalJump{left_block, right_block}, condition_val);
    block = merge_point;
  }
}

void IrBuilder::EliminateUnreachableBlocks() {
  // Skip the first block, which has no ancestors but must run.
  auto new_end =
      std::remove_if(std::next(blocks_.begin()), blocks_.end(), [&](ir::Block::unique_ptr& block) {
        if (!block->IsUnreachable()) {
          return false;
        }

        // Eliminate in reverse order to respect dependencies.
        std::for_each(block->operations.rbegin(), block->operations.rend(), [](ir::ValuePtr val) {
          val->RemoveFromDownstreamPhiFunctions();
          val->Remove();
        });

        // Now disconnect the block from its successors:
        block->VisitSuccessors(
            [&](ir::BlockPtr next) { next->RemoveAncestor(ir::BlockPtr{block}); });

        // move it to dead blocks:
        dead_blocks_.push_back(std::move(block));
        return true;
      });
  blocks_.erase(new_end, blocks_.end());
}

bool CombineSequentialLinearBlocks(const ir::BlockPtr block, const bool is_start_block) {
  if (block->IsEmpty() || (block->IsUnreachable() && !is_start_block)) {
    return false;
  }
  // Look for blocks w/ a single jump at the end:
  const ir::ValuePtr& last_value = block->operations.back();
  if (!last_value->Is<ir::Jump>()) {
    return false;
  }

  // Check if this is a jump to a block w/ one ancestor:
  ir::Jump jump = std::get<ir::Jump>(last_value->Op());
  ir::BlockPtr next_block = jump.Next();
  if (next_block->ancestors.size() == 1) {
    // This block has one ancestor, which is `block`. Combine them:
    // Get rid of our terminating jump:
    block->operations.pop_back();

    // This block is no longer reachable, so notify its successors:
    next_block->VisitSuccessors([&](ir::BlockPtr b) { b->RemoveAncestor(next_block); });

    // Change parent pointer:
    for (const ir::ValuePtr& v : next_block->operations) {
      v->SetParent(block);
    }

    // Copy all the operations:
    block->operations.insert(block->operations.end(), next_block->operations.begin(),
                             next_block->operations.end());
    next_block->operations.clear();

    // We are no longer an ancestor of this block:
    next_block->RemoveAncestor(block);

    // `block` needs to be hooked up as an ancestor
    block->VisitSuccessors([&](ir::BlockPtr b) { b->AddAncestor(block); });
    return true;
  }
  return false;
}

// DFS to check if `target` is an ancestor of `start`.
bool SearchForAncestor(const ir::BlockPtr start, const ir::BlockPtr target) {
  if (start == target) {
    return true;
  }
  for (ir::BlockPtr ancestor : start->ancestors) {
    if (SearchForAncestor(ancestor, target)) {
      return true;
    }
  }
  return false;
}

inline std::optional<ir::BlockPtr> FindDeepestBlock(const std::vector<ir::BlockPtr>& candidates) {
  if (candidates.empty()) {
    return std::nullopt;
  }
  ir::BlockPtr deepest_block = candidates.front();
  for (ir::BlockPtr candidate : candidates) {
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
    const bool is_start_block = block == blocks_.front();
    while (CombineSequentialLinearBlocks(ir::BlockPtr{block}, is_start_block)) {
      // There may be multiple combinations we can do
    }
  }
}

void IrBuilder::LiftValues() {
  std::deque<ir::BlockPtr> block_queue;
  block_queue.push_back(FirstBlock());

  std::unordered_set<ir::BlockPtr> processed;
  processed.reserve(blocks_.size());

  std::vector<ir::BlockPtr> ancestors;
  ancestors.reserve(3);

  while (!block_queue.empty()) {
    const ir::BlockPtr block = block_queue.front();
    block_queue.pop_front();

    processed.insert(block);

    const auto new_end = std::remove_if(
        block->operations.begin(), block->operations.end(), [&](const ir::ValuePtr& v) {
          if (v->IsJump() || v->IsPhi() || v->IsConsumedByPhi()) {
            return false;
          }
          // Determine the blocks the operands to this value are defined in:
          ancestors.clear();
          for (const ir::ValuePtr& operand : v->Operands()) {
            ancestors.push_back(operand->Parent());
          }

          // If the deepest block is this block, we can't move anything.
          ir::BlockPtr deepest_block = FindDeepestBlock(ancestors).value_or(FirstBlock());
          if (deepest_block == block) {
            return false;
          }

          // Otherwise can move this instruction up to `deepest_block` (and remove it from here):
          deepest_block->PushValue(v);
          return true;
        });
    block->operations.erase(new_end, block->operations.end());

    // Queue successors of this block, if all their ancestors have been executed.
    block->VisitSuccessors([&](ir::BlockPtr b) {
      const bool ready = std::all_of(
          b->ancestors.begin(), b->ancestors.end(),
          [&processed](ir::BlockPtr ancestor) { return processed.count(ancestor) > 0; });
      if (ready) {
        block_queue.push_back(b);
      }
    });
  }
}

std::optional<ir::BlockPtr> GetNextCommon(const ir::BlockPtr block) {
  const ir::ValuePtr& jump_op = block->operations.back();
  if (jump_op->Is<ir::Jump>()) {
    return jump_op->As<ir::Jump>().Next();
  } else if (jump_op->Is<ir::ConditionalJump>()) {
    const ir::ConditionalJump& cond = jump_op->As<ir::ConditionalJump>();
    return FindMergePoint(cond.NextTrue(), cond.NextFalse(), SearchDirection::Downwards);
  }
  // this is the last block:
  return std::nullopt;
}

void IrBuilder::DropValues() {
  std::vector<ir::BlockPtr> common_descendants;
  std::vector<ir::ValuePtr> block_scratch;

  for (const std::unique_ptr<ir::Block>& block : blocks_) {
    if (block->IsUnreachable()) {
      continue;
    }

    // Find all the common descendants of this block.
    common_descendants.clear();
    std::optional<ir::BlockPtr> target_block = GetNextCommon(ir::BlockPtr{block});
    while (target_block) {
      common_descendants.push_back(target_block.value());
      target_block = GetNextCommon(target_block.value());
    }

    // Order from deepest to shallowest
    std::reverse(common_descendants.begin(), common_descendants.end());

    // Iterate backwards over the block:
    block_scratch = block->operations;
    std::reverse(block_scratch.begin(), block_scratch.end());

    const auto new_end_rev =
        std::remove_if(block_scratch.begin(), block_scratch.end(), [&](const ir::ValuePtr& val) {
          if (val->IsJump() || val->IsUnused() || val->IsConsumedByPhi()) {
            return false;
          }
          auto block_it = std::find_if(
              common_descendants.begin(), common_descendants.end(),
              [val](ir::BlockPtr descendant_block) {
                return std::all_of(val->Consumers().begin(), val->Consumers().end(),
                                   [&](const ir::ValuePtr& val) {
                                     return SearchForAncestor(val->Parent(), descendant_block);
                                   });
              });
          if (block_it == common_descendants.end()) {
            return false;
          }
          //  we found a block downstream that we can insert into
          (*block_it)->PushValue(val);
          return true;
        });

    // put them back in the block in the correct order:
    block->operations.assign(std::reverse_iterator(new_end_rev), block_scratch.rend());
  }
}

bool EliminateChainedJump(ir::ValuePtr jump_op) {
  if (jump_op->Is<ir::Jump>()) {
    const ir::Jump jump = jump_op->As<ir::Jump>();

    // Where this jump goes to:
    // `jump_op` is in b1, so b1 --> b2
    const ir::BlockPtr b2 = jump.Next();
    if (b2->operations.size() != 1 || !b2->operations.back()->Is<ir::Jump>()) {
      return false;
    }

    // Is this block empty, other than another jump?
    const ir::ValuePtr b2_jump_operation = b2->operations.back();
    OverloadedVisit(
        b2_jump_operation->Op(),
        [jump_op, b2](const ir::Jump& next_jump) {
          const ir::BlockPtr b3 = next_jump.Next();
          // Skip this jump:
          b2->RemoveAncestor(jump_op->Parent());
          jump_op->SetOp(ir::Jump{b3});
          b3->AddAncestor(jump_op->Parent());  //  b1 --> b3
        },
        [jump_op, b2_jump_operation, b2](const ir::ConditionalJump& next_jump) {
          const ir::BlockPtr b3 = next_jump.NextTrue();
          const ir::BlockPtr b4 = next_jump.NextFalse();
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
  for (const ir::Block::unique_ptr& block : blocks_) {
    if (block->IsUnreachable() || block->IsEmpty()) {
      continue;
    }
    while (EliminateChainedJump(block->operations.back())) {
    }
  }
}

inline std::optional<std::pair<ir::ConditionalJump, ir::ValuePtr>> GetConditionalJump(
    ir::BlockPtr block) {
  if (!block->operations.empty()) {
    ir::ValuePtr maybe_jump = block->operations.back();
    if (maybe_jump->Is<ir::ConditionalJump>()) {
      ASSERT_EQUAL(1, maybe_jump->NumOperands());
      return std::make_pair(maybe_jump->As<ir::ConditionalJump>(), maybe_jump->Front());
    }
  }
  return std::nullopt;
}

std::size_t IrBuilder::NumOperations() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](std::size_t total, const ir::Block::unique_ptr& b) {
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
                           if (b->IsEmpty()) {
                             return total;
                           }
                           const ir::ValuePtr& last = b->operations.back();
                           return total + static_cast<std::size_t>(last->IsJump());
                         });
}

template <typename OpType, typename... Args>
ir::ValuePtr IrBuilder::CreateOperation(ir::BlockPtr block, OpType&& op, Args... args) {
  if constexpr (std::is_same_v<ir::Jump, OpType>) {
    op.Next()->ancestors.push_back(block);
  } else if constexpr (std::is_same_v<ir::ConditionalJump, OpType>) {
    op.NextTrue()->ancestors.push_back(block);
    op.NextFalse()->ancestors.push_back(block);
  }
  // Create a new value:
  std::unique_ptr<ir::Value> value =
      std::make_unique<ir::Value>(insertion_point_, block, std::forward<OpType>(op), args...);
  ++insertion_point_;
  // Insert int the provided block:
  block->operations.emplace_back(value.get());
  // This is owned by the values_ vector:
  values_.push_back(std::move(value));
  return block->operations.back();
}

ir::BlockPtr IrBuilder::CreateBlock() {
  ir::Block::unique_ptr block = std::make_unique<ir::Block>(blocks_.size());
  blocks_.push_back(std::move(block));
  return ir::BlockPtr(blocks_.back());
}

struct AstBuilder {
  AstBuilder(std::size_t value_width, const ast::FunctionSignature& signature)
      : value_width_(value_width), signature_(signature) {}

  ast::FunctionDefinition CreateFunction(ir::BlockPtr block) {
    ProcessBlock(block);
    return ast::FunctionDefinition{signature_, std::move(operations_)};
  }

  void ProcessBlock(ir::BlockPtr block) {
    ASSERT(!block->IsEmpty());
    if (stop_set_.count(block)) {
      return;
    }
    operations_.reserve(operations_.capacity() + block->operations.size());

    // Number of operations in the block, ignoring jump at the end:
    const std::size_t len = block->operations.back()->IsJump() ? block->operations.size() - 1
                                                               : block->operations.size();

    // We want to place any `ir::Save` operations at the end before conditional logic, so separate
    // those:
    std::vector<ir::ValuePtr> save_values{};
    save_values.reserve(block->operations.size());

    for (std::size_t i = 0; i < len; ++i) {
      const ir::ValuePtr value = block->operations[i];
      if (value->Is<ir::Save>()) {
        save_values.push_back(value);
      } else if (value->IsPhi()) {
        // Do nothing.
      } else {
        // Declare a temporary:
        Emplace<ast::Declaration>(FormatTemporary(value), ast::ScalarType(), VisitMakePtr(value));

        for (ir::ValuePtr consumer : value->Consumers()) {
          if (consumer->IsPhi()) {
            // Values consumed by phi functions should be assigned to their output variable.
            Emplace<ast::AssignTemporary>(FormatTemporary(consumer),
                                          FormatVariableRefMakePtr(value));
          }
        }
      }
    }

    // order save values by their position in the output:
    std::sort(save_values.begin(), save_values.end(),
              [](ir::ValuePtr a, ir::ValuePtr b) { return a->As<ir::Save>() < b->As<ir::Save>(); });

    // insert outputs
    for (auto it = save_values.begin(); it != save_values.end();) {
      // Find the sub-range of output values that all have the same output argument:
      const OutputKey key = (*it)->As<ir::Save>().key;
      const auto end_it = std::find_if(std::next(it), save_values.end(), [&](ir::ValuePtr v) {
        return key != v->As<ir::Save>().key;
      });

      std::vector<ast::Variant> args{};
      args.reserve(std::distance(it, end_it));
      for (; it != end_it; ++it) {
        const ir::ValuePtr& val = *it;
        args.emplace_back(FormatVariableRef(val->Front()));
      }

      if (key.usage == ExpressionUsage::ReturnValue) {
        Emplace<ast::ConstructReturnValue>(
            key.arg_position, signature_.return_values.at(key.arg_position), std::move(args));
      } else {
        Emplace<ast::AssignOutputArgument>(signature_.arguments.at(key.arg_position),
                                           std::move(args));
      }
    }

    ir::ValuePtr last_op = block->operations.back();
    if (last_op->Is<ir::Jump>()) {
      // just keep appending:
      ir::BlockPtr next = last_op->As<ir::Jump>().Next();
      ProcessBlock(next);
    } else if (last_op->Is<ir::ConditionalJump>()) {
      // For conditionals, we need to determine the
      const ir::ConditionalJump& jump = last_op->As<ir::ConditionalJump>();

      // Find phi functions:
      const ir::BlockPtr merge_point =
          FindMergePoint(jump.NextTrue(), jump.NextFalse(), SearchDirection::Downwards);
      stop_set_.insert(merge_point);

      for (ir::ValuePtr maybe_phi : merge_point->operations) {
        if (maybe_phi->Is<ir::Phi>()) {
          // Also we should declare this variable prior to entering the branch. Not required for
          // languages like Python, but needed for C++, Rust, etc...
          Emplace<ast::Declaration>(FormatTemporary(maybe_phi), ast::ScalarType());
        }
      }

      // move aside the contents of this block, as we are about to descend the branches:
      std::vector<ast::Variant> operations_stashed = std::move(operations_);
      operations_.clear();

      // Process the true branch and save the output:
      ProcessBlock(jump.NextTrue());  //  append to operations_
      std::vector<ast::Variant> operations_true = std::move(operations_);
      operations_.clear();

      // Process the right branch
      ProcessBlock(jump.NextFalse());
      std::vector<ast::Variant> operations_false = std::move(operations_);
      operations_ = std::move(operations_stashed);  //  Put back operations for the current block.

      // Create a conditional
      Emplace<ast::Branch>(ast::VariableRef{FormatTemporary(last_op->Front())},
                           std::move(operations_true), std::move(operations_false));

      stop_set_.erase(merge_point);
      ProcessBlock(merge_point);
    }
  }

  std::string FormatTemporary(const ir::Value& val) const {
    return fmt::format("v{:0>{}}", val.Name(), value_width_);
  }

  std::string FormatTemporary(const ir::ValuePtr val) const { return FormatTemporary(*val); }

  ast::Variant FormatVariableRef(const ir::ValuePtr val) const {
    return ast::VariableRef{FormatTemporary(val)};
  }

  ast::VariantPtr FormatVariableRefMakePtr(const ir::ValuePtr val) const {
    return std::make_shared<const ast::Variant>(ast::VariableRef{FormatTemporary(val)});
  }

  ast::Variant Visit(ir::ValuePtr val) {
    return std::visit(
        [this, &val](const auto& op) -> ast::Variant { return this->operator()(*val, op); },
        val->Op());
  }

  ast::VariantPtr VisitMakePtr(ir::ValuePtr val) {
    auto variant = Visit(val);
    return std::make_shared<const ast::Variant>(std::move(variant));
  }

  template <typename T, typename... Args>
  void Emplace(Args&&... args) {
    operations_.emplace_back(T{std::forward<Args>(args)...});
  }

  ast::Variant operator()(const ir::Value& val, const ir::Add&) {
    return ast::Add{FormatVariableRefMakePtr(val[0]), FormatVariableRefMakePtr(val[1])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::CallUnaryFunc& func) {
    return ast::Call{func.name, FormatVariableRef(val[0])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Cast& cast) {
    return ast::Cast{cast.destination_type, FormatVariableRefMakePtr(val[0])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Compare& compare) {
    return ast::Compare{compare.operation, FormatVariableRefMakePtr(val[0]),
                        FormatVariableRefMakePtr(val[1])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Copy&) {
    return FormatVariableRef(val.Front());
  }

  ast::Variant operator()(const ir::Value& val, const ir::Mul&) {
    return ast::Multiply{FormatVariableRefMakePtr(val[0]), FormatVariableRefMakePtr(val[1])};
  }

  ast::Variant operator()(const ir::Value&, const ir::Load& load) {
    return detail::VisitLambdaWithPolicy<VisitorPolicy::CompileError>(
        load.expr, [this](const auto& inner) -> ast::Variant {
          using T = std::decay_t<decltype(inner)>;
          if constexpr (std::is_same_v<T, Integer>) {
            return ast::IntegerConstant{inner.GetValue()};
          } else if constexpr (std::is_same_v<T, Float>) {
            return ast::FloatConstant{inner.GetValue()};
          } else if constexpr (std::is_same_v<T, Variable>) {
            return ast::VariableRef{inner.GetName()};
          } else if constexpr (std::is_same_v<T, FunctionArgument>) {
            const auto element_index = static_cast<index_t>(inner.ElementIndex());
            return ast::InputValue{signature_.arguments[inner.ArgIndex()], element_index};
          } else {
            throw TypeError("Invalid type in code generation expression: {}", T::NameStr);
          }
        });
  }

  ast::Variant operator()(const ir::Value&, const ir::OutputRequired& oreq) {
    return ast::OutputExists{signature_.arguments[oreq.arg_position]};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Pow&) {
    return ast::Call{BinaryFunctionName::Pow, FormatVariableRef(val[0]), FormatVariableRef(val[1])};
  }

  // Gross, but we need to have these defined.
  // TODO: Things like Jump and Save that do not produce values should maybe be in a separate type?
  ast::Variant operator()(const ir::Value&, const ir::Jump&) const {
    throw AssertionError("Should never be called.");
  }
  ast::Variant operator()(const ir::Value&, const ir::ConditionalJump&) const {
    throw AssertionError("Should never be called.");
  }
  ast::Variant operator()(const ir::Value&, const ir::Save&) const {
    throw AssertionError("Should never be called.");
  }
  ast::Variant operator()(const ir::Value&, const ir::Cond&) const {
    throw AssertionError("Should never be called.");
  }
  ast::Variant operator()(const ir::Value&, const ir::Phi&) const {
    throw AssertionError("Should never be called.");
  }

 private:
  std::size_t value_width_;
  const ast::FunctionSignature& signature_;

  std::vector<ast::Variant> operations_;
  std::unordered_set<ir::BlockPtr> stop_set_;
};

ast::FunctionDefinition IrBuilder::CreateAST(const ast::FunctionSignature& signature) const {
  AstBuilder builder(ValuePrintWidth(), signature);
  return builder.CreateFunction(FirstBlock());
}

}  // namespace math
