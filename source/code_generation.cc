// Copyright 2023 Gareth Cross
#include "code_generation.h"

#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "expressions/all_expressions.h"
#include "hashing.h"
#include "visitor_impl.h"

#include "code_formatter.h"

namespace math {
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

// Visitor for converting an expression tree into static-single-assignment form.
struct IRFormVisitor {
  using Policy = VisitorPolicy::CompileError;
  using ReturnType = Operand;

  // Construct with an output IrBuilder that we write into.
  explicit IRFormVisitor(IrBuilder& output, const PairCountVisitor& pair_count)
      : operations_(output), pair_counts(pair_count) {}

  // Handler for additions and multiplications:
  template <typename T>
  std::enable_if_t<std::is_same_v<T, Multiplication> || std::is_same_v<T, Addition>, Operand> Apply(
      const T& op) {
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
    std::vector<Operand> args;
    args.reserve(op.Arity());
    std::transform(expressions.begin(), expressions.end(), std::back_inserter(args),
                   [this](const Expr& expr) { return VisitStruct(expr, *this); });
    ASSERT(!args.empty());

    // then create multiplications or adds for this expression:
    Operand prev_result = args[0];
    for (std::size_t i = 1; i < args.size(); ++i) {
      if constexpr (std::is_same_v<T, Multiplication>) {
        prev_result = operations_.PushOperation<Mul>(prev_result, args[i]);
      } else {
        prev_result = operations_.PushOperation<Add>(prev_result, args[i]);
      }
    }
    return prev_result;
  }

  Operand Apply(const Expr& input_expression, const Constant&) const { return input_expression; }

  Operand Apply(const Matrix&) const { throw TypeError("Cannot evaluate this on a matrix."); }

  Operand Apply(const UnaryFunction& func) {
    Operand arg = VisitStruct(func.Arg(), *this);
    return operations_.PushOperation<CallUnaryFunc>(func.Func(), std::move(arg));
  }

  Operand Apply(const Expr& input_expression, const Integer&) const { return input_expression; }
  Operand Apply(const Expr& input_expression, const Float&) const { return input_expression; }
  Operand Apply(const Expr& input_expression, const FunctionArgument&) const {
    return input_expression;
  }

  Operand Apply(const Power& pow) {
    Operand base = VisitStruct(pow.Base(), *this);
    Operand exponent = VisitStruct(pow.Exponent(), *this);
    return operations_.PushOperation<Pow>(std::move(base), std::move(exponent));
  }

  Operand Apply(const Expr& input_expression, const Rational&) const { return input_expression; }
  Operand Apply(const Expr& input_expression, const Variable&) const { return input_expression; }

 private:
  IrBuilder& operations_;
  const PairCountVisitor& pair_counts;
};

inline std::size_t HashOperand(const Operand& operand) {
  return std::visit(
      [](const auto& operand) -> std::size_t {
        using T = std::decay_t<decltype(operand)>;
        if constexpr (std::is_same_v<T, Value>) {
          // For values just hash their ID.
          return static_cast<const Value&>(operand).Hash();
        } else {
          // For constants, fall back to the Expr hash implementation.
          return HashExpression(operand);
        }
      },
      operand);
}

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
  for (const Operand& operand : op.args) {
    seed = HashCombine(seed, HashOperand(operand));
  }
  return seed;
}

std::size_t HashOp(const Add& addition) { return HashOpWithArgs(addition); }

std::size_t HashOp(const Mul& mul) { return HashOpWithArgs(mul); }

std::size_t HashOp(const Pow& pow) { return HashOpWithArgs(pow); }

std::size_t HashOp(const Load& load) { return HashOpWithArgs(load); }

std::size_t HashOp(const CallUnaryFunc& call) {
  return HashCombine(HashOpWithArgs(call), static_cast<std::size_t>(call.name));
}

struct OperationHasher {
  std::size_t operator()(const Operation& operation) const {
    return std::visit([](const auto& op) -> std::size_t { return HashOp(op); }, operation);
  }
};

struct OperationEquality {
  bool operator()(const Operation& a, const Operation& b) const {
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
        a, b);
  }
};

}  // namespace ir

IrBuilder::IrBuilder(const std::vector<Expr>& expressions) {
  operations_.reserve(100);
  output_values_.reserve(expressions.size());
  // Count occurrence of some sub-expressions:
  ir::PairCountVisitor pair_visitor{};
  for (const Expr& expr : expressions) {
    VisitStruct(expr, pair_visitor);
  }

  // need to convert

  // Now convert every expression to IR and record the output value:
  for (const Expr& expr : expressions) {
    ir::IRFormVisitor visitor{*this, pair_visitor};
    ir::Operand operand = VisitStruct(expr, visitor);
    if (std::holds_alternative<Expr>(operand)) {
      // If the top-level expression was a constant, we need to insert a copy operation for it:
      operand = PushOperation<ir::Load>(std::get<Expr>(operand));
    }
    // Now the operand is definitely a value:
    ASSERT(std::holds_alternative<ir::Value>(operand));
    insertion_point_ = std::get<ir::Value>(operand);
    output_values_.push_back(insertion_point_);
    insertion_point_.Increment();
  }
}

inline constexpr std::size_t GetPrintWidth(std::size_t num_assignments) {
  std::size_t width = 1;
  for (; num_assignments > 0; ++width) {
    num_assignments /= 10;
  }
  return width;
}

inline void FormatOperand(std::string& output, const ir::Operand& operand,
                          const std::size_t width) {
  std::visit(
      [&](const auto& operand) {
        using T = std::decay_t<decltype(operand)>;
        if constexpr (std::is_same_v<T, ir::Value>) {
          fmt::format_to(std::back_inserter(output), "v{:0>{}}", operand.Id(), width);
        } else {
          // input value of type `Expr`
          output += operand.ToString();
        }
      },
      operand);
}

template <typename Op>
inline void FormatOpWithArgs(std::string& output, const Op& op, const std::size_t width) {
  constexpr int OperationWidth = 4;
  fmt::format_to(std::back_inserter(output), "{:>{}} ", op.ToString(), OperationWidth);
  for (auto it = op.args.begin(); it != op.args.end(); ++it) {
    FormatOperand(output, *it, width);
    if (std::next(it) != op.args.end()) {
      output += ", ";
    }
  }
}

std::string IrBuilder::FormatIR(const ir::Value& value) const {
  const std::vector<ir::Value> traversal_order = GetTraversalOrder(value);
  const std::size_t width = ValuePrintWidth();

  std::string output{};
  for (const ir::Value& val : traversal_order) {
    auto it = operations_.find(val);
    ASSERT(it != operations_.end());

    // Print the operation:
    fmt::format_to(std::back_inserter(output), "v{:0>{}} <- ", val.Id(), width);
    std::visit([&](const auto& op) { FormatOpWithArgs(output, op, width); }, it->second);
    output += "\n";
  }
  output.pop_back();  //  remove final newline
  return output;
}

std::string IrBuilder::FormatIRAllOutputs() const {
  const auto order = GetTraversalOrder();
  const std::size_t width = ValuePrintWidth();
  std::string output{};
  for (auto val : order) {
    auto it = operations_.find(val);
    ASSERT(it != operations_.end());

    // Print the operation:
    fmt::format_to(std::back_inserter(output), "v{:0>{}} <- ", val.Id(), width);
    std::visit([&](const auto& op) { FormatOpWithArgs(output, op, width); }, it->second);
    output += "\n";
  }
  output.pop_back();
  return output;
}

std::size_t IrBuilder::ValuePrintWidth() const { return GetPrintWidth(insertion_point_.Id()); }

inline Expr CreateExpressionForOp(const ir::Add&, std::vector<Expr>&& args) {
  return Addition::FromOperands(args);
}

inline Expr CreateExpressionForOp(const ir::Mul&, std::vector<Expr>&& args) {
  return Multiplication::FromOperands(args);
}

inline Expr CreateExpressionForOp(const ir::Pow&, std::vector<Expr>&& args) {
  ASSERT_EQUAL(2, args.size());
  return Power::Create(args[0], args[1]);
}

inline Expr CreateExpressionForOp(const ir::CallUnaryFunc& func, std::vector<Expr>&& args) {
  ASSERT_EQUAL(1, args.size());
  return CreateUnaryFunction(func.name, args.front());
}

inline Expr CreateExpressionForOp(const ir::Load&, std::vector<Expr>&& args) {
  return args.front();
}

Expr IrBuilder::CreateExpression(const ir::Value& value) const {
  const std::vector<ir::Value> traversal_order = GetTraversalOrder(value);

  std::unordered_map<ir::Value, Expr, ir::ValueHash> value_to_expression{};
  value_to_expression.reserve(traversal_order.size());

  for (const ir::Value& val : traversal_order) {
    const auto it = operations_.find(val);
    ASSERT(it != operations_.end());

    // Visit the operation, and convert it to an expression:
    Expr expr_result = std::visit(
        [&value_to_expression, val](const auto& op) -> Expr {
          // Unpack all the arguments into `Expr` types:
          std::vector<Expr> arguments;
          arguments.reserve(2);
          for (const ir::Operand& arg : op.args) {
            if (std::holds_alternative<ir::Value>(arg)) {
              const ir::Value arg_val = std::get<ir::Value>(arg);
              const auto arg_it = value_to_expression.find(arg_val);
              ASSERT(arg_it != value_to_expression.end(),
                     "Missing value: {} (while computing value: {})", arg_val.Id(), val.Id());
              arguments.push_back(arg_it->second);
            } else {
              arguments.push_back(std::get<Expr>(arg));
            }
          }
          return CreateExpressionForOp(op, std::move(arguments));
        },
        it->second);

    value_to_expression.emplace(val, std::move(expr_result));
  }

  auto final_it = value_to_expression.find(value);
  ASSERT(final_it != value_to_expression.end());
  return final_it->second;
}

void IrBuilder::EliminateDuplicates() {
  // Hash map from operation to value
  std::unordered_map<ir::Operation, ir::Value, ir::OperationHasher, ir::OperationEquality>
      value_for_variant;
  value_for_variant.reserve(operations_.size());

  // determine traversal order:
  const std::vector<ir::Value> ordered_keys = GetTraversalOrder();

  for (const ir::Value value : ordered_keys) {
    auto it = operations_.find(value);
    ASSERT(it != operations_.end());

    // First inline operands that directly reference a copy:
    PropagateCopiedOperands(it->second);

    // Then see if this operation already exists in the map:
    const auto [existing_it, was_inserted] = value_for_variant.emplace(it->second, it->first);
    if (!was_inserted) {
      // Insert another copy of a previously computed value:
      it->second = ir::Load(existing_it->second);
    }
  }

  StripUnusedValues();
}

template <typename T, typename... Args>
ir::Operand IrBuilder::PushOperation(Args&&... args) {
  T operation{std::forward<Args>(args)...};
  const ir::Value value = insertion_point_;
  insertion_point_.Increment();
  operations_.emplace(value, std::move(operation));
  return value;
}

template <typename Handler>
void VisitValueArgs(const ir::Operation& op, Handler handler) {
  std::visit(
      [handler = std::move(handler)](const auto& op) {
        for (const ir::Operand& arg : op.args) {
          if (std::holds_alternative<ir::Value>(arg)) {
            handler(std::get<ir::Value>(arg));
          }
        }
      },
      op);
}

template <typename Handler>
inline void RecursiveTraverse(
    const ir::Value& value,
    const std::unordered_map<ir::Value, ir::Operation, ir::ValueHash>& operations,
    std::unordered_set<ir::Value, ir::ValueHash>& visited, const Handler& handler) {
  auto it = operations.find(value);
  ASSERT(it != operations.end(), "Missing operation for value: {}", value.Id());
  if (visited.count(value)) {
    return;
  }
  visited.insert(value);
  VisitValueArgs(it->second, [&](const ir::Value& val) {
    RecursiveTraverse(val, operations, visited, handler);
  });
  // This is depth first, so record this value after all of its arguments.
  handler(value);
}

std::vector<ir::Value> IrBuilder::GetTraversalOrder(const ir::Value& value) const {
  // We need a set to track what we have already visited. This is because some expressions will
  // be used more than once (after EliminateDuplicates is called).
  std::unordered_set<ir::Value, ir::ValueHash> visited{};
  visited.reserve(operations_.size());

  std::vector<ir::Value> order;
  order.reserve(20);
  RecursiveTraverse(value, operations_, visited,
                    [&](const ir::Value& val) { order.push_back(val); });
  return order;
}

std::vector<ir::Value> IrBuilder::GetTraversalOrder() const {
  std::vector<ir::Value> ordered{};
  ordered.reserve(operations_.size());

  std::unordered_set<ir::Value, ir::ValueHash> visited{};
  visited.reserve(operations_.size());

  for (const ir::Value& value : output_values_) {
    RecursiveTraverse(value, operations_, visited,
                      [&ordered](const ir::Value& val) { ordered.push_back(val); });
  }
  return ordered;
}

struct AstBuilder {
  explicit AstBuilder(std::size_t value_width,
                      std::vector<std::shared_ptr<const ast::Argument>> arguments)
      : value_width_(value_width), input_args_(std::move(arguments)) {}

  template <typename T>
  ast::VariantPtr VisitMakePtr(const T& arg) const {
    return std::make_unique<const ast::Variant>(std::visit(*this, arg));
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

  ast::Variant operator()(const ir::Operand& operand) const { return std::visit(*this, operand); }

  ast::Variant operator()(const ir::Add& mul) const {
    return ast::Add(VisitMakePtr(mul.args[0]), VisitMakePtr(mul.args[1]));
  }

  ast::Variant operator()(const ir::Mul& mul) const {
    return ast::Multiply(VisitMakePtr(mul.args[0]), VisitMakePtr(mul.args[1]));
  }

  ast::Variant operator()(const ir::Pow& pow) const {
    return ast::Call(BinaryFunctionName::Pow, std::visit(*this, pow.args[0]),
                     std::visit(*this, pow.args[1]));
  }

  ast::Variant operator()(const ir::Load& load) const { return std::visit(*this, load.args[0]); }

  ast::Variant operator()(const ir::CallUnaryFunc& func) const {
    return ast::Call(func.name, std::visit(*this, func.args[0]));
  }

 private:
  std::size_t value_width_;
  std::vector<std::shared_ptr<const ast::Argument>> input_args_;
};

std::vector<ast::Variant> IrBuilder::CreateAST(const ast::FunctionSignature& func) {
  std::vector<ast::Variant> ast{};
  ast.reserve(100);

  const auto order = GetTraversalOrder();
  const std::size_t value_width = GetPrintWidth(order.size());

  AstBuilder builder{value_width, func.input_args};

  // first put a block w/ all the temporary values:
  for (const auto& value : order) {
    auto it = operations_.find(value);
    ASSERT(it != operations_.end());
    ast::VariantPtr rhs = builder.VisitMakePtr(it->second);
    std::string name = fmt::format("v{:0>{}}", value.Id(), value_width);
    ast.emplace_back(ast::Declaration{std::move(name), ast::ScalarType(), std::move(rhs)});
  }

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

    //    std::vector<ast::VariableRef> variables;
    //    variables.reserve(dim);
    //    for (std::size_t i = 0; i < dim; ++i) {
    //      ASSERT_LESS(i + output_flat_index, output_values_.size());
    //      variables.push_back(ast::VariableRef{
    //          fmt::format("v{:0>{}}", output_values_[i + output_flat_index].Id(),
    //          value_width)});
    //    }

    output_flat_index += dim;
  }
  ast.emplace_back(ast::ReturnValue(std::move(return_values)));
  return ast;
}

void IrBuilder::PropagateCopiedOperands(ir::Operation& operation) const {
  std::visit(
      [this](auto& op) {
        // Iterate over all operands, and extract arguments that are loads.
        for (ir::Operand& operand : op.args) {
          if (!std::holds_alternative<ir::Value>(operand)) {
            continue;
          }
          const ir::Value arg_value = std::get<ir::Value>(operand);
          auto it = operations_.find(arg_value);
          ASSERT(it != operations_.end(), "Missing operation for value: {}", arg_value.Id());

          // This argument is a load, so reference its argument directly.
          if (std::holds_alternative<ir::Load>(it->second)) {
            operand = std::get<ir::Load>(it->second).args[0];
          }
        }
        // Make sure arguments remain in canonical order
        using OperationType = std::decay_t<decltype(op)>;
        if constexpr (OperationType::IsCommutative()) {
          std::sort(op.args.begin(), op.args.end(), ir::OperandOrderBool{});
        }
      },
      operation);
}

void IrBuilder::StripUnusedValues() {
  std::unordered_set<ir::Value, ir::ValueHash> used_values;
  used_values.reserve(operations_.size());

  // Output values are always used, so put them in the used set.
  // First, we should eliminate any output operations that are just copies:
  for (ir::Value& output_val : output_values_) {
    auto it = operations_.find(output_val);
    if (std::holds_alternative<ir::Load>(it->second)) {
      const ir::Load& load = std::get<ir::Load>(it->second);
      if (std::holds_alternative<ir::Value>(load.args[0])) {
        output_val = std::get<ir::Value>(load.args[0]);
      }
    }
    used_values.insert(output_val);
  }

  for (const auto& pair : operations_) {
    VisitValueArgs(pair.second, [&](const ir::Value& val) { used_values.insert(val); });
  }

  // Nuke anything not in the used values
  for (auto it = operations_.begin(); it != operations_.end();) {
    if (used_values.count(it->first)) {
      ++it;
    } else {
      it = operations_.erase(it);
    }
  }
}

// std::string CodeGeneratorBase::Generate(const ast::FunctionSignature& func,
//                                         const std::vector<ast::Variant>& ast) {
//   CodeFormatter formatter{};
//   GenerateImpl(formatter, func, ast);
//   return formatter.GetOutput();
// }

}  // namespace math
