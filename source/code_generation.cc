// Copyright 2023 Gareth Cross
#include "code_generation.h"

#include <unordered_map>
#include <vector>

#include "expressions/all_expressions.h"
#include "hashing.h"
#include "visitor_impl.h"

namespace math {
namespace ssa {

// Visitor for converting an expression tree into static-single-assignment form.
struct SSAFormVisitor {
  constexpr static VisitorPolicy Policy = VisitorPolicy::CompileError;
  using ReturnType = std::size_t;

  // Construct with an output array that we write into.
  explicit SSAFormVisitor(std::unordered_map<std::size_t, ssa::OperationVariant>& output)
      : operations_(output) {
    if (operations_.empty()) {
      insertion_point_ = 0;
    } else {
      insertion_point_ =
          std::max_element(operations_.begin(), operations_.end(),
                           [&](const auto& a, const auto& b) { return a.first < b.first; })
              ->first +
          1;
    }
  }

  template <typename T, typename... Args>
  std::size_t PushOperation(Args&&... args) {
    T operation{std::forward<Args>(args)...};
    insertion_point_++;
    operations_.emplace(insertion_point_, std::move(operation));
    return insertion_point_;
  }

  template <typename Derived>
  std::size_t Apply(const NAryOp<Derived>& op) {
    static_assert(std::is_same_v<Derived, Addition> || std::is_same_v<Multiplication, Derived>);

    // first recursively transform all the inputs
    std::vector<std::size_t> args;
    args.reserve(op.Arity());
    std::transform(op.begin(), op.end(), std::back_inserter(args),
                   [this](const Expr& expr) { return VisitStruct(expr, *this); });
    ASSERT(!args.empty());

    // now iterate and build new assignments:
    std::size_t prev_result = args[0];
    for (std::size_t i = 1; i < args.size(); ++i) {
      if constexpr (std::is_same_v<Derived, Multiplication>) {
        prev_result = PushOperation<Mul>(prev_result, args[i]);
      } else {
        prev_result = PushOperation<Add>(prev_result, args[i]);
      }
    }
    return prev_result;
  }

  std::size_t Apply(const Constant& constant) {
    return PushOperation<Load>(MakeExpr<Constant>(constant));
  }

  std::size_t Apply(const Matrix&) { throw TypeError("Cannot evaluate this on a matrix."); }

  std::size_t Apply(const UnaryFunction& func) {
    const std::size_t arg = VisitStruct(func.Arg(), *this);
    return PushOperation<CallUnaryFunc>(func.Func(), arg);
  }

  std::size_t Apply(const Integer& integer) {
    return PushOperation<Load>(Integer::Create(integer));
  }

  std::size_t Apply(const Float& f) { return PushOperation<Load>(Float::Create(f)); }

  std::size_t Apply(const Power& pow) {
    const std::size_t base = VisitStruct(pow.Base(), *this);
    const std::size_t exponent = VisitStruct(pow.Exponent(), *this);
    return PushOperation<Pow>(base, exponent);
  }

  std::size_t Apply(const Rational& rational) {
    return PushOperation<Load>(Rational::Create(rational));
  }

  std::size_t Apply(const Variable& var) { return PushOperation<Load>(MakeExpr<Variable>(var)); }

 private:
  std::unordered_map<std::size_t, ssa::OperationVariant>& operations_;
  std::size_t insertion_point_;
};

inline void FormatOperand(std::string& output, const Operand& operand, const std::size_t width) {
  std::visit(
      [&](const auto& operand) {
        using T = std::decay_t<decltype(operand)>;
        if constexpr (std::is_same_v<T, std::size_t>) {
          fmt::format_to(std::back_inserter(output), "v{:0>{}}", operand, width);
        } else {
          // input value of type `Expr`
          output += operand.ToString();
        }
      },
      operand);
}

// Alignment of operations when printed:
constexpr int OperationWidth = 4;

template <typename Op>
inline void FormatOpWithArgs(std::string& output, const Op& op, const std::size_t width) {
  fmt::format_to(std::back_inserter(output), "{:>{}} ", op.ToString(), OperationWidth);
  for (auto it = op.args.begin(); it != op.args.end(); ++it) {
    FormatOperand(output, *it, width);
    if (std::next(it) != op.args.end()) {
      output += ", ";
    }
  }
}

inline void FormatOp(std::string& output, const Add& add, const std::size_t width) {
  FormatOpWithArgs(output, add, width);
}

inline void FormatOp(std::string& output, const Mul& mul, const std::size_t width) {
  FormatOpWithArgs(output, mul, width);
}

inline void FormatOp(std::string& output, const Pow& pow, const std::size_t width) {
  FormatOpWithArgs(output, pow, width);
}

inline void FormatOp(std::string& output, const Load& load, const std::size_t width) {
  FormatOpWithArgs(output, load, width);
}

inline void FormatOp(std::string& output, const CallUnaryFunc& f, const std::size_t width) {
  FormatOpWithArgs(output, f, width);
}

template <typename Container>
inline std::vector<Expr> ExtractArgExpressions(const Container& args,
                                               const std::vector<Expr>& expressions) {
  std::vector<Expr> inputs{};
  inputs.reserve(args.size());
  std::transform(args.begin(), args.end(), std::back_inserter(inputs), [&](const Operand& operand) {
    return std::visit(
        [&](const auto& x) -> Expr {
          using T = std::decay_t<decltype(x)>;
          if constexpr (std::is_same_v<T, std::size_t>) {
            ASSERT_LESS(x, expressions.size());
            return expressions[x];
          } else {
            return x;
          }
        },
        operand);
  });
  return inputs;
}
//
//
// inline Expr CreateExpression(const OperationVariant& operation,
//                             const std::vector<Expr>& expressions) {
//  return std::visit(
//      [&](const auto& op_type) { return CreateExpressionForOp(op_type, expressions); },
//      operation);
//}

inline std::size_t HashOperand(const Operand& operand) {
  return std::visit(
      [](const auto& operand) -> std::size_t {
        using T = std::decay_t<decltype(operand)>;
        if constexpr (std::is_same_v<T, std::size_t>) {
          return std::hash<std::size_t>{}(operand);
        } else {
          return HashExpression(operand);
        }
      },
      operand);
}

inline bool OperandsAreEqual(const Operand& a, const Operand& b) {
  return std::visit(
      [](const auto& a, const auto& b) -> bool {
        using A = std::decay_t<decltype(a)>;
        using B = std::decay_t<decltype(b)>;
        if constexpr (std::is_same_v<A, B>) {
          if constexpr (std::is_same_v<A, std::size_t>) {
            return a == b;
          } else {
            return a.IsIdenticalTo(b);  //  Expr
          }
        } else {
          return false;
        }
      },
      a, b);
}

template <typename T>
struct TypeListFromVariant;

template <typename... Ts>
struct TypeListFromVariant<std::variant<Ts...>> {
  using List = TypeList<Ts...>;
};

template <typename T>
std::size_t HashOpWithArgs(const T& op) {
  std::size_t seed = IndexOfType<T, TypeListFromVariant<OperationVariant>::List>::Value;
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
  std::size_t operator()(const OperationVariant& operation) const {
    return std::visit([](const auto& op) -> std::size_t { return HashOp(op); }, operation);
  }
};

struct OperationEquality {
  bool operator()(const OperationVariant& a, const OperationVariant& b) const {
    if (a.index() != b.index()) {
      return false;
    }
    return std::visit(
        [&](const auto& a, const auto& b) -> bool {
          using A = std::decay_t<decltype(a)>;
          using B = std::decay_t<decltype(b)>;
          if constexpr (std::is_same_v<A, B>) {
            // a and b are the same type, so we can compare them:
            const bool args_equal =
                std::equal(a.args.begin(), a.args.end(), b.args.begin(), &OperandsAreEqual);
            if (!args_equal) {
              return false;
            }
            // TODO: temporary hack
            if constexpr (std::is_same_v<A, CallUnaryFunc>) {
              return a.name == b.name;
            }
            return true;
          } else {
            return false;
          }
        },
        a, b);
  }
};

}  // namespace ssa

std::size_t CreateSSA(const Expr& expression,
                      std::unordered_map<std::size_t, ssa::OperationVariant>& output) {
  return VisitStruct(expression, ssa::SSAFormVisitor{output});
}

inline constexpr std::size_t GetPrintWidth(std::size_t num_assignments) {
  std::size_t width = 1;
  for (; num_assignments > 0; ++width) {
    num_assignments /= 10;
  }
  return width;
}
//
// std::string FormatSSA(const std::vector<ssa::OperationVariant>& assignments) {
//  // # of assignments determines # of digits in the output (align everything in columns).
//  const std::size_t width = GetPrintWidth(assignments.size());
//  std::string output{};
//  for (std::size_t i = 0; i < assignments.size(); ++i) {
//    fmt::format_to(std::back_inserter(output), "v{:0>{}} <- ", i, width);
//    std::visit([&](const auto& op) { ssa::FormatOp(output, op, width); }, assignments[i]);
//    output += "\n";
//  }
//  output.pop_back();  //  remove final newline
//  return output;
//}

template <typename Container>
inline std::vector<Expr> ExtractArgExpressions2(
    const Container& args,
    const std::unordered_map<std::size_t, ssa::OperationVariant>& expressions,
    std::unordered_map<std::size_t, Expr>& output_map) {
  std::vector<Expr> inputs{};
  inputs.reserve(args.size());
  std::transform(args.begin(), args.end(), std::back_inserter(inputs),
                 [&](const ssa::Operand& operand) {
                   return std::visit(
                       [&](const auto& x) -> Expr {
                         using T = std::decay_t<decltype(x)>;
                         if constexpr (std::is_same_v<T, std::size_t>) {
                           auto it = output_map.find(x);
                           if (it == output_map.end()) {
                             Expr expression = ExpressionFromSSA(expressions, x, output_map);
                             it = output_map.emplace(x, std::move(expression)).first;
                           }
                           return it->second;
                         } else {
                           return x;
                         }
                       },
                       operand);
                 });
  return inputs;
}
inline Expr CreateExpressionForOp(
    const ssa::Add& add, const std::unordered_map<std::size_t, ssa::OperationVariant>& expressions,
    std::unordered_map<std::size_t, Expr>& output_map) {
  return Addition::FromOperands(ExtractArgExpressions2(add.args, expressions, output_map));
}

inline Expr CreateExpressionForOp(
    const ssa::Mul& mul, const std::unordered_map<std::size_t, ssa::OperationVariant>& expressions,
    std::unordered_map<std::size_t, Expr>& output_map) {
  return Multiplication::FromOperands(ExtractArgExpressions2(mul.args, expressions, output_map));
}

inline Expr CreateExpressionForOp(
    const ssa::Pow& pow, const std::unordered_map<std::size_t, ssa::OperationVariant>& expressions,
    std::unordered_map<std::size_t, Expr>& output_map) {
  std::vector<Expr> inputs = ExtractArgExpressions2(pow.args, expressions, output_map);
  ASSERT_EQUAL(2, inputs.size());
  return Power::Create(inputs[0], inputs[1]);
}

inline Expr CreateExpressionForOp(
    const ssa::CallUnaryFunc& func,
    const std::unordered_map<std::size_t, ssa::OperationVariant>& expressions,
    std::unordered_map<std::size_t, Expr>& output_map) {
  std::vector<Expr> inputs = ExtractArgExpressions2(func.args, expressions, output_map);
  ASSERT_EQUAL(1, inputs.size());
  return CreateUnaryFunction(func.name, inputs.front());
}

inline Expr CreateExpressionForOp(
    const ssa::Load& load,
    const std::unordered_map<std::size_t, ssa::OperationVariant>& expressions,
    std::unordered_map<std::size_t, Expr>& output_map) {
  std::vector<Expr> inputs = ExtractArgExpressions2(load.args, expressions, output_map);
  ASSERT_EQUAL(1, inputs.size());
  return inputs.front();
}

Expr ExpressionFromSSA(const std::unordered_map<std::size_t, ssa::OperationVariant>& operations,
                       const std::size_t value_index,
                       std::unordered_map<std::size_t, Expr>& output_map) {
  auto it = operations.find(value_index);
  ASSERT(it != operations.end(), "Missing expression for value: {}", value_index);
  return std::visit(
      [&](const auto& op) { return CreateExpressionForOp(op, operations, output_map); },
      it->second);
}

Expr ExpressionFromSSA(const std::unordered_map<std::size_t, ssa::OperationVariant>& operations,
                       const std::size_t value_index) {
  std::unordered_map<std::size_t, Expr> output_map{};
  output_map.reserve(operations.size());
  return ExpressionFromSSA(operations, value_index, output_map);
}

std::string FormatSSA(const std::unordered_map<std::size_t, ssa::OperationVariant>& assignments) {
  // # of assignments determines # of digits in the output (align everything in columns).

  std::vector<std::size_t> keys;
  keys.reserve(assignments.size());
  std::transform(assignments.begin(), assignments.end(), std::back_inserter(keys),
                 [&](const auto& pair) { return pair.first; });
  std::sort(keys.begin(), keys.end());

  const auto max_index = keys.back();

  const std::size_t width = GetPrintWidth(max_index);
  std::string output{};
  for (const std::size_t v : keys) {
    fmt::format_to(std::back_inserter(output), "v{:0>{}} <- ", v, width);
    std::visit([&](const auto& op) { ssa::FormatOp(output, op, width); }, assignments.at(v));
    output += "\n";
  }
  output.pop_back();  //  remove final newline
  return output;
}
//
// Expr ExpressionFromSSA(const std::vector<ssa::OperationVariant>& assignments) {
//  std::vector<Expr> expressions{};
//  expressions.reserve(assignments.size());
//  for (std::size_t i = 0; i < assignments.size(); ++i) {
//    Expr result =
//        std::visit([&](const auto& x) { return CreateExpression(x, expressions); },
//        assignments[i]);
//    expressions.push_back(std::move(result));
//  }
//  return expressions.back();
//}

ssa::OperationVariant CreateLoad(
    const std::size_t source_value,
    const std::unordered_map<std::size_t, ssa::OperationVariant>& value_to_operation) {
  const auto it = value_to_operation.find(source_value);
  ASSERT(it != value_to_operation.end(), "Missing operation for value: {}", source_value);
  if (const ssa::Load* const load = std::get_if<ssa::Load>(&it->second); load != nullptr) {
    return std::visit(
        [](const auto& operand) -> ssa::OperationVariant {
          using T = std::decay_t<decltype(operand)>;
          if constexpr (std::is_same_v<T, std::size_t>) {
            // De-reference to the original value.
            return ssa::Load(operand);
          } else {
            // Otherwise just insert the constant.
            return ssa::Load(operand);
          }
        },
        load->args.front());
  }
  return ssa::Load(source_value);
}

inline void DereferenceConstantOperands(
    ssa::OperationVariant& op,
    const std::unordered_map<std::size_t, ssa::OperationVariant>& operations) {
  std::visit(
      [&](auto& op) {
        for (ssa::Operand& operand : op.args) {
          if (!std::holds_alternative<std::size_t>(operand)) {
            continue;
          }
          auto it = operations.find(std::get<std::size_t>(operand));
          ASSERT(it != operations.end(), "Missing operation for value: {}",
                 std::get<std::size_t>(operand));
          if (std::holds_alternative<ssa::Load>(it->second)) {
            operand = std::get<ssa::Load>(it->second).args[0];
          }
        }
      },
      op);
}

void DereferenceConstants(std::unordered_map<std::size_t, ssa::OperationVariant>& operations) {
  for (auto it = operations.begin(); it != operations.end(); ++it) {
    DereferenceConstantOperands(it->second, operations);
  }
}

void EliminateDuplicates(std::unordered_map<std::size_t, ssa::OperationVariant>& operations) {
  // Hash map from operation to value
  std::unordered_map<ssa::OperationVariant, std::size_t, ssa::OperationHasher,
                     ssa::OperationEquality>
      value_for_variant;
  value_for_variant.reserve(operations.size());

  // determine traversal order:
  std::vector<std::size_t> ordered_keys;
  ordered_keys.reserve(operations.size());
  std::transform(operations.begin(), operations.end(), std::back_inserter(ordered_keys),
                 [](const auto& pair) { return pair.first; });
  std::sort(ordered_keys.begin(), ordered_keys.end());

  for (const std::size_t value : ordered_keys) {
    auto it = operations.find(value);
    ASSERT(it != operations.end());

    const auto [existing_it, was_inserted] = value_for_variant.emplace(it->second, it->first);
    if (!was_inserted) {
      // This operation already exists in the map, so replace this w/ a copy:
      it->second = ssa::Load(existing_it->second);

      // CreateLoad(existing_it->second, operations);
    }
  }
}

}  // namespace math
