// Copyright 2023 Gareth Cross
#include "code_generation.h"

#include <unordered_map>
#include <vector>

#include "expressions/all_expressions.h"
#include "visitor_impl.h"

namespace math {
namespace ssa {

// Visitor for converting an expression tree into static-single-assignment form.
struct SSAFormVisitor {
  constexpr static VisitorPolicy Policy = VisitorPolicy::CompileError;
  using ReturnType = std::size_t;

  // Construct with an output array that we write into.
  explicit SSAFormVisitor(std::vector<AssignmentVariant>& output) : assignments_(output) {}

  template <typename T, typename... OpFields>
  std::size_t PushOperation(std::vector<std::size_t>&& args, OpFields&&... op_fields) {
    const std::size_t index = assignments_.size();
    T operation{std::forward<OpFields>(op_fields)...};
    Operation op;
    op.args = std::move(args);
    if (operation.IsCommutative()) {
      std::sort(op.args.begin(), op.args.end());
    }
    op.op_type = std::move(operation);
    assignments_.emplace_back(std::move(op));
    return index;
  }

  std::size_t PushInputValue(Expr&& expr) {
    const std::size_t index = assignments_.size();
    assignments_.emplace_back(InputValue{std::move(expr)});
    return index;
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
        prev_result = PushOperation<Mul>({prev_result, args[i]});
      } else {
        prev_result = PushOperation<Add>({prev_result, args[i]});
      }
    }
    return prev_result;
  }

  std::size_t Apply(const Constant& constant) {
    return PushInputValue(MakeExpr<Constant>(constant));
  }

  std::size_t Apply(const Matrix&) { throw TypeError("Cannot evaluate this on a matrix."); }

  std::size_t Apply(const UnaryFunction& func) {
    const std::size_t arg = VisitStruct(func.Arg(), *this);
    return PushOperation<CallUnaryFunc>({arg}, func.Func());
  }

  std::size_t Apply(const Integer& integer) { return PushInputValue(Integer::Create(integer)); }

  std::size_t Apply(const Float& f) { return PushInputValue(Float::Create(f)); }

  std::size_t Apply(const Power& pow) {
    const std::size_t base = VisitStruct(pow.Base(), *this);
    const std::size_t exponent = VisitStruct(pow.Exponent(), *this);
    return PushOperation<Pow>({base, exponent});
  }

  std::size_t Apply(const Rational& rational) { return PushInputValue(Rational::Create(rational)); }

  std::size_t Apply(const Variable& var) { return PushInputValue(MakeExpr<Variable>(var)); }

 private:
  std::vector<AssignmentVariant>& assignments_;
};

inline std::string ToString(const Operation& operation, const std::size_t width) {
  const std::string_view prefix =
      std::visit([](const auto& op_type) { return op_type.ToString(); }, operation.op_type);
  std::string result{prefix};
  auto it = operation.args.begin();
  if (it == operation.args.end()) {
    return result;
  }
  fmt::format_to(std::back_inserter(result), " v{:0>{}}", *it, width);
  for (++it; it != operation.args.end(); ++it) {
    fmt::format_to(std::back_inserter(result), ", v{:0>{}}", *it, width);
  }
  return result;
}

inline std::string ToString(const AssignmentVariant& variant, const std::size_t width) {
  return std::visit(
      [&width](const auto& x) {
        using T = std::decay_t<decltype(x)>;
        if constexpr (std::is_same_v<T, InputValue>) {
          return x.value.ToString();
        } else {
          return ToString(x, width);
        }
      },
      variant);
}

inline std::vector<Expr> ExtractArgExpressions(const std::vector<std::size_t>& args,
                                               const std::vector<Expr>& expressions) {
  std::vector<Expr> inputs{};
  inputs.reserve(args.size());
  std::transform(args.begin(), args.end(), std::back_inserter(inputs), [&](std::size_t i) {
    ASSERT_LESS(i, expressions.size());
    return expressions[i];
  });
  return inputs;
}

inline Expr CreateExpressionForOp(const Add&, const std::vector<std::size_t>& args,
                                  const std::vector<Expr>& expressions) {
  return Addition::FromOperands(ExtractArgExpressions(args, expressions));
}

inline Expr CreateExpressionForOp(const Mul&, const std::vector<std::size_t>& args,
                                  const std::vector<Expr>& expressions) {
  return Multiplication::FromOperands(ExtractArgExpressions(args, expressions));
}

inline Expr CreateExpressionForOp(const Pow&, const std::vector<std::size_t>& args,
                                  const std::vector<Expr>& expressions) {
  std::vector<Expr> inputs = ExtractArgExpressions(args, expressions);
  ASSERT_EQUAL(2, inputs.size());
  return Power::Create(inputs[0], inputs[1]);
}

inline Expr CreateExpressionForOp(const CallUnaryFunc& func, const std::vector<std::size_t>& args,
                                  const std::vector<Expr>& expressions) {
  std::vector<Expr> inputs = ExtractArgExpressions(args, expressions);
  ASSERT_EQUAL(1, inputs.size());
  return CreateUnaryFunction(func.name, inputs.front());
}

inline Expr CreateExpression(const Operation& operation, const std::vector<Expr>& expressions) {
  return std::visit(
      [&](const auto& op_type) {
        return CreateExpressionForOp(op_type, operation.args, expressions);
      },
      operation.op_type);
}

inline Expr CreateExpression(const InputValue& x, const std::vector<Expr>&) { return x.value; }

}  // namespace ssa

void CreateSSA(const Expr& expression, std::vector<ssa::AssignmentVariant>& output) {
  VisitStruct(expression, ssa::SSAFormVisitor{output});
}

inline constexpr std::size_t GetPrintWidth(std::size_t num_assignments) {
  std::size_t width = 1;
  for (; num_assignments > 0; ++width) {
    num_assignments /= 10;
  }
  return width;
}

std::string FormatSSA(const std::vector<ssa::AssignmentVariant>& assignments) {
  // # of assignments determines # of digits in the output (align everything in columns).
  const std::size_t width = GetPrintWidth(assignments.size());
  std::string output{};
  for (std::size_t i = 0; i < assignments.size(); ++i) {
    fmt::format_to(std::back_inserter(output), "v{:0>{}} <- {}\n", i, width,
                   ssa::ToString(assignments[i], width));
  }
  output.pop_back();  //  remove final newline
  return output;
}

Expr ExpressionFromSSA(const std::vector<ssa::AssignmentVariant>& assignments) {
  std::vector<Expr> expressions{};
  expressions.reserve(assignments.size());
  for (std::size_t i = 0; i < assignments.size(); ++i) {
    Expr result =
        std::visit([&](const auto& x) { return CreateExpression(x, expressions); }, assignments[i]);
    expressions.push_back(std::move(result));
  }
  return expressions.back();
}

std::vector<ssa::AssignmentVariant> EliminateDuplicates(
    const std::vector<ssa::AssignmentVariant>&) {
  return {};
  //
  //  std::unordered_map<  >
}

}  // namespace math
