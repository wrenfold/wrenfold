// Copyright 2022 Gareth Cross
#include "expressions/addition.h"

#include <unordered_map>

#include "expressions/multiplication.h"
#include "expressions/numeric_expressions.h"
#include "hashing.h"
#include "ordering.h"

namespace math {

Addition::Addition(std::vector<Expr> args) : NAryOp(std::move(args)) {}

// TODO: This logic will need to generalize once we have symbolic matrix expressions.
// We might need a `MatrixAddition` expression.
inline Expr FromMatrixOperands(const std::vector<Expr>& args) {
  std::vector<const Matrix*> matrices;
  matrices.reserve(args.size());
  std::transform(args.begin(), args.end(), std::back_inserter(matrices), [](const Expr& expr) {
    if (const Matrix* const m = TryCast<Matrix>(expr); m != nullptr) {
      return m;
    } else {
      throw TypeError("Cannot add scalar expression of type {} to matrix. Expression contents: {}",
                      expr.TypeName(), expr.ToString());
    }
  });
  ASSERT(!matrices.empty(), "Need at least one matrix");
  // Recursively add up all the arguments.
  Matrix output = *matrices.front();
  for (std::size_t i = 1; i < matrices.size(); ++i) {
    output = output + *matrices[i];
  }
  return MakeExpr<Matrix>(std::move(output));
}

Expr Addition::FromOperands(const std::vector<Expr>& args) {
  const bool input_contains_matrix = std::any_of(args.begin(), args.end(), &TryCast<Matrix>);
  if (input_contains_matrix) {
    return FromMatrixOperands(args);
  }
  AdditionParts parts{args.size()};
  for (const Expr& arg : args) {
    parts.Add(arg);
  }
  parts.Normalize();
  return parts.CreateAddition(std::vector<Expr>{});
}

struct AdditionVisitor {
  constexpr static VisitorPolicy Policy = VisitorPolicy::CompileError;
  using ReturnType = void;

  explicit AdditionVisitor(AdditionParts& parts) : parts(parts) {}

  template <typename T>
  void Apply(const Expr& input_expression, const T& arg) {
    if constexpr (std::is_same_v<T, Addition>) {
      for (const Expr& expr : arg) {
        // Recursively add additions:
        VisitStruct(expr, *this);
      }
    } else if constexpr (std::is_same_v<T, Integer>) {
      parts.rational_term = parts.rational_term + static_cast<Rational>(arg);
    } else if constexpr (std::is_same_v<T, Rational>) {
      parts.rational_term = parts.rational_term + arg;
    } else if constexpr (std::is_same_v<T, Float>) {
      if (!parts.float_term.has_value()) {
        parts.float_term = arg;
      } else {
        parts.float_term = (*parts.float_term) + arg;
      }
    } else if constexpr (std::is_same_v<T, Matrix>) {
      throw TypeError("Cannot add a matrix into a scalar addition expression. Arg type = {}",
                      arg.TypeName());
    } else {
      // Everything else: Just add to the coeff
      auto [coeff, mul] = AsCoefficientAndMultiplicand(input_expression);
      const auto [it, was_inserted] = parts.terms.emplace(std::move(mul), coeff);
      if (!was_inserted) {
        it->second = it->second + coeff;
      }
    }
  }

  AdditionParts& parts;
};

AdditionParts::AdditionParts(const Addition& add) : AdditionParts(add.Arity()) {
  for (const Expr& expr : add) {
    Add(expr);
  }
  Normalize();
}

void AdditionParts::Add(const Expr& arg) {
  if (IsZero(arg)) {
    return;
  }
  VisitStruct(arg, AdditionVisitor{*this});
}

void AdditionParts::Normalize() {
  // Remove anything where the coefficient worked out to zero:
  for (auto it = terms.begin(); it != terms.end();) {
    if (IsZero(it->second)) {
      it = terms.erase(it);
    } else {
      ++it;
    }
  }
}

Expr AdditionParts::CreateAddition(std::vector<Expr>&& args) const {
  args.clear();
  args.reserve(terms.size() + 1);
  if (float_term.has_value()) {
    const Float promoted_rational = static_cast<Float>(rational_term);
    args.push_back(MakeExpr<Float>(float_term.value() + promoted_rational));
  } else if (rational_term.IsZero()) {
    // Don't insert a useless zero in the add.
  } else {
    args.push_back(Rational::Create(rational_term));
  }
  std::transform(terms.begin(), terms.end(), std::back_inserter(args), [](const auto& pair) {
    return Multiplication::FromTwoOperands(pair.first, pair.second);
  });
  std::sort(args.begin(), args.end(), [](const Expr& a, const Expr& b) {
    return VisitBinaryStruct(a, b, OrderVisitor{}) == OrderVisitor::RelativeOrder::LessThan;
  });
  if (args.empty()) {
    return Constants::Zero;
  } else if (args.size() == 1) {
    return args.front();
  }
  return MakeExpr<Addition>(std::move(args));
}

}  // namespace math
