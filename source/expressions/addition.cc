// Copyright 2023 Gareth Cross
#include "expressions/addition.h"

#include <algorithm>
#include <unordered_map>

#include "expressions/matrix.h"
#include "expressions/multiplication.h"
#include "expressions/numeric_expressions.h"
#include "hashing.h"

namespace math {

// TODO: This logic will need to generalize once we have symbolic matrix expressions.
// We might need a `MatrixAddition` expression.
inline Expr AddMatrixOperands(const absl::Span<const Expr>& args) {
  std::vector<const Matrix*> matrices;
  matrices.reserve(args.size());
  std::transform(args.begin(), args.end(), std::back_inserter(matrices), [](const Expr& expr) {
    if (const Matrix* const m = CastPtr<Matrix>(expr); m != nullptr) {
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

Expr Addition::FromOperands(absl::Span<const Expr> args) {
  const bool input_contains_matrix =
      std::any_of(args.begin(), args.end(), [](const Expr& arg) { return arg.Is<Matrix>(); });
  if (input_contains_matrix) {
    return AddMatrixOperands(args);
  }
  AdditionParts parts{args.size()};
  for (const Expr& arg : args) {
    parts.Add(arg);
  }
  parts.Normalize();
  return parts.CreateAddition();
}

struct AdditionVisitor {
  using ReturnType = void;

  explicit AdditionVisitor(AdditionParts& parts) : parts(parts) {}

  template <typename T>
  void operator()(const T& arg, const Expr& input_expression) {
    if constexpr (std::is_same_v<T, Addition>) {
      for (const Expr& expr : arg) {
        // Recursively add additions:
        Visit(expr, [this, &expr](const auto& x) { this->operator()(x, expr); });
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
                      T::NameStr);
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
  AdditionVisitor visitor{*this};
  Visit(arg, [&visitor, &arg](const auto& x) { visitor(x, arg); });
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

Expr AdditionParts::CreateAddition() const {
  Addition::ContainerType args{};
  if (float_term.has_value()) {
    const Float promoted_rational = static_cast<Float>(rational_term);
    args.push_back(MakeExpr<Float>(float_term.value() + promoted_rational));
  } else if (rational_term.IsZero()) {
    // Don't insert a useless zero in the add.
  } else {
    args.push_back(Rational::Create(rational_term));
  }

  args.reserve(args.size() + terms.size());
  std::transform(
      terms.begin(), terms.end(), std::back_inserter(args), [](const std::pair<Expr, Expr>& pair) {
        if (IsOne(pair.second)) {
          return pair.first;
        } else if (!pair.first.Is<Multiplication>() && !pair.second.Is<Multiplication>()) {
          // We can skip calling FromOperands here because we know the first element in
          // the pair is the non-numeric value and the second is the numeric coefficient.
          return MakeExpr<Multiplication>(pair.second, pair.first);
        }
        return Multiplication::FromOperands({pair.first, pair.second});
      });

  if (args.empty()) {
    return Constants::Zero;
  } else if (args.size() == 1) {
    return args.front();
  }

  return MakeExpr<Addition>(std::move(args));
}

}  // namespace math
