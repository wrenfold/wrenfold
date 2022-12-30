#include "plain_formatter.h"

#include <optional>
#include <type_traits>
#include <vector>

#include <fmt/format.h>

#include "assertions.h"
#include "common_visitors.h"
#include "constant_expressions.h"
#include "operation_types.h"
#include "power.h"
#include "unary_operations.h"
#include "variable.h"

namespace math {

struct HasNegativeSign {
  using ReturnType = bool;
  constexpr static VisitorPolicy Policy = VisitorPolicy::NoError;

  constexpr bool Apply(const Negation&) const { return true; }
  bool Apply(const Integer& num) const { return num.GetValue() < 0; }
  bool Apply(const Float& f) const { return f.GetValue() < 0; }
};

void PlainFormatter::Apply(const Addition& expr) {
  ASSERT_GREATER_OR_EQ(expr.Arity(), 2);
  // Format the first arg:
  expr[0].Receive(*this);
  for (std::size_t i = 1; i < expr.Arity(); ++i) {
    const std::optional<const Expr*> negation_inner =
        VisitLambda(expr[i], [](const Negation& n) { return &n.Inner(); });
    if (negation_inner) {
      // Format subtractions in a pretty way.
      output_ += " - ";
      (*negation_inner)->Receive(*this);
    } else {
      output_ += " + ";
      expr[i].Receive(*this);
    }
  }
}

void PlainFormatter::Apply(const Constant& expr) {
  output_ += StringFromSymbolicConstant(expr.GetName());
}

void PlainFormatter::Apply(const Integer& expr) {
  fmt::format_to(std::back_inserter(output_), "{}", expr.GetValue());
}

void PlainFormatter::Apply(const Float& expr) {
  fmt::format_to(std::back_inserter(output_), "{}", expr.GetValue());
}

void PlainFormatter::Apply(const Multiplication& expr) {
  ASSERT_GREATER_OR_EQ(expr.Arity(), 2);

  // Break terms into positive and negative exponents:
  std::vector<std::pair<Expr, Expr>> positive_exp;
  std::vector<std::pair<Expr, Expr>> negative_exp;
  positive_exp.reserve(expr.Arity());
  negative_exp.reserve(expr.Arity());

  for (std::size_t i = 0; i < expr.Arity(); ++i) {
    // Pull the base and exponent:
    auto pair = AsBaseAndExponent(expr[i]);

    // Check if exponent is positive or negative:
    if (VisitStruct(pair.second, HasNegativeSign{}).value_or(false)) {
      // Flip the sign:
      pair.second = Negation::Create(std::move(pair.second));
      negative_exp.push_back(std::move(pair));
    } else {
      positive_exp.push_back(std::move(pair));
    }
  }

  const auto FormatChildren = [this](const std::vector<std::pair<Expr, Expr>>& children,
                                     bool denominator) {
    for (std::size_t i = 0; i + 1 < children.size(); ++i) {
      FormatBaseExponentPair(children[i], denominator);
      output_ += " * ";
    }
    FormatBaseExponentPair(children.back(), denominator);
  };

  // Do numerator, then denominator:
  FormatChildren(positive_exp, false);
  if (!negative_exp.empty()) {
    output_ += " / ";
    if (negative_exp.size() > 1) {
      output_ += "(";
    }
    FormatChildren(negative_exp, true);
    if (negative_exp.size() > 1) {
      output_ += ")";
    }
  }
}

void PlainFormatter::Apply(const NaturalLog& expr) {
  output_ += "ln(";
  expr.Inner().Receive(*this);
  output_ += ")";
}

void PlainFormatter::Apply(const Negation& expr) {
  output_ += "-";
  VisitWithBrackets(expr.Inner());
}

enum class Precedence : int {
  Addition = 0,
  Multiplication,
  Power,
  None = std::numeric_limits<int>::max(),
};

struct PrecedenceVisitor {
  static constexpr VisitorPolicy Policy = VisitorPolicy::NoError;
  using ReturnType = Precedence;

  constexpr Precedence Apply(const Multiplication&) const { return Precedence::Multiplication; }
  constexpr Precedence Apply(const Addition&) const { return Precedence::Addition; }
  constexpr Precedence Apply(const Power&) const { return Precedence::Power; }
};

inline Precedence GetPrecedence(const Expr& expr) {
  return VisitStruct(expr, PrecedenceVisitor{}).value_or(Precedence::None);
}

void PlainFormatter::Apply(const Power& expr) {
  if (GetPrecedence(expr.Base()) <= Precedence::Power) {
    output_ += "(";
    expr.Base().Receive(*this);
    output_ += ")";
  } else {
    expr.Base().Receive(*this);
  }
  output_ += " ^ ";
  if (GetPrecedence(expr.Exponent()) <= Precedence::Power) {
    output_ += "(";
    expr.Exponent().Receive(*this);
    output_ += ")";
  } else {
    expr.Exponent().Receive(*this);
  }
}

void PlainFormatter::Apply(const Variable& expr) { output_ += expr.GetName(); }

void PlainFormatter::VisitWithBrackets(const Expr& expr) {
  if (IsNAryOp(expr)) {
    output_ += "(";
    expr.Receive(*this);
    output_ += ")";
  } else {
    expr.Receive(*this);
  }
}

// TODO: Clean this up a bit:
void PlainFormatter::FormatBaseExponentPair(const std::pair<Expr, Expr>& pair, bool denominator) {
  if (IsOne(pair.second)) {
    const bool apply_brackets = denominator ? IsNAryOp(pair.first) : IsType<Addition>(pair.first);

    if (apply_brackets) {
      output_ += "(";
      pair.first.Receive(*this);
      output_ += ")";
    } else {
      pair.first.Receive(*this);
    }
    return;
  }
  if (IsNAryOp(pair.first)) {
    output_ += "(";
    pair.first.Receive(*this);
    output_ += ")";
  } else {
    pair.first.Receive(*this);
  }
  output_ += " ^ ";
  if (IsNAryOp(pair.first)) {
    output_ += "(";
    pair.second.Receive(*this);
    output_ += ")";
  } else {
    pair.second.Receive(*this);
  }
}

}  // namespace math
