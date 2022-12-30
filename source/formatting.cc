#include "formatting.h"

#include <array>
#include <iostream>
#include <optional>
#include <type_traits>
#include <vector>

#include <fmt/format.h>

#include "assertions.h"
#include "binary_operations.h"
#include "constant_expressions.h"
#include "unary_operations.h"
#include "variable.h"

namespace math {

// Simple visitor that evaluates to true for n-ary operations.
struct IsNaryOpVisitor {
  using ReturnType = bool;
  constexpr static VisitorPolicy Policy = VisitorPolicy::NoError;

  template <typename Derived>
  constexpr ReturnType Apply(const NAryOp<Derived>&) const {
    return true;
  }
};

struct HasNegativeSign {
  using ReturnType = bool;
  constexpr static VisitorPolicy Policy = VisitorPolicy::NoError;

  constexpr bool Apply(const Negation&) const { return true; }

  bool Apply(const Number& num) const { return num.GetValue() < 0; }
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

void PlainFormatter::Apply(const Division&) {}

template <typename T, typename... Ts>
constexpr bool ContainsTypeHelper = std::disjunction_v<std::is_same<T, Ts>...>;

template <typename... Ts>
struct TypeList {};

template <typename T, typename U>
constexpr bool ContainsType = false;

template <typename T, typename... Ts>
constexpr bool ContainsType<T, TypeList<Ts...>> = ContainsTypeHelper<T, Ts...>;

struct AsBaseExponent {
  using ReturnType = std::pair<Expr, Expr>;
  constexpr static VisitorPolicy Policy = VisitorPolicy::CompileError;

  AsBaseExponent(const Expr& input) : input_(input) {}

  // Power is broken into base and exponent:
  ReturnType Apply(const Power& op) const { return std::make_pair(op.Base(), op.Exponent()); }

  template <typename Derived>
  ReturnType Apply(const NAryOp<Derived>&) {
    return std::make_pair(input_, Constants::One);
  }

  using IgnoredTypes = TypeList<Division, NaturalLog, Negation, Number, Variable, Constant>;

  // All the ignored types are dispatched through this method.
  template <typename T>
  std::enable_if_t<ContainsType<T, IgnoredTypes>, ReturnType> Apply(const T&) {
    return std::make_pair(input_, Constants::One);
  }

 private:
  Expr input_;
};

void PlainFormatter::Apply(const Multiplication& expr) {
  ASSERT_GREATER_OR_EQ(expr.Arity(), 2);

  // Break terms into positive and negative exponents:
  std::vector<std::pair<Expr, Expr>> positive_exp;
  std::vector<std::pair<Expr, Expr>> negative_exp;
  positive_exp.reserve(expr.Arity());
  negative_exp.reserve(expr.Arity());

  for (std::size_t i = 0; i < expr.Arity(); ++i) {
    // Pull the base and exponent:
    AsBaseExponent visitor{expr[i]};
    auto pair = VisitStruct(expr[i], visitor).value();

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

void PlainFormatter::Apply(const Number& expr) {
  fmt::format_to(std::back_inserter(output_), "{}", expr.GetValue());
}

void PlainFormatter::Apply(const Power& expr) {
  output_ += "pow(";
  expr.Base().Receive(*this);
  output_ += ", ";
  expr.Exponent().Receive(*this);
  output_ += ")";
}

void PlainFormatter::Apply(const Variable& expr) { output_ += expr.GetName(); }

void PlainFormatter::VisitWithBrackets(const Expr& expr) {
  if (VisitStruct(expr, IsNaryOpVisitor{}).value_or(false)) {
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
    const bool apply_brackets = denominator
                                    ? VisitStruct(pair.first, IsNaryOpVisitor{}).value_or(false)
                                    : IsType<Addition>(pair.first);

    if (apply_brackets) {
      output_ += "(";
      pair.first.Receive(*this);
      output_ += ")";
    } else {
      pair.first.Receive(*this);
    }
    return;
  }
  if (VisitStruct(pair.first, IsNaryOpVisitor{}).value_or(false)) {
    output_ += "(";
    pair.first.Receive(*this);
    output_ += ")";
  } else {
    pair.first.Receive(*this);
  }
  output_ += " ^ ";
  if (VisitStruct(pair.second, IsNaryOpVisitor{}).value_or(false)) {
    output_ += "(";
    pair.second.Receive(*this);
    output_ += ")";
  } else {
    pair.second.Receive(*this);
  }
}

struct TreeFormatter {
  using ReturnType = void;

  // Generate a compile-time error if we forget a visitor here.
  constexpr static VisitorPolicy Policy = VisitorPolicy::CompileError;

  // Add indentation to the output string.
  void ApplyIndentation() {
    if (indentations_.empty()) {
      return;
    }
    // For each left branch depth, we need to add a line.
    // Right branches only need space.
    for (std::size_t i = 0; i + 1 < indentations_.size(); ++i) {
      if (indentations_[i]) {
        output_ += "│  ";
      } else {
        output_ += "   ";
      }
    }
    if (indentations_.back()) {
      output_ += "├─ ";
    } else {
      // Final right branch is the end of this tree.
      output_ += "└─ ";
    }
  }

  template <typename... Args>
  void AppendName(const char* fmt_str, Args&&... args) {
    ApplyIndentation();
    fmt::format_to(std::back_inserter(output_), fmt_str, std::forward<Args>(args)...);
    output_ += "\n";
  }

  void VisitLeft(const Expr& expr) {
    indentations_.push_back(true);
    VisitStruct(expr, *this);
    indentations_.pop_back();
  }

  void VisitRight(const Expr& expr) {
    indentations_.push_back(false);
    VisitStruct(expr, *this);
    indentations_.pop_back();
  }

  template <typename Derived>
  void Apply(const NAryOp<Derived>& op) {
    AppendName("{}:", op.Name());
    for (std::size_t i = 0; i + 1 < op.Arity(); ++i) {
      VisitLeft(op[i]);
    }
    VisitRight(op[op.Arity() - 1]);
  }

  void Apply(const Division& op) {
    AppendName("Division:");
    VisitLeft(op.Numerator());
    VisitRight(op.Denominator());
  }

  void Apply(const Power& op) {
    AppendName("Power:");
    VisitLeft(op.Base());
    VisitRight(op.Exponent());
  }

  void Apply(const NaturalLog& log) {
    AppendName("NaturalLog:");
    VisitRight(log.Inner());
  }

  void Apply(const Negation& neg) {
    AppendName("Negation:");
    VisitRight(neg.Inner());
  }

  void Apply(const Number& neg) { AppendName("Number ({})", neg.GetValue()); }

  void Apply(const Variable& var) { AppendName("Variable ({})", var.GetName()); }

  void Apply(const Constant& constant) {
    AppendName("Constant ({})", StringFromSymbolicConstant(constant.GetName()));
  }

  // Get the output string via move.
  void TakeOutput(std::string& output) { output = std::move(output_); }

 private:
  // The indentation pattern at our current tree depth.
  // True indicates a left branch, false indicates a right branch.
  std::vector<unsigned char> indentations_;
  // The final output
  std::string output_;
};

static void RightTrimInPlace(std::string& str) {
  while (!str.empty() && std::isspace(str.back())) {
    str.pop_back();
  }
}

std::string FormatDebugTree(const Expr& expr) {
  TreeFormatter formatter{};
  VisitStruct(expr, formatter);
  std::string output;
  formatter.TakeOutput(output);
  // Somewhat hacky. The formatter appends a superfluous newline on the last element. I think
  // this is tricky to avoid w/o knowing the tree depth apriori. Instead, just trim it from the
  // end.
  RightTrimInPlace(output);
  return output;
}

}  // namespace math
