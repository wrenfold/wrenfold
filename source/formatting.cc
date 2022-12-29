#include "formatting.h"

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

void PlainFormatter::Apply(const Addition& expr) {
  ASSERT_GREATER_OR_EQ(expr.Arity(), 2);
  // Format the first arg:
  expr[0].Receive(*this);
  for (std::size_t i = 1; i < expr.Arity(); ++i) {
    const std::optional<const Expr*> negation_inner =
        TryVisit(expr[i], [](const Negation& n) { return &n.Inner(); });
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

void PlainFormatter::Apply(const Division& expr) {
  if (Visit(expr.Numerator(), IsNaryOpVisitor{}).value_or(false)) {
    VisitWithBrackets(expr.Numerator());
  } else {
    expr.Numerator().Receive(*this);
  }
  output_ += " / ";
  if (Visit(expr.Denominator(), IsNaryOpVisitor{}).value_or(false)) {
    VisitWithBrackets(expr.Denominator());
  } else {
    expr.Denominator().Receive(*this);
  }
}

void PlainFormatter::Apply(const Multiplication& expr) {
  ASSERT_GREATER_OR_EQ(expr.Arity(), 2);

  const auto FormatChild = [this](const Expr& child) {
    // Additions are lower precedent that multiplication, so insert brackets:
    const bool lower_precedence =
        TryVisit(child, [](const Addition&) constexpr { return true; }).value_or(false);
    if (lower_precedence) {
      VisitWithBrackets(child);
    } else {
      child.Receive(*this);
    }
  };

  for (std::size_t i = 0; i + 1 < expr.Arity(); ++i) {
    FormatChild(expr[i]);
    output_ += " * ";
  }
  FormatChild(expr[expr.Arity() - 1]);
}

void PlainFormatter::Apply(const NaturalLog& expr) {
  output_ += "ln(";
  expr.Inner().Receive(*this);
  output_ += ")";
}

void PlainFormatter::Apply(const Negation& expr) {
  output_ += "-";
  if (Visit(expr.Inner(), IsNaryOpVisitor{}).value_or(false)) {
    VisitWithBrackets(expr.Inner());
  } else {
    expr.Inner().Receive(*this);
  }
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
  output_ += "(";
  expr.Receive(*this);
  output_ += ")";
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
    Visit(expr, *this);
    indentations_.pop_back();
  }

  void VisitRight(const Expr& expr) {
    indentations_.push_back(false);
    Visit(expr, *this);
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
    AppendName("{}:", "Division:");
    VisitLeft(op.Numerator());
    VisitRight(op.Denominator());
  }

  void Apply(const Power& op) {
    AppendName("{}:", "Power:");
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
  Visit(expr, formatter);
  std::string output;
  formatter.TakeOutput(output);
  // Somewhat hacky. The formatter appends a superfluous newline on the last element. I think
  // this is tricky to avoid w/o knowing the tree depth apriori. Instead, just trim it from the
  // end.
  RightTrimInPlace(output);
  return output;
}

}  // namespace math
