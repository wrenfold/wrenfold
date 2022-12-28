#include "formatting.h"

#include <iostream>
#include <optional>
#include <type_traits>
#include <vector>

#include <fmt/format.h>

#include "binary_operations.h"
#include "constant_expressions.h"
#include "unary_operations.h"
#include "variable.h"

namespace math {

static int GetPrecedence(const Expr& expr) {
  const OperationBase* const op = expr.GetRaw<OperationBase>();
  if (op) {
    return op->Precedence();
  }
  return std::numeric_limits<int>::max();
}

// Helper for appropriately applying brackets, considering operator precedence.
void PlainFormatter::FormatBinaryOp(const int parent_precedence, const char* const op_str,
                                    const Expr& a, const Expr& b) {
  if (parent_precedence > GetPrecedence(a)) {
    output_ += "(";
    a.Receive(*this);
    output_ += ")";
  } else {
    a.Receive(*this);
  }
  fmt::format_to(std::back_inserter(output_), " {} ", op_str);
  if (parent_precedence > GetPrecedence(b)) {
    output_ += "(";
    b.Receive(*this);
    output_ += ")";
  } else {
    b.Receive(*this);
  }
}

void PlainFormatter::Apply(const Addition& expr) {
  FormatBinaryOp(Addition::OperatorPrecedence, "+", expr.First(), expr.Second());
}

void PlainFormatter::Apply(const Constant& expr) {
  switch (expr.GetName()) {
    case SymbolicConstants::Pi:
      output_ += "pi";
      break;
    case SymbolicConstants::Euler:
      output_ += "e";
      break;
    default:
      output_ += "<UNKNOWN CONSTANT>";
      break;
  }
}

void PlainFormatter::Apply(const Division& expr) {
  FormatBinaryOp(Division::OperatorPrecedence, "/", expr.First(), expr.Second());
}

void PlainFormatter::Apply(const Multiplication& expr) {
  FormatBinaryOp(Multiplication::OperatorPrecedence, "*", expr.First(), expr.Second());
}

void PlainFormatter::Apply(const NaturalLog& expr) {
  output_ += "ln(";
  expr.Inner().Receive(*this);
  output_ += ")";
}

struct NegateNeedsBracketsVisitor {
  using ReturnType = bool;

  template <typename Derived>
  constexpr bool Apply(const BinaryOp<Derived>&) const {
    return true;
  }
};

void PlainFormatter::Apply(const Negation& expr) {
  output_ += "-";

  const std::optional<bool> needs_brackets = Visit(expr.Inner(), NegateNeedsBracketsVisitor{});
  if (needs_brackets.value_or(false)) {
    output_ += "(";
    expr.Inner().Receive(*this);
    output_ += ")";
  } else {
    expr.Inner().Receive(*this);
  }
}

void PlainFormatter::Apply(const Number& expr) {
  fmt::format_to(std::back_inserter(output_), "{}", expr.GetValue());
}

void PlainFormatter::Apply(const Power& expr) {
  output_ += "pow(";
  expr.First().Receive(*this);
  output_ += ", ";
  expr.Second().Receive(*this);
  output_ += ")";
}

void PlainFormatter::Apply(const Subtraction& expr) {
  FormatBinaryOp(Subtraction::OperatorPrecedence, "-", expr.First(), expr.Second());
}

void PlainFormatter::Apply(const Variable& expr) { output_ += expr.GetName(); }

struct TreeFormatter {
  using ReturnType = void;

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
  void Apply(const BinaryOp<Derived>& op) {
    AppendName("{}:", op.Name());
    VisitLeft(op.First());
    VisitRight(op.Second());
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
  // Somewhat hacky. The formatter appends a superfluous newline on the last element. I think this
  // is tricky to avoid w/o knowing the tree depth apriori. Instead, just trim it from the end.
  RightTrimInPlace(output);
  return output;
}

}  // namespace math
