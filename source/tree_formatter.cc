// Copyright 2022 Gareth Cross
#include <fmt/format.h>
#include <vector>

#include "expression.h"
#include "expressions/all_expressions.h"
#include "visitor_impl.h"

namespace math {

static void RightTrimInPlace(std::string& str) {
  while (!str.empty() && std::isspace(str.back())) {
    str.pop_back();
  }
}

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
  void AppendName(const std::string_view fmt_str, Args&&... args) {
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

  void operator()(const Addition& op) {
    AppendName("Addition:");
    auto it = op.begin();
    for (; std::next(it) != op.end(); ++it) {
      VisitLeft(*it);
    }
    VisitRight(*it);
  }

  void operator()(const Multiplication& op) {
    AppendName("Multiplication:");
    auto it = op.begin();
    for (; std::next(it) != op.end(); ++it) {
      VisitLeft(*it);
    }
    VisitRight(*it);
  }

  void operator()(const Matrix& mat) {
    // TODO: Print the (row, col) index for each element.
    AppendName("Matrix ({}, {}):", mat.NumRows(), mat.NumCols());
    const auto& elements = mat.Data();
    for (std::size_t i = 0; i + 1 < elements.size(); ++i) {
      VisitLeft(elements[i]);
    }
    VisitRight(elements.back());
  }

  void operator()(const Power& op) {
    AppendName("Power:");
    VisitLeft(op.Base());
    VisitRight(op.Exponent());
  }

  void operator()(const Infinity&) { AppendName("Infinity"); }

  void operator()(const Integer& neg) { AppendName("Integer ({})", neg.GetValue()); }

  void operator()(const Float& neg) { AppendName("Float ({})", neg.GetValue()); }

  void operator()(const FunctionArgument& arg) {
    AppendName("FunctionArgument ({}, {})", arg.ArgIndex(), arg.ElementIndex());
  }

  void operator()(const Rational& rational) {
    AppendName("Rational ({} / {})", rational.Numerator(), rational.Denominator());
  }

  void operator()(const Relational& relational) {
    AppendName("Relational ({})", relational.OperationString());
    VisitLeft(relational.Left());
    VisitRight(relational.Right());
  }

  void operator()(const UnaryFunction& func) {
    AppendName("UnaryFunction ({}):", func.Name());
    VisitRight(func.Arg());
  }

  void operator()(const Variable& var) { AppendName("Variable ({})", var.GetName()); }

  void operator()(const Conditional& conditional) {
    AppendName("Conditional:");
    VisitLeft(conditional.Condition());
    VisitLeft(conditional.IfBranch());
    VisitRight(conditional.ElseBranch());
  }

  void operator()(const Constant& constant) {
    AppendName("Constant ({})", StringFromSymbolicConstant(constant.GetName()));
  }

  // Get the output string via move.
  void TakeOutput(std::string& output) {
    // Somewhat hacky. The formatter appends a superfluous newline on the last element. I think
    // this is tricky to avoid w/o knowing the tree depth apriori. Instead, just trim it from the
    // end.
    RightTrimInPlace(output_);
    output = std::move(output_);
  }

 private:
  // The indentation pattern at our current tree depth.
  // True indicates a left branch, false indicates a right branch.
  std::vector<unsigned char> indentations_;
  // The final output
  std::string output_;
};

std::string FormatDebugTree(const Expr& expr) {
  TreeFormatter formatter{};
  Visit(expr, formatter);
  std::string output;
  formatter.TakeOutput(output);
  return output;
}

}  // namespace math
