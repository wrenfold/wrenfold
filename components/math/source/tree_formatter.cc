// Copyright 2022 Gareth Cross
#include <vector>

#include "expression.h"
#include "expressions/all_expressions.h"
#include "fmt_imports.h"
#include "matrix_expression.h"
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
    absl::InlinedVector<Expr, 16> terms;
    terms.reserve(op.arity());
    std::copy(op.begin(), op.end(), std::back_inserter(terms));
    std::sort(terms.begin(), terms.end(), [](const auto& a, const auto& b) {
      auto acm = as_coeff_and_mul(a);
      auto bcm = as_coeff_and_mul(b);
      return ExpressionOrder(acm.second, bcm.second) == RelativeOrder::LessThan;
    });

    AppendName("Addition:");
    auto it = terms.begin();
    for (; std::next(it) != terms.end(); ++it) {
      VisitLeft(*it);
    }
    VisitRight(*it);
  }

  void operator()(const Derivative& diff) {
    AppendName("Derivative (order = {}):", diff.order());
    VisitLeft(diff.differentiand());
    VisitRight(diff.argument());
  }

  void operator()(const Multiplication& op) {
    absl::InlinedVector<Expr, 16> terms;
    terms.reserve(op.arity());
    std::copy(op.begin(), op.end(), std::back_inserter(terms));
    std::sort(terms.begin(), terms.end(), [](const auto& a, const auto& b) {
      const auto abe = as_base_and_exp(a);
      const auto bbe = as_base_and_exp(b);
      return ExpressionOrder(abe.first, bbe.first) == RelativeOrder::LessThan;
    });

    AppendName("Multiplication:");
    auto it = terms.begin();
    for (; std::next(it) != terms.end(); ++it) {
      VisitLeft(*it);
    }
    VisitRight(*it);
  }

  void operator()(const Matrix& mat) {
    // TODO: Print the (row, col) index for each element.
    AppendName("Matrix ({}, {}):", mat.rows(), mat.cols());
    const auto& elements = mat.data();
    for (std::size_t i = 0; i + 1 < elements.size(); ++i) {
      VisitLeft(elements[i]);
    }
    VisitRight(elements.back());
  }

  void operator()(const Power& op) {
    AppendName("Power:");
    VisitLeft(op.base());
    VisitRight(op.exponent());
  }

  void operator()(const Infinity&) { AppendName("Infinity"); }

  void operator()(const Integer& neg) { AppendName("Integer ({})", neg.get_value()); }

  void operator()(const Float& neg) { AppendName("Float ({})", neg.get_value()); }

  void operator()(const FunctionArgument& arg) {
    AppendName("FunctionArgument ({}, {})", arg.arg_index(), arg.element_index());
  }

  void operator()(const Rational& rational) {
    AppendName("Rational ({} / {})", rational.numerator(), rational.denominator());
  }

  void operator()(const Relational& relational) {
    AppendName("Relational ({})", relational.operation_string());
    VisitLeft(relational.left());
    VisitRight(relational.right());
  }

  void operator()(const Function& func) {
    AppendName("Function ({}):", func.function_name());
    auto it = func.begin();
    for (; std::next(it) != func.end(); ++it) {
      VisitLeft(*it);
    }
    VisitRight(*it);
  }

  void operator()(const Variable& var) { AppendName("Variable ({})", var.name()); }

  void operator()(const Conditional& conditional) {
    AppendName("Conditional:");
    VisitLeft(conditional.condition());
    VisitLeft(conditional.if_branch());
    VisitRight(conditional.else_branch());
  }

  void operator()(const Constant& constant) {
    AppendName("Constant ({})", string_from_symbolic_constant(constant.name()));
  }

  // Get the output string via move.
  std::string TakeOutput() {
    // Somewhat hacky. The formatter appends a superfluous newline on the last element. I think
    // this is tricky to avoid w/o knowing the tree depth apriori. Instead, just trim it from the
    // end.
    RightTrimInPlace(output_);
    return std::move(output_);
  }

 private:
  // The indentation pattern at our current tree depth.
  // True indicates a left branch, false indicates a right branch.
  std::vector<unsigned char> indentations_;
  // The final output
  std::string output_;
};

std::string Expr::ToExpressionTreeString() const {
  TreeFormatter formatter{};
  Visit(*this, formatter);
  return formatter.TakeOutput();
}

std::string MatrixExpr::ToExpressionTreeString() const {
  TreeFormatter formatter{};
  formatter(AsMatrix());
  return formatter.TakeOutput();
}

}  // namespace math
