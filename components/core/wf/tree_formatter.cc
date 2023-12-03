// Copyright 2022 Gareth Cross
#include <vector>

#include "wf/expression.h"
#include "wf/expressions/all_expressions.h"
#include "wf/fmt_imports.h"
#include "wf/matrix_expression.h"
#include "wf/visitor_impl.h"

namespace math {

static void right_trim_in_place(std::string& str) {
  while (!str.empty() && std::isspace(str.back())) {
    str.pop_back();
  }
}

struct tree_formatter {
  // Add indentation to the output string.
  void apply_indentation() {
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
  void append_name(const std::string_view fmt_str, Args&&... args) {
    apply_indentation();
    fmt::format_to(std::back_inserter(output_), fmt_str, std::forward<Args>(args)...);
    output_ += "\n";
  }

  void visit_left(const Expr& expr) {
    indentations_.push_back(true);
    visit(expr, *this);
    indentations_.pop_back();
  }

  void visit_right(const Expr& expr) {
    indentations_.push_back(false);
    visit(expr, *this);
    indentations_.pop_back();
  }

  void operator()(const Addition& op) {
    absl::InlinedVector<Expr, 16> terms;
    terms.reserve(op.arity());
    std::copy(op.begin(), op.end(), std::back_inserter(terms));
    std::sort(terms.begin(), terms.end(), [](const auto& a, const auto& b) {
      auto acm = as_coeff_and_mul(a);
      auto bcm = as_coeff_and_mul(b);
      return expression_order(acm.second, bcm.second) == RelativeOrder::LessThan;
    });

    append_name("Addition:");
    auto it = terms.begin();
    for (; std::next(it) != terms.end(); ++it) {
      visit_left(*it);
    }
    visit_right(*it);
  }

  void operator()(const Derivative& diff) {
    append_name("Derivative (order = {}):", diff.order());
    visit_left(diff.differentiand());
    visit_right(diff.argument());
  }

  void operator()(const Multiplication& op) {
    absl::InlinedVector<Expr, 16> terms;
    terms.reserve(op.arity());
    std::copy(op.begin(), op.end(), std::back_inserter(terms));
    std::sort(terms.begin(), terms.end(), [](const auto& a, const auto& b) {
      const auto abe = as_base_and_exp(a);
      const auto bbe = as_base_and_exp(b);
      return expression_order(abe.first, bbe.first) == RelativeOrder::LessThan;
    });

    append_name("Multiplication:");
    auto it = terms.begin();
    for (; std::next(it) != terms.end(); ++it) {
      visit_left(*it);
    }
    visit_right(*it);
  }

  void operator()(const Matrix& mat) {
    // TODO: Print the (row, col) index for each element.
    append_name("Matrix ({}, {}):", mat.rows(), mat.cols());
    const auto& elements = mat.data();
    for (std::size_t i = 0; i + 1 < elements.size(); ++i) {
      visit_left(elements[i]);
    }
    visit_right(elements.back());
  }

  void operator()(const Power& op) {
    append_name("Power:");
    visit_left(op.base());
    visit_right(op.exponent());
  }

  void operator()(const Infinity&) { append_name("{}", Infinity::NameStr); }

  void operator()(const Integer& neg) { append_name("Integer ({})", neg.get_value()); }

  void operator()(const Float& neg) { append_name("Float ({})", neg.get_value()); }

  void operator()(const Rational& rational) {
    append_name("Rational ({} / {})", rational.numerator(), rational.denominator());
  }

  void operator()(const Relational& relational) {
    append_name("Relational ({})", relational.operation_string());
    visit_left(relational.left());
    visit_right(relational.right());
  }

  void operator()(const Function& func) {
    append_name("Function ({}):", func.function_name());
    auto it = func.begin();
    for (; std::next(it) != func.end(); ++it) {
      visit_left(*it);
    }
    visit_right(*it);
  }

  void operator()(const Undefined&) { append_name(Undefined::NameStr); }

  void operator()(const Variable& var) {
    append_name("{} ({}, {})", Variable::NameStr, var.to_string(),
                string_from_number_set(var.set()));
  }

  void operator()(const CastBool& cast) {
    append_name("CastBool:");
    visit_right(cast.arg());
  }

  void operator()(const Conditional& conditional) {
    append_name("Conditional:");
    visit_left(conditional.condition());
    visit_left(conditional.if_branch());
    visit_right(conditional.else_branch());
  }

  void operator()(const Constant& constant) {
    append_name("Constant ({})", string_from_symbolic_constant(constant.name()));
  }

  // Get the output string via move.
  std::string take_output() {
    // Somewhat hacky. The formatter appends a superfluous newline on the last element. I think
    // this is tricky to avoid w/o knowing the tree depth apriori. Instead, just trim it from the
    // end.
    right_trim_in_place(output_);
    return std::move(output_);
  }

 private:
  // The indentation pattern at our current tree depth.
  // True indicates a left branch, false indicates a right branch.
  std::vector<unsigned char> indentations_;
  // The final output
  std::string output_;
};

std::string Expr::to_expression_tree_string() const {
  tree_formatter formatter{};
  visit(*this, formatter);
  return formatter.take_output();
}

std::string MatrixExpr::to_expression_tree_string() const {
  tree_formatter formatter{};
  formatter(as_matrix());
  return formatter.take_output();
}

}  // namespace math
