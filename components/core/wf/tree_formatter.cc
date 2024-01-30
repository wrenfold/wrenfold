// Copyright 2022 Gareth Cross
#include <vector>

#include "wf/compound_expression.h"
#include "wf/expression.h"
#include "wf/expressions/all_expressions.h"
#include "wf/fmt_imports.h"
#include "wf/matrix_expression.h"
#include "wf/visit.h"

namespace wf {

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

  template <typename T>
  void visit_left(const T& expr) {
    indentations_.push_back(true);
    visit(expr, *this);
    indentations_.pop_back();
  }

  template <typename T>
  void visit_right(const T& expr) {
    indentations_.push_back(false);
    visit(expr, *this);
    indentations_.pop_back();
  }

  template <typename Container>
  void visit_all(const Container& container) {
    auto it = container.begin();
    for (; std::next(it) != container.end(); ++it) {
      visit_left(*it);
    }
    visit_right(*it);
  }

  void operator()(const Expr& x) { visit(x, *this); }

  void operator()(const MatrixExpr& m) { operator()(m.as_matrix()); }

  void operator()(const addition& op) {
    absl::InlinedVector<Expr, 16> terms;
    terms.reserve(op.size());
    std::copy(op.begin(), op.end(), std::back_inserter(terms));
    std::sort(terms.begin(), terms.end(), [](const auto& a, const auto& b) {
      auto acm = as_coeff_and_mul(a);
      auto bcm = as_coeff_and_mul(b);
      return determine_order(acm.second, bcm.second) == relative_order::less_than;
    });

    append_name("Addition:");
    visit_all(terms);
  }

  void operator()(const compound_expression_element& el) {
    append_name("{} (index = {}):", compound_expression_element::name_str, el.index());
    visit_right(el.provenance());
  }

  void operator()(const external_function_invocation& invocation) {
    append_name("{} (function = `{}`):", external_function_invocation::name_str,
                invocation.function().name());
    visit_all(invocation.args());
  }

  void operator()(const custom_type_argument& arg) {
    append_name("{} (type = {}, index = {})", custom_type_argument::name_str, arg.type().name(),
                arg.arg_index());
  }

  void operator()(const custom_type_construction& construct) {
    append_name("{} (type = {}):", custom_type_construction::name_str, construct.type().name());
    visit_all(construct.args());
  }

  void operator()(const derivative& diff) {
    append_name("Derivative (order = {}):", diff.order());
    visit_left(diff.differentiand());
    visit_right(diff.argument());
  }

  void operator()(const multiplication& op) {
    absl::InlinedVector<Expr, 16> terms;
    terms.reserve(op.size());
    std::copy(op.begin(), op.end(), std::back_inserter(terms));
    std::sort(terms.begin(), terms.end(), [](const auto& a, const auto& b) {
      const auto abe = as_base_and_exp(a);
      const auto bbe = as_base_and_exp(b);
      return determine_order(abe.first, bbe.first) == relative_order::less_than;
    });

    append_name("Multiplication:");
    auto it = terms.begin();
    for (; std::next(it) != terms.end(); ++it) {
      visit_left(*it);
    }
    visit_right(*it);
  }

  void operator()(const matrix& mat) {
    // TODO: Print the (row, col) index for each element.
    append_name("Matrix ({}, {}):", mat.rows(), mat.cols());
    const auto& elements = mat.data();
    for (std::size_t i = 0; i + 1 < elements.size(); ++i) {
      visit_left(elements[i]);
    }
    visit_right(elements.back());
  }

  void operator()(const power& op) {
    append_name("Power:");
    visit_left(op.base());
    visit_right(op.exponent());
  }

  void operator()(const complex_infinity&) { append_name("{}", complex_infinity::name_str); }

  void operator()(const integer_constant& neg) { append_name("Integer ({})", neg.get_value()); }

  void operator()(const float_constant& neg) { append_name("Float ({})", neg.get_value()); }

  void operator()(const rational_constant& rational) {
    append_name("Rational ({} / {})", rational.numerator(), rational.denominator());
  }

  void operator()(const relational& relational) {
    append_name("Relational ({})", relational.operation_string());
    visit_left(relational.left());
    visit_right(relational.right());
  }

  void operator()(const function& func) {
    append_name("Function ({}):", func.function_name());
    auto it = func.begin();
    for (; std::next(it) != func.end(); ++it) {
      visit_left(*it);
    }
    visit_right(*it);
  }

  void operator()(const undefined&) { append_name(undefined::name_str); }

  void operator()(const variable& var) {
    append_name("{} ({}, {})", variable::name_str, var.to_string(),
                string_from_number_set(var.set()));
  }

  void operator()(const cast_bool& cast) {
    append_name("CastBool:");
    visit_right(cast.arg());
  }

  void operator()(const conditional& conditional) {
    append_name("Conditional:");
    visit_left(conditional.condition());
    visit_left(conditional.if_branch());
    visit_right(conditional.else_branch());
  }

  void operator()(const symbolic_constant& constant) {
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

std::string compound_expr::to_expression_tree_string() const {
  tree_formatter formatter{};
  visit(*this, formatter);
  return formatter.take_output();
}

}  // namespace wf
