// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/tree_formatter.h"

#include <vector>

#include "wf/compound_expression.h"
#include "wf/expression.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"
#include "wf/matrix_expression.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

// TODO: We could optimize this to use caching of repeated expressions, but it is not
//  particularly performance critical at the moment.
namespace wf {

void tree_formatter_visitor::operator()(const scalar_expr& x) { visit(x, *this); }
void tree_formatter_visitor::operator()(const matrix_expr& m) { visit(m, *this); }
void tree_formatter_visitor::operator()(const compound_expr& c) { visit(c, *this); }
void tree_formatter_visitor::operator()(const boolean_expr& b) { visit(b, *this); }

void tree_formatter_visitor::operator()(const addition& add) {
  format_append("{}:", addition::name_str);
  visit_all(add.sorted_terms());
}

void tree_formatter_visitor::operator()(const boolean_constant& b) {
  format_append("{} ({})", boolean_constant::name_str, b.value() ? "true" : "false");
}

void tree_formatter_visitor::operator()(const complex_infinity&) {
  format_append(complex_infinity::name_str);
}

void tree_formatter_visitor::operator()(const compound_expression_element& el) {
  format_append("{} (index = {}):", compound_expression_element::name_str, el.index());
  visit_right(el.provenance());
}

void tree_formatter_visitor::operator()(const conditional& conditional) {
  format_append("{}:", conditional::name_str);
  visit_left(conditional.condition());
  visit_left(conditional.if_branch());
  visit_right(conditional.else_branch());
}

void tree_formatter_visitor::operator()(const custom_type_argument& arg) {
  format_append("{} (type = {}, index = {})", custom_type_argument::name_str, arg.type().name(),
                arg.arg_index());
}

void tree_formatter_visitor::operator()(const custom_type_construction& construct) {
  format_append("{} (type = {}):", custom_type_construction::name_str, construct.type().name());
  visit_all(construct.children());
}

void tree_formatter_visitor::operator()(const derivative& diff) {
  format_append("{} (order = {}):", derivative::name_str, diff.order());
  visit_all(diff);
}

void tree_formatter_visitor::operator()(const external_function_invocation& invocation) {
  format_append("{} (function = `{}`):", external_function_invocation::name_str,
                invocation.function().name());
  visit_all(invocation.children());
}

void tree_formatter_visitor::operator()(const float_constant& f) {
  format_append("{} ({})", float_constant::name_str, f.value());
}

void tree_formatter_visitor::operator()(const built_in_function_invocation& func) {
  format_append("{} ({}):", built_in_function_invocation::name_str, func.function_name());
  visit_all(func);
}

void tree_formatter_visitor::operator()(const imaginary_unit&) {
  format_append("{}", imaginary_unit::name_str);
}

void tree_formatter_visitor::operator()(const integer_constant& i) {
  format_append("{} ({})", integer_constant::name_str, i.value());
}

void tree_formatter_visitor::operator()(const iverson_bracket& bracket) {
  format_append("{}:", iverson_bracket::name_str);
  visit_all(bracket);
}

void tree_formatter_visitor::operator()(const matrix& mat) {
  format_append("{} ({}, {}):", matrix::name_str, mat.rows(), mat.cols());
  visit_all(mat);
}

void tree_formatter_visitor::operator()(const multiplication& op) {
  format_append("{}:", multiplication::name_str);
  visit_all(op.sorted_terms());
}

void tree_formatter_visitor::operator()(const power& pow) {
  format_append("{}:", power::name_str);
  visit_all(pow);
}

void tree_formatter_visitor::operator()(const rational_constant& r) {
  format_append("{} ({} / {})", rational_constant::name_str, r.numerator(), r.denominator());
}

void tree_formatter_visitor::operator()(const relational& relational) {
  format_append("{} ({})", relational::name_str, relational.operation_string());
  visit_all(relational);
}

void tree_formatter_visitor::operator()(const symbolic_constant& constant) {
  format_append("{} ({})", symbolic_constant::name_str,
                string_from_symbolic_constant(constant.name()));
}

void tree_formatter_visitor::operator()(const substitution& subs) {
  format_append("{}:", substitution::name_str);
  visit_all(subs);
}

void tree_formatter_visitor::operator()(const symbolic_function_invocation& invocation) {
  format_append("{} ({}):", symbolic_function_invocation::name_str, invocation.function().name());
  visit_all(invocation);
}

void tree_formatter_visitor::operator()(const undefined&) { format_append(undefined::name_str); }

void tree_formatter_visitor::operator()(const unevaluated& u) {
  format_append("{}:", unevaluated::name_str);
  visit_all(u);
}

void tree_formatter_visitor::operator()(const variable& var) {
  format_append("{} ({}, {})", variable::name_str, var.to_string(),
                string_from_number_set(var.set()));
}

static void right_trim_in_place(std::string& str) {
  while (!str.empty() && std::isspace(str.back())) {
    str.pop_back();
  }
}

std::string tree_formatter_visitor::take_output() {
  // Somewhat hacky. The formatter appends a superfluous newline on the last element. I think
  // this is tricky to avoid w/o knowing the tree depth apriori. Instead, just trim it from the
  // end.
  right_trim_in_place(output_);
  return std::move(output_);
}

void tree_formatter_visitor::apply_indentation() {
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
void tree_formatter_visitor::format_append(const std::string_view fmt_str, Args&&... args) {
  apply_indentation();
  fmt::format_to(std::back_inserter(output_), fmt_str, std::forward<Args>(args)...);
  output_ += "\n";
}

template <typename T>
void tree_formatter_visitor::visit_left(const T& expr) {
  indentations_.push_back(true);
  visit(expr, *this);
  indentations_.pop_back();
}

template <typename T>
void tree_formatter_visitor::visit_right(const T& expr) {
  indentations_.push_back(false);
  visit(expr, *this);
  indentations_.pop_back();
}

template <typename Container>
void tree_formatter_visitor::visit_all(const Container& container) {
  auto it = container.begin();
  for (; std::next(it) != container.end(); ++it) {
    visit_left(*it);
  }
  visit_right(*it);
}

}  // namespace wf
