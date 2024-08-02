// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "plain_formatter.h"

#include "wf/expression_visitor.h"
#include "wf/utility/assertions.h"
#include "wf/utility/strings.h"
#include "wf/utility/third_party_imports.h"
#include "wf/utility_visitors.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

void plain_formatter::operator()(const scalar_expr& x) { visit(x, *this); }
void plain_formatter::operator()(const matrix_expr& x) { visit(x, *this); }
void plain_formatter::operator()(const boolean_expr& x) { visit(x, *this); }

void plain_formatter::operator()(const addition& add) {
  WF_ASSERT_GE(add.size(), 2);

  // Sort into canonical order:
  absl::InlinedVector<scalar_expr, 8> terms{add.begin(), add.end()};
  std::sort(terms.begin(), terms.end(), [](const scalar_expr& a, const scalar_expr& b) {
    const std::pair<scalar_expr, scalar_expr> a_coeff_mul = as_coeff_and_mul(a);
    const std::pair<scalar_expr, scalar_expr> b_coeff_mul = as_coeff_and_mul(b);
    return order_by(std::get<1>(a_coeff_mul), std::get<1>(b_coeff_mul))
        .and_then_by(std::get<0>(a_coeff_mul), std::get<0>(b_coeff_mul))
        .is_less_than();
  });

  for (std::size_t i = 0; i < terms.size(); ++i) {
    if (const auto [coeff, multiplicand] = as_coeff_and_mul(terms[i]); is_negative_number(coeff)) {
      if (i == 0) {
        // For the first term, just negate it:
        output_ += "-";
      } else {
        // Subsequent terms are written as subtractions:
        output_ += " - ";
      }
      if (is_negative_one(coeff)) {
        // Don't multiply by negative one:
        format_precedence(precedence::addition, multiplicand);
      } else {
        format_precedence(precedence::addition, -terms[i]);
      }
    } else {
      if (i > 0) {
        output_ += " + ";
      }
      if (is_one(coeff)) {
        format_precedence(precedence::addition, multiplicand);
      } else {
        format_precedence(precedence::addition, terms[i]);
      }
    }
  }
}

void plain_formatter::operator()(const boolean_constant& b) {
  if (b) {
    // Capitalized for consistency with python.
    output_ += "True";
  } else {
    output_ += "False";
  }
}

void plain_formatter::operator()(const compound_expression_element& el) {
  const auto format_access_sequence = [this, &el](const custom_type& custom) {
    const std::vector<access_variant> sequence = determine_access_sequence(custom, el.index());
    for (const auto& v : sequence) {
      overloaded_visit(
          v,
          [&](const field_access& f) {
            fmt::format_to(std::back_inserter(output_), ".{}", f.field_name());
          },
          [&](const matrix_access& m) {
            fmt::format_to(std::back_inserter(output_), "[{}, {}]", m.row(), m.col());
          });
    }
  };

  visit(el.provenance(),
        make_overloaded(
            [&](const external_function_invocation& invocation) {
              // Format the function call:
              this->operator()(invocation);
              overloaded_visit(
                  invocation.function().return_type(), [](const scalar_type) constexpr {},
                  [&](const matrix_type& mat) {
                    // Access matrix element:
                    const auto [row, col] = mat.compute_indices(el.index());
                    fmt::format_to(std::back_inserter(output_), "[{}, {}]", row, col);
                  },
                  format_access_sequence);
            },
            [&](const custom_type_argument& arg) {
              // Name followed by the access sequence:
              operator()(arg);
              format_access_sequence(arg.type());
            },
            [&](const custom_type_construction& construction) {
              operator()(construction.at(el.index()));
            }));
}

// TODO: We need to do something smarter when formatting matrix args to functions.
void plain_formatter::operator()(const external_function_invocation& invocation) {
  fmt::format_to(std::back_inserter(output_), "{}(", invocation.function().name());
  auto it = invocation.begin();
  if (it != invocation.end()) {
    visit(*it, *this);
  }
  for (++it; it != invocation.end(); ++it) {
    output_ += ", ";
    visit(*it, *this);
  }
  output_ += ")";
}

void plain_formatter::operator()(const custom_type_argument& arg) {
  fmt::format_to(std::back_inserter(output_), "$arg({})", arg.arg_index());
}

void plain_formatter::operator()(const custom_type_construction& construct) {
  fmt::format_to(std::back_inserter(output_), "{}(<{} expressions>)", construct.type().name(),
                 construct.size());
}

void plain_formatter::operator()(const conditional& conditional) {
  output_ += "where(";
  operator()(conditional.condition());
  output_ += ", ";
  operator()(conditional.if_branch());
  output_ += ", ";
  operator()(conditional.else_branch());
  output_ += ")";
}

void plain_formatter::operator()(const symbolic_constant& constant) {
  output_ += string_from_symbolic_constant(constant.name());
}

void plain_formatter::operator()(const derivative& derivative) {
  output_ += "Derivative(";
  operator()(derivative.differentiand());
  output_ += ", ";
  operator()(derivative.argument());
  if (derivative.order() > 1) {
    fmt::format_to(std::back_inserter(output_), ", {})", derivative.order());
  } else {
    output_ += ")";
  }
}

void plain_formatter::operator()(const complex_infinity&) {
  fmt::format_to(std::back_inserter(output_), "zoo");
}

// Capitalized for consistency with sympy.
void plain_formatter::operator()(const imaginary_unit&) { output_ += "I"; }

void plain_formatter::operator()(const integer_constant& num) {
  fmt::format_to(std::back_inserter(output_), "{}", num.value());
}

void plain_formatter::operator()(const iverson_bracket& bracket) {
  output_ += "iverson(";
  operator()(bracket.arg());
  output_ += ")";
}

void plain_formatter::operator()(const float_constant& num) {
  fmt::format_to(std::back_inserter(output_), "{}", num.value());
}

void plain_formatter::operator()(const matrix& mat) {
  WF_ASSERT_GE(mat.rows(), 0);
  WF_ASSERT_GE(mat.cols(), 0);
  if (mat.size() == 0) {
    // Empty matrix:
    output_ += "[]";
    return;
  }
  output_ += "[";
  for (index_t i = 0; i < mat.rows(); ++i) {
    output_ += "[";
    const index_t last_col = mat.cols() - 1;
    for (index_t j = 0; j < last_col; ++j) {
      operator()(mat(i, j));
      output_ += ", ";
    }
    operator()(mat(i, last_col));
    // Insert a comma and new-line if another row is coming.
    if (i + 1 < mat.rows()) {
      output_ += "], ";
    } else {
      output_ += "]";
    }
  }
  output_ += "]";
}

void plain_formatter::operator()(const multiplication& mul) {
  WF_ASSERT_GE(mul.size(), 2);

  // Break multiplication up into numerator and denominator:
  const multiplication_format_parts info = get_formatting_info(mul);
  if (info.is_negative) {
    output_ += "-";
  }

  const auto format_element =
      [this](const std::variant<integer_constant, float_constant, power>& element) {
        overloaded_visit(
            element, [this](const integer_constant constant) { this->operator()(constant); },
            [this](const float_constant constant) { this->operator()(constant); },
            [this](const power& pow) {
              if (is_one(pow.exponent())) {
                this->format_precedence(precedence::multiplication, pow.base());
              } else {
                this->format_power(pow.base(), pow.exponent());
              }
            });
      };

  format_element(info.numerator.front());
  for (std::size_t i = 1; i < info.numerator.size(); ++i) {
    output_ += "*";
    format_element(info.numerator[i]);
  }

  if (info.denominator.empty()) {
    return;
  }
  output_ += "/";

  if (info.denominator.size() > 1) {
    output_ += "(";
    format_element(info.denominator.front());
    for (std::size_t i = 1; i < info.denominator.size(); ++i) {
      output_ += "*";
      format_element(info.denominator[i]);
    }
    output_ += ")";
  } else {
    format_element(info.denominator.front());
  }
}

void plain_formatter::operator()(const built_in_function_invocation& func) {
  fmt::format_to(std::back_inserter(output_), "{}(", func.function_name());
  auto it = func.begin();
  if (it != func.end()) {
    operator()(*it);
  }
  for (++it; it != func.end(); ++it) {
    output_ += ", ";
    operator()(*it);
  }
  output_ += ")";
}

void plain_formatter::operator()(const power& pow) {
  if (is_negative_one(pow.exponent())) {
    output_ += "1/";
    format_precedence(precedence::multiplication, pow.base());
  } else {
    format_power(pow.base(), pow.exponent());
  }
}

void plain_formatter::operator()(const rational_constant& rational) {
  fmt::format_to(std::back_inserter(output_), "{}/{}", rational.numerator(),
                 rational.denominator());
}

void plain_formatter::operator()(const relational& relational) {
  format_precedence(precedence::relational, relational.left());
  fmt::format_to(std::back_inserter(output_), " {} ", relational.operation_string());
  format_precedence(precedence::relational, relational.right());
}

void plain_formatter::operator()(const symbolic_function_invocation& invocation) {
  output_ += invocation.function().name();
  output_ += "(";
  if (auto it = invocation.begin(); it != invocation.end()) {
    operator()(*it);
    for (; it != invocation.end(); ++it) {
      output_ += ", ";
      operator()(*it);
    }
  }
  output_ += ")";
}

void plain_formatter::operator()(const undefined&) { output_.append("nan"); }

void plain_formatter::operator()(const unevaluated& u) {
  output_ += "(";
  operator()(u.contents());
  output_ += ")";
}

void plain_formatter::operator()(const variable& var) { output_.append(var.to_string()); }

void plain_formatter::format_precedence(const precedence parent, const scalar_expr& expr) {
  if (get_precedence(expr) <= parent) {
    output_ += "(";
    operator()(expr);
    output_ += ")";
  } else {
    operator()(expr);
  }
}

void plain_formatter::format_power(const scalar_expr& base, const scalar_expr& exponent) {
  format_precedence(precedence::power, base);
  output_ += "**";
  format_precedence(precedence::power, exponent);
}

}  // namespace wf
