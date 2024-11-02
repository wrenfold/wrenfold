// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "plain_formatter.h"

#include "wf/expression_visitor.h"
#include "wf/utility/assertions.h"
#include "wf/utility/overloaded_visit.h"
#include "wf/utility/third_party_imports.h"
#include "wf/utility_visitors.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

template <typename T, typename>
std::string_view plain_formatter::operator()(const T& x) {
  if constexpr (is_any_expression_v<T>) {
    return std::visit(*this, x);
  } else {
    auto& map = cache_.get<T>();
    if (auto it = map.find(x); it != map.end()) {
      return std::string_view{*it->second};
    } else {
      std::string converted = visit(x, *this);
      // Putting into unique_ptr is a bit gross, but we need pointer stability.
      // I tried an alternative here where the strings are stored in an array, and accessed by
      // index. It produced no meaningful improvement on the benchmark though.
      const auto [it_inserted, _] =
          map.emplace(x, std::make_unique<std::string>(std::move(converted)));
      return std::string_view{*it_inserted->second};
    }
  }
}

std::string plain_formatter::convert(const scalar_expr& expr) { return convert_impl(expr); }
std::string plain_formatter::convert(const boolean_expr& expr) { return convert_impl(expr); }
std::string plain_formatter::convert(const matrix_expr& expr) { return convert_impl(expr); }
std::string plain_formatter::convert(const compound_expr& expr) { return convert_impl(expr); }

template <typename T>
std::string plain_formatter::convert_impl(const T& expr) {
  plain_formatter fmt{};
  fmt(expr);
  // Take the output by moving it:
  auto& map = fmt.cache_.get<T>();
  auto it = map.find(expr);
  WF_ASSERT(it != map.end(), "Object must exist in the map");
  return std::move(*it->second);
}

std::string plain_formatter::operator()(const addition& add) {
  WF_ASSERT_GE(add.size(), 2);
  std::string output{};

  // Sort into canonical order:
  absl::InlinedVector<scalar_expr, 8> terms{add.begin(), add.end()};
  std::sort(terms.begin(), terms.end(), [](const scalar_expr& a, const scalar_expr& b) {
    const auto [a_coeff, a_mul] = as_coeff_and_mul(a);
    const auto [b_coeff, b_mul] = as_coeff_and_mul(b);
    // Order first by multiplicand and then by numerical coefficient:
    return order_by(a_mul, b_mul).and_then_by(a_coeff, b_coeff).is_less_than();
  });

  for (std::size_t i = 0; i < terms.size(); ++i) {
    if (const auto [coeff, multiplicand] = as_coeff_and_mul(terms[i]); is_negative_number(coeff)) {
      if (i == 0) {
        // For the first term, just negate it:
        output += "-";
      } else {
        // Subsequent terms are written as subtractions:
        output += " - ";
      }
      if (is_negative_one(coeff)) {
        // Don't multiply by negative one:
        format_precedence(output, precedence::addition, multiplicand);
      } else {
        format_precedence(output, precedence::addition, -terms[i]);
      }
    } else {
      if (i > 0) {
        output += " + ";
      }
      if (is_one(coeff)) {
        format_precedence(output, precedence::addition, multiplicand);
      } else {
        format_precedence(output, precedence::addition, terms[i]);
      }
    }
  }
  return output;
}

std::string plain_formatter::operator()(const boolean_constant& b) const {
  // Capitalized for consistency with python.
  return static_cast<bool>(b) ? "True" : "False";
}

std::string plain_formatter::operator()(const compound_expression_element& el) {
  std::string output{};

  const auto format_access_sequence = [&el, &output](const custom_type& custom) {
    const std::vector<access_variant> sequence = determine_access_sequence(custom, el.index());
    for (const auto& v : sequence) {
      overloaded_visit(
          v,
          [&](const field_access& f) {
            fmt::format_to(std::back_inserter(output), ".{}", f.field_name());
          },
          [&](const matrix_access& m) {
            fmt::format_to(std::back_inserter(output), "[{}, {}]", m.row(), m.col());
          });
    }
  };

  output += operator()(el.provenance());

  // TODO: This feels needlessly complicated. If `external_function_invocation` existed in different
  // expression types we could clean this up a bit, because `compound_expression_element` would not
  // need to exist as a special scalar expression.
  visit(el.provenance(),
        make_overloaded(
            [&](const external_function_invocation& invocation) {
              overloaded_visit(
                  invocation.function().return_type(), [](const scalar_type) constexpr {},
                  [&](const matrix_type& mat) {
                    // Access matrix element:
                    const auto [row, col] = mat.compute_indices(el.index());
                    fmt::format_to(std::back_inserter(output), "[{}, {}]", row, col);
                  },
                  format_access_sequence);
            },
            [&](const custom_type_argument& arg) {
              // Name followed by the access sequence:
              format_access_sequence(arg.type());
            },
            [&](const custom_type_construction& construction) {
              operator()(construction.at(el.index()));
            }));

  return output;
}

std::string plain_formatter::operator()(const external_function_invocation& invocation) {
  std::string output = fmt::format("{}(", invocation.function().name());
  auto it = invocation.begin();
  if (it != invocation.end()) {
    output += operator()(*it);
  }
  for (++it; it != invocation.end(); ++it) {
    output += ", ";
    output += operator()(*it);
  }
  output += ")";
  return output;
}

std::string plain_formatter::operator()(const custom_type_argument& arg) const {
  // TODO: Print the argument name here instead of this uninformative index.
  return fmt::format("$arg({})", arg.arg_index());
}

std::string plain_formatter::operator()(const custom_type_construction& construct) {
  // TODO: Make this prettier.
  return fmt::format("{}(<{} expressions>)", construct.type().name(), construct.size());
}

std::string plain_formatter::operator()(const conditional& conditional) {
  return fmt::format(
      "where({}, {}, {})", operator()(conditional.condition()), operator()(conditional.if_branch()),
                                                                operator()(
                                                                    conditional.else_branch()));
}

std::string plain_formatter::operator()(const symbolic_constant& constant) const {
  return std::string{string_from_symbolic_constant(constant.name())};
}

std::string plain_formatter::operator()(const derivative& derivative) {
  std::string output = fmt::format("Derivative({}, {}", operator()(derivative.differentiand()),
                                                        operator()(derivative.argument()));
  if (derivative.order() > 1) {
    fmt::format_to(std::back_inserter(output), ", {})", derivative.order());
  } else {
    output += ")";
  }
  return output;
}

std::string plain_formatter::operator()(const complex_infinity&) const { return "zoo"; }

// Capitalized for consistency with sympy.
std::string plain_formatter::operator()(const imaginary_unit&) const { return "I"; }

std::string plain_formatter::operator()(const integer_constant& num) const {
  return fmt::format("{}", num.value());
}

std::string plain_formatter::operator()(const iverson_bracket& bracket) {
  return fmt::format("iverson({})", operator()(bracket.arg()));
}

std::string plain_formatter::operator()(const float_constant& num) const {
  return fmt::format("{}", num.value());
}

std::string plain_formatter::operator()(const matrix& mat) {
  WF_ASSERT_GE(mat.rows(), 0);
  WF_ASSERT_GE(mat.cols(), 0);
  if (mat.size() == 0) {
    // Empty matrix:
    return "[]";
  }
  std::string output = "[";
  for (index_t i = 0; i < mat.rows(); ++i) {
    output += "[";
    const index_t last_col = mat.cols() - 1;
    for (index_t j = 0; j < last_col; ++j) {
      output += operator()(mat(i, j));
      output += ", ";
    }
    output += operator()(mat(i, last_col));
    // Insert a comma and new-line if another row is coming.
    if (i + 1 < mat.rows()) {
      output += "], ";
    } else {
      output += "]";
    }
  }
  output += "]";
  return output;
}

std::string plain_formatter::operator()(const multiplication& mul) {
  WF_ASSERT_GE(mul.size(), 2);
  std::string output;

  // Break multiplication up into numerator and denominator:
  const multiplication_format_parts info = get_formatting_info(mul);
  if (info.is_negative) {
    output += "-";
  }

  const auto format_element =
      [&](const std::variant<integer_constant, float_constant, power>& element) {
        overloaded_visit(
            element,
            [&](const power& pow) {
              if (is_one(pow.exponent())) {
                format_precedence(output, precedence::multiplication, pow.base());
              } else {
                format_power(output, pow.base(), pow.exponent());
              }
            },
            [&](const auto& constant) {
              fmt::format_to(std::back_inserter(output), "{}", constant.value());
            });
      };

  format_element(info.numerator.front());
  for (std::size_t i = 1; i < info.numerator.size(); ++i) {
    output += "*";
    format_element(info.numerator[i]);
  }

  if (info.denominator.empty()) {
    return output;
  }
  output += "/";

  if (info.denominator.size() > 1) {
    output += "(";
    format_element(info.denominator.front());
    for (std::size_t i = 1; i < info.denominator.size(); ++i) {
      output += "*";
      format_element(info.denominator[i]);
    }
    output += ")";
  } else {
    format_element(info.denominator.front());
  }
  return output;
}

std::string plain_formatter::operator()(const built_in_function_invocation& func) {
  std::string output{func.function_name()};
  output += "(";
  auto it = func.begin();
  if (it != func.end()) {
    output += operator()(*it);
  }
  for (++it; it != func.end(); ++it) {
    output += ", ";
    output += operator()(*it);
  }
  output += ")";
  return output;
}

std::string plain_formatter::operator()(const power& pow) {
  std::string output;
  if (is_negative_one(pow.exponent())) {
    output += "1/";
    format_precedence(output, precedence::multiplication, pow.base());
  } else {
    format_power(output, pow.base(), pow.exponent());
  }
  return output;
}

std::string plain_formatter::operator()(const rational_constant& rational) const {
  return fmt::format("{}/{}", rational.numerator(), rational.denominator());
}

std::string plain_formatter::operator()(const relational& relational) {
  std::string output{};
  format_precedence(output, precedence::relational, relational.left());
  fmt::format_to(std::back_inserter(output), " {} ", relational.operation_string());
  format_precedence(output, precedence::relational, relational.right());
  return output;
}

std::string plain_formatter::operator()(const substitution& subs) {
  return fmt::format("Subs({}, {}, {})", operator()(subs.input()), operator()(subs.target()),
                                                                   operator()(subs.replacement()));
}

std::string plain_formatter::operator()(const symbolic_function_invocation& invocation) {
  std::string output = invocation.function().name();
  output += "(";
  if (auto it = invocation.begin(); it != invocation.end()) {
    output += operator()(*it);
    for (++it; it != invocation.end(); ++it) {
      output += ", ";
      output += operator()(*it);
    }
  }
  output += ")";
  return output;
}

std::string plain_formatter::operator()(const undefined&) const { return "nan"; }

std::string plain_formatter::operator()(const unevaluated& u) {
  return fmt::format("({})", operator()(u.contents()));
}

std::string plain_formatter::operator()(const variable& var) const { return var.to_string(); }

void plain_formatter::format_precedence(std::string& output, const precedence parent,
                                        const scalar_expr& expr) {
  if (get_precedence(expr) <= parent) {
    output += "(";
    output += operator()(expr);
    output += ")";
  } else {
    output += operator()(expr);
  }
}

void plain_formatter::format_power(std::string& output, const scalar_expr& base,
                                   const scalar_expr& exponent) {
  format_precedence(output, precedence::power, base);
  output += "**";
  format_precedence(output, precedence::power, exponent);
}

}  // namespace wf
