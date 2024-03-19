// Copyright 2024 Gareth Cross
#include "wf/distribute.h"

#include <algorithm>

#include "wf/assertions.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"

namespace wf {

scalar_expr distribute_visitor::operator()(const scalar_expr& input) {
  return cache_.get_or_insert(input, [this](const scalar_expr& x) { return visit(x, *this); });
}

compound_expr distribute_visitor::operator()(const compound_expr& input) {
  return cache_.get_or_insert(
      input, [this](const compound_expr& x) { return map_compound_expressions(x, *this); });
}

boolean_expr distribute_visitor::operator()(const boolean_expr& input) {
  return cache_.get_or_insert(input, [this](const boolean_expr& x) { return visit(x, *this); });
}

matrix_expr distribute_visitor::operator()(const matrix_expr& input) {
  return cache_.get_or_insert(
      input, [this](const matrix_expr& m) { return map_matrix_expression(m, *this); });
}

scalar_expr distribute_visitor::operator()(const addition& add) { return add.map_children(*this); }

boolean_expr distribute_visitor::operator()(const boolean_constant&,
                                            const boolean_expr& arg) const {
  return arg;
}

scalar_expr distribute_visitor::operator()(const iverson_bracket& cast) {
  return cast.map_children(*this);
}
scalar_expr distribute_visitor::operator()(const compound_expression_element& el) {
  return el.map_children(*this);
}
scalar_expr distribute_visitor::operator()(const complex_infinity&) const {
  return constants::complex_infinity;
}
scalar_expr distribute_visitor::operator()(const conditional& conditional) {
  return conditional.map_children(*this);
}
scalar_expr distribute_visitor::operator()(const derivative& diff) {
  return diff.map_children(*this);
}

scalar_expr distribute_visitor::operator()(const imaginary_unit&) const {
  return constants::imaginary_unit;
}

scalar_expr distribute_visitor::operator()(const integer_constant&, const scalar_expr& arg) const {
  return arg;
}

scalar_expr distribute_visitor::operator()(const float_constant&, const scalar_expr& arg) const {
  return arg;
}
scalar_expr distribute_visitor::operator()(const function& f) { return f.map_children(*this); }

scalar_expr distribute_visitor::operator()(const multiplication& mul) {
  return distribute_multiplied_terms(mul);
}

scalar_expr distribute_visitor::operator()(const power& pow) {
  scalar_expr b = operator()(pow.base());
  scalar_expr e = operator()(pow.exponent());

  // If the base is an addition, we should expand the power.
  if (b.is_type<addition>()) {
    if (const integer_constant* exp_int = get_if<const integer_constant>(e); exp_int != nullptr) {
      const checked_int abs_exp = abs(exp_int->get_value());

      scalar_expr distributed = distribute_power(std::move(b), static_cast<std::uint64_t>(abs_exp));
      if (exp_int->is_positive()) {
        return distributed;
      } else if (exp_int->is_negative()) {
        return power::create(std::move(distributed), constants::negative_one);
      }
    } else if (const rational_constant* exp_rational = get_if<const rational_constant>(e);
               exp_rational != nullptr && exp_rational->denominator() == 2 &&
               abs(exp_rational->numerator()) > 1) {
      // This can be interpreted as integer power of sqrt(...).
      // We take sqrt() of the inner expression, then distribute it to the power of the numerator.
      const checked_int abs_exp = abs(exp_rational->numerator());

      scalar_expr distributed = distribute_power(sqrt(b), static_cast<std::uint64_t>(abs_exp));
      if (exp_rational->is_positive()) {
        return distributed;
      } else if (exp_rational->is_negative()) {
        return power::create(std::move(distributed), constants::negative_one);
      }
    }
  }
  return power::create(std::move(b), std::move(e));
}

scalar_expr distribute_visitor::operator()(const rational_constant&, const scalar_expr& arg) const {
  return arg;
}

boolean_expr distribute_visitor::operator()(const relational& relation) {
  return relation.map_children(*this);
}

scalar_expr distribute_visitor::operator()(const symbolic_constant&, const scalar_expr& arg) const {
  return arg;
}
scalar_expr distribute_visitor::operator()(const undefined&) const { return constants::undefined; }
scalar_expr distribute_visitor::operator()(const variable&, const scalar_expr& arg) const {
  return arg;
}

scalar_expr distribute_visitor::distribute_power(scalar_expr base, std::size_t power) {
  WF_ASSERT_GREATER(power, 0);
  scalar_expr result = constants::one;
  for (;;) {
    if (power & 1) {
      result = distribute_multiplied_terms(result, base);
    }
    power /= 2;
    if (power == 0) {
      break;
    }
    base = distribute_multiplied_terms(base, base);
  }
  return result;
}

// Create a span from input expression `x`. If `x` is an addition, the span will be over the terms
// of the addition. Otherwise it will be a single length span containing just `x`.
static absl::Span<const scalar_expr> expression_as_span(const scalar_expr& x) noexcept {
  if (const addition* add = get_if<const addition>(x); add != nullptr) {
    return add->as_span();
  } else {
    return {&x, 1};
  }
}

scalar_expr distribute_visitor::distribute_multiplied_terms(const scalar_expr& a,
                                                            const scalar_expr& b) {
  const auto span_a = expression_as_span(a);
  const auto span_b = expression_as_span(b);

  std::vector<scalar_expr> output_terms{};
  output_terms.reserve(span_a.size() * span_b.size());
  for (const scalar_expr& x : span_a) {
    for (const scalar_expr& y : span_b) {
      output_terms.push_back(x * y);
    }
  }
  return addition::from_operands(output_terms);
}

template <typename Container>
scalar_expr distribute_visitor::distribute_multiplied_terms(const Container& multiplied_terms) {
  WF_ASSERT_GREATER_OR_EQ(multiplied_terms.size(), 1);
  std::vector<scalar_expr> output_terms = transform_map<std::vector>(multiplied_terms, *this);
  while (output_terms.size() > 1) {
    scalar_expr top = std::move(output_terms.back());
    output_terms.pop_back();
    output_terms.back() = distribute_multiplied_terms(output_terms.back(), top);
  }
  return output_terms.front();
}

scalar_expr distribute(const scalar_expr& arg) { return visit(arg, distribute_visitor{}); }

}  // namespace wf
