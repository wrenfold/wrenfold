// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/distribute.h"

#include <algorithm>

#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"
#include "wf/utility/assertions.h"

namespace wf {

scalar_expr distribute_visitor::operator()(const scalar_expr& input) {
  return cache_.get_or_insert(input, [this](const scalar_expr& x) { return visit(x, *this); });
}

compound_expr distribute_visitor::operator()(const compound_expr& input) {
  return cache_.get_or_insert(input, [this](const compound_expr& x) { return visit(x, *this); });
}

boolean_expr distribute_visitor::operator()(const boolean_expr& input) {
  return cache_.get_or_insert(input, [this](const boolean_expr& x) { return visit(x, *this); });
}

matrix_expr distribute_visitor::operator()(const matrix_expr& input) {
  return cache_.get_or_insert(input, [this](const matrix_expr& m) { return visit(m, *this); });
}

scalar_expr distribute_visitor::operator()(const multiplication& mul) {
  return distribute_multiplied_terms(mul);
}

scalar_expr distribute_visitor::operator()(const power& pow) {
  scalar_expr b = operator()(pow.base());
  scalar_expr e = operator()(pow.exponent());

  // If the base is an addition, we should expand the power.
  if (b.is_type<addition>()) {
    if (const integer_constant* exp_int = get_if<const integer_constant>(e); exp_int != nullptr) {
      const checked_int abs_exp = abs(exp_int->value());

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

template <typename T, typename X, typename>
X distribute_visitor::operator()(const T& concrete, const X& expr) {
  if constexpr (T::is_leaf_node) {
    return expr;
  } else {
    return concrete.map_children(*this);
  }
}

scalar_expr distribute_visitor::distribute_power(scalar_expr base, std::size_t power) {
  WF_ASSERT_GT(power, 0);
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
// of the addition. Otherwise, it will be a single length span containing just `x`.
static absl::Span<const scalar_expr> expression_as_span(const scalar_expr& x) noexcept {
  if (const addition* add = get_if<const addition>(x); add != nullptr) {
    return add->children();
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
      scalar_expr product = x * y;
      if (const multiplication* m = get_if<const multiplication>(product);
          m != nullptr && any_of(*m, [&](const scalar_expr& z) { return z.is_type<addition>(); })) {
        // This can occur if powers combine to form new additions in the product we just built.
        // For example: y * (1 + x)**(1/2) * (1 + x)**(1/2) --> y*(1 + x)
        // So we distribute again:
        output_terms.push_back(operator()(product));
      } else {
        output_terms.push_back(std::move(product));
      }
    }
  }
  return addition::from_operands(output_terms);
}

scalar_expr distribute_visitor::distribute_multiplied_terms(
    const multiplication& multiplied_terms) {
  WF_ASSERT_GE(multiplied_terms.size(), 1);
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
