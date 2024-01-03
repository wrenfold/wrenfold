// Copyright 2023 Gareth Cross
#include "wf/expressions/addition.h"

#include <algorithm>

#include "wf/expressions/matrix.h"
#include "wf/expressions/multiplication.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"
#include "wf/visit.h"

namespace wf {

Expr addition::from_operands(absl::Span<const Expr> args) {
  WF_ASSERT(!args.empty(), "Cannot call from_operands with an empty span.");
  if (args.size() < 2) {
    return args.front();
  }

  if (std::any_of(args.begin(), args.end(), &is_undefined)) {
    return constants::undefined;
  }

  // TODO: extract common denominator?
  addition_parts parts{args.size()};
  for (const Expr& arg : args) {
    parts.add_terms(arg);
  }
  parts.normalize_coefficients();
  return parts.create_addition();
}

struct addition_visitor {
  explicit addition_visitor(addition_parts& parts) : parts(parts) {}

  void operator()(const addition& arg) {
    for (const Expr& expr : arg) {
      // Recursively add additions:
      visit_with_expr(expr, *this);
    }
  }

  void operator()(const integer_constant& i) {
    parts.rational_term = parts.rational_term + static_cast<rational_constant>(i);
  }
  void operator()(const rational_constant& r) { parts.rational_term = parts.rational_term + r; }
  void operator()(const float_constant& f) {
    if (!parts.float_term.has_value()) {
      parts.float_term = f;
    } else {
      parts.float_term = (*parts.float_term) + f;
    }
  }

  constexpr void operator()(const complex_infinity&) noexcept { ++parts.num_infinities; }

  using excluded_types =
      type_list<addition, integer_constant, rational_constant, float_constant, complex_infinity>;

  template <typename T, typename = enable_if_does_not_contain_type_t<T, excluded_types>>
  void operator()(const T&, const Expr& input_expression) {
    // Everything else: Just add to the coeff
    auto [coeff, mul] = as_coeff_and_mul(input_expression);

    if (mul.is_type<addition>()) {
      // This is probably not great for performance, but shouldn't be _that_ common.
      Expr distributed = input_expression.distribute();
      operator()(cast_checked<addition>(distributed));
      return;
    }

    const auto [it, was_inserted] = parts.terms.emplace(std::move(mul), coeff);
    if (!was_inserted) {
      it->second = it->second + coeff;
    }
  }

  addition_parts& parts;
};

addition_parts::addition_parts(const addition& add) : addition_parts(add.size()) {
  for (const Expr& expr : add) {
    add_terms(expr);
  }
  normalize_coefficients();
}

void addition_parts::add_terms(const Expr& arg) {
  if (is_zero(arg)) {
    return;
  }

  addition_visitor visitor{*this};
  visit_with_expr(arg, visitor);
}

void addition_parts::normalize_coefficients() {
  // Remove anything where the coefficient worked out to zero:
  for (auto it = terms.begin(); it != terms.end();) {
    if (is_zero(it->second)) {
      it = terms.erase(it);
    } else {
      ++it;
    }
  }
}

Expr addition_parts::create_addition() const {
  addition::container_type args{};

  // handle infinities in the input
  if (num_infinities > 1) {
    // any additive/subtractive combination of complex infinities is undefined
    return constants::undefined;
  } else if (num_infinities > 0) {
    args.push_back(constants::complex_infinity);
  }

  // If we didn't add infinity, now consider float/rational.
  if (args.empty()) {
    if (float_term.has_value()) {
      const float_constant promoted_rational = static_cast<float_constant>(rational_term);
      args.push_back(make_expr<float_constant>(float_term.value() + promoted_rational));
    } else if (rational_term.is_zero()) {
      // Don't insert a useless zero in the add.
    } else {
      args.push_back(Expr(rational_term));
    }
  }

  args.reserve(args.size() + terms.size());
  std::transform(terms.begin(), terms.end(), std::back_inserter(args),
                 [](const std::pair<Expr, Expr>& pair) {
                   if (is_one(pair.second)) {
                     return pair.first;
                   } else if (!pair.first.is_type<multiplication>() &&
                              !pair.second.is_type<multiplication>()) {
                     // We can skip calling FromOperands here because we know the first element in
                     // the pair is the non-numeric value and the second is the numeric coefficient.
                     return make_expr<multiplication>(pair.second, pair.first);
                   }
                   return multiplication::from_operands({pair.first, pair.second});
                 });

  if (args.empty()) {
    return constants::zero;
  } else if (args.size() == 1) {
    return args.front();
  }

  return make_expr<addition>(std::move(args));
}

}  // namespace wf
