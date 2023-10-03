// Copyright 2023 Gareth Cross
#include "expressions/addition.h"

#include <algorithm>
#include <unordered_map>

#include "expressions/matrix.h"
#include "expressions/multiplication.h"
#include "expressions/numeric_expressions.h"
#include "hashing.h"

namespace math {

Expr Addition::from_operands(absl::Span<const Expr> args) {
  // TODO: extract common denominator?
  AdditionParts parts{args.size()};
  for (const Expr& arg : args) {
    parts.add_terms(arg);
  }
  parts.normalize_coefficients();
  return parts.create_addition();
}

struct AdditionVisitor {
  explicit AdditionVisitor(AdditionParts& parts) : parts(parts) {}

  void operator()(const Addition& arg) {
    for (const Expr& expr : arg) {
      // Recursively add additions:
      visit_with_expr(expr, *this);
    }
  }

  void operator()(const Integer& i) {
    parts.rational_term = parts.rational_term + static_cast<Rational>(i);
  }
  void operator()(const Rational& r) { parts.rational_term = parts.rational_term + r; }
  void operator()(const Float& f) {
    if (!parts.float_term.has_value()) {
      parts.float_term = f;
    } else {
      parts.float_term = (*parts.float_term) + f;
    }
  }

  using ExcludedTypes = type_list<Addition, Integer, Rational, Float, Matrix>;

  template <typename T, typename = enable_if_does_not_contain_type_t<T, ExcludedTypes>>
  void operator()(const T&, const Expr& input_expression) {
    // Everything else: Just add to the coeff
    auto [coeff, mul] = as_coeff_and_mul(input_expression);
    ASSERT(!mul.is_type<Addition>(), "TODO: Should just silently merge cases like this");

    const auto [it, was_inserted] = parts.terms.emplace(std::move(mul), coeff);
    if (!was_inserted) {
      it->second = it->second + coeff;
    }
  }

  AdditionParts& parts;
};

AdditionParts::AdditionParts(const Addition& add) : AdditionParts(add.arity()) {
  for (const Expr& expr : add) {
    add_terms(expr);
  }
  normalize_coefficients();
}

void AdditionParts::add_terms(const Expr& arg) {
  if (is_zero(arg)) {
    return;
  }
  AdditionVisitor visitor{*this};
  visit_with_expr(arg, visitor);
}

void AdditionParts::normalize_coefficients() {
  // Remove anything where the coefficient worked out to zero:
  for (auto it = terms.begin(); it != terms.end();) {
    if (is_zero(it->second)) {
      it = terms.erase(it);
    } else {
      ++it;
    }
  }
}

Expr AdditionParts::create_addition() const {
  Addition::ContainerType args{};
  if (float_term.has_value()) {
    const Float promoted_rational = static_cast<Float>(rational_term);
    args.push_back(make_expr<Float>(float_term.value() + promoted_rational));
  } else if (rational_term.is_zero()) {
    // Don't insert a useless zero in the add.
  } else {
    args.push_back(Rational::create(rational_term));
  }

  args.reserve(args.size() + terms.size());
  std::transform(terms.begin(), terms.end(), std::back_inserter(args),
                 [](const std::pair<Expr, Expr>& pair) {
                   if (is_one(pair.second)) {
                     return pair.first;
                   } else if (!pair.first.is_type<Multiplication>() &&
                              !pair.second.is_type<Multiplication>()) {
                     // We can skip calling FromOperands here because we know the first element in
                     // the pair is the non-numeric value and the second is the numeric coefficient.
                     return make_expr<Multiplication>(pair.second, pair.first);
                   }
                   return Multiplication::from_operands({pair.first, pair.second});
                 });

  if (args.empty()) {
    return Constants::Zero;
  } else if (args.size() == 1) {
    return args.front();
  }

  return make_expr<Addition>(std::move(args));
}

}  // namespace math
