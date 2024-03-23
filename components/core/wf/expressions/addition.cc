// Copyright 2023 Gareth Cross
#include "wf/expressions/addition.h"

#include "wf/expression_visitor.h"
#include "wf/expressions/multiplication.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"

namespace wf {

std::vector<scalar_expr> addition::sorted_terms() const {
  std::vector<scalar_expr> result{terms_.begin(), terms_.end()};
  std::sort(result.begin(), result.end(), wf::expression_order_struct{});
  return result;
}

scalar_expr addition::from_operands(const absl::Span<const scalar_expr> args) {
  WF_ASSERT(!args.empty(), "Cannot call from_operands with an empty span.");
  if (args.size() < 2) {
    return args.front();
  }

  if (any_of(args, &is_undefined)) {
    return constants::undefined;
  }

  // TODO: extract common denominator?
  addition_parts parts{args.size()};
  for (const scalar_expr& arg : args) {
    parts.add_terms(arg);
  }
  parts.normalize_coefficients();
  return parts.create_addition();
}

void addition::sort_terms() {
  // Place into a deterministic (but otherwise mostly arbitrary) order.
  std::sort(terms_.begin(), terms_.end(), [](const scalar_expr& a, const scalar_expr& b) {
    if (a.hash() < b.hash()) {
      return true;
    } else if (a.hash() > b.hash()) {
      return false;
    } else {
      // There could be a collision, so we fall back to a slow path here.
      return expression_order_struct{}(a, b);
    }
  });
}

void addition_parts::operator()(const addition& arg) {
  for (const scalar_expr& expr : arg) {
    add_terms(expr);
  }
}

void addition_parts::operator()(const integer_constant& i) {
  rational_term = rational_term + static_cast<rational_constant>(i);
}

void addition_parts::operator()(const rational_constant& r) { rational_term = rational_term + r; }

void addition_parts::operator()(const float_constant& f) noexcept {
  float_term = float_term.value_or(float_constant{0.0}) + f;
}

void addition_parts::operator()(const complex_infinity&) noexcept { ++num_infinities; }

// This method handles other things that are _not_:
// - numeric constants
// - complex infinity
// - additions
template <typename T, typename>
void addition_parts::operator()(const T&, const scalar_expr& input_expression) {
  auto [coeff, mul] = as_coeff_and_mul(input_expression);
  if (const auto [it, was_inserted] = terms.emplace(std::move(mul), coeff); !was_inserted) {
    it->second = it->second + coeff;
  }
}

addition_parts::addition_parts(const addition& add) : addition_parts(add.size()) {
  for (const scalar_expr& expr : add) {
    add_terms(expr);
  }
  normalize_coefficients();
}

void addition_parts::add_terms(const scalar_expr& arg) {
  if (is_zero(arg)) {
    return;
  }
  visit(arg, *this);
}

void addition_parts::normalize_coefficients() {
  // Remove anything where the coefficient worked out to zero:
  map_erase_if(terms, [](const auto& pair) { return is_zero(pair.second); });
}

scalar_expr addition_parts::create_addition() const {
  addition::container_type args{};

  // handle infinities in the input
  const bool includes_infinity = num_infinities > 0;
  if (num_infinities > 1) {
    // any additive/subtractive combination of complex infinities is undefined
    return constants::undefined;
  } else if (includes_infinity) {
    args.push_back(constants::complex_infinity);
  }

  // If we didn't add infinity, now consider float/rational. Otherwise numeric values are folded
  // into complex infinity.
  if (!includes_infinity) {
    if (float_term.has_value()) {
      args.push_back(scalar_expr(static_cast<float_constant>(rational_term) + *float_term));
    } else if (rational_term.is_zero()) {
      // Don't insert a useless zero in the add.
    } else {
      args.push_back(scalar_expr(rational_term));
    }
  }

  args.reserve(args.size() + terms.size());
  for (const auto& [multiplicand, coeff] : terms) {
    if (is_one(coeff)) {
      args.push_back(multiplicand);
    } else if (!multiplicand.is_type<multiplication>() && !coeff.is_type<multiplication>()) {
      // We can skip calling `from_operands` here because we know `multiplicand` is a symbolic
      // expression and `coeff` is a numerical coefficient.
      args.emplace_back(std::in_place_type_t<multiplication>{}, coeff, multiplicand);
    } else {
      args.push_back(multiplication::from_operands({multiplicand, coeff}));
    }
  }

  if (args.empty()) {
    return constants::zero;
  } else if (args.size() == 1) {
    return std::move(args.front());
  }
  return make_expr<addition>(std::move(args));
}

}  // namespace wf
