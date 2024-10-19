// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/expressions/addition.h"

#include "wf/expression_visitor.h"
#include "wf/expressions/multiplication.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"

namespace wf {

std::vector<scalar_expr> addition::sorted_terms() const {
  std::vector<scalar_expr> result{terms_.begin(), terms_.end()};
  std::sort(result.begin(), result.end(), order_struct<scalar_expr>{});
  return result;
}

// Define rules for adding constants:
struct add_numeric_constants {
  constexpr integer_constant operator()(const integer_constant a, const integer_constant b) const {
    return integer_constant{a.value() + b.value()};
  }

  // Promote int to rational:
  template <typename B,
            typename = enable_if_contains_type_t<B, integer_constant, rational_constant>>
  constexpr rational_constant operator()(const rational_constant a, const B b) const {
    return a + static_cast<rational_constant>(b);
  }

  // Promote int and rational to float:
  template <typename B, typename = enable_if_contains_type_t<B, float_constant, integer_constant,
                                                             rational_constant>>
  constexpr addition_parts::constant_coeff operator()(const float_constant a, const B b) const {
    if (const float_constant sum = a + static_cast<float_constant>(b); !sum.is_zero()) {
      return sum;
    }
    return integer_constant{0};
  }

  // Anything plus undefined is undefined.
  template <typename B>
  constexpr undefined operator()(const undefined, const B) const noexcept {
    return {};
  }

  // zoo + zoo is undefined.
  constexpr undefined operator()(const complex_infinity, const complex_infinity) const noexcept {
    return {};
  }

  // Anything plus complex infinity is complex infinity, except complex infinity itself and
  // undefined.
  template <typename B,
            typename = enable_if_does_not_contain_type_t<B, undefined, complex_infinity>>
  constexpr complex_infinity operator()(const complex_infinity, const B) const noexcept {
    return {};
  }

  // Disallow implicit conversion to the variant with enable_if guard.
  template <typename V, typename T, typename = enable_if_same_t<V, addition_parts::constant_coeff>>
  constexpr addition_parts::constant_coeff operator()(const V& a, const T b) const {
    return std::visit(
        [b](auto a_concrete) constexpr -> addition_parts::constant_coeff {
          if constexpr (is_invocable_v<add_numeric_constants, decltype(a_concrete), T>) {
            return add_numeric_constants{}(a_concrete, b);
          } else {
            return add_numeric_constants{}(b, a_concrete);
          }
        },
        a);
  }

  static std::optional<scalar_expr> apply(const scalar_expr& a, const scalar_expr& b) {
    const auto coeff = visit_binary(
        a, b, [](const auto& x, const auto& y) -> std::optional<addition_parts::constant_coeff> {
          if constexpr (is_invocable_v<add_numeric_constants, decltype(x), decltype(y)>) {
            return add_numeric_constants{}(x, y);
          } else if constexpr (is_invocable_v<add_numeric_constants, decltype(y), decltype(x)>) {
            return add_numeric_constants{}(y, x);
          } else {
            return std::nullopt;
          }
        });
    if (coeff.has_value()) {
      return std::visit([](auto value) { return scalar_expr(value); }, *coeff);
    }
    return std::nullopt;
  }
};

scalar_expr addition::from_operands(const absl::Span<const scalar_expr> args) {
  if (args.empty()) {
    throw invalid_argument_error("Need at least one operand to construct addition.");
  }
  if (args.size() < 2) {
    return args.front();
  }

  if (args.size() == 2) {
    if (std::optional<scalar_expr> numeric_sum = add_numeric_constants::apply(args[0], args[1]);
        numeric_sum.has_value()) {
      return *std::move(numeric_sum);
    }
  }

#ifdef WF_USE_PMR_MAP
  std::array<char, 1024> buffer{};
  std::pmr::monotonic_buffer_resource pmr{buffer.data(), buffer.size()};
  addition_parts parts{&pmr, args.size()};
#else
  addition_parts parts{args.size()};
#endif
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
      return determine_order(a, b) == relative_order::less_than;
    }
  });
}

void addition_parts::operator()(const addition& arg) {
  for (const scalar_expr& expr : arg) {
    add_terms(expr);
  }
}

template <typename T, typename>
void addition_parts::operator()(const T& value, const scalar_expr& input_expression) {
  if constexpr (type_list_contains_v<T, type_list_from_variant_t<constant_coeff>>) {
    coeff = add_numeric_constants{}(coeff, value);
  } else {
    auto [c, mul] = as_coeff_and_mul(input_expression);
    if (const auto [it, was_inserted] = terms.emplace(std::move(mul), c); !was_inserted) {
      it->second = it->second + c;
    }
  }
}

addition_parts::addition_parts(const addition& add) {
  terms.reserve(add.size());
  for (const scalar_expr& expr : add) {
    add_terms(expr);
  }
  normalize_coefficients();
}

void addition_parts::add_terms(const scalar_expr& arg) { visit(arg, *this); }

void addition_parts::normalize_coefficients() {
  // Remove anything where the coefficient worked out to zero:
  map_erase_if(terms, [](const auto& pair) { return is_zero(pair.second); });
}

scalar_expr addition_parts::create_addition() const {
  addition::container_type args{};

  scalar_expr constant_coeff_expr =
      overloaded_visit(coeff, [](const auto x) { return scalar_expr(x); });

  if (is_undefined(constant_coeff_expr)) {
    return constants::undefined;
  }
  if (!is_zero(constant_coeff_expr)) {
    args.push_back(std::move(constant_coeff_expr));
  }

  args.reserve(args.size() + terms.size());
  for (const auto& [multiplicand, term_coeff] : terms) {
    if (is_one(term_coeff)) {
      args.push_back(multiplicand);
    } else if (!multiplicand.is_type<multiplication>() && !term_coeff.is_type<multiplication>()) {
      // We can skip calling `from_operands` here because we know `multiplicand` is a symbolic
      // expression and `coeff` is a numerical coefficient.
      args.emplace_back(std::in_place_type_t<multiplication>{}, term_coeff, multiplicand);
    } else {
      args.push_back(multiplication::from_operands({multiplicand, term_coeff}));
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
