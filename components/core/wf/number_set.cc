// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/number_set.h"

#include "wf/enumerations.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"

namespace wf {

constexpr static bool is_non_negative(const number_set set) noexcept {
  return set == number_set::real_non_negative || set == number_set::real_positive;
}

constexpr static bool contains_zero(const number_set set) noexcept {
  return set != number_set::real_positive;
}

constexpr number_set combine_sets_add(number_set a, number_set b) noexcept {
  if (b < a) {
    return combine_sets_add(b, a);
  }
  static_assert(number_set::real_positive < number_set::real_non_negative);
  if (b == number_set::real_non_negative && a == number_set::real_positive) {
    // if (x > 0) and (y >= 0) then (x + y > 0)
    return number_set::real_positive;
  }
  return b;  //  take the larger value in the enum
}

constexpr number_set combine_sets_mul(number_set a, number_set b) noexcept {
  return std::max(a, b);
}

class determine_set_visitor {
 public:
  template <typename Container, typename Callable>
  number_set handle_add_or_mul(const Container& container, Callable callable) const {
    WF_ASSERT_GE(container.size(), 1);
    auto it = container.begin();
    number_set set = determine_numeric_set(*it);
    for (it = std::next(it); it != container.end(); ++it) {
      if (set == number_set::unknown) {
        // Can't become less specific than unknown.
        return set;
      }
      set = callable(set, determine_numeric_set(*it));
    }
    return set;
  }

  number_set operator()(const addition& add) const {
    return handle_add_or_mul(add, &combine_sets_add);
  }

  constexpr number_set operator()(const iverson_bracket&) const noexcept {
    return number_set::real_non_negative;
  }

  number_set operator()(const compound_expression_element& el) const {
    auto overloads = make_overloaded(
        [&](const external_function_invocation& invocation) -> number_set {
          return overloaded_visit(
              invocation.function().return_type(),
              [](const scalar_type) constexpr { return number_set::real; },
              [](const matrix_type) constexpr { return number_set::real; },
              [&](const custom_type&) constexpr -> number_set {
                // TODO: When integer is added, we should reason about that here.
                return number_set::real;
              });
        },
        [](const custom_type_argument&) -> number_set {
          // TODO: Reason about real vs integer.
          return number_set::real;
        },
        [&](const custom_type_construction& construct) {
          const auto& element = construct.at(el.index());
          WF_ASSERT(std::holds_alternative<scalar_expr>(element));
          return visit(std::get<scalar_expr>(element), *this);
        });
    return visit(el.provenance(), overloads);
  }

  number_set operator()(const conditional& cond) const {
    const number_set left = determine_numeric_set(cond.if_branch());
    const number_set right = determine_numeric_set(cond.else_branch());
    return std::max(left, right);
  }

  constexpr number_set operator()(const symbolic_constant&) const noexcept {
    return number_set::real_positive;
  }

  constexpr number_set operator()(const derivative&) const noexcept { return number_set::unknown; }

  number_set operator()(const multiplication& mul) const {
    return handle_add_or_mul(mul, &combine_sets_mul);
  }

  number_set operator()(const built_in_function_invocation& func) const {
    absl::InlinedVector<number_set, 4> args{};
    std::transform(func.begin(), func.end(), std::back_inserter(args), &determine_numeric_set);
    WF_ASSERT_GE(args.size(), 1);

    if (std::count(args.begin(), args.end(), number_set::unknown) > 0) {
      return number_set::unknown;
    }

    switch (func.enum_value()) {
      case built_in_function::cos:
      case built_in_function::sin: {
        if (is_real_set(args[0])) {
          return number_set::real;
        }
        return number_set::complex;
      }
      case built_in_function::tan:
        // Any real argument could be +/- pi/2, which is unknown
        return number_set::unknown;
      case built_in_function::arccos:
      case built_in_function::arcsin:
      case built_in_function::arctan:
        return number_set::unknown;  //  TODO: implement inverse trig functions.
      case built_in_function::cosh: {
        if (is_real_set(args[0])) {
          // cosh(x) >= 1 for real x
          return number_set::real_positive;
        }
        return number_set::unknown;
      }
      case built_in_function::sinh:
      case built_in_function::tanh: {
        if (is_real_set(args[0])) {
          // sinh(x) and tanh(x) are odd on the real number line.
          return args[0];
        }
        return number_set::unknown;
      }
      case built_in_function::arccosh:
      case built_in_function::arcsinh:
      case built_in_function::arctanh:
        return number_set::unknown;
      case built_in_function::log: {
        if (args[0] == number_set::real_positive) {
          return number_set::real_positive;
        }
        // Otherwise could be zero.
        return number_set::unknown;
      }
      case built_in_function::abs: {
        if (args[0] == number_set::real_positive) {
          return number_set::real_positive;
        }
        return number_set::real_non_negative;
      }
      case built_in_function::signum: {
        if (is_real_set(args[0])) {
          return args[0];
        }
        return number_set::unknown;
      }
      case built_in_function::floor: {
        if (args[0] == number_set::real_positive) {
          // floor(x) where x > 0 could be 0, so floor(x) >= 0
          return number_set::real_non_negative;
        }
        return args[0];
      }
      case built_in_function::arctan2:
        // Can always be undefined.
        return number_set::unknown;
    }
    throw type_error("Invalid function: {}\n", func.function_name());
  }

  // In spite of its name, complex infinity is not part of the complex numbers.
  constexpr number_set operator()(const complex_infinity&) const noexcept {
    return number_set::unknown;
  }

  template <typename T>
  constexpr number_set handle_numeric(const T& val) const noexcept {
    if (val.is_positive()) {
      return number_set::real_positive;
    }
    if (!val.is_negative()) {
      return number_set::real_non_negative;
    }
    return number_set::real;
  }

  constexpr number_set operator()(const imaginary_unit&) const noexcept {
    return number_set::complex;
  }

  constexpr number_set operator()(const integer_constant& i) const noexcept {
    return handle_numeric(i);
  }
  constexpr number_set operator()(const float_constant& f) const noexcept {
    return handle_numeric(f);
  }

  number_set operator()(const power& pow) const {
    const number_set base = determine_numeric_set(pow.base());
    const number_set exp = determine_numeric_set(pow.exponent());
    if (base == number_set::complex || exp == number_set::complex || base == number_set::unknown ||
        exp == number_set::unknown) {
      return number_set::unknown;
    }

    if (contains_zero(base) && !is_non_negative(exp)) {
      // If the exponent might be negative, we can't say confidently this isn't a division by zero.
      return number_set::unknown;
    }

    if (is_real_set(base)) {
      if (const integer_constant* exp_int = get_if<const integer_constant>(pow.exponent());
          exp_int != nullptr && exp_int->is_even()) {
        return contains_zero(base) ? number_set::real_non_negative : number_set::real_positive;
      }
      if (is_non_negative(base) && is_non_negative(exp)) {
        // base is either real_non_negative or real_positive
        return base;
      }
      return number_set::real;
    }
    return number_set::unknown;
  }

  constexpr number_set operator()(const rational_constant& r) const noexcept {
    return handle_numeric(r);
  }

  // Relational is always 0 or 1, so it must be non-negative.
  constexpr number_set operator()(const relational&) const noexcept {
    return number_set::real_non_negative;
  }

  constexpr number_set operator()(const symbolic_function_invocation&) const noexcept {
    // TODO: Support specifying set on the function.
    return number_set::unknown;
  }

  constexpr number_set operator()(const substitution&) const noexcept {
    // We cannot reason about this without knowing details of the function.
    return number_set::unknown;
  }

  constexpr number_set operator()(const undefined&) const noexcept {
    // Cannot establish
    return number_set::unknown;
  }

  number_set operator()(const unevaluated& u) const { return determine_numeric_set(u.contents()); }

  constexpr number_set operator()(const variable& var) const noexcept { return var.set(); }
};

number_set determine_numeric_set(const scalar_expr& x) { return visit(x, determine_set_visitor{}); }

}  // namespace wf
