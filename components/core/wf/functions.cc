#include "wf/functions.h"

#include "wf/common_visitors.h"
#include "wf/expressions/all_expressions.h"
#include "wf/integer_utils.h"
#include "wf/matrix_expression.h"

namespace wf {
using namespace wf::custom_literals;

template <typename Callable>
std::optional<scalar_expr> operate_on_float(const scalar_expr& arg, Callable&& method) {
  if (const float_constant* const f = cast_ptr<const float_constant>(arg); f != nullptr) {
    const auto value = f->get_value();
    const auto result = method(value);
    if (result == value) {
      // Don't allocate if no change occurred.
      return arg;
    } else {
      return scalar_expr(static_cast<float_constant::value_type>(result));
    }
  }
  return {};
}

scalar_expr log(const scalar_expr& x) {
  if (x.is_identical_to(constants::euler)) {
    return constants::one;
  }
  if (is_one(x)) {
    return constants::zero;
  }
  if (is_zero(x)) {
    // log(0) does not exist. In the context of limits, it can be -∞ (but not otherwise).
    return constants::undefined;
  }
  if (is_complex_infinity(x) || is_undefined(x)) {
    return constants::undefined;  //  log(z-∞) is ∞, but we can't represent that.
  }
  if (std::optional<scalar_expr> f =
          operate_on_float(x, static_cast<double (*)(double)>(&std::log));
      f.has_value()) {
    return *std::move(f);
  }
  // TODO: Check for negative values.
  return make_expr<function>(built_in_function::ln, x);
}

scalar_expr pow(const scalar_expr& x, const scalar_expr& y) { return power::create(x, y); }

std::optional<rational_constant> try_cast_to_rational(const scalar_expr& expr) {
  if (const rational_constant* const r = cast_ptr<const rational_constant>(expr); r != nullptr) {
    return *r;
  } else if (const integer_constant* const i = cast_ptr<const integer_constant>(expr);
             i != nullptr) {
    return static_cast<rational_constant>(*i);
  }
  return {};
}

// TODO: Support common multiples of pi/3, pi/4, pi/6, etc.
scalar_expr cos(const scalar_expr& arg) {
  const auto [coeff, multiplicand] = as_coeff_and_mul(arg);
  if (is_pi(multiplicand)) {
    if (const std::optional<rational_constant> r = try_cast_to_rational(coeff); r.has_value()) {
      const rational_constant r_mod_pi = mod_pi_rational(*r);
      // Do some very basic simplification:
      if (r_mod_pi.is_zero()) {
        return constants::one;
      } else if (r_mod_pi.is_one()) {
        return constants::negative_one;
      } else if (r_mod_pi == rational_constant{1, 2} || r_mod_pi == rational_constant{-1, 2}) {
        return constants::zero;
      }
      return make_expr<function>(built_in_function::cos, scalar_expr(r_mod_pi) * constants::pi);
    }
  } else if (is_zero(coeff)) {
    return constants::one;
  }

  // Make signs canonical automatically:
  if (is_negative_number(arg)) {
    return cos(-arg);
  }

  // For floats, evaluate immediately:
  if (std::optional<scalar_expr> result =
          operate_on_float(arg, static_cast<double (*)(double x)>(&std::cos));
      result.has_value()) {
    return *std::move(result);
  }
  if (arg.is_type<complex_infinity>() || is_undefined(arg)) {
    return constants::undefined;
  }
  // TODO: Check for phase offsets.
  return make_expr<function>(built_in_function::cos, arg);
}

scalar_expr sin(const scalar_expr& arg) {
  const auto [coeff, multiplicand] = as_coeff_and_mul(arg);
  if (is_pi(multiplicand)) {
    if (const std::optional<rational_constant> r = try_cast_to_rational(coeff); r.has_value()) {
      const rational_constant r_mod_pi = mod_pi_rational(*r);
      // Do some very basic simplification:
      if (r_mod_pi.is_zero() || r_mod_pi.is_one()) {
        return constants::zero;
      } else if (r_mod_pi == rational_constant{1, 2}) {
        return constants::one;
      } else if (r_mod_pi == rational_constant{-1, 2}) {
        return constants::negative_one;
      }
      return make_expr<function>(built_in_function::sin, scalar_expr(r_mod_pi) * constants::pi);
    }
  } else if (is_zero(arg)) {
    return constants::zero;
  }
  if (is_negative_number(arg)) {
    return -sin(-arg);
  }
  if (std::optional<scalar_expr> result =
          operate_on_float(arg, static_cast<double (*)(double x)>(&std::sin));
      result.has_value()) {
    return *std::move(result);
  }
  if (arg.is_type<complex_infinity>() || is_undefined(arg)) {
    return constants::undefined;
  }
  return make_expr<function>(built_in_function::sin, arg);
}

inline rational_constant convert_to_tan_range(const rational_constant& r) {
  const rational_constant one{1, 1};
  if (r > rational_constant{1, 2}) {
    return r - one;
  } else if (r < rational_constant{-1, 2}) {
    return one + r;
  }
  return r;
}

inline scalar_expr pi_over_two() {
  static const scalar_expr val = constants::pi / 2_s;
  return val;
}

scalar_expr tan(const scalar_expr& arg) {
  const auto [coeff, multiplicand] = as_coeff_and_mul(arg);
  if (is_pi(multiplicand)) {
    if (const std::optional<rational_constant> r = try_cast_to_rational(coeff); r.has_value()) {
      // Map into [-pi/2, pi/2]:
      const rational_constant r_mod_half_pi = convert_to_tan_range(mod_pi_rational(*r));
      // Do some very basic simplification:
      if (r_mod_half_pi.is_zero()) {
        return constants::zero;
      } else if (r_mod_half_pi == rational_constant{1, 2} ||
                 r_mod_half_pi == rational_constant{-1, 2}) {
        // Complex infinity.
        return constants::complex_infinity;
      }
      return make_expr<function>(built_in_function::tan,
                                 scalar_expr(r_mod_half_pi) * pi_over_two());
    }
  } else if (is_zero(arg)) {
    return constants::zero;
  }
  if (is_negative_number(arg)) {
    return -tan(-arg);
  }
  if (std::optional<scalar_expr> result =
          operate_on_float(arg, static_cast<double (*)(double x)>(&std::tan));
      result.has_value()) {
    return *std::move(result);
  }
  if (arg.is_type<complex_infinity>() || is_undefined(arg)) {
    return constants::undefined;
  }
  return make_expr<function>(built_in_function::tan, arg);
}

// TODO: Support inverting trig operations when the interval is specified, ie. acos(cos(x)) -> x
// TODO: Support some common numerical values, ie. acos(1 / sqrt(2)) -> pi/4
scalar_expr acos(const scalar_expr& arg) {
  if (is_zero(arg)) {
    return pi_over_two();
  } else if (is_one(arg)) {
    return constants::zero;
  } else if (is_negative_one(arg)) {
    return constants::pi;
  } else if (is_undefined(arg) || is_complex_infinity(arg)) {
    return constants::undefined;
  }
  return make_expr<function>(built_in_function::arccos, arg);
}

scalar_expr asin(const scalar_expr& arg) {
  if (is_zero(arg)) {
    return constants::zero;
  } else if (is_one(arg)) {
    return pi_over_two();
  } else if (is_negative_one(arg)) {
    return -pi_over_two();
  } else if (is_negative_number(arg)) {
    return -asin(-arg);
  } else if (is_undefined(arg) || is_complex_infinity(arg)) {
    return constants::undefined;
  }
  return make_expr<function>(built_in_function::arcsin, arg);
}

inline scalar_expr pi_over_four() {
  static const scalar_expr val = constants::pi / 4_s;
  return val;
}

scalar_expr atan(const scalar_expr& arg) {
  if (is_zero(arg)) {
    return constants::zero;
  } else if (is_one(arg)) {
    return pi_over_four();
  } else if (is_negative_one(arg)) {
    return -pi_over_four();
  } else if (is_negative_number(arg)) {
    return -atan(-arg);
  } else if (is_undefined(arg) || is_complex_infinity(arg)) {
    return constants::undefined;
  }
  return make_expr<function>(built_in_function::arctan, arg);
}

// Support some very basic simplifications for numerical inputs.
struct atan2_visitor {
  std::optional<scalar_expr> operator()(const float_constant& y, const float_constant& x) const {
    return scalar_expr(std::atan2(y.get_value(), x.get_value()));
  }

  std::optional<scalar_expr> operator()(const integer_constant& y,
                                        const integer_constant& x) const {
    static const scalar_expr pi_over_two = constants::pi / 2;
    static const scalar_expr neg_pi_over_two = -pi_over_two;

    if (y.get_value() == 0 && x.get_value() == 1) {
      return constants::zero;
    } else if (y.get_value() == 1 && x.get_value() == 0) {
      return pi_over_two;
    } else if (y.get_value() == 0 && x.get_value() == -1) {
      return constants::pi;
    } else if (y.get_value() == -1 && x.get_value() == 0) {
      return neg_pi_over_two;
    } else if (y.abs() == x.abs() && x.get_value() != 0) {
      static const std::array<scalar_expr, 4> quadrant_solutions = {
          constants::pi / 4,
          3 * constants::pi / 4,
          -constants::pi / 4,
          -3 * constants::pi / 4,
      };
      return quadrant_solutions[(y.get_value() < 0) * 2 + (x.get_value() < 0)];
    }
    return std::nullopt;
  }

  template <typename A, typename B>
  std::optional<scalar_expr> operator()(const A&, const B&) const {
    if constexpr (std::is_same_v<A, complex_infinity> || std::is_same_v<B, complex_infinity> ||
                  std::is_same_v<A, undefined> || std::is_same_v<B, undefined>) {
      return constants::undefined;
    }
    return std::nullopt;
  }
};

scalar_expr atan2(const scalar_expr& y, const scalar_expr& x) {
  if (std::optional<scalar_expr> maybe_simplified = visit_binary(y, x, atan2_visitor{})) {
    return *std::move(maybe_simplified);
  }
  // TODO: Implement simplifications for atan2.
  return make_expr<function>(built_in_function::arctan2, y, x);
}

scalar_expr sqrt(const scalar_expr& arg) {
  static const scalar_expr one_half = constants::one / 2_s;
  return power::create(arg, one_half);
}

scalar_expr abs(const scalar_expr& arg) {
  if (const function* func = cast_ptr<const function>(arg);
      func != nullptr && func->enum_value() == built_in_function::abs) {
    // abs(abs(x)) --> abs(x)
    return arg;
  }
  if (const std::optional<rational_constant> r = try_cast_to_rational(arg); r.has_value()) {
    // If the inner argument is a negative integer or rational, just flip it.
    if (r->numerator() >= 0) {
      WF_ASSERT_GREATER(r->denominator(), 0);
      return arg;
    }
    return scalar_expr(rational_constant{-r->numerator(), r->denominator()});
  }
  // Evaluate floats immediately:
  if (std::optional<scalar_expr> result =
          operate_on_float(arg, static_cast<double (*)(double x)>(&std::abs));
      result.has_value()) {
    return *std::move(result);
  }
  if (const symbolic_constant* constant = cast_ptr<const symbolic_constant>(arg);
      constant != nullptr) {
    if (const auto as_double = double_from_symbolic_constant(constant->name());
        compare_int_float(0, as_double).value() != relative_order::greater_than) {
      // Constant that is already positive.
      return arg;
    }
  }
  if (is_complex_infinity(arg) || is_undefined(arg)) {
    return constants::undefined;
  }
  // TODO: Add simplifications for real inputs, like powers.
  // TODO: Add simplifications for multiplications.
  return make_expr<function>(built_in_function::abs, arg);
}

// TODO: Add simplifications for expressions like:
//  signum(-x) --> -signum(x)
//  signum(5 * x) --> signum(x)
struct signum_visitor {
  // https://stackoverflow.com/questions/1903954/
  template <typename T>
  static constexpr int sign(T val) noexcept {
    return (static_cast<T>(0) < val) - (val < static_cast<T>(0));
  }

  // scalar_expr constructor will convert to `One` or `negative_one` constants for us
  std::optional<scalar_expr> operator()(const integer_constant& i) const {
    return scalar_expr{sign(i.get_value())};
  }
  std::optional<scalar_expr> operator()(const rational_constant& r) const {
    return scalar_expr{sign(r.numerator())};
  }
  std::optional<scalar_expr> operator()(const float_constant& f) const {
    WF_ASSERT(!f.is_nan());
    return scalar_expr{sign(f.get_value())};
  }

  std::optional<scalar_expr> operator()(const symbolic_constant& c) const {
    // Conversion to float is valid for all the constants we currently support:
    const auto cf = double_from_symbolic_constant(c.name());
    return scalar_expr{sign(cf)};
  }

  std::optional<scalar_expr> operator()(const function& func, const scalar_expr& func_expr) const {
    if (func.enum_value() == built_in_function::signum) {
      // sgn(sgn(x)) --> sgn(x), valid for real and complex
      return func_expr;
    }
    return std::nullopt;
  }

  std::optional<scalar_expr> operator()(const undefined&) const { return constants::undefined; }

  // Handle all other cases.
  template <typename T, typename = enable_if_does_not_contain_type_t<
                            T, integer_constant, rational_constant, float_constant,
                            symbolic_constant, function, undefined>>
  std::optional<scalar_expr> operator()(const T&) const {
    return std::nullopt;
  }
};

scalar_expr signum(const scalar_expr& arg) {
  if (std::optional<scalar_expr> maybe_simplified = visit(arg, signum_visitor{});
      maybe_simplified.has_value()) {
    return *std::move(maybe_simplified);
  }
  return make_expr<function>(built_in_function::signum, arg);
}

struct floor_visitor {
  std::optional<scalar_expr> operator()(const integer_constant&, const scalar_expr& arg) const {
    return arg;
  }

  std::optional<scalar_expr> operator()(const rational_constant& r) const {
    const auto [int_part, _] = r.normalized();
    if (r.is_negative()) {
      // Round down (away from zero) for negative values:
      return scalar_expr{int_part.get_value() - 1};
    }
    return scalar_expr{int_part.get_value()};
  }

  std::optional<scalar_expr> operator()(const float_constant& f) const {
    WF_ASSERT(!f.is_nan());
    const auto floored = std::floor(f.get_value());
    return scalar_expr{static_cast<std::int64_t>(floored)};
  }

  std::optional<scalar_expr> operator()(const function& func, const scalar_expr& arg) const {
    // If the argument is already an integer, floor does nothing:
    if (func.enum_value() == built_in_function::floor) {
      return arg;
    }
    return std::nullopt;
  }

  std::optional<scalar_expr> operator()(const symbolic_constant& c) const {
    const auto cf = double_from_symbolic_constant(c.name());
    return scalar_expr{static_cast<std::int64_t>(std::floor(cf))};
  }

  std::optional<scalar_expr> operator()(const complex_infinity&) const {
    return constants::complex_infinity;
  }
  std::optional<scalar_expr> operator()(const undefined&) const { return constants::undefined; }

  template <typename T, typename = enable_if_does_not_contain_type_t<
                            T, integer_constant, rational_constant, float_constant, function,
                            symbolic_constant, complex_infinity, undefined>>
  std::optional<scalar_expr> operator()(const T&) const noexcept {
    return std::nullopt;
  }
};

scalar_expr floor(const scalar_expr& arg) {
  if (std::optional<scalar_expr> maybe_simplified = visit(arg, floor_visitor{});
      maybe_simplified.has_value()) {
    return *std::move(maybe_simplified);
  }
  return scalar_expr{std::in_place_type_t<function>{}, built_in_function::floor, arg};
}

// Max and min are implemented as conditionals. That way:
// - The same conditionals can be combined in the output code.
// - When differentiating max/min, the selected derivative matches the selected argument.
scalar_expr max(const scalar_expr& a, const scalar_expr& b) { return where(a < b, b, a); }
scalar_expr min(const scalar_expr& a, const scalar_expr& b) { return where(b < a, b, a); }

scalar_expr where(const scalar_expr& condition, const scalar_expr& if_true,
                  const scalar_expr& if_false) {
  return conditional::create(condition, if_true, if_false);
}

matrix_expr where(const scalar_expr& condition, const matrix_expr& if_true,
                  const matrix_expr& if_false) {
  const matrix& mat_true = if_true.as_matrix();
  const matrix& mat_false = if_false.as_matrix();

  // dimensions of left and right operands must match:
  if (mat_true.rows() != mat_false.rows() || mat_true.cols() != mat_false.cols()) {
    throw dimension_error(
        "dimension mismatch between operands to where(). if shape = [{}, {}], else shape = [{}, "
        "{}]",
        mat_true.rows(), mat_true.cols(), mat_false.rows(), mat_false.cols());
  }

  // For now, we just create a matrix of conditionals. Maybe add a conditional matrix type?
  std::vector<scalar_expr> conditionals;
  conditionals.reserve(mat_true.size());
  std::transform(
      mat_true.begin(), mat_true.end(), mat_false.begin(), std::back_inserter(conditionals),
      [&](const scalar_expr& a, const scalar_expr& b) { return where(condition, a, b); });
  return matrix_expr::create(mat_true.rows(), mat_true.cols(), std::move(conditionals));
}

struct bool_cast_visitor {
  std::optional<scalar_expr> operator()(const symbolic_constant& c) const {
    if (c.name() == symbolic_constant_enum::boolean_true) {
      return constants::one;
    } else if (c.name() == symbolic_constant_enum::boolean_false) {
      return constants::zero;
    }
    return std::nullopt;
  }

  std::optional<scalar_expr> operator()(const relational&, const scalar_expr& arg) const {
    return make_expr<cast_bool>(arg);
  }

  template <typename T, typename = enable_if_does_not_contain_type_t<T, relational>>
  std::optional<scalar_expr> operator()(const T&) const noexcept {
    return std::nullopt;
  }
};

scalar_expr cast_int_from_bool(const scalar_expr& bool_expression) {
  std::optional<scalar_expr> result = visit(bool_expression, bool_cast_visitor{});
  if (!result) {
    throw type_error("Expression of type `{}` is not a boolean arg: {}",
                     bool_expression.type_name(), bool_expression);
  }
  return *std::move(result);
}

}  // namespace wf
