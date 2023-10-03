#include "functions.h"

#include "common_visitors.h"
#include "expressions/all_expressions.h"
#include "integer_utils.h"
#include "matrix_expression.h"

namespace math {
using namespace math::custom_literals;

Expr log(const Expr& x) {
  if (x.is_identical_to(Constants::Euler)) {
    return Constants::One;
  }
  if (is_one(x)) {
    return Constants::Zero;
  }
  // TODO: Check for negative values.
  return make_expr<Function>(BuiltInFunctionName::Log, x);
}

Expr pow(const Expr& x, const Expr& y) { return Power::create(x, y); }

template <typename Callable>
std::optional<Expr> OperateOnFloat(const Expr& arg, Callable&& method) {
  if (const Float* const f = cast_ptr<Float>(arg); f != nullptr) {
    const auto value = f->get_value();
    const auto result = method(value);
    if (result == value) {
      // Don't allocate if no change occurred.
      return arg;
    } else {
      return Float::create(static_cast<Float::FloatType>(result));
    }
  }
  return {};
}

std::optional<Rational> TryCastToRational(const Expr& expr) {
  if (const Rational* const r = cast_ptr<Rational>(expr); r != nullptr) {
    return *r;
  } else if (const Integer* const i = cast_ptr<Integer>(expr); i != nullptr) {
    return static_cast<Rational>(*i);
  }
  return {};
}

// TODO: Support common multiples of pi/3, pi/4, pi/6, etc.
Expr cos(const Expr& arg) {
  const auto [coeff, multiplicand] = as_coeff_and_mul(arg);
  if (is_pi(multiplicand)) {
    if (const std::optional<Rational> r = TryCastToRational(coeff); r.has_value()) {
      const Rational r_mod_pi = mod_pi_rational(*r);
      // Do some very basic simplification:
      if (r_mod_pi.is_zero()) {
        return Constants::One;
      } else if (r_mod_pi.is_one()) {
        return Constants::NegativeOne;
      } else if (r_mod_pi == Rational{1, 2} || r_mod_pi == Rational{-1, 2}) {
        return Constants::Zero;
      }
      return make_expr<Function>(BuiltInFunctionName::Cos,
                                Rational::create(r_mod_pi) * Constants::Pi);
    }
  } else if (is_zero(coeff)) {
    return Constants::One;
  }

  // Make signs canonical automatically:
  if (is_negative_number(arg)) {
    return cos(-arg);
  }

  // For floats, evaluate immediately:
  if (std::optional<Expr> result = OperateOnFloat(arg, [](double x) { return std::cos(x); });
      result.has_value()) {
    return *result;
  }
  // TODO: Check for phase offsets.
  return make_expr<Function>(BuiltInFunctionName::Cos, arg);
}

Expr sin(const Expr& arg) {
  const auto [coeff, multiplicand] = as_coeff_and_mul(arg);
  if (is_pi(multiplicand)) {
    if (const std::optional<Rational> r = TryCastToRational(coeff); r.has_value()) {
      const Rational r_mod_pi = mod_pi_rational(*r);
      // Do some very basic simplification:
      if (r_mod_pi.is_zero() || r_mod_pi.is_one()) {
        return Constants::Zero;
      } else if (r_mod_pi == Rational{1, 2}) {
        return Constants::One;
      } else if (r_mod_pi == Rational{-1, 2}) {
        return Constants::NegativeOne;
      }
      return make_expr<Function>(BuiltInFunctionName::Sin,
                                Rational::create(r_mod_pi) * Constants::Pi);
    }
  } else if (is_zero(arg)) {
    return Constants::Zero;
  }
  if (is_negative_number(arg)) {
    return -sin(-arg);
  }
  if (std::optional<Expr> result = OperateOnFloat(arg, [](double x) { return std::sin(x); });
      result.has_value()) {
    return *result;
  }
  return make_expr<Function>(BuiltInFunctionName::Sin, arg);
}

inline Rational ConvertToTanRange(const Rational& r) {
  const Rational one{1, 1};
  if (r > Rational{1, 2}) {
    return r - one;
  } else if (r < Rational{-1, 2}) {
    return one + r;
  }
  return r;
}

inline Expr PiOverTwo() {
  static const Expr Value = Constants::Pi / 2_s;
  return Value;
}

Expr tan(const Expr& arg) {
  const auto [coeff, multiplicand] = as_coeff_and_mul(arg);
  if (is_pi(multiplicand)) {
    if (const std::optional<Rational> r = TryCastToRational(coeff); r.has_value()) {
      // Map into [-pi/2, pi/2]:
      const Rational r_mod_half_pi = ConvertToTanRange(mod_pi_rational(*r));
      // Do some very basic simplification:
      if (r_mod_half_pi.is_zero()) {
        return Constants::Zero;
      } else if (r_mod_half_pi == Rational{1, 2} || r_mod_half_pi == Rational{-1, 2}) {
        // Infinity, as in the projectively scaled real numbers.
        return Constants::Infinity;
      }
      return make_expr<Function>(BuiltInFunctionName::Tan,
                                Rational::create(r_mod_half_pi) * PiOverTwo());
    }
  } else if (is_zero(arg)) {
    return Constants::Zero;
  }
  if (is_negative_number(arg)) {
    return -tan(-arg);
  }
  if (std::optional<Expr> result = OperateOnFloat(arg, [](double x) { return std::tan(x); });
      result.has_value()) {
    return *result;
  }
  return make_expr<Function>(BuiltInFunctionName::Tan, arg);
}

// TODO: Support inverting trig operations when the interval is specified, ie. acos(cos(x)) -> x
// TODO: Support some common numerical values, ie. acos(1 / sqrt(2)) -> pi/4
Expr acos(const Expr& arg) {
  if (is_zero(arg)) {
    return PiOverTwo();
  } else if (is_one(arg)) {
    return Constants::Zero;
  } else if (is_negative_one(arg)) {
    return Constants::Pi;
  }
  return make_expr<Function>(BuiltInFunctionName::ArcCos, arg);
}

Expr asin(const Expr& arg) {
  if (is_zero(arg)) {
    return Constants::Zero;
  } else if (is_one(arg)) {
    return PiOverTwo();
  } else if (is_negative_one(arg)) {
    return -PiOverTwo();
  } else if (is_negative_number(arg)) {
    return -asin(-arg);
  }
  return make_expr<Function>(BuiltInFunctionName::ArcSin, arg);
}

inline Expr PiOverFour() {
  static const Expr Value = Constants::Pi / 4_s;
  return Value;
}

Expr atan(const Expr& arg) {
  if (is_zero(arg)) {
    return Constants::Zero;
  } else if (is_one(arg)) {
    return PiOverFour();
  } else if (is_negative_one(arg)) {
    return -PiOverFour();
  } else if (is_negative_number(arg)) {
    return -atan(-arg);
  }
  return make_expr<Function>(BuiltInFunctionName::ArcTan, arg);
}

// Support some very basic simplifications for numerical inputs.
struct Atan2Visitor {
  std::optional<Expr> operator()(const Float& y, const Float& x) const {
    return Float::create(std::atan2(y.get_value(), x.get_value()));
  }

  std::optional<Expr> operator()(const Integer& y, const Integer& x) const {
    static const Expr pi_over_two = Constants::Pi / 2;
    static const Expr neg_pi_over_two = -pi_over_two;

    if (y.get_value() == 0 && x.get_value() == 1) {
      return Constants::Zero;
    } else if (y.get_value() == 1 && x.get_value() == 0) {
      return pi_over_two;
    } else if (y.get_value() == 0 && x.get_value() == -1) {
      return Constants::Pi;
    } else if (y.get_value() == -1 && x.get_value() == 0) {
      return neg_pi_over_two;
    } else if (y.abs() == x.abs() && x.get_value() != 0) {
      static const std::array<Expr, 4> quadrant_solutions = {
          Constants::Pi / 4,
          3 * Constants::Pi / 4,
          -Constants::Pi / 4,
          -3 * Constants::Pi / 4,
      };
      return quadrant_solutions[(y.get_value() < 0) * 2 + (x.get_value() < 0)];
    }
    return std::nullopt;
  }

  template <typename A, typename B>
  std::optional<Expr> operator()(const A&, const B&) const {
    return std::nullopt;
  }
};

Expr atan2(const Expr& y, const Expr& x) {
  std::optional<Expr> maybe_simplified = VisitBinary(y, x, Atan2Visitor{});
  if (maybe_simplified) {
    return std::move(*maybe_simplified);
  }
  // TODO: Implement simplifications for atan2.
  return make_expr<Function>(BuiltInFunctionName::Arctan2, y, x);
}

Expr sqrt(const Expr& arg) {
  static const Expr one_half = Constants::One / 2_s;
  return Power::create(arg, one_half);
}

Expr abs(const Expr& arg) {
  if (const Function* func = cast_ptr<Function>(arg);
      func != nullptr && func->enum_value() == BuiltInFunctionName::Abs) {
    // abs(abs(x)) --> abs(x)
    return arg;
  }
  if (const std::optional<Rational> r = TryCastToRational(arg); r.has_value()) {
    // If the inner argument is a negative integer or rational, just flip it.
    if (r->numerator() >= 0) {
      ASSERT_GREATER(r->denominator(), 0);
      return arg;
    }
    return Rational::create(-r->numerator(), r->denominator());
  }
  // Evaluate floats immediately:
  if (std::optional<Expr> result = OperateOnFloat(arg, [](double x) { return std::abs(x); });
      result.has_value()) {
    return *result;
  }
  if (const Constant* constant = cast_ptr<Constant>(arg); constant != nullptr) {
    const auto as_double = double_from_symbolic_constant(constant->name());
    if (compare_int_float(0, as_double).value() != RelativeOrder::GreaterThan) {
      // Constant that is already positive.
      return arg;
    }
  }
  // TODO: Add simplifications for real inputs, like powers.
  // TODO: Add simplifications for multiplications.
  return make_expr<Function>(BuiltInFunctionName::Abs, arg);
}

// TODO: Add simplifications for expressions like:
//  signum(-x) --> -signum(x)
//  signum(5 * x) --> signum(x)
struct SignumVisitor {
  // https://stackoverflow.com/questions/1903954/
  template <typename T>
  static constexpr int sign(T val) {
    return (static_cast<T>(0) < val) - (val < static_cast<T>(0));
  }

  // Expr constructor will convert to `One` or `NegativeOne` constants for us
  std::optional<Expr> operator()(const Integer& i) const { return Expr{sign(i.get_value())}; }
  std::optional<Expr> operator()(const Rational& r) const { return Expr{sign(r.numerator())}; }
  std::optional<Expr> operator()(const Float& f) const {
    ASSERT(!std::isnan(f.get_value()));
    return Expr{sign(f.get_value())};
  }

  std::optional<Expr> operator()(const Constant& c) const {
    // Conversion to float is valid for all the constants we currently support:
    const auto cf = double_from_symbolic_constant(c.name());
    return Expr{sign(cf)};
  }

  std::optional<Expr> operator()(const Function& func, const Expr& func_expr) const {
    if (func.enum_value() == BuiltInFunctionName::Signum) {
      // sgn(sgn(x)) --> sgn(x), valid for real and complex
      return func_expr;
    }
    return std::nullopt;
  }

  // Handle all other cases.
  template <typename T,
            typename = enable_if_does_not_contain_type_t<T, Integer, Rational, Float, Constant, Function>>
  std::optional<Expr> operator()(const T&) const {
    return std::nullopt;
  }
};

Expr signum(const Expr& arg) {
  std::optional<Expr> maybe_simplified = VisitWithExprArg(arg, SignumVisitor{});
  if (maybe_simplified) {
    return std::move(*maybe_simplified);
  }
  return make_expr<Function>(BuiltInFunctionName::Signum, arg);
}

// Max and min are implemented as conditionals. That way:
// - The same conditionals can be combined in the output code.
// - When differentiating max/min, the selected derivative matches the selected argument.
Expr max(const Expr& a, const Expr& b) { return where(a < b, b, a); }
Expr min(const Expr& a, const Expr& b) { return where(b < a, b, a); }

Expr where(const Expr& condition, const Expr& if_true, const Expr& if_false) {
  return Conditional::create(condition, if_true, if_false);
}

MatrixExpr where(const Expr& condition, const MatrixExpr& if_true, const MatrixExpr& if_false) {
  const Matrix& mat_true = if_true.as_matrix();
  const Matrix& mat_false = if_false.as_matrix();

  // dimensions of left and right operands must match:
  if (mat_true.rows() != mat_false.rows() || mat_true.cols() != mat_false.cols()) {
    throw DimensionError(
        "dimension mismatch between operands to where(). if shape = [{}, {}], else shape = [{}, "
        "{}]",
        mat_true.rows(), mat_true.cols(), mat_false.rows(), mat_false.cols());
  }

  // For now, we just create a matrix of conditionals. Maybe add a conditional matrix type?
  std::vector<Expr> conditionals;
  conditionals.reserve(mat_true.size());
  std::transform(mat_true.begin(), mat_true.end(), mat_false.begin(),
                 std::back_inserter(conditionals),
                 [&](const Expr& a, const Expr& b) { return where(condition, a, b); });
  return MatrixExpr::create(mat_true.rows(), mat_true.cols(), std::move(conditionals));
}

}  // namespace math
