#include "functions.h"

#include "common_visitors.h"
#include "expressions/all_expressions.h"
#include "integer_utils.h"
#include "matrix_expression.h"

namespace math {
using namespace math::custom_literals;

Expr log(const Expr& x) {
  if (x.IsIdenticalTo(Constants::Euler)) {
    return Constants::One;
  }
  if (IsOne(x)) {
    return Constants::Zero;
  }
  // TODO: Check for negative values.
  return MakeExpr<Function>(BuiltInFunctionName::Log, x);
}

Expr pow(const Expr& x, const Expr& y) { return Power::Create(x, y); }

template <typename Callable>
std::optional<Expr> OperateOnFloat(const Expr& arg, Callable&& method) {
  if (const Float* const f = CastPtr<Float>(arg); f != nullptr) {
    const auto value = f->GetValue();
    const auto result = method(value);
    if (result == value) {
      // Don't allocate if no change occurred.
      return arg;
    } else {
      return Float::Create(static_cast<Float::FloatType>(result));
    }
  }
  return {};
}

std::optional<Rational> TryCastToRational(const Expr& expr) {
  if (const Rational* const r = CastPtr<Rational>(expr); r != nullptr) {
    return *r;
  } else if (const Integer* const i = CastPtr<Integer>(expr); i != nullptr) {
    return static_cast<Rational>(*i);
  }
  return {};
}

// TODO: Support common multiples of pi/3, pi/4, pi/6, etc.
Expr cos(const Expr& arg) {
  const auto [coeff, multiplicand] = AsCoefficientAndMultiplicand(arg);
  if (IsPi(multiplicand)) {
    if (const std::optional<Rational> r = TryCastToRational(coeff); r.has_value()) {
      const Rational r_mod_pi = ModPiRational(*r);
      // Do some very basic simplification:
      if (r_mod_pi.IsZero()) {
        return Constants::One;
      } else if (r_mod_pi.IsOne()) {
        return Constants::NegativeOne;
      } else if (r_mod_pi == Rational{1, 2} || r_mod_pi == Rational{-1, 2}) {
        return Constants::Zero;
      }
      return MakeExpr<Function>(BuiltInFunctionName::Cos,
                                Rational::Create(r_mod_pi) * Constants::Pi);
    }
  } else if (IsZero(coeff)) {
    return Constants::One;
  }

  // Make signs canonical automatically:
  if (IsNegativeNumber(arg)) {
    return cos(-arg);
  }

  // For floats, evaluate immediately:
  if (std::optional<Expr> result = OperateOnFloat(arg, [](double x) { return std::cos(x); });
      result.has_value()) {
    return *result;
  }
  // TODO: Check for phase offsets.
  return MakeExpr<Function>(BuiltInFunctionName::Cos, arg);
}

Expr sin(const Expr& arg) {
  const auto [coeff, multiplicand] = AsCoefficientAndMultiplicand(arg);
  if (IsPi(multiplicand)) {
    if (const std::optional<Rational> r = TryCastToRational(coeff); r.has_value()) {
      const Rational r_mod_pi = ModPiRational(*r);
      // Do some very basic simplification:
      if (r_mod_pi.IsZero() || r_mod_pi.IsOne()) {
        return Constants::Zero;
      } else if (r_mod_pi == Rational{1, 2}) {
        return Constants::One;
      } else if (r_mod_pi == Rational{-1, 2}) {
        return Constants::NegativeOne;
      }
      return MakeExpr<Function>(BuiltInFunctionName::Sin,
                                Rational::Create(r_mod_pi) * Constants::Pi);
    }
  } else if (IsZero(arg)) {
    return Constants::Zero;
  }
  if (IsNegativeNumber(arg)) {
    return -sin(-arg);
  }
  if (std::optional<Expr> result = OperateOnFloat(arg, [](double x) { return std::sin(x); });
      result.has_value()) {
    return *result;
  }
  return MakeExpr<Function>(BuiltInFunctionName::Sin, arg);
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
  const auto [coeff, multiplicand] = AsCoefficientAndMultiplicand(arg);
  if (IsPi(multiplicand)) {
    if (const std::optional<Rational> r = TryCastToRational(coeff); r.has_value()) {
      // Map into [-pi/2, pi/2]:
      const Rational r_mod_half_pi = ConvertToTanRange(ModPiRational(*r));
      // Do some very basic simplification:
      if (r_mod_half_pi.IsZero()) {
        return Constants::Zero;
      } else if (r_mod_half_pi == Rational{1, 2} || r_mod_half_pi == Rational{-1, 2}) {
        // Infinity, as in the projectively scaled real numbers.
        return Constants::Infinity;
      }
      return MakeExpr<Function>(BuiltInFunctionName::Tan,
                                Rational::Create(r_mod_half_pi) * PiOverTwo());
    }
  } else if (IsZero(arg)) {
    return Constants::Zero;
  }
  if (IsNegativeNumber(arg)) {
    return -tan(-arg);
  }
  if (std::optional<Expr> result = OperateOnFloat(arg, [](double x) { return std::tan(x); });
      result.has_value()) {
    return *result;
  }
  return MakeExpr<Function>(BuiltInFunctionName::Tan, arg);
}

// TODO: Support inverting trig operations when the interval is specified, ie. acos(cos(x)) -> x
// TODO: Support some common numerical values, ie. acos(1 / sqrt(2)) -> pi/4
Expr acos(const Expr& arg) {
  if (IsZero(arg)) {
    return PiOverTwo();
  } else if (IsOne(arg)) {
    return Constants::Zero;
  } else if (IsNegativeOne(arg)) {
    return Constants::Pi;
  }
  return MakeExpr<Function>(BuiltInFunctionName::ArcCos, arg);
}

Expr asin(const Expr& arg) {
  if (IsZero(arg)) {
    return Constants::Zero;
  } else if (IsOne(arg)) {
    return PiOverTwo();
  } else if (IsNegativeOne(arg)) {
    return -PiOverTwo();
  } else if (IsNegativeNumber(arg)) {
    return -asin(-arg);
  }
  return MakeExpr<Function>(BuiltInFunctionName::ArcSin, arg);
}

inline Expr PiOverFour() {
  static const Expr Value = Constants::Pi / 4_s;
  return Value;
}

Expr atan(const Expr& arg) {
  if (IsZero(arg)) {
    return Constants::Zero;
  } else if (IsOne(arg)) {
    return PiOverFour();
  } else if (IsNegativeOne(arg)) {
    return -PiOverFour();
  } else if (IsNegativeNumber(arg)) {
    return -atan(-arg);
  }
  return MakeExpr<Function>(BuiltInFunctionName::ArcTan, arg);
}

// Support some very basic simplifications for numerical inputs.
struct Atan2Visitor {
  std::optional<Expr> operator()(const Float& y, const Float& x) const {
    return Float::Create(std::atan2(y.GetValue(), x.GetValue()));
  }

  std::optional<Expr> operator()(const Integer& y, const Integer& x) const {
    static const Expr pi_over_two = Constants::Pi / 2;
    static const Expr neg_pi_over_two = -pi_over_two;

    if (y.GetValue() == 0 && x.GetValue() == 1) {
      return Constants::Zero;
    } else if (y.GetValue() == 1 && x.GetValue() == 0) {
      return pi_over_two;
    } else if (y.GetValue() == 0 && x.GetValue() == -1) {
      return Constants::Pi;
    } else if (y.GetValue() == -1 && x.GetValue() == 0) {
      return neg_pi_over_two;
    } else if (y.Abs() == x.Abs() && x.GetValue() != 0) {
      static const std::array<Expr, 4> quadrant_solutions = {
          Constants::Pi / 4,
          3 * Constants::Pi / 4,
          -Constants::Pi / 4,
          -3 * Constants::Pi / 4,
      };
      return quadrant_solutions[(y.GetValue() < 0) * 2 + (x.GetValue() < 0)];
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
  return MakeExpr<Function>(BuiltInFunctionName::Arctan2, y, x);
}

Expr sqrt(const Expr& arg) {
  static const Expr one_half = Constants::One / 2_s;
  return Power::Create(arg, one_half);
}

Expr abs(const Expr& arg) {
  if (const Function* func = CastPtr<Function>(arg);
      func != nullptr && func->Func() == BuiltInFunctionName::Abs) {
    // abs(abs(x)) --> abs(x)
    return arg;
  }
  if (const std::optional<Rational> r = TryCastToRational(arg); r.has_value()) {
    // If the inner argument is a negative integer or rational, just flip it.
    if (r->Numerator() >= 0) {
      ASSERT_GREATER(r->Denominator(), 0);
      return arg;
    }
    return Rational::Create(-r->Numerator(), r->Denominator());
  }
  // Evaluate floats immediately:
  if (std::optional<Expr> result = OperateOnFloat(arg, [](double x) { return std::abs(x); });
      result.has_value()) {
    return *result;
  }
  if (const Constant* constant = CastPtr<Constant>(arg); constant != nullptr) {
    const auto as_double = DoubleFromSymbolicConstant(constant->GetName());
    if (CompareIntFloat(0, as_double).value() != RelativeOrder::GreaterThan) {
      // Constant that is already positive.
      return arg;
    }
  }
  // TODO: Add simplifications for real inputs, like powers.
  // TODO: Add simplifications for multiplications.
  return MakeExpr<Function>(BuiltInFunctionName::Abs, arg);
}

Expr where(const Expr& condition, const Expr& if_true, const Expr& if_false) {
  const Matrix* mat_true = CastPtr<Matrix>(if_true);
  const Matrix* mat_false = CastPtr<Matrix>(if_false);
  if (mat_true || mat_false) {
    if (!mat_true || !mat_false) {
      throw TypeError(
          "Cannot mix matrix and scalar expressions in the if/else clauses. if = {}, else = {}",
          if_true.TypeName(), if_false.TypeName());
    }

    // dimensions of left and right operands must match:
    if (mat_true->NumRows() != mat_false->NumRows() ||
        mat_true->NumCols() != mat_false->NumCols()) {
      throw DimensionError(
          "Dimension mismatch between operands to where(). if shape = [{}, {}], else shape = [{}, "
          "{}]",
          mat_true->NumRows(), mat_true->NumCols(), mat_false->NumRows(), mat_false->NumCols());
    }

    // For now, we just create a matrix of conditionals. Maybe add a conditional matrix type?
    std::vector<Expr> conditionals;
    conditionals.reserve(mat_true->Size());
    std::transform(mat_true->begin(), mat_true->end(), mat_false->begin(),
                   std::back_inserter(conditionals),
                   [&](const Expr& a, const Expr& b) { return where(condition, a, b); });
    return MatrixExpr::Create(mat_true->NumRows(), mat_true->NumCols(), std::move(conditionals))
        .AsExpr();
  }
  return Conditional::Create(condition, if_true, if_false);
}

}  // namespace math
