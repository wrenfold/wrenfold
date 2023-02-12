#include "functions.h"

#include "common_visitors.h"
#include "expressions/all_expressions.h"

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
  return MakeExpr<UnaryFunction>(UnaryFunctionName::Log, x);
}

Expr pow(const Expr& x, const Expr& y) { return Power::Create(x, y); }

template <typename Callable>
std::optional<Expr> OperateOnFloat(const Expr& arg, Callable&& method) {
  if (const Float* const f = TryCast<Float>(arg); f != nullptr) {
    const auto value = f->GetValue();
    return Float::Create(method(value));
  }
  return {};
}

std::optional<Rational> TryCastToRational(const Expr& expr) {
  if (const Rational* const r = TryCast<Rational>(expr); r != nullptr) {
    return *r;
  } else if (const Integer* const i = TryCast<Integer>(expr); i != nullptr) {
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
      return MakeExpr<UnaryFunction>(UnaryFunctionName::Cos,
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
  return MakeExpr<UnaryFunction>(UnaryFunctionName::Cos, arg);
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
      return MakeExpr<UnaryFunction>(UnaryFunctionName::Sin,
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
  return MakeExpr<UnaryFunction>(UnaryFunctionName::Sin, arg);
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
      return MakeExpr<UnaryFunction>(UnaryFunctionName::Tan,
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
  return MakeExpr<UnaryFunction>(UnaryFunctionName::Tan, arg);
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
  return MakeExpr<UnaryFunction>(UnaryFunctionName::ArcCos, arg);
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
  return MakeExpr<UnaryFunction>(UnaryFunctionName::ArcSin, arg);
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
  return MakeExpr<UnaryFunction>(UnaryFunctionName::ArcTan, arg);
}

Expr sqrt(const Expr& arg) {
  static const Expr one_half = Constants::One / 2_s;
  return Power::Create(arg, one_half);
}

}  // namespace math
