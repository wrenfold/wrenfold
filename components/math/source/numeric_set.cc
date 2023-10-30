// Copyright 2023 Gareth Cross
#include "enumerations.h"
#include "expressions/all_expressions.h"
#include "operations.h"
#include "visitor_impl.h"

namespace math {

constexpr inline bool is_real(const NumberSet set) noexcept {
  switch (set) {
    case NumberSet::Unknown:
      return false;
    case NumberSet::Real:
    case NumberSet::RealNonNegative:
      return true;
    case NumberSet::Complex:
      return false;
  }
  return false;
}

constexpr inline bool is_non_negative(const NumberSet set) noexcept {
  if (set == NumberSet::RealNonNegative) {
    return true;
  }
  return false;
}

// Determine which set encompasses both `a` and `b`.
static NumberSet parent_set(NumberSet a, NumberSet b) {
  if (a == b) {
    return a;
  } else if (a == NumberSet::Unknown || b == NumberSet::Unknown) {
    return NumberSet::Unknown;
  } else if (a == NumberSet::Complex || b == NumberSet::Complex) {
    return NumberSet::Complex;
  } else if (is_real(a) || is_real(b)) {
    return NumberSet::Real;
  }
  return NumberSet::Unknown;
}

class DetermineSetVisitor {
 public:
  template <typename Container>
  NumberSet handle_add_or_mul(const Container& container) const {
    ZEN_ASSERT_GREATER_OR_EQ(container.arity(), 1);
    auto it = container.begin();
    NumberSet set = determine_numeric_set(*it);
    for (it = std::next(it); it != container.end(); ++it) {
      if (set == NumberSet::Unknown) {
        // Can't become less specific than unknown.
        return set;
      }
      set = parent_set(set, determine_numeric_set(*it));
    }
    return set;
  }

  NumberSet operator()(const Addition& add) const { return handle_add_or_mul(add); }

  NumberSet operator()(const Conditional& cond) const {
    NumberSet left = determine_numeric_set(cond.if_branch());
    NumberSet right = determine_numeric_set(cond.else_branch());
    return parent_set(left, right);
  }

  constexpr NumberSet operator()(const Constant&) const noexcept {
    return NumberSet::RealNonNegative;
  }

  constexpr NumberSet operator()(const Derivative&) const noexcept { return NumberSet::Unknown; }

  NumberSet operator()(const Multiplication& mul) const { return handle_add_or_mul(mul); }

  NumberSet operator()(const Function& func) {
    absl::InlinedVector<NumberSet, 4> args{};
    std::transform(func.begin(), func.end(), std::back_inserter(args), &determine_numeric_set);
    ZEN_ASSERT_GREATER_OR_EQ(args.size(), 1);

    if (std::count(args.begin(), args.end(), NumberSet::Unknown) > 0) {
      return NumberSet::Unknown;
    }

    switch (func.enum_value()) {
      case BuiltInFunctionName::Cos:
      case BuiltInFunctionName::Sin: {
        if (is_real(args[0])) {
          return NumberSet::Real;
        }
        return NumberSet::Complex;
      }
      case BuiltInFunctionName::Tan:
        // Any real argument could be +/- pi/2, which is unknown
        return NumberSet::Unknown;
      case BuiltInFunctionName::ArcCos:
      case BuiltInFunctionName::ArcSin:
      case BuiltInFunctionName::ArcTan:
        return NumberSet::Unknown;  //  TODO: implement inverse trig functions.
      case BuiltInFunctionName::Log:
        return NumberSet::Unknown;
      case BuiltInFunctionName::Sqrt: {
        if (is_non_negative(args[0])) {
          return NumberSet::RealNonNegative;
        }
        return NumberSet::Complex;
      }
      case BuiltInFunctionName::Abs: {
        if (is_real(args[0])) {
          return NumberSet::RealNonNegative;
        }
        return NumberSet::RealNonNegative;
      }
      case BuiltInFunctionName::Signum: {
        if (is_non_negative(args[0])) {
          return NumberSet::RealNonNegative;
        } else if (is_real(args[0])) {
          return NumberSet::Real;
        } else {
          return NumberSet::Unknown;
        }
      }
      case BuiltInFunctionName::Arctan2:
        // Can always be undefined.
        return NumberSet::Unknown;
      case BuiltInFunctionName::Pow: {
        throw TypeError("Should not happen.");
      }
      case BuiltInFunctionName::ENUM_SIZE:
        break;
    }
    throw TypeError("Invalid function: {}\n", func.function_name());
  }

  // In spite of its name, complex infinity is not part of the complex numbers.
  constexpr NumberSet operator()(const Infinity&) const noexcept { return NumberSet::Unknown; }

  constexpr NumberSet operator()(const Integer& i) const noexcept {
    return i.is_negative() ? NumberSet::Real : NumberSet::RealNonNegative;
  }

  constexpr NumberSet operator()(const Float& f) const noexcept {
    return f.is_negative() ? NumberSet::Real : NumberSet::RealNonNegative;
  }

  NumberSet operator()(const Power& pow) const {
    NumberSet base = determine_numeric_set(pow.base());
    NumberSet exp = determine_numeric_set(pow.exponent());
    if (base == NumberSet::Real && is_non_negative(exp)) {
      if (const Integer* exp_int = cast_ptr<Integer>(pow.exponent());
          exp_int != nullptr && exp_int->is_even()) {
        return NumberSet::RealNonNegative;
      }
      return NumberSet::Real;
    }
    if (is_non_negative(base) && is_non_negative(exp)) {
      return NumberSet::RealNonNegative;
    }
    return NumberSet::Unknown;
  }

  constexpr NumberSet operator()(const Rational& r) const noexcept {
    return r.is_negative() ? NumberSet::Real : NumberSet::RealNonNegative;
  }

  // Relational is always 0 or 1, so it must be non-negative.
  constexpr NumberSet operator()(const Relational&) const noexcept {
    return NumberSet::RealNonNegative;
  }

  constexpr NumberSet operator()(const Undefined&) const noexcept {
    // Cannot establish
    return NumberSet::Unknown;
  }

  constexpr NumberSet operator()(const Variable& var) const noexcept { return var.set(); }
};

NumberSet determine_numeric_set(const Expr& x) { return visit(x, DetermineSetVisitor{}); }

}  // namespace math
