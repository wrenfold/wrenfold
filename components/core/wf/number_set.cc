// Copyright 2023 Gareth Cross
#include "wf/enumerations.h"
#include "wf/expressions/all_expressions.h"
#include "wf/operations.h"
#include "wf/visitor_impl.h"

namespace math {

constexpr static bool is_non_negative(const NumberSet set) noexcept {
  return set == NumberSet::RealNonNegative || set == NumberSet::RealPositive;
}

constexpr static bool contains_zero(const NumberSet set) noexcept {
  return set != NumberSet::RealPositive;
}

constexpr NumberSet combine_sets_add(NumberSet a, NumberSet b) noexcept {
  if (b < a) {
    return combine_sets_add(b, a);
  }
  static_assert(NumberSet::RealPositive < NumberSet::RealNonNegative);
  if (b == NumberSet::RealNonNegative && a == NumberSet::RealPositive) {
    // if (x > 0) and (y >= 0) then (x + y > 0)
    return NumberSet::RealPositive;
  }
  return b;  //  take the larger value in the enum
}

constexpr NumberSet combine_sets_mul(NumberSet a, NumberSet b) noexcept { return std::max(a, b); }

class DetermineSetVisitor {
 public:
  template <typename Container, typename Callable>
  NumberSet handle_add_or_mul(const Container& container, Callable callable) const {
    ZEN_ASSERT_GREATER_OR_EQ(container.arity(), 1);
    auto it = container.begin();
    NumberSet set = determine_numeric_set(*it);
    for (it = std::next(it); it != container.end(); ++it) {
      if (set == NumberSet::Unknown) {
        // Can't become less specific than unknown.
        return set;
      }
      set = callable(set, determine_numeric_set(*it));
    }
    return set;
  }

  NumberSet operator()(const Addition& add) const {
    return handle_add_or_mul(add, &combine_sets_add);
  }

  constexpr NumberSet operator()(const CastBool&) const noexcept {
    return NumberSet::RealNonNegative;
  }

  NumberSet operator()(const Conditional& cond) const {
    NumberSet left = determine_numeric_set(cond.if_branch());
    NumberSet right = determine_numeric_set(cond.else_branch());
    return std::max(left, right);
  }

  constexpr NumberSet operator()(const Constant& c) const noexcept {
    if (c.name() == SymbolicConstants::False) {
      return NumberSet::RealNonNegative;
    }
    return NumberSet::RealPositive;
  }

  constexpr NumberSet operator()(const Derivative&) const noexcept { return NumberSet::Unknown; }

  NumberSet operator()(const Multiplication& mul) const {
    return handle_add_or_mul(mul, &combine_sets_mul);
  }

  NumberSet operator()(const Function& func) {
    absl::InlinedVector<NumberSet, 4> args{};
    std::transform(func.begin(), func.end(), std::back_inserter(args), &determine_numeric_set);
    ZEN_ASSERT_GREATER_OR_EQ(args.size(), 1);

    if (std::count(args.begin(), args.end(), NumberSet::Unknown) > 0) {
      return NumberSet::Unknown;
    }

    switch (func.enum_value()) {
      case BuiltInFunction::Cos:
      case BuiltInFunction::Sin: {
        if (is_real_set(args[0])) {
          return NumberSet::Real;
        }
        return NumberSet::Complex;
      }
      case BuiltInFunction::Tan:
        // Any real argument could be +/- pi/2, which is unknown
        return NumberSet::Unknown;
      case BuiltInFunction::ArcCos:
      case BuiltInFunction::ArcSin:
      case BuiltInFunction::ArcTan:
        return NumberSet::Unknown;  //  TODO: implement inverse trig functions.
      case BuiltInFunction::Log: {
        if (args[0] == NumberSet::RealPositive) {
          return NumberSet::RealPositive;
        }
        // Otherwise could be zero.
        return NumberSet::Unknown;
      }
      case BuiltInFunction::Abs: {
        if (args[0] == NumberSet::RealPositive) {
          return NumberSet::RealPositive;
        }
        return NumberSet::RealNonNegative;
      }
      case BuiltInFunction::Signum: {
        if (is_real_set(args[0])) {
          return args[0];
        }
        return NumberSet::Unknown;
      }
      case BuiltInFunction::Arctan2:
        // Can always be undefined.
        return NumberSet::Unknown;
    }
    throw TypeError("Invalid function: {}\n", func.function_name());
  }

  // In spite of its name, complex infinity is not part of the complex numbers.
  constexpr NumberSet operator()(const Infinity&) const noexcept { return NumberSet::Unknown; }

  template <typename T>
  constexpr NumberSet handle_numeric(const T& val) const noexcept {
    if (val.is_positive()) {
      return NumberSet::RealPositive;
    }
    if (!val.is_negative()) {
      return NumberSet::RealNonNegative;
    }
    return NumberSet::Real;
  }

  constexpr NumberSet operator()(const Integer& i) const noexcept { return handle_numeric(i); }
  constexpr NumberSet operator()(const Float& f) const noexcept { return handle_numeric(f); }

  NumberSet operator()(const Power& pow) const {
    NumberSet base = determine_numeric_set(pow.base());
    NumberSet exp = determine_numeric_set(pow.exponent());
    if (base == NumberSet::Complex || exp == NumberSet::Complex || base == NumberSet::Unknown ||
        exp == NumberSet::Unknown) {
      return NumberSet::Unknown;
    }

    if (contains_zero(base) && !is_non_negative(exp)) {
      // If the exponent might be negative, we can't say confidently this isn't a division by zero.
      return NumberSet::Unknown;
    }

    if (is_real_set(base)) {
      if (const Integer* exp_int = cast_ptr<Integer>(pow.exponent());
          exp_int != nullptr && exp_int->is_even()) {
        return contains_zero(base) ? NumberSet::RealNonNegative : NumberSet::RealPositive;
      }
      if (is_non_negative(base) && is_non_negative(exp)) {
        // base is either RealNonNegative or RealPositive
        return base;
      }
      return NumberSet::Real;
    }
    return NumberSet::Unknown;
  }

  constexpr NumberSet operator()(const Rational& r) const noexcept { return handle_numeric(r); }

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
