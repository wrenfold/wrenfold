// Copyright 2023 Gareth Cross
#include "expressions/relational.h"

#include "constants.h"
#include "expressions/all_expressions.h"
#include "integer_utils.h"
#include "visitor_impl.h"

namespace math {

template <typename T, typename List>
using IfOneOf = std::enable_if_t<ContainsType<T, List>>;

using IntAndRational = TypeList<Integer, Rational>;

struct CompareNumerics {
  // Int and rational can be compared:
  template <typename A, typename B, typename = IfOneOf<A, IntAndRational>,
            typename = IfOneOf<B, IntAndRational>>
  bool operator()(const A& a, const B& b) const {
    return static_cast<Rational>(a) < static_cast<Rational>(b);
  }

  static double FloatFromConstant(const Constant& c) {
    switch (c.GetName()) {
      case SymbolicConstants::Euler:
        return M_E;
      case SymbolicConstants::Pi:
        return M_PI;
      case SymbolicConstants::True:
        return 1.0;
      case SymbolicConstants::False:
        return 0.0;
    }
    throw TypeError("Invalid comparison with constant: {}",
                    StringFromSymbolicConstant(c.GetName()));
  }

  bool operator()(const Integer& a, const Constant& b) const {
    // This must have a value since float will not be nan.
    return CompareIntFloat(a.GetValue(), FloatFromConstant(b)).value() == RelativeOrder::LessThan;
  }
  bool operator()(const Constant& a, const Integer& b) const {
    return CompareIntFloat(b.GetValue(), FloatFromConstant(a)).value() ==
           RelativeOrder::GreaterThan;
  }

  // Floating point comparison should be viable for constants, there is no ambiguity from the
  // float precision for the set of constants that we have.
  bool operator()(const Constant& a, const Constant& b) const {
    return FloatFromConstant(a) < FloatFromConstant(b);
  }

  bool operator()(const Float& a, const Float& b) const { return a < b; }

  // Integer and float:
  bool operator()(const Integer& a, const Float& b) const {
    const auto result = CompareIntFloat(a.GetValue(), b.GetValue());
    ASSERT(result.has_value(), "Invalid float value: {}", b);
    return result.value() == RelativeOrder::LessThan;
  }

  bool operator()(const Float& a, const Integer& b) const {
    const auto result = CompareIntFloat(b.GetValue(), a.GetValue());
    ASSERT(result.has_value(), "Invalid float value: {}", b);
    return result.value() == RelativeOrder::GreaterThan;
  }
};

namespace detail {
// For detecting if `SupportsComparison` is supported.
template <typename, typename, typename = void>
constexpr bool SupportsComparison = false;
template <typename Argument1, typename Argument2>
constexpr bool SupportsComparison<Argument1, Argument2,
                                  decltype(CompareNumerics{}(std::declval<const Argument1>(),
                                                             std::declval<const Argument2>()),
                                           void())> = true;
}  // namespace detail

enum class TriState {
  // The relational is always true.
  True,
  // The relational is always false.
  False,
  // We cannot determine the value yet.
  Unknown,
};

struct RelationalSimplification {
  using ReturnType = TriState;
  using Policy = VisitorPolicy::NoError;

  explicit RelationalSimplification(RelationalOperation operation) : operation_(operation) {}

  // Handle cases where both operators are numeric or constant values.
  template <typename A, typename B, typename = std::enable_if_t<detail::SupportsComparison<A, B>>>
  TriState Apply(const A& a, const B& b) {
    const bool a_lt_b = CompareNumerics{}(a, b);
    const bool b_lt_a = CompareNumerics{}(b, a);
    if (operation_ == RelationalOperation::LessThan) {
      return a_lt_b ? TriState::True : TriState::False;
    } else if (operation_ == RelationalOperation::Equal) {
      return (!a_lt_b && !b_lt_a) ? TriState::True : TriState::False;
    }
    ASSERT(operation_ == RelationalOperation::LessThanOrEqual, "Invalid relational operation: {}",
           StringFromRelationalOperation(operation_));
    // either `a` < `b`, or: `a` >= `b` and `b` is not less than `a`, so `a` == `b`
    return a_lt_b || !b_lt_a ? TriState::True : TriState::False;
  }

  RelationalOperation operation_;
};

Expr Relational::Create(RelationalOperation operation, Expr left, Expr right) {
  // See if this relational automatically simplifies to a boolean constant:
  const TriState simplified = VisitBinaryStruct(left, right, RelationalSimplification{operation})
                                  .value_or(TriState::Unknown);
  if (simplified == TriState::True) {
    return Constants::True;
  } else if (simplified == TriState::False) {
    return Constants::False;
  }
  return MakeExpr<Relational>(operation, std::move(left), std::move(right));
}

}  // namespace math