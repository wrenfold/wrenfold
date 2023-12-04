// Copyright 2023 Gareth Cross
#include "wf/expressions/relational.h"

#include "wf/constants.h"
#include "wf/expressions/all_expressions.h"
#include "wf/integer_utils.h"
#include "wf/visitor_impl.h"

namespace math {

struct CompareNumerics {
  // Int and rational can be compared:
  template <typename A, typename B, typename = enable_if_contains_type_t<A, Integer, Rational>,
            typename = enable_if_contains_type_t<B, Integer, Rational>>
  bool operator()(const A& a, const B& b) const {
    return static_cast<Rational>(a) < static_cast<Rational>(b);
  }

  static double float_from_constant(const Constant& c) {
    const double value = double_from_symbolic_constant(c.name());
    if (std::isnan(value)) {
      throw type_error("Invalid comparison with constant: {}",
                       string_from_symbolic_constant(c.name()));
    }
    return value;
  }

  bool operator()(const Integer& a, const Constant& b) const {
    // This must have a value since float will not be nan.
    return compare_int_float(a.get_value(), float_from_constant(b)).value() ==
           relative_order::less_than;
  }
  bool operator()(const Constant& a, const Integer& b) const {
    return compare_int_float(b.get_value(), float_from_constant(a)).value() ==
           relative_order::greater_than;
  }

  // Floating point comparison should be viable for constants, there is no ambiguity from the
  // float precision for the set of constants that we have.
  bool operator()(const Constant& a, const Constant& b) const {
    return float_from_constant(a) < float_from_constant(b);
  }

  bool operator()(const Float& a, const Float& b) const { return a < b; }

  // Integer and float:
  bool operator()(const Integer& a, const Float& b) const {
    const auto result = compare_int_float(a.get_value(), b.get_value());
    WF_ASSERT(result.has_value(), "Invalid float value: {}", b);
    return result.value() == relative_order::less_than;
  }

  bool operator()(const Float& a, const Integer& b) const {
    const auto result = compare_int_float(b.get_value(), a.get_value());
    WF_ASSERT(result.has_value(), "Invalid float value: {}", b);
    return result.value() == relative_order::greater_than;
  }
};

namespace detail {
// For detecting if `CompareNumerics` is supported.
template <typename, typename, typename = void>
constexpr bool supports_comparison_v = false;
template <typename Argument1, typename Argument2>
constexpr bool supports_comparison_v<Argument1, Argument2,
                                     decltype(CompareNumerics{}(std::declval<const Argument1>(),
                                                                std::declval<const Argument2>()),
                                              void())> = true;
}  // namespace detail

struct RelationalSimplification {
  explicit RelationalSimplification(relational_operation operation) : operation_(operation) {}

  template <typename A, typename B>
  TriState operator()(const A& a, const B& b) {
    if constexpr (detail::supports_comparison_v<A, B>) {
      // Handle cases where both operators are numeric or constant values.
      const bool a_lt_b = CompareNumerics{}(a, b);
      const bool b_lt_a = CompareNumerics{}(b, a);
      if (operation_ == relational_operation::less_than) {
        return a_lt_b ? TriState::True : TriState::False;
      } else if (operation_ == relational_operation::equal) {
        return (!a_lt_b && !b_lt_a) ? TriState::True : TriState::False;
      }
      WF_ASSERT(operation_ == relational_operation::less_than_or_equal,
                "Invalid relational operation: {}", string_from_relational_operation(operation_));
      // either `a` < `b`, or: `a` >= `b` and `b` is not less than `a`, so `a` == `b`
      return a_lt_b || !b_lt_a ? TriState::True : TriState::False;
    } else {
      return TriState::Unknown;
    }
  }

  relational_operation operation_;
};

Expr Relational::create(relational_operation operation, Expr left, Expr right) {
  if (is_complex_infinity(left) || is_complex_infinity(right) || is_undefined(left) ||
      is_undefined(right)) {
    throw type_error("Cannot construct relational with types: {} {} {}", left.type_name(),
                     string_from_relational_operation(operation), right.type_name());
  }
  // See if this relational automatically simplifies to a boolean constant:
  const TriState simplified = visit_binary(left, right, RelationalSimplification{operation});
  if (simplified == TriState::True) {
    return constants::boolean_true;
  } else if (simplified == TriState::False) {
    return constants::boolean_false;
  }
  if (operation == relational_operation::equal) {
    // We put equality operations into a canonical order.
    if (expression_order(left, right) != relative_order::less_than) {
      std::swap(left, right);
    }
  }
  return make_expr<Relational>(operation, std::move(left), std::move(right));
}

}  // namespace math
