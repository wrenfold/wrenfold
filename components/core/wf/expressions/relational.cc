// Copyright 2023 Gareth Cross
#include "wf/expressions/relational.h"

#include "wf/constants.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"
#include "wf/integer_utils.h"

namespace wf {

struct CompareNumerics {
  // Int and rational can be compared:
  template <typename A, typename B,
            typename = enable_if_contains_type_t<A, integer_constant, rational_constant>,
            typename = enable_if_contains_type_t<B, integer_constant, rational_constant>>
  bool operator()(const A& a, const B& b) const {
    return static_cast<rational_constant>(a) < static_cast<rational_constant>(b);
  }

  static double float_from_constant(const symbolic_constant& c) {
    const double value = double_from_symbolic_constant(c.name());
    if (std::isnan(value)) {
      throw type_error("Invalid comparison with constant: {}",
                       string_from_symbolic_constant(c.name()));
    }
    return value;
  }

  bool operator()(const integer_constant& a, const symbolic_constant& b) const {
    // This must have a value since float will not be nan.
    return compare_int_float(a.value().value(), float_from_constant(b)).value() ==
           relative_order::less_than;
  }
  bool operator()(const symbolic_constant& a, const integer_constant& b) const {
    return compare_int_float(b.value().value(), float_from_constant(a)).value() ==
           relative_order::greater_than;
  }

  // Floating point comparison should be viable for constants, there is no ambiguity from the
  // float precision for the set of constants that we have.
  bool operator()(const symbolic_constant& a, const symbolic_constant& b) const {
    return float_from_constant(a) < float_from_constant(b);
  }

  bool operator()(const float_constant& a, const float_constant& b) const { return a < b; }

  // Integer and float:
  bool operator()(const integer_constant& a, const float_constant& b) const {
    const auto result = compare_int_float(a.value().value(), b.value());
    WF_ASSERT(result.has_value(), "Invalid float value: {}", b);
    return result.value() == relative_order::less_than;
  }

  bool operator()(const float_constant& a, const integer_constant& b) const {
    const auto result = compare_int_float(b.value().value(), a.value());
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
  tri_state operator()(const A& a, const B& b) {
    if constexpr (detail::supports_comparison_v<A, B>) {
      // Handle cases where both operators are numeric or constant values.
      const bool a_lt_b = CompareNumerics{}(a, b);
      const bool b_lt_a = CompareNumerics{}(b, a);
      if (operation_ == relational_operation::less_than) {
        return a_lt_b ? tri_state::True : tri_state::False;
      } else if (operation_ == relational_operation::equal) {
        return (!a_lt_b && !b_lt_a) ? tri_state::True : tri_state::False;
      }
      WF_ASSERT(operation_ == relational_operation::less_than_or_equal,
                "Invalid relational operation: {}", string_from_relational_operation(operation_));
      // either `a` < `b`, or: `a` >= `b` and `b` is not less than `a`, so `a` == `b`
      return a_lt_b || !b_lt_a ? tri_state::True : tri_state::False;
    } else {
      return tri_state::unknown;
    }
  }

  relational_operation operation_;
};

boolean_expr relational::create(relational_operation operation, scalar_expr left,
                                scalar_expr right) {
  if (is_complex_infinity(left) || is_complex_infinity(right) || is_undefined(left) ||
      is_undefined(right)) {
    throw type_error("Cannot construct relational with types: {} {} {}", left.type_name(),
                     string_from_relational_operation(operation), right.type_name());
  }

  // See if this relational automatically simplifies to a boolean constant:
  if (const tri_state simplified = visit_binary(left, right, RelationalSimplification{operation});
      simplified == tri_state::True) {
    return constants::boolean_true;
  } else if (simplified == tri_state::False) {
    return constants::boolean_false;
  }
  if (operation == relational_operation::equal) {
    // We put equality operations into a canonical order.
    if (determine_order(left, right) != relative_order::less_than) {
      std::swap(left, right);
    }
  }
  return boolean_expr(std::in_place_type_t<relational>{}, operation, std::move(left),
                      std::move(right));
}

}  // namespace wf
