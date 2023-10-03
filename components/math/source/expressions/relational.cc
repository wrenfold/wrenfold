// Copyright 2023 Gareth Cross
#include "expressions/relational.h"

#include "constants.h"
#include "expressions/all_expressions.h"
#include "integer_utils.h"
#include "visitor_impl.h"

namespace math {

template <typename T, typename... Ts>
using enable_if_one_of_t = std::enable_if_t<list_contains_type_v<T, Ts...>>;

struct CompareNumerics {
  // Int and rational can be compared:
  template <typename A, typename B, typename = enable_if_one_of_t<A, Integer, Rational>,
            typename = enable_if_one_of_t<B, Integer, Rational>>
  bool operator()(const A& a, const B& b) const {
    return static_cast<Rational>(a) < static_cast<Rational>(b);
  }

  static double float_from_constant(const Constant& c) {
    const double value = double_from_symbolic_constant(c.name());
    if (std::isnan(value)) {
      throw TypeError("Invalid comparison with constant: {}",
                      string_from_symbolic_constant(c.name()));
    }
    return value;
  }

  bool operator()(const Integer& a, const Constant& b) const {
    // This must have a value since float will not be nan.
    return compare_int_float(a.get_value(), float_from_constant(b)).value() ==
           RelativeOrder::LessThan;
  }
  bool operator()(const Constant& a, const Integer& b) const {
    return compare_int_float(b.get_value(), float_from_constant(a)).value() ==
           RelativeOrder::GreaterThan;
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
    ASSERT(result.has_value(), "Invalid float value: {}", b);
    return result.value() == RelativeOrder::LessThan;
  }

  bool operator()(const Float& a, const Integer& b) const {
    const auto result = compare_int_float(b.get_value(), a.get_value());
    ASSERT(result.has_value(), "Invalid float value: {}", b);
    return result.value() == RelativeOrder::GreaterThan;
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

enum class TriState {
  // The relational is always true.
  True,
  // The relational is always false.
  False,
  // We cannot determine the value yet.
  Unknown,
};

struct RelationalSimplification {
  explicit RelationalSimplification(RelationalOperation operation) : operation_(operation) {}

  template <typename A, typename B>
  TriState operator()(const A& a, const B& b) {
    if constexpr (detail::supports_comparison_v<A, B>) {
      // Handle cases where both operators are numeric or constant values.
      const bool a_lt_b = CompareNumerics{}(a, b);
      const bool b_lt_a = CompareNumerics{}(b, a);
      if (operation_ == RelationalOperation::LessThan) {
        return a_lt_b ? TriState::True : TriState::False;
      } else if (operation_ == RelationalOperation::Equal) {
        return (!a_lt_b && !b_lt_a) ? TriState::True : TriState::False;
      }
      ASSERT(operation_ == RelationalOperation::LessThanOrEqual, "Invalid relational operation: {}",
             string_from_relational_operation(operation_));
      // either `a` < `b`, or: `a` >= `b` and `b` is not less than `a`, so `a` == `b`
      return a_lt_b || !b_lt_a ? TriState::True : TriState::False;
    } else {
      return TriState::Unknown;
    }
  }

  RelationalOperation operation_;
};

Expr Relational::create(RelationalOperation operation, Expr left, Expr right) {
  // See if this relational automatically simplifies to a boolean constant:
  const TriState simplified = visit_binary(left, right, RelationalSimplification{operation});
  if (simplified == TriState::True) {
    return Constants::True;
  } else if (simplified == TriState::False) {
    return Constants::False;
  }
  if (operation == RelationalOperation::Equal) {
    // We put equality operations into a canonical order.
    if (expression_order(left, right) != RelativeOrder::LessThan) {
      std::swap(left, right);
    }
  }
  return make_expr<Relational>(operation, std::move(left), std::move(right));
}

}  // namespace math
