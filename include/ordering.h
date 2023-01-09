// Copyright 2022 Gareth Cross
#pragma once

#include "assertions.h"
#include "expressions/all_expressions.h"

namespace math {

// Visitor that can be used to sort expressions by determining their relative order.
// TODO: Move into cc file.
struct OrderVisitor {
  // Describe the relative order of two expressions (a, b)
  enum class RelativeOrder {
    // a < b
    LessThan = -1,
    // a == b
    Equal = 0,
    // a > b
    GreaterThan = 1,
  };
  using ReturnType = RelativeOrder;

  using OrderOfConstants = TypeList<Float, Integer, Rational, Constant, Variable, Multiplication,
                                    Addition, Power, NaturalLog>;

  // Every type in the approved type list must appear here, or we get a compile error:
  static_assert(TypeListSize<OrderOfConstants>::Value == TypeListSize<ApprovedTypeList>::Value);

  template <typename A, typename B>
  RelativeOrder Apply(const A& a, const B& b) {
    if constexpr (!std::is_same_v<A, B>) {
      return IndexOfType<A, OrderOfConstants>::Value < IndexOfType<B, OrderOfConstants>::Value
                 ? RelativeOrder::LessThan
                 : RelativeOrder::GreaterThan;
    } else {
      // Otherwise call a method that compares equivalent types:
      // This will generate a compiler error if we forget types here.
      return Compare(a, b);
    }
  }

  // This visitor applies to any two members of `OrderOfConstants` that are _not_ the same type.
  template <typename Numeric>
  std::enable_if_t<ContainsTypeHelper<Numeric, Float, Integer, Rational, Constant>, RelativeOrder>
  Compare(const Numeric& a, const Numeric& b) const {
    if (a < b) {
      return RelativeOrder::LessThan;
    } else if (b < a) {
      return RelativeOrder::GreaterThan;
    }
    return RelativeOrder::Equal;
  }

  template <typename Derived>
  RelativeOrder Compare(const NAryOp<Derived>& a, const NAryOp<Derived>& b) const {
    // For multiplication and addition, sort lexicographically by recursively invoking OrderVisitor.
    const std::vector<Expr>& args_a = a.Args();
    const std::vector<Expr>& args_b = b.Args();

    auto it_a = args_a.begin();
    auto it_b = args_b.begin();
    for (; it_a != args_a.end() && it_b != args_b.end(); ++it_a, ++it_b) {
      const std::optional<RelativeOrder> result = VisitBinaryStruct(*it_a, *it_b, OrderVisitor{});
      ASSERT(result.has_value(), "Order visitor failed to implement a comparison");
      if (*result == RelativeOrder::LessThan) {
        return RelativeOrder::LessThan;
      } else if (*result == RelativeOrder::GreaterThan) {
        return RelativeOrder::GreaterThan;
      }
    }
    if (it_a == args_a.end() && it_b != args_b.end()) {
      return RelativeOrder::LessThan;  // `a` is shorter:
    } else if (it_a != args_a.end() && it_b == args_b.end()) {
      return RelativeOrder::GreaterThan;  //  `b` is shorter
    }
    return RelativeOrder::Equal;  //  they are equal
  }

  RelativeOrder Compare(const Power& a, const Power& b) const {
    const std::optional<RelativeOrder> base_order =
        VisitBinaryStruct(a.Base(), b.Base(), OrderVisitor{});
    ASSERT(base_order.has_value());
    if (*base_order == RelativeOrder::LessThan) {
      return RelativeOrder::LessThan;
    } else if (*base_order == RelativeOrder::GreaterThan) {
      return RelativeOrder::GreaterThan;
    }
    // Otherwise order is determined by the exponent:
    const std::optional<RelativeOrder> exponent_order =
        VisitBinaryStruct(a.Exponent(), b.Exponent(), OrderVisitor{});
    ASSERT(exponent_order.has_value());
    return *exponent_order;
  }

  RelativeOrder Compare(const NaturalLog& a, const NaturalLog& b) const {
    const std::optional<RelativeOrder> arg_order =
        VisitBinaryStruct(a.Inner(), b.Inner(), OrderVisitor{});
    ASSERT(arg_order.has_value());
    return *arg_order;
  }

  RelativeOrder Compare(const Variable& a, const Variable& b) const {
    if (a.GetName() < b.GetName()) {
      return RelativeOrder::LessThan;
    } else if (a.GetName() > b.GetName()) {
      return RelativeOrder::GreaterThan;
    }
    return RelativeOrder::Equal;
  }
};

}  // namespace math
