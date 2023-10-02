// Copyright 2023 Gareth Cross
#include "assertions.h"
#include "expression.h"
#include "expressions/all_expressions.h"

namespace math {

// Visitor that can be used to sort expressions by determining their relative order.
struct OrderVisitor {
  using ReturnType = RelativeOrder;

  using OrderOfTypes =
      TypeList<Float, Integer, Rational, Constant, Infinity, Variable, FunctionArgument,
               Multiplication, Addition, Power, Function, Relational, Conditional, Derivative>;

  // Every type in the approved type list must appear here, or we get a compile error:
  static_assert(TypeListSize<OrderOfTypes>::Value == TypeListSize<ExpressionTypeList>::Value);

  template <typename A, typename B>
  RelativeOrder operator()(const A& a, const B& b) {
    if constexpr (!std::is_same_v<A, B>) {
      return IndexOfType<A, OrderOfTypes>::Value < IndexOfType<B, OrderOfTypes>::Value
                 ? RelativeOrder::LessThan
                 : RelativeOrder::GreaterThan;
    } else {
      // Otherwise call a method that compares equivalent types:
      // This will generate a compiler error if we forget types here.
      return Compare(a, b);
    }
  }

  // This visitor applies to any two members of `OrderOfTypes` that are _not_ the same type.
  template <typename Numeric, typename = std::enable_if_t<
                                  ContainsTypeHelper<Numeric, Float, Integer, Rational, Constant>>>
  RelativeOrder Compare(const Numeric& a, const Numeric& b) const {
    if (a < b) {
      return RelativeOrder::LessThan;
    } else if (b < a) {
      return RelativeOrder::GreaterThan;
    }
    return RelativeOrder::Equal;
  }

  RelativeOrder Compare(const Conditional& a, const Conditional& b) const {
    if (const RelativeOrder o = ExpressionOrder(a.condition(), b.condition());
        o != RelativeOrder::Equal) {
      return o;
    }
    if (const RelativeOrder o = ExpressionOrder(a.if_branch(), b.if_branch());
        o != RelativeOrder::Equal) {
      return o;
    }
    if (const RelativeOrder o = ExpressionOrder(a.else_branch(), b.else_branch());
        o != RelativeOrder::Equal) {
      return o;
    }
    return RelativeOrder::Equal;
  }

  constexpr RelativeOrder Compare(const Infinity&, const Infinity&) const {
    return RelativeOrder::Equal;
  }

  RelativeOrder Compare(const Addition& a, const Addition& b) const {
    return LexicographicalCompare(a.begin(), a.end(), b.begin(), b.end());
  }

  RelativeOrder Compare(const Derivative& a, const Derivative& b) const {
    if (a.order() < b.order()) {
      return RelativeOrder::LessThan;
    } else if (a.order() > b.order()) {
      return RelativeOrder::GreaterThan;
    }
    return LexicographicalCompare(a.begin(), a.end(), b.begin(), b.end());
  }

  RelativeOrder Compare(const Multiplication& a, const Multiplication& b) const {
    return LexicographicalCompare(a.begin(), a.end(), b.begin(), b.end());
  }

  RelativeOrder Compare(const Power& a, const Power& b) const {
    const RelativeOrder base_order = VisitBinary(a.base(), b.base(), OrderVisitor{});
    if (base_order == RelativeOrder::LessThan) {
      return RelativeOrder::LessThan;
    } else if (base_order == RelativeOrder::GreaterThan) {
      return RelativeOrder::GreaterThan;
    }
    // Otherwise order is determined by the exponent:
    return VisitBinary(a.exponent(), b.exponent(), OrderVisitor{});
  }

  RelativeOrder Compare(const Function& a, const Function& b) const {
    // First compare by name:
    const int name_comp = a.function_name().compare(b.function_name());
    if (name_comp > 0) {
      return RelativeOrder::GreaterThan;
    } else if (name_comp < 0) {
      return RelativeOrder::LessThan;
    }
    return LexicographicalCompare(a.begin(), a.end(), b.begin(), b.end());
  }

  RelativeOrder Compare(const FunctionArgument& a, const FunctionArgument& b) const {
    const auto p_a = std::make_pair(a.arg_index(), a.element_index());
    const auto p_b = std::make_pair(b.arg_index(), b.element_index());
    if (p_a < p_b) {
      return RelativeOrder::LessThan;
    } else if (p_a > p_b) {
      return RelativeOrder::GreaterThan;
    }
    return RelativeOrder::Equal;
  }

  RelativeOrder Compare(const Relational& a, const Relational& b) const {
    if (a.operation() < b.operation()) {
      return RelativeOrder::LessThan;
    } else if (a.operation() > b.operation()) {
      return RelativeOrder::GreaterThan;
    }
    const RelativeOrder base_order = VisitBinary(a.left(), b.left(), OrderVisitor{});
    if (base_order == RelativeOrder::LessThan) {
      return RelativeOrder::LessThan;
    } else if (base_order == RelativeOrder::GreaterThan) {
      return RelativeOrder::GreaterThan;
    }
    return VisitBinary(a.right(), b.right(), OrderVisitor{});
  }

  RelativeOrder Compare(const Variable& a, const Variable& b) const {
    if (a.name() < b.name()) {
      return RelativeOrder::LessThan;
    } else if (a.name() > b.name()) {
      return RelativeOrder::GreaterThan;
    }
    return RelativeOrder::Equal;
  }

  // Order two sequences lexicographically.
  template <typename Iterator>
  static RelativeOrder LexicographicalCompare(Iterator begin_a, Iterator end_a, Iterator begin_b,
                                              Iterator end_b) {
    for (; begin_a != end_a && begin_b != end_b; ++begin_a, ++begin_b) {
      const RelativeOrder result = ExpressionOrder(*begin_a, *begin_b);
      if (result == RelativeOrder::LessThan) {
        return RelativeOrder::LessThan;
      } else if (result == RelativeOrder::GreaterThan) {
        return RelativeOrder::GreaterThan;
      }
    }
    if (begin_a == end_a && begin_b != end_b) {
      return RelativeOrder::LessThan;  // `a` is shorter:
    } else if (begin_a != end_a && begin_b == end_b) {
      return RelativeOrder::GreaterThan;  // `b` is shorter
    }
    return RelativeOrder::Equal;  //  they are equal
  }
};

RelativeOrder ExpressionOrder(const Expr& a, const Expr& b) {
  return VisitBinary(a, b, OrderVisitor{});
}

}  // namespace math
