// Copyright 2023 Gareth Cross
#include "assertions.h"
#include "expression.h"
#include "expressions/all_expressions.h"

namespace math {

// Visitor that can be used to sort expressions by determining their relative order.
struct OrderVisitor {
  using ReturnType = RelativeOrder;
  using Policy = VisitorPolicy::CompileError;

  using OrderOfTypes = TypeList<Float, Integer, Rational, Constant, Variable, FunctionArgument,
                                Multiplication, Addition, Power, UnaryFunction, Matrix>;

  // Every type in the approved type list must appear here, or we get a compile error:
  static_assert(TypeListSize<OrderOfTypes>::Value == TypeListSize<ApprovedTypeList>::Value);

  template <typename A, typename B>
  RelativeOrder Apply(const A& a, const B& b) {
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
      const RelativeOrder result = VisitBinaryStruct(*it_a, *it_b, OrderVisitor{});
      if (result == RelativeOrder::LessThan) {
        return RelativeOrder::LessThan;
      } else if (result == RelativeOrder::GreaterThan) {
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

  RelativeOrder Compare(const Matrix& a, const Matrix& b) const {
    const auto dims_a = std::make_pair(a.NumRows(), a.NumCols());
    const auto dims_b = std::make_pair(b.NumRows(), b.NumCols());
    if (dims_a < dims_b) {
      return RelativeOrder::LessThan;
    } else if (dims_a > dims_b) {
      return RelativeOrder::GreaterThan;
    }
    return LexicographicalCompare(a.begin(), a.end(), b.begin(), b.end());
  }

  RelativeOrder Compare(const Power& a, const Power& b) const {
    const RelativeOrder base_order = VisitBinaryStruct(a.Base(), b.Base(), OrderVisitor{});
    if (base_order == RelativeOrder::LessThan) {
      return RelativeOrder::LessThan;
    } else if (base_order == RelativeOrder::GreaterThan) {
      return RelativeOrder::GreaterThan;
    }
    // Otherwise order is determined by the exponent:
    return VisitBinaryStruct(a.Exponent(), b.Exponent(), OrderVisitor{});
  }

  RelativeOrder Compare(const UnaryFunction& a, const UnaryFunction& b) const {
    // First compare by name:
    const int name_comp = a.Name().compare(b.Name());
    if (name_comp > 0) {
      return RelativeOrder::GreaterThan;
    } else if (name_comp < 0) {
      return RelativeOrder::LessThan;
    }
    // Then compare by value of the argument:
    return VisitBinaryStruct(a.Arg(), b.Arg(), OrderVisitor{});
  }

  RelativeOrder Compare(const FunctionArgument& a, const FunctionArgument& b) const {
    const auto p_a = std::make_pair(a.ArgIndex(), a.ElementIndex());
    const auto p_b = std::make_pair(b.ArgIndex(), b.ElementIndex());
    if (p_a < p_b) {
      return RelativeOrder::LessThan;
    } else if (p_a > p_b) {
      return RelativeOrder::GreaterThan;
    }
    return RelativeOrder::Equal;
  }

  RelativeOrder Compare(const Variable& a, const Variable& b) const {
    if (a.GetName() < b.GetName()) {
      return RelativeOrder::LessThan;
    } else if (a.GetName() > b.GetName()) {
      return RelativeOrder::GreaterThan;
    }
    return RelativeOrder::Equal;
  }

  // Order two sequences lexicographically.
  template <typename Iterator>
  static RelativeOrder LexicographicalCompare(Iterator begin_a, Iterator end_a, Iterator begin_b,
                                              Iterator end_b) {
    for (; begin_a != end_a && begin_b != end_b; ++begin_a, ++begin_b) {
      const RelativeOrder result = VisitBinaryStruct(*begin_a, *begin_b, OrderVisitor{});
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
  return VisitBinaryStruct(a, b, OrderVisitor{});
}

}  // namespace math
