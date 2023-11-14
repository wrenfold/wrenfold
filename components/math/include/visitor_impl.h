// Copyright 2023 Gareth Cross
#pragma once
#include "assertions.h"
#include "expression.h"
#include "template_utils.h"
#include "visitor_base.h"

// All expression definitions need to be available to static_cast.
#include "expressions/all_expressions.h"

namespace math {

// Accepts a visitor struct or lambda and applies it to the provided expression.
// The return type will be deduced from the visitor itself.
template <typename VisitorType>
auto visit(const Expr& expr, VisitorType&& visitor) {
  // Deduce the return type by invoking the operator() w/ the different expression types.
  // TODO: For now we allow one single ReturnType. We could allow returning std::variant<>.
  // using ReturnType = call_operator_return_types_t<VisitorType, ExpressionTypeList>;
  if (expr.is_type<Addition>()) {
    return visitor(cast_unchecked<Addition>(expr));
  } else if (expr.is_type<CastBool>()) {
    return visitor(cast_unchecked<CastBool>(expr));
  } else if (expr.is_type<Conditional>()) {
    return visitor(cast_unchecked<Conditional>(expr));
  } else if (expr.is_type<Constant>()) {
    return visitor(cast_unchecked<Constant>(expr));
  } else if (expr.is_type<Derivative>()) {
    return visitor(cast_unchecked<Derivative>(expr));
  } else if (expr.is_type<Float>()) {
    return visitor(cast_unchecked<Float>(expr));
  } else if (expr.is_type<Function>()) {
    return visitor(cast_unchecked<Function>(expr));
  } else if (expr.is_type<Infinity>()) {
    return visitor(cast_unchecked<Infinity>(expr));
  } else if (expr.is_type<Integer>()) {
    return visitor(cast_unchecked<Integer>(expr));
  } else if (expr.is_type<Multiplication>()) {
    return visitor(cast_unchecked<Multiplication>(expr));
  } else if (expr.is_type<Power>()) {
    return visitor(cast_unchecked<Power>(expr));
  } else if (expr.is_type<Rational>()) {
    return visitor(cast_unchecked<Rational>(expr));
  } else if (expr.is_type<Relational>()) {
    return visitor(cast_unchecked<Relational>(expr));
  } else if (expr.is_type<Undefined>()) {
    return visitor(cast_unchecked<Undefined>(expr));
  } else {
    ZEN_ASSERT(expr.is_type<Variable>(), "Neglected to implement if-else switch for type: {}",
               expr.type_name());
    return visitor(cast_unchecked<Variable>(expr));
  }
}

// Version of `visit` that accepts visitors that can re-use the input expression.
// Methods on the visitor can optionally be of the form operator()(x, expr) where `x`
// is the underlying expression type and `expr` is the abstract `Expr`. This allows
// optionally re-using the expression to avoid allocation.
template <typename VisitorType>
auto visit_with_expr(const Expr& expr, VisitorType&& visitor) {
  return visit(expr, [&expr, &visitor](const auto& x) {
    using T = std::decay_t<decltype(x)>;

    // Make sure this is not ambiguous:
    static_assert(
        (has_binary_call_operator_v<VisitorType, T, Expr> ||
         has_call_operator_v<VisitorType, T>)&&!(has_binary_call_operator_v<VisitorType, T, Expr> &&
                                                 has_call_operator_v<VisitorType, T>),
        "Visitor must support either unary or binary operator(), but not both.");

    if constexpr (has_binary_call_operator_v<VisitorType, T, Expr>) {
      return visitor(x, expr);
    } else {
      return visitor(x);
    }
  });
}

// Visit two expressions with a struct that accepts two concrete types in its operator()(...)
// signature. The struct must declare a ReturnType associated type.
template <typename VisitorType>
auto visit_binary(const Expr& u, const Expr& v, VisitorType&& handler) {
  return visit(u, [&handler, &v](const auto& typed_u) {
    return visit(v, [&handler, &typed_u](const auto& typed_v) {
      // Check if we can visit
      using TypeU = std::decay_t<decltype(typed_u)>;
      using TypeV = std::decay_t<decltype(typed_v)>;
      static_assert(!std::is_const_v<decltype(handler)>);
      static_assert(has_binary_call_operator_v<std::decay_t<VisitorType>, TypeU, TypeV>,
                    "Binary visitor fails to implement a required operator() method.");
      return handler(typed_u, typed_v);
    });
  });
}

}  // namespace math
