#include "operation_utils.h"

#include "assertions.hpp"
#include "binary_operations.h"
#include "constant_expressions.h"
#include "constants.h"
#include "unary_operations.h"

// #define VERBOSE_OPS

namespace math {

ExpressionBaseConstPtr CreateMultiplication(const ExpressionBaseConstPtr& a,
                                            const ExpressionBaseConstPtr& b) {
  ASSERT(a);
  ASSERT(b);
  if (IsZero(a) || IsZero(b)) {
    return Constants::Zero;
  }
  if (IsOne(a)) {
    return b;
  }
  if (IsOne(b)) {
    return a;
  }
  return MakeExprBase<Multiplication>(a, b);
}

ExpressionBaseConstPtr CreateAddition(const ExpressionBaseConstPtr& a,
                                      const ExpressionBaseConstPtr& b) {
  ASSERT(a);
  ASSERT(b);
  // Simplify the case where one or more operands is zero
  if (IsZero(a)) {
    return b;
  } else if (IsZero(b)) {
    return a;
  }
#ifdef VERBOSE_OPS
  fmt::print(TEXT("Creating Addition({}, {})\n"), ToPlainString(a), ToPlainString(b));
#endif
  return MakeExprBase<Addition>(a, b);
}

ExpressionBaseConstPtr CreateSubtraction(const ExpressionBaseConstPtr& a,
                                         const ExpressionBaseConstPtr& b) {
  ASSERT(a);
  ASSERT(b);
  // Simplify the case where one or more operands is zero
  if (IsZero(a)) {
    return CreateNegation(b);
  } else if (IsZero(b)) {
    return a;
  }
  if (a->IsIdenticalTo(b)) {
    return Constants::Zero;
  }
#ifdef VERBOSE_OPS
  fmt::print(TEXT("Creating Subtraction({}, {})\n"), ToPlainString(a), ToPlainString(b));
#endif
  return MakeExprBase<Subtraction>(a, b);
}

ExpressionBaseConstPtr CreateNegation(const ExpressionBaseConstPtr& x) {
  ASSERT(x);
  if (IsZero(x)) {
    return Constants::Zero;
  }
  // If this is already negated, flip it back:
  if (const Negate* const as_negate = x->As<Negate>()) {
    return as_negate->Inner();
  }
  return MakeExprBase<Negate>(x);
}

ExpressionBaseConstPtr CreateDivision(const ExpressionBaseConstPtr&,
                                      const ExpressionBaseConstPtr&) {
  return {};  //  todo: impl
}

}  // namespace math
