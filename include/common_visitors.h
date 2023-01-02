// Copyright 2022 Gareth Cross
#pragma once
#include <algorithm>

#include "expressions/constant_expressions.h"
#include "visitor_impl.h"

// This file is intended to contain common utility visitors.
namespace math {

template <typename Derived>
class NAryOp;

// Visitor that checks for integral values.
struct IsIntegralValueVisitor {
  using ReturnType = bool;
  constexpr static VisitorPolicy Policy = VisitorPolicy::NoError;
  constexpr bool Apply(const Integer&) const { return true; }
};

inline bool IsIntegralValue(const Expr& expr) {
  return VisitStruct(expr, IsIntegralValueVisitor{}).value_or(false);
}

struct IsNumericVisitor {
  using ReturnType = bool;
  constexpr static VisitorPolicy Policy = VisitorPolicy::NoError;
  constexpr bool Apply(const Integer&) const { return true; }
  constexpr bool Apply(const Float&) const { return true; }
};

inline bool IsNumeric(const Expr& expr) {
  return VisitStruct(expr, IsNumericVisitor{}).value_or(false);
}

struct IsNegativeNumberVisitor {
  using ReturnType = bool;
  constexpr static VisitorPolicy Policy = VisitorPolicy::NoError;
  bool Apply(const Integer& num) const { return num.GetValue() < 0; }
  bool Apply(const Float& f) const { return f.GetValue() < 0; }
};

inline bool IsNegativeNumber(const Expr& expr) {
  return VisitStruct(expr, IsNegativeNumberVisitor{}).value_or(false);
}

struct PrecedenceVisitor {
  static constexpr VisitorPolicy Policy = VisitorPolicy::NoError;
  using ReturnType = Precedence;

  constexpr Precedence Apply(const Multiplication&) const { return Precedence::Multiplication; }
  constexpr Precedence Apply(const Addition&) const { return Precedence::Addition; }
  constexpr Precedence Apply(const Power&) const { return Precedence::Power; }
};

inline Precedence GetPrecedence(const Expr& expr) {
  return VisitStruct(expr, PrecedenceVisitor{}).value_or(Precedence::None);
}

// Simple visitor that evaluates to true for n-ary operations.
struct IsNAryOpVisitor {
  using ReturnType = bool;
  constexpr static VisitorPolicy Policy = VisitorPolicy::NoError;

  template <typename Derived>
  constexpr ReturnType Apply(const NAryOp<Derived>&) const {
    return true;
  }
};

inline bool IsNAryOp(const Expr& expr) {
  return VisitStruct(expr, IsNAryOpVisitor{}).value_or(false);
}

}  // namespace math
