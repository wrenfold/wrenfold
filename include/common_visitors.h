// Copyright 2022 Gareth Cross
#pragma once
#include <algorithm>

#include "template_helpers.h"
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
