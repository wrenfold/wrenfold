// Copyright 2022 Gareth Cross
#pragma once
#include "expression.h"
#include "expressions/all_expressions.h"
#include "functions.h"
#include "operations_fwd.h"
#include "visitor_base.h"

namespace math {

using namespace math::custom_literals;

// Visitor that takes the derivative of an input expression.
class DiffVisitor {
 public:
  constexpr static VisitorPolicy Policy = VisitorPolicy::CompileError;
  using ReturnType = Expr;

  // Construct w/ const reference to the variable to differentiate wrt to.
  // Must remain in scope for the duration of evaluation.
  explicit DiffVisitor(const Variable& argument) : argument_(argument) {}

  // Differentiate every argument to make a new sum.
  Expr Apply(const Addition& add) {
    std::vector<Expr> terms;
    terms.reserve(add.Arity());
    std::transform(add.begin(), add.end(), std::back_inserter(terms),
                   [this](const Expr& x) { return VisitStruct(x, *this); });
    return Addition::FromOperands(terms);
  }

  Expr Apply(const Constant&) { return Constants::Zero; }

  // Do product expansion over all terms in the multiplication:
  // a * b
  // a' * b + a * b'
  // a * b * c
  // a' * b * c + a * b' * c + a * b * c'
  Expr Apply(const Multiplication& mul) {
    std::vector<Expr> add_terms;  //  TODO: Small vector.
    add_terms.reserve(mul.Arity());
    // Differentiate wrt every argument:
    for (std::size_t i = 0; i < mul.Arity(); ++i) {
      std::vector<Expr> mul_terms;
      mul_terms.reserve(mul.Arity());
      for (std::size_t j = 0; j < mul.Arity(); ++j) {
        if (j == i) {
          mul_terms.push_back(VisitStruct(mul[j], *this));
        } else {
          mul_terms.push_back(mul[j]);
        }
      }
      add_terms.push_back(Multiplication::FromOperands(mul_terms));
    }
    // TODO: Move, don't copy.
    return Addition::FromOperands(add_terms);
  }
  // Cos, Sin, Tan, ArcCos, ArcSin, ArcTan, NaturalLog
  Expr Apply(const UnaryFunction& func) {
    // Differentiate the argument:
    Expr d_arg = VisitStruct(func.Arg(), *this);
    if (IsZero(d_arg)) {
      // If zero, we don't need to do any further operations.
      return Constants::Zero;
    }
    // TODO: Make global constants for 2, one half, etc...
    switch (func.Func()) {
      case UnaryFunctionName::Cos:
        // cos(f(x)) --> -sin(f(x)) * f'(x)
        return -sin(func.Arg()) * d_arg;
      case UnaryFunctionName::Sin:
        // sin(f(x)) --> cos(f(x)) * f'(x)
        return cos(func.Arg()) * d_arg;
      case UnaryFunctionName::Tan:
        // tan(f(x)) --> sec^2(f(x)) * f'(x) --> 1/cos^2(f(x)) * f'(x)
        return pow(cos(func.Arg()), -2) * d_arg;
      case UnaryFunctionName::ArcCos:
        // acos(f(x)) --> -f'(x) / sqrt(1 - f(x)^2)
        return -pow(Constants::One - pow(func.Arg(), 2), -1_s / 2) * d_arg;
      case UnaryFunctionName::ArcSin:
        // asin(f(x)) --> f'(x) / sqrt(1 - f(x)^2)
        return pow(Constants::One - pow(func.Arg(), 2), -1_s / 2) * d_arg;
      case UnaryFunctionName::ArcTan:
        // atan(f(x)) --> f'(x) / (f(x)^2 + 1)
        return d_arg / (pow(func.Arg(), 2) + Constants::One);
      case UnaryFunctionName::Log:
        // log(f(x)) --> 1/f(x) * f'(x)
        return Power::Create(func.Arg(), Constants::NegativeOne) * d_arg;
      case UnaryFunctionName::ENUM_SIZE:
        break;
    }
    ASSERT(false, "Invalid unary function: {}", func.Name());
    return Constants::Zero;
  }

  Expr Apply(const Integer&) { return Constants::Zero; }

  Expr Apply(const Float&) { return Constants::Zero; }

  Expr Apply(const Power& pow) {
    const Expr& a = pow.Base();
    const Expr& b = pow.Exponent();
    const Expr a_diff = VisitStruct(a, *this);
    const Expr b_diff = VisitStruct(b, *this);
    // TODO: Check if a_diff and b_diff are zero.
    return b * Power::Create(a, b - Constants::One) * a_diff +
           Power::Create(a, b) * log(a) * b_diff;
  }

  Expr Apply(const Rational&) const { return Constants::Zero; }

  Expr Apply(const Variable& var) {
    if (var.IsIdenticalToImplTyped(argument_)) {
      return Constants::One;
    }
    return Constants::Zero;
  }

 private:
  Variable argument_;
};

}  // namespace math
