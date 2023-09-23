// Copyright 2023 Gareth Cross
#include "expressions/all_expressions.h"

#include <algorithm>

#include "error_types.h"

namespace math {

using namespace math::custom_literals;

// Visitor that takes the derivative of an input expression.
template <typename T>
class DiffVisitor {
 public:
  static_assert(std::is_same_v<T, Variable> || std::is_same_v<T, FunctionArgument>);
  using ReturnType = Expr;

  // Construct w/ const reference to the variable to differentiate wrt to.
  // Must remain in scope for the duration of evaluation.
  explicit DiffVisitor(const T& argument) : argument_(argument) {}

  // Differentiate every argument to make a new sum.
  Expr operator()(const Addition& add) {
    return MapChildren(add, [&](const Expr& x) { return Visit(x, *this); });
  }

  // TODO: This should insert a dirac delta function at the transition point (if the condition
  // is a function of the variable we are differentiating).
  Expr operator()(const Conditional& cond) {
    return where(cond.Condition(), Visit(cond.IfBranch(), *this), Visit(cond.ElseBranch(), *this));
  }

  Expr operator()(const Constant&) const { return Constants::Zero; }

  // Element-wise derivative of matrix.
  Expr operator()(const Matrix& mat) {
    return MapChildren(mat, [this](const Expr& x) { return Visit(x, *this); });
  }

  // Do product expansion over all terms in the multiplication:
  // a * b
  // a' * b + a * b'
  // a * b * c
  // a' * b * c + a * b' * c + a * b * c'
  Expr operator()(const Multiplication& mul) {
    std::vector<Expr> add_terms;  //  TODO: Small vector.
    add_terms.reserve(mul.Arity());
    // Differentiate wrt every argument:
    for (std::size_t i = 0; i < mul.Arity(); ++i) {
      std::vector<Expr> mul_terms;
      mul_terms.reserve(mul.Arity());
      for (std::size_t j = 0; j < mul.Arity(); ++j) {
        if (j == i) {
          mul_terms.push_back(Visit(mul[j], *this));
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
  Expr operator()(const Function& func) {
    // Differentiate the arguments:
    Function::ContainerType d_args{};
    std::transform(func.begin(), func.end(), std::back_inserter(d_args),
                   [this](const Expr& arg) { return Visit(arg, *this); });

    const bool all_derivatives_zero = std::all_of(d_args.begin(), d_args.end(), &IsZero);
    if (all_derivatives_zero) {
      // If zero, we don't need to do any further operations.
      return Constants::Zero;
    }

    // TODO: Make global constants for 2, one half, etc...
    static const Expr one_half = 1_s / 2;
    static const Expr negative_one_half = -1_s / 2;

    const auto& args = func.Args();
    switch (func.Func()) {
      case BuiltInFunctionName::Cos:
        // cos(f(x)) --> -sin(f(x)) * f'(x)
        return -sin(args[0]) * d_args[0];
      case BuiltInFunctionName::Sin:
        // sin(f(x)) --> cos(f(x)) * f'(x)
        return cos(args[0]) * d_args[0];
      case BuiltInFunctionName::Tan:
        // tan(f(x)) --> sec^2(f(x)) * f'(x) --> 1/cos^2(f(x)) * f'(x)
        return pow(cos(args[0]), -2) * d_args[0];
      case BuiltInFunctionName::ArcCos:
        // acos(f(x)) --> -f'(x) / sqrt(1 - f(x)^2)
        return -pow(Constants::One - pow(args[0], 2), negative_one_half) * d_args[0];
      case BuiltInFunctionName::ArcSin:
        // asin(f(x)) --> f'(x) / sqrt(1 - f(x)^2)
        return pow(Constants::One - pow(args[0], 2), negative_one_half) * d_args[0];
      case BuiltInFunctionName::ArcTan:
        // atan(f(x)) --> f'(x) / (f(x)^2 + 1)
        return d_args[0] / (pow(args[0], 2) + Constants::One);
      case BuiltInFunctionName::Log:
        // log(f(x)) --> 1/f(x) * f'(x)
        return Power::Create(args[0], Constants::NegativeOne) * d_args[0];
      case BuiltInFunctionName::Sqrt:
        // sqrt(f(x)) --> (1/2) * f'(x) / sqrt(f(x))
        return pow(args[0], negative_one_half) * one_half * d_args[0];
      case BuiltInFunctionName::Abs:
        // |f(x)| --> f(x)/|f(x)| * f'(x)
        // TODO: Add complex argument version.
        return args[0] / abs(args[0]) * d_args[0];
      case BuiltInFunctionName::Arctan2: {
        const Expr sum_squared = args[0] * args[0] + args[1] * args[1];
        const Expr& y_diff = Visit(args[0], *this);
        const Expr& x_diff = Visit(args[1], *this);
        if (IsZero(y_diff) && IsZero(x_diff)) {
          return Constants::Zero;
        }
        // atan2(y(u), x(u))/du = -y/(y^2 + x^2) * x'(u) + x/(y^2 + x^2) * y'(u)
        return -(args[0] * x_diff) / sum_squared + (args[1] * y_diff) / sum_squared;
      }
      case BuiltInFunctionName::Pow:
        return PowerDiff(args[0], args[1]);
      case BuiltInFunctionName::ENUM_SIZE:
        break;
    }
    ASSERT(false, "Invalid unary function: {}", func.Name());
    return Constants::Zero;
  }

  Expr operator()(const Infinity&) const { return Constants::Zero; }
  Expr operator()(const Integer&) const { return Constants::Zero; }
  Expr operator()(const Float&) const { return Constants::Zero; }

  Expr operator()(const FunctionArgument& arg) const {
    if constexpr (std::is_same_v<T, FunctionArgument>) {
      if (argument_.IsIdenticalTo(arg)) {
        return Constants::One;
      }
    }
    return Constants::Zero;
  }

  Expr operator()(const Power& pow) { return PowerDiff(pow.Base(), pow.Exponent()); }

  Expr PowerDiff(const Expr& a, const Expr& b) {
    const Expr a_diff = Visit(a, *this);
    const Expr b_diff = Visit(b, *this);
    if (IsZero(a_diff) && IsZero(b_diff)) {
      return Constants::Zero;
    }
    return b * Power::Create(a, b - Constants::One) * a_diff +
           Power::Create(a, b) * log(a) * b_diff;
  }

  Expr operator()(const Rational&) const { return Constants::Zero; }

  Expr operator()(const Relational& relational) const {
    throw TypeError("Cannot differentiate expression of type `{}`: {} {} {}", Relational::NameStr,
                    relational.Left().ToString(), relational.OperationString(),
                    relational.Right().ToString());
  }

  Expr operator()(const Variable& var) const {
    if constexpr (std::is_same_v<Variable, T>) {
      if (var.IsIdenticalTo(argument_)) {
        return Constants::One;
      }
    }
    return Constants::Zero;
  }

 private:
  const T& argument_;
};

template <typename T>
inline Expr DiffTyped(const Expr& expr, const T& arg, const int reps) {
  DiffVisitor<T> visitor{arg};
  Expr result = expr;
  for (int i = 0; i < reps; ++i) {
    result = Visit(result, visitor);
  }
  return result;
}

Expr Diff(const Expr& differentiand, const Expr& arg, const int reps) {
  ASSERT_GREATER_OR_EQ(reps, 0);
  if (const Variable* var = CastPtr<Variable>(arg); var != nullptr) {
    return DiffTyped<Variable>(differentiand, *var, reps);
  } else if (const FunctionArgument* func = CastPtr<FunctionArgument>(arg); func != nullptr) {
    return DiffTyped<FunctionArgument>(differentiand, *func, reps);
  } else {
    throw TypeError(
        "Argument to diff must be of type Variable or FunctionArgument. Received expression "
        "of type: {}",
        arg.TypeName());
  }
}

}  // namespace math
