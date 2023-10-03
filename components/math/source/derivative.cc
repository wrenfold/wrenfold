// Copyright 2023 Gareth Cross
#include "expressions/all_expressions.h"

#include <algorithm>

#include "error_types.h"

namespace math {

using namespace math::custom_literals;

// Visitor that checks if an expression is a function of `target`.
// Returns true if `target` appears as a sub-expression in anything we visit.
template <typename T>
struct IsFunctionOfVisitor {
  explicit IsFunctionOfVisitor(const T& target) : target_(target) {}

  template <typename U>
  bool operator()(const U& x) {
    if constexpr (std::is_same_v<U, T>) {
      return target_.is_identical_to(x);
    } else if constexpr (!U::IsLeafNode) {
      // True if any of the children satisfy this.
      bool contained_in_child = false;
      x.iterate([this, &contained_in_child](const Expr& expr) {
        if (Visit(expr, *this)) {
          contained_in_child = true;
        }
      });
      return contained_in_child;
    } else {
      return false;
    }
  }

  const T& target_;
};

// Visitor that takes the derivative of an input expression.
template <typename T>
class DiffVisitor {
 public:
  static_assert(std::is_same_v<T, Variable> || std::is_same_v<T, FunctionArgument>);

  // Construct w/ const reference to the variable to differentiate wrt to.
  // Must remain in scope for the duration of evaluation.
  explicit DiffVisitor(const T& argument, const Expr& argument_abstract)
      : argument_(argument), argument_abstract_(argument_abstract) {}

  // Differentiate every argument to make a new sum.
  Expr operator()(const Addition& add) {
    return add.map_children([&](const Expr& x) { return VisitWithExprArg(x, *this); });
  }

  // TODO: This is not strictly correct. If the condition is a function of `x` (where x is the
  //  the variable wrt we are differentiating), we should insert the dirac delta function.
  //  That said, this is more useful practically in most cases.
  Expr operator()(const Conditional& cond) {
    return where(cond.condition(), VisitWithExprArg(cond.if_branch(), *this),
                 VisitWithExprArg(cond.else_branch(), *this));
  }

  Expr operator()(const Constant&) const { return Constants::Zero; }

  // Derivative of an abstract derivative expression.
  Expr operator()(const Derivative& derivative, const Expr& derivative_abstract) const {
    const bool is_relevant = Visit(derivative.differentiand(), IsFunctionOfVisitor<T>{argument_});
    if (!is_relevant) {
      return Constants::Zero;
    }
    return Derivative::create(derivative_abstract, argument_abstract_, 1);
  }

  // Do product expansion over all terms in the multiplication:
  // a * b
  // a' * b + a * b'
  // a * b * c
  // a' * b * c + a * b' * c + a * b * c'
  Expr operator()(const Multiplication& mul) {
    std::vector<Expr> add_terms;  //  TODO: Small vector.
    add_terms.reserve(mul.arity());
    // Differentiate wrt every argument:
    for (std::size_t i = 0; i < mul.arity(); ++i) {
      std::vector<Expr> mul_terms;
      mul_terms.reserve(mul.arity());
      for (std::size_t j = 0; j < mul.arity(); ++j) {
        if (j == i) {
          mul_terms.push_back(VisitWithExprArg(mul[j], *this));
        } else {
          mul_terms.push_back(mul[j]);
        }
      }
      add_terms.push_back(Multiplication::from_operands(mul_terms));
    }
    // TODO: Move, don't copy.
    return Addition::from_operands(add_terms);
  }

  // Cos, Sin, Tan, ArcCos, ArcSin, ArcTan, NaturalLog
  Expr operator()(const Function& func) {
    // Differentiate the arguments:
    Function::ContainerType d_args{};
    std::transform(func.begin(), func.end(), std::back_inserter(d_args),
                   [this](const Expr& arg) { return VisitWithExprArg(arg, *this); });

    const bool all_derivatives_zero = std::all_of(d_args.begin(), d_args.end(), &is_zero);
    if (all_derivatives_zero) {
      // If zero, we don't need to do any further operations.
      return Constants::Zero;
    }

    // TODO: Make global constants for 2, one half, etc...
    static const Expr one_half = 1_s / 2;
    static const Expr negative_one_half = -1_s / 2;

    const auto& args = func.args();
    switch (func.enum_value()) {
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
        return Power::create(args[0], Constants::NegativeOne) * d_args[0];
      case BuiltInFunctionName::Sqrt:
        // sqrt(f(x)) --> (1/2) * f'(x) / sqrt(f(x))
        return pow(args[0], negative_one_half) * one_half * d_args[0];
      case BuiltInFunctionName::Abs:
        // |f(x)| --> f(x)/|f(x)| * f'(x)
        // TODO: Add complex argument version.
        return args[0] / abs(args[0]) * d_args[0];
      case BuiltInFunctionName::Signum:
        // signum(f(x)) --> d[heaviside(f(x)) - heaviside(-f(x))]/dx = 2 * dirac(f(x)) * f'(x)
        // However, we don't have dirac - so we leave this abstract.
        return Derivative::create(signum(args[0]), argument_abstract_, 1) * d_args[0];
      case BuiltInFunctionName::Arctan2: {
        const Expr sum_squared = args[0] * args[0] + args[1] * args[1];
        const Expr y_diff = VisitWithExprArg(args[0], *this);
        const Expr x_diff = VisitWithExprArg(args[1], *this);
        if (is_zero(y_diff) && is_zero(x_diff)) {
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
    ASSERT(false, "Invalid unary function: {}", func.function_name());
    return Constants::Zero;
  }

  Expr operator()(const Infinity&) const { return Constants::Zero; }
  Expr operator()(const Integer&) const { return Constants::Zero; }
  Expr operator()(const Float&) const { return Constants::Zero; }

  Expr operator()(const FunctionArgument& arg) const {
    if constexpr (std::is_same_v<T, FunctionArgument>) {
      if (argument_.is_identical_to(arg)) {
        return Constants::One;
      }
    }
    return Constants::Zero;
  }

  Expr operator()(const Power& pow) { return PowerDiff(pow.base(), pow.exponent()); }

  Expr PowerDiff(const Expr& a, const Expr& b) {
    const Expr a_diff = VisitWithExprArg(a, *this);
    const Expr b_diff = VisitWithExprArg(b, *this);
    if (is_zero(a_diff) && is_zero(b_diff)) {
      return Constants::Zero;
    }
    return b * Power::create(a, b - Constants::One) * a_diff +
           Power::create(a, b) * log(a) * b_diff;
  }

  Expr operator()(const Rational&) const { return Constants::Zero; }

  Expr operator()(const Relational& relational) const {
    throw TypeError("Cannot differentiate expression of type `{}`: {} {} {}", Relational::NameStr,
                    relational.left().to_string(), relational.operation_string(),
                    relational.right().to_string());
  }

  Expr operator()(const Variable& var) const {
    if constexpr (std::is_same_v<Variable, T>) {
      if (var.is_identical_to(argument_)) {
        return Constants::One;
      }
    }
    return Constants::Zero;
  }

 private:
  const T& argument_;
  const Expr& argument_abstract_;
};

template <typename T>
inline Expr diff_typed(const Expr& expr, const T& arg, const Expr& arg_abstract, const int reps) {
  DiffVisitor<T> visitor{arg, arg_abstract};
  Expr result = expr;
  for (int i = 0; i < reps; ++i) {
    result = VisitWithExprArg(result, visitor);
  }
  return result;
}

Expr diff(const Expr& differentiand, const Expr& arg, const int reps) {
  ASSERT_GREATER_OR_EQ(reps, 0);
  if (const Variable* var = cast_ptr<Variable>(arg); var != nullptr) {
    return diff_typed<Variable>(differentiand, *var, arg, reps);
  } else if (const FunctionArgument* func = cast_ptr<FunctionArgument>(arg); func != nullptr) {
    return diff_typed<FunctionArgument>(differentiand, *func, arg, reps);
  } else {
    throw TypeError(
        "Argument to diff must be of type Variable or FunctionArgument. Received expression "
        "of type: {}",
        arg.type_name());
  }
}

}  // namespace math
