// Copyright 2023 Gareth Cross
#include "wf/derivative.h"

#include <algorithm>

#include "wf/error_types.h"
#include "wf/expressions/all_expressions.h"
#include "wf/matrix_expression.h"
#include "wf_runtime/span.h"

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
      return std::any_of(x.begin(), x.end(),
                         [this](const Expr& expr) { return visit(expr, *this); });
    } else {
      return false;
    }
  }

  const T& target_;
};

DiffVisitor::DiffVisitor(const Expr& argument) : argument_(argument) {
  if (!argument.is_type<Variable>()) {
    throw TypeError(
        "Argument to diff must be of type Variable. Received expression "
        "of type: {}",
        argument.type_name());
  }
}

Expr DiffVisitor::apply(const Expr& expression) { return visit_with_expr(expression, *this); }

Expr DiffVisitor::cached_visit(const Expr& expr) {
  auto it = cache_.find(expr);
  if (it != cache_.end()) {
    return it->second;
  }
  Expr result = visit_with_expr(expr, *this);
  auto [it_inserted, _] = cache_.emplace(expr, std::move(result));
  return it_inserted->second;
}

// Differentiate every argument to make a new sum.
Expr DiffVisitor::operator()(const Addition& add) {
  return add.map_children([this](const Expr& expr) { return cached_visit(expr); });
}

Expr DiffVisitor::operator()(const CastBool&, const Expr& expr) {
  return Derivative::create(expr, argument_, 1);
}

// TODO: This is not strictly correct. If the condition is a function of `x` (where x is the
//  the variable wrt we are differentiating), we should insert the dirac delta function.
//  That said, this is more useful practically in most cases.
Expr DiffVisitor::operator()(const Conditional& cond) {
  return where(cond.condition(), cached_visit(cond.if_branch()), cached_visit(cond.else_branch()));
}

Expr DiffVisitor::operator()(const Constant&) const { return Constants::Zero; }

// Derivative of an abstract derivative expression.
Expr DiffVisitor::operator()(const Derivative& derivative, const Expr& derivative_abstract) const {
  const Variable& argument = cast_unchecked<Variable>(argument_);
  const bool is_relevant =
      visit(derivative.differentiand(), IsFunctionOfVisitor<Variable>{argument});
  if (!is_relevant) {
    return Constants::Zero;
  }
  return Derivative::create(derivative_abstract, argument_, 1);
}

// Do product expansion over all terms in the multiplication:
// a * b
// a' * b + a * b'
// a * b * c
// a' * b * c + a * b' * c + a * b * c'
Expr DiffVisitor::operator()(const Multiplication& mul) {
  std::vector<Expr> add_terms;  //  TODO: Small vector.
  add_terms.reserve(mul.arity());

  // Differentiate wrt every argument:
  for (std::size_t i = 0; i < mul.arity(); ++i) {
    std::vector<Expr> mul_terms;
    mul_terms.reserve(mul.arity());
    for (std::size_t j = 0; j < mul.arity(); ++j) {
      if (j == i) {
        mul_terms.push_back(cached_visit(mul[j]));
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
Expr DiffVisitor::operator()(const Function& func) {
  // Differentiate the arguments:
  Function::ContainerType d_args{};
  std::transform(func.begin(), func.end(), std::back_inserter(d_args),
                 [this](const Expr& arg) { return cached_visit(arg); });

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
    case BuiltInFunction::Cos:
      // cos(f(x)) --> -sin(f(x)) * f'(x)
      return -sin(args[0]) * d_args[0];
    case BuiltInFunction::Sin:
      // sin(f(x)) --> cos(f(x)) * f'(x)
      return cos(args[0]) * d_args[0];
    case BuiltInFunction::Tan:
      // tan(f(x)) --> sec^2(f(x)) * f'(x) --> 1/cos^2(f(x)) * f'(x)
      return pow(cos(args[0]), -2) * d_args[0];
    case BuiltInFunction::ArcCos:
      // acos(f(x)) --> -f'(x) / sqrt(1 - f(x)^2)
      return -pow(Constants::One - pow(args[0], 2), negative_one_half) * d_args[0];
    case BuiltInFunction::ArcSin:
      // asin(f(x)) --> f'(x) / sqrt(1 - f(x)^2)
      return pow(Constants::One - pow(args[0], 2), negative_one_half) * d_args[0];
    case BuiltInFunction::ArcTan:
      // atan(f(x)) --> f'(x) / (f(x)^2 + 1)
      return d_args[0] / (pow(args[0], 2) + Constants::One);
    case BuiltInFunction::Log:
      // log(f(x)) --> 1/f(x) * f'(x)
      return Power::create(args[0], Constants::NegativeOne) * d_args[0];
    case BuiltInFunction::Abs:
      // |f(x)| --> f(x)/|f(x)| * f'(x)
      // TODO: Add complex argument version.
      return args[0] / abs(args[0]) * d_args[0];
    case BuiltInFunction::Signum: {
      // signum(f(x)) --> d[heaviside(f(x)) - heaviside(-f(x))]/dx = 2 * dirac(f(x)) * f'(x)
      // However, we don't have dirac - so we leave this abstract.
      return Derivative::create(signum(args[0]), argument_, 1);
    }
    case BuiltInFunction::Arctan2: {
      const Expr sum_squared = args[0] * args[0] + args[1] * args[1];
      const Expr y_diff = visit_with_expr(args[0], *this);
      const Expr x_diff = visit_with_expr(args[1], *this);
      if (is_zero(y_diff) && is_zero(x_diff)) {
        return Constants::Zero;
      }
      // atan2(y(u), x(u))/du = -y/(y^2 + x^2) * x'(u) + x/(y^2 + x^2) * y'(u)
      return -(args[0] * x_diff) / sum_squared + (args[1] * y_diff) / sum_squared;
    }
  }
  ZEN_ASSERT(false, "Invalid unary function: {}", func.function_name());
  return Constants::Zero;
}

Expr DiffVisitor::operator()(const Infinity&) const { return Constants::Zero; }
Expr DiffVisitor::operator()(const Integer&) const { return Constants::Zero; }
Expr DiffVisitor::operator()(const Float&) const { return Constants::Zero; }
Expr DiffVisitor::operator()(const Power& pow) {
  const Expr& a = pow.base();
  const Expr& b = pow.exponent();
  const Expr a_diff = cached_visit(a);
  const Expr b_diff = cached_visit(b);
  if (is_zero(a_diff) && is_zero(b_diff)) {
    return Constants::Zero;
  }
  return b * Power::create(a, b - Constants::One) * a_diff + Power::create(a, b) * log(a) * b_diff;
}

Expr DiffVisitor::operator()(const Rational&) const { return Constants::Zero; }

Expr DiffVisitor::operator()(const Relational&, const Expr& rel_expr) const {
  // Cannot differentiate relationals, so insert an abstract expression.
  return Derivative::create(rel_expr, argument_, 1);
}

Expr DiffVisitor::operator()(const Undefined&) const { return Constants::Undefined; }

Expr DiffVisitor::operator()(const Variable& var) const {
  const Variable& argument = cast_unchecked<Variable>(argument_);
  if (var.is_identical_to(argument)) {
    return Constants::One;
  }
  return Constants::Zero;
}

Expr diff(const Expr& function, const Expr& var, const int reps) {
  ZEN_ASSERT_GREATER_OR_EQ(reps, 0);
  DiffVisitor visitor{var};
  Expr result = function;
  for (int i = 0; i < reps; ++i) {
    result = visit_with_expr(result, visitor);
  }
  return result;
}

MatrixExpr jacobian(const absl::Span<const Expr> functions, const absl::Span<const Expr> vars) {
  if (functions.empty()) {
    throw TypeError("Need at least one function to differentiate.");
  }
  if (vars.empty()) {
    throw TypeError("Need at least one variable to differentiate with respect to.");
  }

  std::vector<Expr> result{};
  result.resize(functions.size() * vars.size(), Constants::Zero);

  // Crate row-major span over `result`:
  const auto output_dims = make_value_pack(functions.size(), vars.size());
  const auto output_strides = make_value_pack(vars.size(), constant<1>{});
  auto result_span = make_span(result.data(), output_dims, output_strides);

  for (std::size_t col = 0; col < vars.size(); ++col) {
    // We cache derivative expressions, so that every row reuses the same cache:
    DiffVisitor diff_visitor{vars[col]};
    for (std::size_t row = 0; row < functions.size(); ++row) {
      result_span(row, col) = diff_visitor.apply(functions[row]);
    }
  }

  return MatrixExpr::create(static_cast<index_t>(functions.size()),
                            static_cast<index_t>(vars.size()), std::move(result));
}

}  // namespace math
