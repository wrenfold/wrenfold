// Copyright 2023 Gareth Cross
#include "wf/derivative.h"

#include <algorithm>

#include "wf/error_types.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"
#include "wf/matrix_expression.h"
#include "wrenfold/span.h"

namespace wf {

using namespace wf::custom_literals;

// Visitor that checks if an expression is a function of `target`.
// Returns true if `target` appears as a sub-expression in anything we visit.
template <typename T>
struct is_function_of_visitor {
  explicit is_function_of_visitor(const T& target) noexcept : target_(target) {}

  bool operator()(const scalar_expr& expr) const { return visit(expr, *this); }
  bool operator()(const boolean_expr& expr) const { return visit(expr, *this); }
  bool operator()(const matrix_expr& expr) const { return visit(expr, *this); }

  template <typename U>
  bool operator()(const U& x) const {
    if constexpr (std::is_same_v<U, T>) {
      return are_identical(target_, x);
    } else if constexpr (!U::is_leaf_node) {
      if constexpr (std::is_same_v<conditional, U>) {
        // TODO: A generic method of iterating over mixed-type expressions like `conditional`.
        return operator()(x.condition()) || operator()(x.if_branch()) ||
                                            operator()(x.else_branch());
      } else {
        return std::any_of(x.begin(), x.end(),
                           [this](const auto& expr) { return visit(expr, *this); });
      }
    } else {
      return false;
    }
  }

  const T& target_;
};

derivative_visitor::derivative_visitor(const scalar_expr& argument,
                                       const non_differentiable_behavior behavior)
    : argument_(argument), non_diff_behavior_(behavior) {
  if (!argument.is_type<variable, compound_expression_element>()) {
    throw type_error(
        "Argument to diff must be of type `{}` or `{}`. Received expression "
        "of type: {}",
        variable::name_str, compound_expression_element::name_str, argument.type_name());
  }
}

scalar_expr derivative_visitor::apply(const scalar_expr& expression) {
  if (const auto it = cache_.find(expression); it != cache_.end()) {
    return it->second;
  }
  scalar_expr result = visit(expression, *this);
  auto [it_inserted, _] = cache_.emplace(expression, std::move(result));
  return it_inserted->second;
}

// Differentiate every argument to make a new sum.
scalar_expr derivative_visitor::operator()(const addition& add) {
  return add.map_children([this](const scalar_expr& expr) { return apply(expr); });
}

scalar_expr derivative_visitor::operator()(const compound_expression_element& el,
                                           const scalar_expr& expr) const {
  if (const compound_expression_element* arg = get_if<const compound_expression_element>(argument_);
      arg != nullptr && are_identical(*arg, el)) {
    return constants::one;
  }
  if (non_diff_behavior_ == non_differentiable_behavior::abstract) {
    return derivative::create(expr, argument_, 1);
  } else {
    return constants::zero;
  }
}

// TODO: This is not strictly correct. If the condition is a function of `x` (where x is the
//  the variable wrt we are differentiating), we should insert the dirac delta function.
//  That said, this is more useful practically in most cases.
scalar_expr derivative_visitor::operator()(const conditional& cond) {
  return where(cond.condition(), apply(cond.if_branch()), apply(cond.else_branch()));
}

scalar_expr derivative_visitor::operator()(const symbolic_constant&) const {
  return constants::zero;
}

// Derivative of an abstract derivative expression.
scalar_expr derivative_visitor::operator()(const derivative& derivative,
                                           const scalar_expr& derivative_abstract) const {
  return visit(argument_, [&](const auto& arg) -> scalar_expr {
    using T = std::decay_t<decltype(arg)>;
    if constexpr (type_list_contains_v<T, variable, compound_expression_element>) {
      if (const bool is_relevant = is_function_of_visitor<T>{arg}(derivative.differentiand());
          !is_relevant) {
        return constants::zero;
      }
      return derivative::create(derivative_abstract, argument_, 1);
    } else {
      WF_ASSERT_ALWAYS("Can only take derivatives wrt `{}` and `{}` expressions.",
                       variable::name_str, compound_expression_element::name_str);
    }
  });
}

// Do product expansion over all terms in the multiplication:
// a * b
// a' * b + a * b'
// a * b * c
// a' * b * c + a * b' * c + a * b * c'
scalar_expr derivative_visitor::operator()(const multiplication& mul) {
  std::vector<scalar_expr> add_terms;  //  TODO: Small vector.
  add_terms.reserve(mul.size());

  // Differentiate wrt every argument:
  for (std::size_t i = 0; i < mul.size(); ++i) {
    std::vector<scalar_expr> mul_terms;
    mul_terms.reserve(mul.size());
    for (std::size_t j = 0; j < mul.size(); ++j) {
      if (j == i) {
        mul_terms.push_back(apply(mul[j]));
      } else {
        mul_terms.push_back(mul[j]);
      }
    }
    add_terms.push_back(multiplication::from_operands(mul_terms));
  }

  // TODO: Move, don't copy.
  return addition::from_operands(add_terms);
}

// Cos, Sin, Tan, ArcCos, ArcSin, ArcTan, NaturalLog
scalar_expr derivative_visitor::operator()(const function& func, const scalar_expr& func_abstract) {
  // Differentiate the arguments:
  function::container_type d_args = transform_map<function::container_type>(
      func, [this](const scalar_expr& arg) { return apply(arg); });

  if (const bool all_derivatives_zero = std::all_of(d_args.begin(), d_args.end(), &is_zero);
      all_derivatives_zero) {
    // If zero, we don't need to do any further operations.
    return constants::zero;
  }

  static const scalar_expr one_half = 1_s / 2;
  static const scalar_expr negative_one_half = -1_s / 2;

  const auto& args = func.args();
  switch (func.enum_value()) {
    case built_in_function::cos:
      // cos(f(x)) --> -sin(f(x)) * f'(x)
      return -sin(args[0]) * d_args[0];
    case built_in_function::sin:
      // sin(f(x)) --> cos(f(x)) * f'(x)
      return cos(args[0]) * d_args[0];
    case built_in_function::tan:
      // tan(f(x)) --> sec^2(f(x)) * f'(x) --> 1/cos^2(f(x)) * f'(x)
      return pow(cos(args[0]), -2) * d_args[0];
    case built_in_function::arccos:
      // acos(f(x)) --> -f'(x) / sqrt(1 - f(x)^2)
      return -pow(constants::one - pow(args[0], 2), negative_one_half) * d_args[0];
    case built_in_function::arcsin:
      // asin(f(x)) --> f'(x) / sqrt(1 - f(x)^2)
      return pow(constants::one - pow(args[0], 2), negative_one_half) * d_args[0];
    case built_in_function::arctan:
      // atan(f(x)) --> f'(x) / (f(x)^2 + 1)
      return d_args[0] / (pow(args[0], 2) + constants::one);
    case built_in_function::cosh:
      // cosh(f(x) --> sinh(f(x)) * f'(x)
      return d_args[0] * sinh(args[0]);
    case built_in_function::sinh:
      // sinh(f(x)) --> cosh(f(x)) * f'(x)
      return d_args[0] * cosh(args[0]);
    case built_in_function::tanh:
      // tanh(f(x)) --> (1 - tanh(f(x)**2) * f'(x) (or  1/cosh^2(f(x)) * f'(x))
      return (1 - pow(func_abstract, 2)) * d_args[0];
    case built_in_function::arccosh: {
      // cosh(f(x)) --> 1 / sqrt(f(x)**2 - 1) * 1 / sqrt(f(x)**2 + 1) * f'(x)
      return pow(sqrt(args[0] + 1) * sqrt(args[0] - 1), constants::negative_one) * d_args[0];
    }
    case built_in_function::arcsinh: {
      // sinh(f(x)) --> 1 / sqrt(f(x)**2 + 1) * f'(x)
      return pow(pow(args[0], 2) + 1, negative_one_half) * d_args[0];
    }
    case built_in_function::arctanh: {
      // atanh(f(x)) --> 1 / (1 - f(x)**2) * f'(x)
      return pow(1 - pow(args[0], 2), constants::negative_one) * d_args[0];
    }
    case built_in_function::ln:
      // log(f(x)) --> 1/f(x) * f'(x)
      return power::create(args[0], constants::negative_one) * d_args[0];
    case built_in_function::abs:
      // |f(x)| --> f(x)/|f(x)| * f'(x)
      // TODO: Add complex argument version.
      return args[0] / abs(args[0]) * d_args[0];
    case built_in_function::signum: {
      // signum(f(x)) --> d[heaviside(f(x)) - heaviside(-f(x))]/dx = 2 * dirac(f(x)) * f'(x)
      // However, we don't have dirac - so we leave this abstract.
      if (non_diff_behavior_ == non_differentiable_behavior::abstract) {
        return derivative::create(signum(args[0]), argument_, 1);
      } else {
        return constants::zero;
      }
    }
    case built_in_function::floor: {
      // Derivative of floor(x) is 0 for integers, and indeterminate everywhere else.
      if (non_diff_behavior_ == non_differentiable_behavior::abstract) {
        return derivative::create(floor(args[0]), argument_, 1);
      } else {
        return constants::zero;
      }
    }
    case built_in_function::arctan2: {
      const scalar_expr& y_diff = d_args[0];
      const scalar_expr& x_diff = d_args[1];
      if (is_zero(y_diff) && is_zero(x_diff)) {
        return constants::zero;
      }
      // atan2(y(u), x(u))/du = -y/(y^2 + x^2) * x'(u) + x/(y^2 + x^2) * y'(u)
      const scalar_expr sum_squared = args[0] * args[0] + args[1] * args[1];
      return -(args[0] * x_diff) / sum_squared + (args[1] * y_diff) / sum_squared;
    }
  }
  WF_ASSERT_ALWAYS("Invalid unary function: {}", func.function_name());
}

scalar_expr derivative_visitor::operator()(const complex_infinity&) const {
  return constants::zero;
}

scalar_expr derivative_visitor::operator()(const imaginary_unit&) const { return constants::zero; }

scalar_expr derivative_visitor::operator()(const integer_constant&) const {
  return constants::zero;
}

scalar_expr derivative_visitor::operator()(const iverson_bracket&, const scalar_expr& arg) const {
  if (non_diff_behavior_ == non_differentiable_behavior::abstract) {
    return derivative::create(arg, argument_, 1);
  } else {
    return constants::zero;
  }
}

scalar_expr derivative_visitor::operator()(const float_constant&) const { return constants::zero; }

scalar_expr derivative_visitor::operator()(const power& pow) {
  const scalar_expr& base = pow.base();
  const scalar_expr& exp = pow.exponent();
  const scalar_expr base_diff = apply(base);
  const scalar_expr exp_diff = apply(exp);
  if (is_zero(base_diff) && is_zero(exp_diff)) {
    return constants::zero;
  }
  return exp * power::create(base, exp - constants::one) * base_diff +
         power::create(base, exp) * log(base) * exp_diff;
}

scalar_expr derivative_visitor::operator()(const rational_constant&) const {
  return constants::zero;
}

scalar_expr derivative_visitor::operator()(const relational&, const scalar_expr& rel_expr) const {
  if (non_diff_behavior_ == non_differentiable_behavior::abstract) {
    // Cannot differentiate relationals, so insert an abstract expression.
    return derivative::create(rel_expr, argument_, 1);
  } else {
    return constants::zero;
  }
}

scalar_expr derivative_visitor::operator()(const undefined&) const { return constants::undefined; }

scalar_expr derivative_visitor::operator()(const variable& var) const {
  if (const variable* arg = get_if<const variable>(argument_);
      arg != nullptr && are_identical(*arg, var)) {
    return constants::one;
  }
  return constants::zero;
}

scalar_expr diff(const scalar_expr& function, const scalar_expr& var, const int reps,
                 const non_differentiable_behavior behavior) {
  if (reps < 0) {
    throw invalid_argument_error("Derivative order must be >= 0, received: {}", reps);
  }
  derivative_visitor visitor{var, behavior};
  scalar_expr result = function;
  for (int i = 0; i < reps; ++i) {
    result = visitor.apply(result);
  }
  return result;
}

matrix_expr jacobian(const absl::Span<const scalar_expr> functions,
                     const absl::Span<const scalar_expr> vars,
                     const non_differentiable_behavior behavior) {
  if (functions.empty()) {
    throw type_error("Need at least one function to differentiate.");
  }
  if (vars.empty()) {
    throw type_error("Need at least one variable to differentiate with respect to.");
  }

  std::vector<scalar_expr> result{};
  result.resize(functions.size() * vars.size(), constants::zero);

  // Crate row-major span over `result`:
  const auto output_dims = make_value_pack(functions.size(), vars.size());
  const auto output_strides = make_value_pack(vars.size(), constant<1>{});
  const auto result_span = make_span(result.data(), output_dims, output_strides);

  for (std::size_t col = 0; col < vars.size(); ++col) {
    // We cache derivative expressions, so that every row reuses the same cache:
    derivative_visitor diff_visitor{vars[col], behavior};
    for (std::size_t row = 0; row < functions.size(); ++row) {
      result_span(row, col) = diff_visitor.apply(functions[row]);
    }
  }

  return matrix_expr::create(static_cast<index_t>(functions.size()),
                             static_cast<index_t>(vars.size()), std::move(result));
}

}  // namespace wf
