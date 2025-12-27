// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/derivative.h"

#include <algorithm>

#include "wf/enumerations.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/variable.h"
#include "wf/matrix_expression.h"
#include "wf/number_set.h"
#include "wf/utility/error_types.h"
#include "wf/utility/scoped_trace.h"
#include "wf/utility_visitors.h"

#include "wrenfold/span.h"

namespace wf {

using namespace wf::custom_literals;

inline bool is_derivative_of_symbolic_function_invocation(const scalar_expr& expr) {
  if (const derivative* diff = get_if<const derivative>(expr); diff != nullptr) {
    return diff->differentiand().is_type<symbolic_function_invocation>();
  }
  return false;
}

derivative_visitor::derivative_visitor(const scalar_expr& argument,
                                       const non_differentiable_behavior behavior)
    : argument_(argument), non_diff_behavior_(behavior) {
  if (!argument.is_type<variable, function_argument_variable, unique_variable,
                        compound_expression_element, symbolic_function_invocation>() &&
      !is_derivative_of_symbolic_function_invocation(argument)) {
    throw type_error(
        "Argument to diff must be of type `{}`, `{}`, `{}`, `{}`, `{}`, or a `{}` expression "
        "where the  differentiand is a symbolic function invocation. Received expression "
        "of type: `{}` (value = {})",
        variable::name_str, function_argument_variable::name_str, unique_variable::name_str,
        compound_expression_element::name_str, symbolic_function_invocation::name_str,
        derivative::name_str, argument.type_name(), argument);
  }
}

scalar_expr derivative_visitor::operator()(const scalar_expr& expression) {
  if (const auto it = cache_.find(expression); it != cache_.end()) {
    return it->second;
  }
  scalar_expr result = visit(expression, *this);
  auto [it_inserted, _] = cache_.emplace(expression, std::move(result));
  return it_inserted->second;
}

// Differentiate every argument to make a new sum.
scalar_expr derivative_visitor::operator()(const addition& add) { return add.map_children(*this); }

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
  return where(cond.condition(), operator()(cond.if_branch()), operator()(cond.else_branch()));
}

scalar_expr derivative_visitor::operator()(const symbolic_constant&) const {
  return constants::zero;
}

// Derivative of an abstract derivative expression.
scalar_expr derivative_visitor::operator()(const derivative& deriv,
                                           const scalar_expr& derivative_abstract) const {
  if (const derivative* arg_deriv = get_if<const derivative>(argument_);
      arg_deriv != nullptr && are_identical(deriv, *arg_deriv)) {
    // Support taking derivatives wrt derivatives of symbolic function invocations.
    return constants::one;
  }
  if (const bool is_relevant = is_function_of(deriv.differentiand(), argument_); !is_relevant) {
    return constants::zero;
  }
  if (argument_.is_type<symbolic_function_invocation>() &&
      are_identical(deriv.differentiand(), argument_)) {
    // `derivative` is diff(f(x, y), x), and we are taking the derivative wrt f(x, y) itself.
    // The derivative of the differentiand becomes one, so: diff(1, x) --> zero.
    return constants::zero;
  }
  return derivative::create(derivative_abstract, argument_, 1);
}

// Do product expansion over all terms in the multiplication:
// a * b
// a' * b + a * b'
// a * b * c
// a' * b * c + a * b' * c + a * b * c'
scalar_expr derivative_visitor::operator()(const multiplication& mul) {
  absl::InlinedVector<scalar_expr, 8> add_terms;
  add_terms.reserve(mul.size());

  absl::InlinedVector<scalar_expr, 8> mul_terms{};
  mul_terms.reserve(mul.size());

  // Differentiate wrt every argument:
  for (std::size_t i = 0; i < mul.size(); ++i) {
    mul_terms.clear();
    if (scalar_expr mul_i_diff = operator()(mul[i]); !is_zero(mul_i_diff)) {
      mul_terms.push_back(std::move(mul_i_diff));

      // TODO: Avoid copying all the scalar_exprs twice here.
      std::copy_n(mul.begin(), i, std::back_inserter(mul_terms));
      std::copy_n(mul.begin() + i + 1, mul.size() - i - 1, std::back_inserter(mul_terms));
      add_terms.push_back(multiplication::from_operands(mul_terms));
    }
  }

  if (add_terms.empty()) {
    return constants::zero;
  }
  return addition::from_operands(add_terms);
}

// Cos, Sin, Tan, ArcCos, ArcSin, ArcTan, NaturalLog
scalar_expr derivative_visitor::operator()(const built_in_function_invocation& func,
                                           const scalar_expr& func_abstract) {
  // Differentiate the arguments:
  auto d_args = transform_map<built_in_function_invocation::container_type>(func, *this);

  if (all_of(d_args, &is_zero)) {
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
    case built_in_function::exp:
      // exp(f(x)) --> exp(f(x)) * f'(x)
      return func_abstract * d_args[0];
    case built_in_function::log:
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

scalar_expr derivative_visitor::operator()(const function_argument_variable& var) const {
  if (const auto* arg = get_if<const function_argument_variable>(argument_);
      arg != nullptr && are_identical(*arg, var)) {
    return constants::one;
  }
  return constants::zero;
}

scalar_expr derivative_visitor::operator()(const power& pow) {
  const scalar_expr& base = pow.base();
  const scalar_expr& exp = pow.exponent();
  const scalar_expr base_diff = operator()(base);
  const scalar_expr exp_diff = operator()(exp);
  if (is_zero(base_diff) && is_zero(exp_diff)) {
    return constants::zero;
  }
  return exp * power::create(base, exp - constants::one) * base_diff +
         power::create(base, exp) * log(base) * exp_diff;
}

scalar_expr derivative_visitor::operator()(const rational_constant&) const {
  return constants::zero;
}

// By construction, stop_derivative has a derivative of zero.
scalar_expr derivative_visitor::operator()(const stop_derivative&) const { return constants::zero; }

scalar_expr derivative_visitor::operator()(const substitution& sub,
                                           const scalar_expr& sub_abstract) {
  // If the target of the substitution is a function of the `x`, play it safe and create an
  // abstract derivative expression.
  if (is_function_of(sub.target(), argument_)) {
    return derivative::create(sub_abstract, argument_, 1);
  }

  const scalar_expr input_diff = operator()(sub.input());
  const scalar_expr replacement_diff = operator()(sub.replacement());

  if (is_zero(replacement_diff)) {
    // The replacement is not a function of `x`.
    // The input expression may still be:
    // d[ sub(f(x, y), y, g(z)) ] / dx --> sub(df(x, y)/dx, y, g(z))
    return substitution::create(input_diff, sub.target(), sub.replacement());
  }

  // The replacement is a function of `x`.
  const scalar_expr replacement_total_diff =
      replacement_diff * std::invoke([&] {
        if (sub.target().is_type<variable, unique_variable, function_argument_variable>()) {
          // The target is itself a variable, so create: sub(f(y).diff(y), y, g(x))
          return substitution::create(sub.input().diff(sub.target()), sub.target(),
                                      sub.replacement());
        } else if (sub.replacement()
                       .is_type<variable, unique_variable, function_argument_variable>()) {
          // We don't need to introduce another variable, just take the derivative:
          // d[sub(f(y), y, z)]/dz --> diff(sub(f(y), y, z), z) * dz/dx
          // Either dz/dx (replacement_diff) will be one or zero.
          return derivative::create(sub_abstract, sub.replacement(), 1);
        } else {
          // The target and substitution are both functions.
          // So replace the target with a temporary, and take the derivative wrt that.
          // Then replace the temporary with the replacement expression.
          const scalar_expr sub_var =
              make_unique_variable_symbol(determine_numeric_set(sub.replacement()));
          return substitution::create(
              derivative::create(substitution::create(sub.input(), sub.target(), sub_var), sub_var,
                                 1),
              sub_var, sub.replacement());
        }
      });

  // Both replacement and input expression may be a function of `x`:
  // d[ subs(f(x, y), y, g(x)) ] / dx -->
  //    d[ subs(f(x, y), y, u) ]/du * g'(x) + subs(df(x, y)/dx, y, g(x))
  // If `input_diff` is zero, this will simplify to just `replacement_total_diff`.
  return replacement_total_diff + substitution::create(input_diff, sub.target(), sub.replacement());
}

// df(a, b)/dx --> f(a, b)/da * da/x + f(a, b)/db * db/x
scalar_expr derivative_visitor::operator()(const symbolic_function_invocation& func,
                                           const scalar_expr& func_abstract) {
  if (const symbolic_function_invocation* arg =
          get_if<const symbolic_function_invocation>(argument_);
      arg != nullptr && are_identical(*arg, func)) {
    // We are taking the derivative wrt a symbolic function invocation (and it exactly
    // matches `func`), so just return one: d(f(t))/d(f(t)) --> 1
    return constants::one;
  }

  // Compute derivatives of every argument.
  const auto args_diff = transform_map<std::vector>(func.children(), *this);

  // Check how many args were irrelevant:
  const std::size_t num_zero = std::count_if(args_diff.begin(), args_diff.end(), &is_zero);
  if (num_zero == args_diff.size()) {
    // No arguments are relevant.
    return constants::zero;
  }

  if (num_zero + 1 == args_diff.size() &&
      1 == std::count_if(args_diff.begin(), args_diff.end(), &is_one)) {
    // All but one argument is irrelevant, and that argument matches the variable exactly.
    // In this case, we can just insert a derivative expression:
    // d[f(x, y)]/dx --> diff(f(x, y), x)
    return derivative::create(func_abstract, argument_, 1);
  }

  // More than one argument is relevant, we need to introduce substitutions:
  //  d[f(g(x), h(x))]/dx --> f(u1, h(x))/du1 * g'(x) + f(g(x), u2)/du2 * h'(x)
  const auto sum_args = transform_enumerate_map<std::vector>(
      func.children(), [&](const std::size_t arg_index, const scalar_expr& func_arg) {
        if (is_zero(args_diff[arg_index])) {
          // This argument is not a function of the variable we are differentiating with respect
          // to, so it does not contribute to the total derivative.
          return constants::zero;
        }

        // We have a function f(g(x)). We replace u = g(x), then differentiate df(u)/du and
        // substitute in u = g(x).
        const scalar_expr sub_var = make_unique_variable_symbol(determine_numeric_set(argument_));

        // Create new invocation of f(...) that replaces argument `arg_index` with `u`.
        symbolic_function_invocation::container_type replaced_args = func.children();
        replaced_args[arg_index] = sub_var;

        // Construct df/du * du/dx
        return args_diff[arg_index] *
               substitution::create(
                   derivative::create(make_expr<symbolic_function_invocation>(
                                          func.function(), std::move(replaced_args)),
                                      sub_var, 1),
                   sub_var, func_arg);
      });
  return addition::from_operands(sum_args);
}

scalar_expr derivative_visitor::operator()(const undefined&) const { return constants::undefined; }

scalar_expr derivative_visitor::operator()(const unevaluated& u) { return u.map_children(*this); }

scalar_expr derivative_visitor::operator()(const unique_variable& var) const {
  if (const unique_variable* arg = get_if<const unique_variable>(argument_);
      arg != nullptr && are_identical(*arg, var)) {
    return constants::one;
  }
  return constants::zero;
}

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
    result = visitor(result);
  }
  return result;
}

matrix_expr jacobian(const absl::Span<const scalar_expr> functions,
                     const absl::Span<const scalar_expr> vars,
                     const non_differentiable_behavior behavior) {
  WF_FUNCTION_TRACE();
  if (functions.empty()) {
    throw dimension_error("Need at least one function to differentiate.");
  }
  if (vars.empty()) {
    throw dimension_error("Need at least one variable to differentiate with respect to.");
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
      result_span(row, col) = diff_visitor(functions[row]);
    }
  }

  return matrix_expr::create(static_cast<index_t>(functions.size()),
                             static_cast<index_t>(vars.size()), std::move(result));
}

}  // namespace wf
