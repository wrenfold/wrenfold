// Copyright 2024 Gareth Cross
#include "wf/real_imaginary.h"

#include "wf/expression_visitor.h"

namespace wf {

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(const scalar_expr& expr) {
  if (const auto it = cache_.find(expr); it != cache_.end()) {
    return it->second;
  }
  const auto [it, _] = cache_.emplace(expr, visit(expr, *this));
  return it->second;
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(const addition&) {
  WF_ASSERT_ALWAYS("TODO");
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(
    const complex_infinity&) const {
  return std::make_tuple(constants::undefined, constants::undefined);
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(
    const compound_expression_element&, const scalar_expr& arg) const {
  // It is assumed that compound expressions can only be real. This may be relaxed in future
  // if we ever support generating code that uses std::complex, for example.
  return std::make_tuple(arg, constants::zero);
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(const conditional&) {
  WF_ASSERT_ALWAYS("TODO");
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(const derivative&,
                                                                        const scalar_expr&) const {
  WF_ASSERT_ALWAYS("TODO");
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(
    const float_constant&, const scalar_expr& arg) const {
  return std::make_tuple(arg, constants::zero);
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(
    const function& func, const scalar_expr& func_abstract) {
  switch (func.enum_value()) {
    case built_in_function::cos:
      break;
    case built_in_function::sin:
      break;
    case built_in_function::tan:
      break;
    case built_in_function::arccos:
      break;
    case built_in_function::arcsin:
      break;
    case built_in_function::arctan:
      break;
    case built_in_function::ln:
      break;
    case built_in_function::abs:
      break;
    case built_in_function::signum:
      break;
    case built_in_function::floor:
      break;
    case built_in_function::re:
      return std::make_tuple(func_abstract, constants::zero);
    case built_in_function::im:
      return std::make_tuple(constants::zero, func_abstract);
    case built_in_function::arctan2:
      break;
  }
  WF_ASSERT_ALWAYS("Unhandled enum value: {}", string_from_built_in_function(func.enum_value()));
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(
    const imaginary_unit&) const {
  return std::make_tuple(constants::zero, constants::one);
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(
    const integer_constant&, const scalar_expr& arg) const {
  return std::make_tuple(arg, constants::zero);
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(
    const iverson_bracket&, const scalar_expr& arg) const {
  // Value of iverson bracket is 0 or 1, which is real.
  return std::make_tuple(arg, constants::zero);
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(const multiplication&) {
  WF_ASSERT_ALWAYS("TODO");
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(const power&) {
  WF_ASSERT_ALWAYS("TODO");
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(
    const rational_constant&, const scalar_expr& arg) const {
  return std::make_tuple(arg, constants::zero);
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(
    const symbolic_constant&, const scalar_expr& arg) const {
  return std::make_tuple(arg, constants::zero);
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(const undefined&) const {
  return std::make_tuple(constants::undefined, constants::undefined);
}

std::tuple<scalar_expr, scalar_expr> real_imaginary_visitor::operator()(
    const variable& var, const scalar_expr& arg) const {
  switch (var.set()) {
    case number_set::real_positive:
    case number_set::real_non_negative:
    case number_set::real:
      return std::make_tuple(arg, constants::zero);
    case number_set::complex:
    case number_set::unknown:
      // This variable could be complex, so make re(...) and im(...) function calls:
      return std::make_tuple(make_expr<function>(built_in_function::re, arg),
                             make_expr<function>(built_in_function::im, arg));
  }
  WF_ASSERT_ALWAYS("Unhandled enum value: {}", string_from_number_set(var.set()));
}

std::tuple<scalar_expr, scalar_expr> real_imag(const scalar_expr& arg) {
  return visit(arg, real_imaginary_visitor{});
}

}  // namespace wf
