// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <nanobind/nanobind.h>
#include <nanobind/operators.h>

#include "wf/expression.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/variable.h"
#include "wf/matrix_expression.h"
#include "wf/utility/assertions.h"

namespace wf {

namespace py = nanobind;
using namespace py::literals;

// Visitor that converts wrenfold expressions to sympy expressions.
class sympy_conversion_visitor {
 private:
  py::object get_sympy_attr(const std::string_view name) const {
    py::object attr = py::getattr(sp_, name.data(), py::none());
    WF_ASSERT(!attr.is_none(), "Failed to get attribute from sympy module: {}", name);
    return attr;
  }

  template <typename... Args>
  py::object invoke_sympy_object(const std::string_view name, Args&&... args) const {
    const py::object func = get_sympy_attr(name);
    return func(std::forward<Args>(args)...);
  }

 public:
  explicit sympy_conversion_visitor(py::module_ sp, const bool evaluate)
      : sp_(std::move(sp)), evaluate_(evaluate) {}

  // TODO: Cache expressions during conversion, at least for scalars.
  py::object operator()(const scalar_expr& expr) { return visit(expr, *this); }
  py::object operator()(const boolean_expr& expr) { return visit(expr, *this); }
  py::object operator()(const matrix_expr& expr) { return visit(expr, *this); }

  // Convert all elements of a container into a python tuple.
  template <typename Container>
  py::tuple convert_to_args(const Container& container) {
    py::list args;
    for (const auto& element : container) {
      args.append(operator()(element));
    }
    return py::tuple();
  }

  py::object operator()(const addition& add) {
    return invoke_sympy_object("Add", *convert_to_args(add), "evaluate"_a = evaluate_);
  }

  py::object operator()(const boolean_constant& constant) const {
    return get_sympy_attr(constant.value() ? "true" : "false");
  }

  py::object operator()(const built_in_function_invocation& func) {
    // Absolute value need special treatment since there is no `abs` in sympy.
    const std::string_view func_name = func.enum_value() == built_in_function::abs
                                           ? "Abs"
                                           : string_from_built_in_function(func.enum_value());
    return invoke_sympy_object(func_name, *convert_to_args(func), "evaluate"_a = evaluate_);
  }

  py::object operator()(const complex_infinity&) const { return get_sympy_attr("zoo"); }

  py::object operator()(const conditional& cond) {
    const auto& x = cond.if_branch();
    const auto& y = cond.else_branch();
    if (cond.condition().is_identical_to(y < x)) {
      // max(...)
      return invoke_sympy_object("Max", operator()(x), operator()(y), "evaluate"_a = evaluate_);
    } else if (cond.condition().is_identical_to(x < y)) {
      // min(...)
      return invoke_sympy_object("Min", operator()(x), operator()(y), "evaluate"_a = evaluate_);
    }
    return invoke_sympy_object(
        "Piecewise", py::make_tuple(operator()(cond.if_branch()), operator()(cond.condition())),
        py::make_tuple(operator()(cond.else_branch()), true), "evaluate"_a = evaluate_);
  }

  py::object operator()(const derivative& diff) {
    return invoke_sympy_object(
        "Derivative", operator()(diff.differentiand()), operator()(diff.argument()), diff.order());
  }

  py::object operator()(const float_constant& flt) const {
    return invoke_sympy_object("Float", flt.value());
  }

  py::object operator()(const imaginary_unit&) const { return get_sympy_attr("I"); }

  py::object operator()(const integer_constant& constant) const {
    return invoke_sympy_object("Integer", constant.value().value());
  }

  py::object operator()(const iverson_bracket& iverson) {
    // Sympy has no iverson, but we can fake it:
    // https://github.com/sympy/sympy/issues/20151
    return invoke_sympy_object("Piecewise", py::make_tuple(1, operator()(iverson.arg())),
                               py::make_tuple(0, true), "evaluate"_a = evaluate_);
  }

  py::object operator()(const matrix& mat) {
    // Create a list of lists in row-major order.
    py::list rows{};
    for (int i = 0; i < mat.rows(); ++i) {
      py::list cols{};
      for (int j = 0; j < mat.cols(); ++j) {
        cols.append(operator()(mat.get_unchecked(i, j)));
      }
      rows.append(cols);
    }
    return invoke_sympy_object("Matrix", rows);
  }

  py::object operator()(const multiplication& mul) {
    return invoke_sympy_object("Mul", *convert_to_args(mul), "evaluate"_a = evaluate_);
  }

  py::object operator()(const power& pow) {
    return invoke_sympy_object("Pow", operator()(pow.base()), operator()(pow.exponent()),
                               "evaluate"_a = evaluate_);
  }

  py::object operator()(const rational_constant& rational) const {
    return invoke_sympy_object("Rational", rational.numerator().value(),
                               rational.denominator().value());
  }

  static std::string_view sympy_relational_from_operation(const relational_operation op) {
    switch (op) {
      case relational_operation::less_than:
        return "Lt";
      case relational_operation::less_than_or_equal:
        return "Le";
      case relational_operation::equal:
        return "Eq";
    }
    WF_ASSERT_ALWAYS("Unhandled relational operation: {}", string_from_relational_operation(op));
  }

  py::object operator()(const relational& relational) {
    return invoke_sympy_object(sympy_relational_from_operation(relational.operation()),
                               operator()(relational.left()), operator()(relational.right()),
                               "evaluate"_a = evaluate_);
  }

  py::object operator()(const substitution& substitution) {
    return invoke_sympy_object(
        "Subs", operator()(substitution.input()), operator()(substitution.target()),
                                                  operator()(substitution.replacement()),
        "evaluate"_a = evaluate_);
  }

  py::object operator()(const symbolic_constant& constant) const {
    switch (constant.name()) {
      case symbolic_constant_enum::euler:
        return get_sympy_attr("E");
      case symbolic_constant_enum::pi:
        return get_sympy_attr("pi");
    }
    WF_ASSERT_ALWAYS("Unhandled symbolic constant: {}",
                     string_from_symbolic_constant(constant.name()));
  }

  py::object operator()(const symbolic_function_invocation& invocation) {
    // TODO: Add ability to place assumptions on `symbolic_function_invocation`, and allow passing
    // them here.
    const py::object function = invoke_sympy_object("Function", invocation.function().name());
    return function(*convert_to_args(invocation));
  }

  py::object operator()(const undefined&) const { return get_sympy_attr("nan"); }

  py::object operator()(const unevaluated& p) {
    return invoke_sympy_object("UnevaluatedExpr", *convert_to_args(p));
  }

  static py::dict get_variable_kwargs(const number_set set) {
    py::dict kwargs{};
    switch (set) {
      case number_set::real_positive: {
        kwargs["positive"] = true;
      } break;
      case number_set::real_non_negative: {
        kwargs["nonnegative"] = true;
      } break;
      case number_set::real: {
        kwargs["real"] = true;
      } break;
      case number_set::complex: {
        kwargs["complex"] = true;
      } break;
      case number_set::unknown:
        break;
    }
    return kwargs;
  }

  py::object operator()(const variable& var) const {
    return invoke_sympy_object("symbols", var.name(), **get_variable_kwargs(var.set()));
  }

  template <typename T>
  py::object operator()(const T&) const {
    throw type_error("Type `{}` cannot be converted to sympy.", T::name_str);
  }

 private:
  // The sympy module.
  py::module_ sp_;

  // Whether to use evaluate=True when constructing sympy expressions.
  bool evaluate_;
};

template <typename T>
py::object to_sympy(const T& expr, const py::module_& sp, const bool evaluate) {
  return sympy_conversion_visitor{sp, evaluate}(expr);
}

void wrap_sympy_conversion(py::module_& m) {
  m.def("_to_sympy_impl", &to_sympy<scalar_expr>, py::arg("expr"), py::arg("sp"),
        py::arg("evaluate"), "OMIT_FROM_SPHINX");
  m.def("_to_sympy_impl", &to_sympy<boolean_expr>, py::arg("expr"), py::arg("sp"),
        py::arg("evaluate"), "OMIT_FROM_SPHINX");
  m.def("_to_sympy_impl", &to_sympy<matrix_expr>, py::arg("expr"), py::arg("sp"),
        py::arg("evaluate"), "OMIT_FROM_SPHINX");
}

}  // namespace wf
