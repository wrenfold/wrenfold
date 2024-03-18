// Copyright 2023 Gareth Cross
#include <optional>
#include <vector>

#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/complex.h>
#include <pybind11/operators.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/constants.h"
#include "wf/expression.h"
#include "wf/expressions/special_constants.h"
#include "wf/expressions/variable.h"
#include "wf/functions.h"
#include "wf/numerical_casts.h"

#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

// Create symbols from CSV list of names.
inline std::variant<scalar_expr, py::list> create_symbols_from_str(const std::string_view csv) {
  py::list variables{};
  std::string name{};
  for (const char c : csv) {
    if (std::isspace(c) || c == ',') {
      if (!name.empty()) {
        auto var = make_expr<variable>(std::move(name));
        variables.append(std::move(var));
        name = std::string();
      }
      continue;
    }
    name += c;
  }
  if (!name.empty()) {
    auto var = make_expr<variable>(std::move(name));
    variables.append(std::move(var));
  }
  if (variables.size() == 1) {
    return variables[0].cast<scalar_expr>();
  }
  return variables;
}

// Traverse the provided iterable, inspecting elements. If the inner element is a
// string, then parse it into symbols. If the inner element is an iterable, recurse on it.
inline std::variant<scalar_expr, py::list> create_symbols_from_str(const py::iterable& iterable) {
  py::list result{};
  for (const py::handle& handle : iterable) {
    // Each element of the iterable could be a string, or a nested iterable:
    auto casted = py::cast<std::variant<std::string_view, py::iterable>>(handle);
    auto symbols = std::visit([](const auto& input) { return create_symbols_from_str(input); },
                              std::move(casted));
    result.append(std::move(symbols));
  }
  return result;
}

std::variant<scalar_expr, py::list> create_symbols_from_str_or_iterable(
    std::variant<std::string_view, py::iterable> arg) {
  return std::visit([](const auto& input) { return create_symbols_from_str(input); },
                    std::move(arg));
}

scalar_expr substitute_variables_wrapper(
    const scalar_expr& self, const std::vector<std::tuple<scalar_expr, scalar_expr>>& pairs) {
  return self.substitute_variables(pairs);
}

auto eval_wrapper(const scalar_expr& self) { return maybe_numerical_cast(self.eval()); }

// Defined in matrix_wrapper.cc
void wrap_matrix_operations(py::module_& m);

// Defined in codegen_wrapper.cc
void wrap_codegen_operations(py::module_& m);

// Defined in code_formatting_wrapper.cc
void wrap_code_formatting_operations(py::module_& m);

// Defined in compound_expression_wrapper.cc
void wrap_compound_expression(py::module_& m);

// Defined in boolean_expression_wrapper.cc
void wrap_boolean_expression(py::module_& m);

// Defined in geometry_wrapper.cc
void wrap_geometry_operations(py::module_& m);
}  // namespace wf

// ReSharper disable CppIdenticalOperandsInBinaryExpression
PYBIND11_MODULE(PY_MODULE_NAME, m) {
  using namespace wf;

  // Primary expression type:
  wrap_class<scalar_expr>(m, "Expr")
      // Implicit construction from numerics:
      .def(py::init<std::int64_t>())
      .def(py::init<double>())
      // String conversion:
      .def("__repr__", &scalar_expr::to_string)
      .def("expression_tree_str", &scalar_expr::to_expression_tree_string,
           "Retrieve the expression tree as a pretty-printed string.")
      .def_property_readonly("type_name", [](const scalar_expr& self) { return self.type_name(); })
      // Operations:
      .def(
          "diff",
          [](const scalar_expr& self, const scalar_expr& var, int order, bool use_abstract) {
            return self.diff(var, order,
                             use_abstract ? non_differentiable_behavior::abstract
                                          : non_differentiable_behavior::constant);
          },
          "var"_a, py::arg("order") = 1, py::arg("use_abstract") = false,
          "Differentiate the expression with respect to the specified variable.")
      .def("distribute", &scalar_expr::distribute, "Expand products of additions and subtractions.")
      .def("subs", &scalar_expr::subs, py::arg("target"), py::arg("substitute"),
           "Replace the `target` expression with `substitute` in the expression tree.")
      .def("subs_variables", &substitute_variables_wrapper, py::arg("pairs"),
           "Substitute a list of variable expressions.")
      .def("eval", &eval_wrapper, "Evaluate into floating point expression.")
      .def(
          "collect",
          [](const scalar_expr& self, const scalar_expr& term) { return self.collect(term); },
          "term"_a, "Collect powers of the provided expression.")
      .def(
          "collect",
          [](const scalar_expr& self, const std::vector<scalar_expr>& terms) {
            return wf::collect_many(self, terms);
          },
          "terms"_a, "Collect powers of the provided expressions.")
      // Operators:
      .def(py::self + py::self)
      .def(py::self - py::self)
      .def(py::self * py::self)
      .def(py::self / py::self)
      .def(-py::self)
      .def("__pow__", &wf::pow, py::is_operator())
      .def(
          "__rpow__",
          [](const scalar_expr& self, const scalar_expr& other) { return pow(other, self); },
          py::is_operator())
      .def(py::self > py::self)
      .def(py::self >= py::self)
      .def(py::self < py::self)
      .def(py::self <= py::self)
      // Operators involving integers (int on left side):
      .def(std::int64_t() + py::self)
      .def(std::int64_t() - py::self)
      .def(std::int64_t() * py::self)
      .def(std::int64_t() / py::self)
      .def(std::int64_t() > py::self)
      .def(std::int64_t() >= py::self)
      .def(std::int64_t() < py::self)
      .def(std::int64_t() <= py::self)
      // Operators involving doubles (double on left side):
      .def(double() + py::self)
      .def(double() - py::self)
      .def(double() * py::self)
      .def(double() / py::self)
      .def(double() > py::self)
      .def(double() >= py::self)
      .def(double() < py::self)
      .def(double() <= py::self)
      // Override conversion to boolean, so we don't coerce non-boolean expressions.
      .def(
          "__bool__",
          [](const scalar_expr& self) {
            throw type_error(
                "Expression of type `{}` cannot be coerced to boolean. Only expressions sym.true "
                "and sym.false can be evaluated for truthiness.",
                self.type_name());
          },
          py::doc("Coerce expression to bool."));

  py::implicitly_convertible<std::int64_t, scalar_expr>();
  py::implicitly_convertible<double, scalar_expr>();

  // Methods for declaring expressions:
  m.def("symbols", &create_symbols_from_str_or_iterable, py::arg("arg"),
        "Create variables from a string or an iterable of strings.");
  m.def(
      "integer", [](const std::int64_t value) { return scalar_expr{value}; }, "value"_a,
      "Create an integer expression.");
  m.def(
      "float", [](const double value) { return scalar_expr{value}; }, "value"_a,
      "Create a float expression.");

  // Built-in functions:
  m.def("log", &wf::log, "arg"_a, "Natural log.");
  m.def("pow", &wf::pow, "base"_a, "exp"_a, "Evaluates to: base ** exp.");
  m.def("cos", &wf::cos, "arg"_a, "Cosine function.");
  m.def("sin", &wf::sin, "arg"_a, "Sine function.");
  m.def("tan", &wf::tan, "arg"_a, "Tan function.");
  m.def("acos", &wf::acos, "arg"_a, "Arc-cosine function.");
  m.def("asin", &wf::asin, "arg"_a, "Arc-sine function.");
  m.def("atan", &wf::atan, "arg"_a, "Arc-tangent function.");
  m.def("cosh", &wf::cosh, "arg"_a, "Hyperbolic cosine function.");
  m.def("sinh", &wf::sinh, "arg"_a, "Hyperbolic sine function.");
  m.def("tanh", &wf::tanh, "arg"_a, "Hyperbolic tan function.");
  m.def("acosh", &wf::acosh, "arg"_a, "Hyperbolic arc-cosine function.");
  m.def("asinh", &wf::asinh, "arg"_a, "Hyperbolic arc-sine function.");
  m.def("atanh", &wf::atanh, "arg"_a, "Hyperbolic arc-tangent function.");
  m.def("sqrt", &wf::sqrt, "arg"_a, "Square-root function.");
  m.def("abs", static_cast<scalar_expr (*)(const scalar_expr&)>(&wf::abs), "arg"_a,
        "Absolute value function.");
  m.def("signum", &wf::signum, "arg"_a, "Signum/sign function.");
  m.def("floor", &wf::floor, "arg"_a, "Floor function.");
  m.def("atan2", &wf::atan2, "y"_a, "x"_a, "2-argument arc-tangent function.");

  m.def("max", &wf::max, "a"_a, "b"_a, "Maximum of two scalar values.");
  m.def("min", &wf::min, "a"_a, "b"_a, "Minimum of two scalar values.");
  m.def("where",
        static_cast<scalar_expr (*)(const boolean_expr&, const scalar_expr&, const scalar_expr&)>(
            &wf::where),
        "condition"_a, "if_true"_a, "if_false"_a, "If-else statement.");

  m.def("equals",
        static_cast<boolean_expr (*)(const scalar_expr&, const scalar_expr&)>(&operator==), "a"_a,
        "b"_a, py::doc("Boolean expression that is true when both operands are equal."));

  m.def("iverson", &wf::iverson, "arg"_a,
        "Convert a boolean expression to an integer via the iverson bracket.");

  // Special constants:
  m.attr("euler") = constants::euler;
  m.attr("zoo") = constants::complex_infinity;
  m.attr("one") = constants::one;
  m.attr("pi") = constants::pi;
  m.attr("zero") = constants::zero;
  m.attr("imaginary_unit") = constants::imaginary_unit;
  m.attr("I") = constants::imaginary_unit;

  // Exceptions:
  py::register_exception<arithmetic_error>(m, "ArithmeticError");
  py::register_exception<assertion_error>(m, "AssertionError");
  py::register_exception<dimension_error>(m, "DimensionError");
  py::register_exception<domain_error>(m, "DomainError");
  py::register_exception<invalid_argument_error>(m, "InvalidArgumentError");
  py::register_exception<type_error>(m, "TypeError");

  // Include other wrappers in this module:
  wrap_matrix_operations(m);
  wrap_compound_expression(m);
  wrap_boolean_expression(m);

  auto m_geo = m.def_submodule("geometry", "Wrapped geometry methods.");
  wrap_geometry_operations(m_geo);

  auto m_codegen = m.def_submodule("codegen", "Wrapped code-generation types.");
  wrap_codegen_operations(m_codegen);
  wrap_code_formatting_operations(m_codegen);
}  // PYBIND11_MODULE
