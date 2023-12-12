// Copyright 2023 Gareth Cross
#include <optional>
#include <vector>

#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/operators.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/constants.h"
#include "wf/expression.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/variable.h"
#include "wf/functions.h"

#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

// Create symbols from CSV list of names.
inline std::variant<Expr, py::list> create_symbols_from_str(const std::string_view csv) {
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
    return variables[0].cast<Expr>();
  }
  return variables;
}

// Traverse the provided iterable, inspecting elements. If the inner element is a
// string, then parse it into symbols. If the inner element is an iterable, recurse on it.
inline std::variant<Expr, py::list> create_symbols_from_str(const py::iterable& iterable) {
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

std::variant<Expr, py::list> create_symbols_from_str_or_iterable(
    std::variant<std::string_view, py::iterable> arg) {
  return std::visit([](const auto& input) { return create_symbols_from_str(input); },
                    std::move(arg));
}

bool convert_expr_to_bool(const Expr& self) {
  if (const symbolic_constant* c = cast_ptr<symbolic_constant>(self); c != nullptr) {
    if (c->name() == symbolic_constant_enum::boolean_true) {
      return true;
    } else if (c->name() == symbolic_constant_enum::boolean_false) {
      return false;
    }
  }
  throw type_error(
      "Expression of type `{}` cannot be coerced to boolean. Only expressions sym.true and "
      "sym.false can be evaluated for truthiness.",
      self.type_name());
}

Expr substitute_variables_wrapper(const Expr& self,
                                  const std::vector<std::tuple<Expr, Expr>>& pairs) {
  return self.substitute_variables(pairs);
}

// Defined in matrix_wrapper.cc
void wrap_matrix_operations(py::module_& m);

// Defined in codegen_wrapper.cc
void wrap_codegen_operations(py::module_& m);

// Defined in geometry_wrapper.cc
void wrap_geometry_operations(py::module_& m);
}  // namespace wf

PYBIND11_MODULE(PY_MODULE_NAME, m) {
  using namespace wf;

  // Primary expression type:
  py::class_<Expr>(m, "Expr")
      // Implicit construction from numerics:
      .def(py::init<std::int64_t>())
      .def(py::init<double>())
      // String conversion:
      .def("__repr__", &Expr::to_string)
      .def("expression_tree_str", &Expr::to_expression_tree_string,
           "Retrieve the expression tree as a pretty-printed string.")
      .def(
          "is_identical_to",
          [](const Expr& self, const Expr& other) { return self.is_identical_to(other); },
          "other"_a, "Test if two expressions have identical expression trees.")
      .def_property_readonly("type_name", &Expr::type_name)
      // Operations:
      .def(
          "diff",
          [](const Expr& self, const Expr& var, int order, bool use_abstract) {
            return self.diff(var, order,
                             use_abstract ? non_differentiable_behavior::abstract
                                          : non_differentiable_behavior::constant);
          },
          "var"_a, py::arg("order") = 1, py::arg("use_abstract") = false,
          "Differentiate the expression with respect to the specified variable.")
      .def("distribute", &Expr::distribute, "Expand products of additions and subtractions.")
      .def("subs", &Expr::subs, py::arg("target"), py::arg("substitute"),
           "Replace the `target` expression with `substitute` in the expression tree.")
      .def("subs_variables", &substitute_variables_wrapper, py::arg("pairs"),
           "Substitute a list of variable expressions.")
      .def("eval", &try_convert_to_numeric, "Evaluate into float expression.")
      .def(
          "collect", [](const Expr& self, const Expr& term) { return self.collect(term); },
          "term"_a, "Collect powers of the provided expression.")
      .def(
          "collect",
          [](const Expr& self, const std::vector<Expr>& terms) {
            return wf::collect_many(self, terms);
          },
          "terms"_a, "Collect powers of the provided expressions.")
      // Operators:
      .def(py::self + py::self)
      .def(py::self - py::self)
      .def(py::self * py::self)
      .def(py::self / py::self)
      .def(-py::self)
      .def("__pow__", &wf::pow)
      .def(py::self > py::self)
      .def(py::self >= py::self)
      .def(py::self < py::self)
      .def(py::self <= py::self)
      .def(py::self == py::self)
      // Operators involving integers (int on left side):
      .def(std::int64_t() + py::self)
      .def(std::int64_t() - py::self)
      .def(std::int64_t() * py::self)
      .def(std::int64_t() / py::self)
      .def(std::int64_t() > py::self)
      .def(std::int64_t() >= py::self)
      .def(std::int64_t() < py::self)
      .def(std::int64_t() <= py::self)
      .def(std::int64_t() == py::self)
      // Operators involving doubles (double on left side):
      .def(double() + py::self)
      .def(double() - py::self)
      .def(double() * py::self)
      .def(double() / py::self)
      .def(double() > py::self)
      .def(double() >= py::self)
      .def(double() < py::self)
      .def(double() <= py::self)
      .def(double() == py::self)
      // Override conversion to boolean, so we don't coerce non-boolean expressions.
      .def("__bool__", &convert_expr_to_bool, py::doc("Coerce expression to bool."));

  py::implicitly_convertible<std::int64_t, Expr>();
  py::implicitly_convertible<double, Expr>();

  // Methods for declaring expressions:
  m.def("symbols", &create_symbols_from_str_or_iterable, py::arg("arg"),
        "Create variables from a string or an iterable of strings.");
  m.def(
      "integer", [](std::int64_t value) { return Expr{value}; }, "value"_a,
      "Create an integer expression.");
  m.def(
      "float", [](double value) { return Expr{value}; }, "value"_a, "Create a float expression.");

  // Built-in functions:
  m.def("log", &wf::log, "arg"_a, "Natural log.");
  m.def("pow", &wf::pow, "base"_a, "exp"_a, "Evaluates to: base ** exp.");
  m.def("cos", &wf::cos, "arg"_a, "Cosine function.");
  m.def("sin", &wf::sin, "arg"_a, "Sine function.");
  m.def("tan", &wf::tan, "arg"_a, "Tan function.");
  m.def("acos", &wf::acos, "arg"_a, "Arc-cosine function.");
  m.def("asin", &wf::asin, "arg"_a, "Arc-sine function.");
  m.def("atan", &wf::atan, "arg"_a, "Arc-tangent function.");
  m.def("sqrt", &wf::sqrt, "arg"_a, "Square-root function.");
  m.def("abs", &wf::abs, "arg"_a, "Absolute value function.");
  m.def("signum", &wf::signum, "arg"_a, "Signum/sign function.");
  m.def("atan2", &wf::atan2, "y"_a, "x"_a, "2-argument arc-tangent function.");

  m.def("max", &wf::max, "a"_a, "b"_a, "Maximum of two scalar values.");
  m.def("min", &wf::min, "a"_a, "b"_a, "Minimum of two scalar values.");
  m.def("where", static_cast<Expr (*)(const Expr&, const Expr&, const Expr&)>(&wf::where),
        "condition"_a, "if_true"_a, "if_false"_a, "If-else statement.");

  // Special constants:
  m.attr("euler") = constants::euler;
  m.attr("zoo") = constants::complex_infinity;
  m.attr("one") = constants::one;
  m.attr("pi") = constants::pi;
  m.attr("zero") = constants::zero;
  m.attr("true") = constants::boolean_true;
  m.attr("false") = constants::boolean_false;

  // TODO: This is unused - can probably be removed unless we want to expose the `function` type
  // in python.
  // Function enums.
  py::enum_<built_in_function>(m, "BuiltInFunction")
      .value("Cos", built_in_function::cos)
      .value("Sin", built_in_function::sin)
      .value("Tan", built_in_function::tan)
      .value("ArcCos", built_in_function::arccos)
      .value("ArcSin", built_in_function::arcsin)
      .value("ArcTan", built_in_function::arctan)
      .value("Log", built_in_function::ln)
      .value("Abs", built_in_function::abs)
      .value("Signum", built_in_function::signum)
      .value("Arctan2", built_in_function::arctan2)
      .def(
          "to_string", [](built_in_function name) { return string_from_built_in_function(name); },
          py::doc("Convert to string."));

  // Exceptions:
  py::register_exception<assertion_error>(m, "AssertionError");
  py::register_exception<dimension_error>(m, "DimensionError");
  py::register_exception<type_error>(m, "TypeError");

  // Include other wrappers in this module:
  wrap_matrix_operations(m);

  auto m_geo = m.def_submodule("geometry", "Wrapped geometry methods.");
  wrap_geometry_operations(m_geo);

  auto m_codegen = m.def_submodule("codegen", "Wrapped code-generation types.");
  wrap_codegen_operations(m_codegen);
}  // PYBIND11_MODULE
