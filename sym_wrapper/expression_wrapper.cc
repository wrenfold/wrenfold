// Copyright 2023 Gareth Cross
#include <optional>
#include <vector>

#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/operators.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "constants.h"
#include "expression.h"
#include "expressions/numeric_expressions.h"
#include "expressions/variable.h"
#include "functions.h"
#include "plain_formatter.h"

namespace py = pybind11;
using namespace py::literals;
using namespace math;

// Create symbols from CSV list of names.
inline std::variant<Expr, py::list> CreateSymbolsFrom(const std::string_view csv) {
  py::list variables{};
  std::string name{};
  for (const char c : csv) {
    if (std::isspace(c) || c == ',') {
      if (!name.empty()) {
        auto var = MakeExpr<Variable>(std::move(name));
        variables.append(std::move(var));
        name = std::string();
      }
      continue;
    }
    name += c;
  }
  if (!name.empty()) {
    auto var = MakeExpr<Variable>(std::move(name));
    variables.append(std::move(var));
  }
  if (variables.size() == 1) {
    return variables[0].cast<Expr>();
  }
  return variables;
}

inline std::variant<Expr, py::list> CreateSymbolsFrom(const py::iterable& iterable) {
  py::list result{};
  for (const py::handle& handle : iterable) {
    // Each element of the iterable could be a string, or a nested iterable:
    auto casted = py::cast<std::variant<std::string_view, py::iterable>>(handle);
    auto symbols =
        std::visit([](const auto& input) { return CreateSymbolsFrom(input); }, std::move(casted));
    result.append(std::move(symbols));
  }
  return result;
}

std::variant<Expr, py::list> CreateSymbols(std::variant<std::string_view, py::iterable> arg) {
  return std::visit([](const auto& input) { return CreateSymbolsFrom(input); }, std::move(arg));
}

std::variant<double, Expr> EvalToNumeric(const Expr& self) {
  Expr eval = self.Eval();
  if (const Float* f = CastPtr<Float>(eval); f != nullptr) {
    return f->GetValue();
  }
  return eval;
}

namespace math {
// Defined in matrix_wrapper_methods.cc
void WrapMatrixOperations(py::module_& m);
}  // namespace math

PYBIND11_MODULE(pysym, m) {
  // Primary expression type:
  py::class_<Expr>(m, "Expr")
      // Implicit construction from numerics:
      .def(py::init<std::int64_t>())
      .def(py::init<double>())
      // String conversion:
      .def("__repr__",
           [](const Expr& self) {
             PlainFormatter formatter{PowerStyle::Python};
             Visit(self, formatter);
             return formatter.GetOutput();
           })
      .def("expression_tree_str", &FormatDebugTree,
           "Retrieve the expression tree as a pretty-printed string.")
      .def(
          "print_expression_tree",
          [](const Expr& self) { fmt::print("{}\n", FormatDebugTree(self)); },
          "Print the expression tree to standard out.")
      .def(
          "is_identical_to",
          [](const Expr& self, const Expr& other) { return self.IsIdenticalTo(other); }, "other"_a,
          "Test if two expressions have identical expression trees.")
      .def_property_readonly("type_name", &Expr::TypeName)
      // Operations:
      .def("diff", &Expr::Diff, "var"_a, py::arg("order") = 1,
           "Differentiate the expression with respect to the specified variable.")
      .def("distribute", &Expr::Distribute, "Expand products of additions and subtractions.")
      .def("subs", &Expr::Subs, py::arg("target"), py::arg("substitute"),
           "Replace the `target` expression with `substitute` in the expression tree.")
      .def("eval", &EvalToNumeric, "Evaluate into float expression.")
      // Operators:
      .def(py::self + py::self)
      .def(py::self - py::self)
      .def(py::self * py::self)
      .def(py::self / py::self)
      .def(-py::self)
      .def("__pow__", &math::pow)
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
      .def(double() == py::self);

  py::implicitly_convertible<std::int64_t, Expr>();
  py::implicitly_convertible<double, Expr>();

  // Methods for declaring expressions:
  m.def("symbols", &CreateSymbols, py::arg("arg"),
        "Create variables from a string or an iterable of strings.");
  m.def(
      "integer", [](std::int64_t value) { return Expr{value}; }, "value"_a,
      "Create an integer expression.");

  // Built-in functions:
  m.def("log", &math::log, "arg"_a, "Natural log.");
  m.def("pow", &math::pow, "base"_a, "exp"_a, "Evaluates to: base ** exp.");
  m.def("cos", &math::cos, "arg"_a, "Cosine function.");
  m.def("sin", &math::sin, "arg"_a, "Sine function.");
  m.def("tan", &math::tan, "arg"_a, "Tan function.");
  m.def("acos", &math::acos, "arg"_a, "Arc-cosine function.");
  m.def("asin", &math::asin, "arg"_a, "Arc-sine function.");
  m.def("atan", &math::atan, "arg"_a, "Arc-tangent function.");
  m.def("sqrt", &math::sqrt, "arg"_a, "Square-root function.");
  m.def("abs", &math::abs, "arg"_a, "Absolute value function.");
  m.def("atan2", &math::atan2, "y"_a, "x"_a, "2-argument arc-tangent function.");
  m.def("where", &math::where, "condition"_a, "if_true"_a, "if_false"_a, "If-else statement.");

  // Special constants:
  m.attr("euler") = Constants::Euler;
  m.attr("inf") = Constants::Infinity;
  m.attr("one") = Constants::One;
  m.attr("pi") = Constants::Pi;
  m.attr("zero") = Constants::Zero;

  // Exceptions:
  py::register_exception<DimensionError>(m, "DimensionError");
  py::register_exception<TypeError>(m, "TypeError");

  // Include other wrappers in this module:
  WrapMatrixOperations(m);
}  // PYBIND11_MODULE
