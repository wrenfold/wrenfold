// Copyright 2023 Gareth Cross
#include <nanobind/nanobind.h>
#include <nanobind/operators.h>
#include <nanobind/stl/string.h>
#include <nanobind/stl/string_view.h>

#include "constants.h"
#include "expression.h"
#include "expressions/numeric_expressions.h"
#include "functions.h"
#include "plain_formatter.h"
#include "tree_formatter.h"

namespace nb = nanobind;
using namespace nb::literals;
using namespace math;

std::string ExprRepr(const Expr& expression) {
  PlainFormatter formatter{PowerStyle::Python};
  expression.Receive(formatter);
  return formatter.GetOutput();
}

NB_MODULE(mc, m) {
  // Primary expression type:
  nb::class_<Expr>(m, "Expr")
      // Implicit construction from numerics:
      .def(nb::init_implicit<std::int64_t>())
      .def(nb::init_implicit<double>())
      // String conversion:
      .def("__repr__", &ExprRepr)
      .def("expression_tree_str", &FormatDebugTree,
           "Retrieve the expression tree as a pretty-printed string.")
      .def(
          "print_expression_tree", [](const Expr& x) { fmt::print("{}\n", FormatDebugTree(x)); },
          "Print the expression tree to standard out.")
      .def(
          "is_identical_to",
          [](const Expr& self, const Expr& other) { return self.IsIdenticalTo(other); }, "other"_a,
          "Test if two expressions have identical expression trees.")
      .def_property_readonly("type_name", &Expr::TypeName)
      // Derivatives:
      .def("diff", &Expr::Diff, "var"_a, nb::arg("order") = 1,
           "Differentiate the expression with respect to the specified variable.")
      // Operators:
      .def(nb::self + nb::self)
      .def(nb::self - nb::self)
      .def(nb::self * nb::self)
      .def(nb::self / nb::self)
      .def(-nb::self)
      .def("__pow__", &math::pow)
      // Operators involving integers (int on left side):
      .def(std::int64_t() + nb::self)
      .def(std::int64_t() - nb::self)
      .def(std::int64_t() * nb::self)
      .def(std::int64_t() / nb::self)
      // Operators involving doubles (double on left side):
      .def(double() + nb::self)
      .def(double() - nb::self)
      .def(double() * nb::self)
      .def(double() / nb::self);

  // Methods for declaring expressions:
  m.def(
      "symbol", [](std::string_view name) { return Expr(name); }, "name"_a,
      "Declare a named symbol.");
  m.def(
      "integer", [](std::int64_t value) { return Expr{value}; }, "value"_a,
      "Create an integer expression.");

  // Built-in functions:
  m.def("pow", &math::pow, "base"_a, "exponent"_a, "Evaluates to: base ** exponent.");
  m.def("cos", &math::cos, "arg"_a, "Cosine function.");
  m.def("sin", &math::sin, "arg"_a, "Sine function.");
  m.def("tan", &math::tan, "arg"_a, "Tan function.");
  m.def("acos", &math::acos, "arg"_a, "Arc-cosine function.");
  m.def("asin", &math::asin, "arg"_a, "Arc-sine function.");
  m.def("atan", &math::atan, "arg"_a, "Arc-tangent function.");
  m.def("sqrt", &math::sqrt, "arg"_a, "Square-root function.");

  // Special constants:
  m.attr("euler") = Constants::Euler;
  m.attr("inf") = Constants::Infinity;
  m.attr("one") = Constants::One;
  m.attr("pi") = Constants::Pi;
  m.attr("zero") = Constants::Zero;
}
