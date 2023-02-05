// Copyright 2023 Gareth Cross
#include <nanobind/nanobind.h>
#include <nanobind/operators.h>
#include <nanobind/stl/string.h>
#include <nanobind/stl/string_view.h>

#include "expression.h"
#include "expressions/numeric_expressions.h"
#include "functions.h"
#include "plain_formatter.h"
#include "tree_formatter.h"

namespace nb = nanobind;
using namespace nb::literals;
using namespace math;

std::string ExprRepr(const Expr& expression) {
  // TODO: Arg for python style formatting.
  PlainFormatter formatter{};
  expression.Receive(formatter);
  return formatter.GetOutput();
}

// This type is so that we can pass numeric values to methods that accept Expr.
struct ExprOrValue {
  explicit ExprOrValue(const Expr& expr) : v(expr) {}
  explicit ExprOrValue(const std::int64_t value) : v(to_expr_cast(value)) {}

  // Allow implicit cast to expression.
  operator const Expr&() const { return v; }

  Expr v;
};

NB_MODULE(mc, m) {
  nb::class_<ExprOrValue>(m, "ExprOrValue")
      .def(nb::init_implicit<Expr>())
      .def(nb::init_implicit<std::int64_t>());

  // Primary expression type:
  nb::class_<Expr>(m, "Expr")
      // String conversion:
      .def("__repr__", &ExprRepr)
      .def("expression_tree_str", &FormatDebugTree,
           "Retrieve the expression tree as a pretty-printed string.")
      .def(
          "print_expression_tree", [](const Expr& x) { fmt::print("{}\n", FormatDebugTree(x)); },
          "Print the expression tree to standard out.")
      // Operations we can do on expressions:
      .def("diff", &Expr::Diff, "var"_a, nb::arg("order") = 1,
           "Differentiate the expression wrt the specified variable.")
      // Operators:
      .def(nb::self + nb::self)
      .def(nb::self - nb::self)
      .def(nb::self * nb::self)
      .def(nb::self / nb::self)
      .def(-nb::self)
      .def(nb::self + std::int64_t())
      .def(nb::self - std::int64_t())
      .def(nb::self * std::int64_t())
      .def(nb::self / std::int64_t())
      .def(std::int64_t() + nb::self)
      .def(std::int64_t() - nb::self)
      .def(std::int64_t() * nb::self)
      .def(std::int64_t() / nb::self);

  // Methods for declaring expressions:
  m.def(
      "symbol", [](std::string_view name) { return Expr(name); }, "name"_a,
      "Declare a named symbol.");
  m.def(
      "int", [](std::int64_t value) { return to_expr_cast(value); }, "value"_a,
      "Create an integer expression.");

  // Built-in functions:
  m.def(
      "pow",
      [](const ExprOrValue& base, const ExprOrValue& exponent) { return pow(base, exponent); },
      "base"_a, "exponent"_a, "Evaluates to: base ** exponent.");
  m.def(
      "cos", [](const ExprOrValue& arg) { return cos(arg); }, "arg"_a, "Cosine function.");
  m.def(
      "sin", [](const ExprOrValue& arg) { return sin(arg); }, "arg"_a, "Sine function.");
  m.def(
      "tan", [](const ExprOrValue& arg) { return tan(arg); }, "arg"_a, "Tan function.");
}
