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

#include "docs/expression_wrapper.h"
#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

// Create symbols from CSV list of names.
static std::variant<scalar_expr, py::list> create_symbols_from_str(const std::string_view csv,
                                                                   const number_set set) {
  py::list variables{};
  std::string name{};
  for (const char c : csv) {
    if (std::isspace(c) || c == ',') {
      if (!name.empty()) {
        variables.append(make_expr<variable>(std::move(name), set));
        name = std::string();
      }
      continue;
    }
    name += c;
  }
  if (!name.empty()) {
    variables.append(make_expr<variable>(std::move(name), set));
  }
  if (variables.size() == 1) {
    return variables[0].cast<scalar_expr>();
  }
  return variables;
}

// Traverse the provided iterable, inspecting elements. If the inner element is a
// string, then parse it into symbols. If the inner element is an iterable, recurse on it.
static std::variant<scalar_expr, py::list> create_symbols_from_str(const py::iterable& iterable,
                                                                   const number_set set) {
  py::list result{};
  for (const py::handle& handle : iterable) {
    // Each element of the iterable could be a string, or a nested iterable:
    auto symbols =
        std::visit([set](const auto& input) { return create_symbols_from_str(input, set); },
                   py::cast<std::variant<std::string_view, py::iterable>>(handle));
    result.append(std::move(symbols));
  }
  return result;
}

// Convert optional boolean arguments into `number_set`.
inline number_set determine_set_from_flags(const bool real, const bool positive,
                                           const bool nonnegative, const bool complex) {
  if (const bool is_real = real || positive || nonnegative; is_real && complex) {
    throw invalid_argument_error("Symbols cannot be both real and complex.");
  }
  if (positive) {
    return number_set::real_positive;
  } else if (nonnegative) {
    return number_set::real_non_negative;
  } else if (real) {
    return number_set::real;
  } else if (complex) {
    return number_set::complex;
  }
  return number_set::unknown;
}

// To imitate sympy, we support a list of bool flags to specify assumptions.
// We check for incompatible arrangements in this functions.
std::variant<scalar_expr, py::list> create_symbols_from_str_or_iterable(
    const std::variant<std::string_view, py::iterable>& arg, const bool real, const bool positive,
    const bool nonnegative, const bool complex) {
  return std::visit(
      [&](const auto& input) {
        return create_symbols_from_str(
            input, determine_set_from_flags(real, positive, nonnegative, complex));
      },
      arg);
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

// Defined in ast_wrapper.cc
void wrap_ast_operations(py::module_& m);

// Defined in code_formatting_wrapper.cc
void wrap_code_formatting_operations(py::module_& m);

// Defined in compound_expression_wrapper.cc
void wrap_compound_expression(py::module_& m);

// Defined in boolean_expression_wrapper.cc
void wrap_boolean_expression(py::module_& m);

// Defined in geometry_wrapper.cc
void wrap_geometry_operations(py::module_& m);

// Defined in sympy_conversion.cc
void wrap_sympy_conversion(py::module_& m);
}  // namespace wf

// ReSharper disable CppIdenticalOperandsInBinaryExpression
PYBIND11_MODULE(PY_MODULE_NAME, m) {
  using namespace wf;

  // For types to show up correctly in docstrings, we need to wrap `boolean_expr` first.
  wrap_boolean_expression(m);

  // Primary expression type:
  wrap_class<scalar_expr>(m, "Expr")
      // Implicit construction from numerics:
      .def(py::init<std::int64_t>())
      .def(py::init<double>())
      // String conversion:
      .def("__repr__", &scalar_expr::to_string)
      .def("expression_tree_str", &scalar_expr::to_expression_tree_string,
           docstrings::scalar_expr_expression_tree_str.data())
      .def_property_readonly(
          "type_name", [](const scalar_expr& self) { return self.type_name(); },
          docstrings::scalar_expr_type_name.data())
      // Operations:
      .def(
          "diff",
          [](const scalar_expr& self, const scalar_expr& var, const int order,
             const bool use_abstract) {
            return self.diff(var, order,
                             use_abstract ? non_differentiable_behavior::abstract
                                          : non_differentiable_behavior::constant);
          },
          "var"_a, py::arg("order") = 1, py::arg("use_abstract") = false,
          docstrings::scalar_expr_diff.data())
      .def("distribute", &scalar_expr::distribute, docstrings::scalar_expr_distribute.data())
      .def("subs", &scalar_expr::subs, py::arg("target"), py::arg("substitute"),
           "Replace the `target` expression with `substitute` in the expression tree.")
      .def("subs_variables", &substitute_variables_wrapper, py::arg("pairs"),
           "Substitute a list of variable expressions.")
      .def("eval", &eval_wrapper, docstrings::scalar_expr_eval.data())
      .def(
          "collect",
          [](const scalar_expr& self, const scalar_expr& term) { return self.collect(term); },
          "term"_a, "Overload of ``collect`` that accepts a single variable.")
      .def(
          "collect",
          [](const scalar_expr& self, const std::vector<scalar_expr>& terms) {
            return wf::collect_many(self, terms);
          },
          "terms"_a, docstrings::scalar_expr_collect.data())
      // Operators:
      .def(py::self + py::self)
      .def(py::self - py::self)
      .def(py::self * py::self)
      .def(py::self / py::self)
      .def(-py::self)
      .def("__pow__", &wf::pow, py::is_operator(), "other_a")
      .def(
          "__rpow__",
          [](const scalar_expr& self, const scalar_expr& other) { return pow(other, self); },
          py::is_operator(), "other_a")
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
          py::doc("Coerce expression to bool."))
      .doc() = "A scalar-valued symbolic expression.";

  py::implicitly_convertible<std::int64_t, scalar_expr>();
  py::implicitly_convertible<double, scalar_expr>();

  // Methods for declaring expressions:
  m.def("symbols", &create_symbols_from_str_or_iterable, py::arg("names"), py::arg("real") = false,
        py::arg("positive") = false, py::arg("nonnegative") = false, py::arg("complex") = false,
        docstrings::symbols.data());
  m.def(
      "integer", [](const std::int64_t value) { return scalar_expr{value}; }, "value"_a,
      docstrings::integer.data());
  m.def(
      "float", [](const double value) { return scalar_expr{value}; }, "value"_a,
      docstrings::float_.data());
  m.def(
      "rational",
      [](const std::int64_t n, const std::int64_t d) {
        if (d == 0) {
          throw wf::arithmetic_error("Rational denominator must be non-zero: {} / {}", n, d);
        }
        return scalar_expr{rational_constant{n, d}};
      },
      py::arg("n"), py::arg("d"), docstrings::rational.data());

  // Built-in functions:
  m.def("log", &wf::log, "arg"_a, docstrings::log.data());
  m.def("pow", &wf::pow, "base"_a, "exp"_a, docstrings::pow.data());
  m.def("cos", &wf::cos, "arg"_a, docstrings::cos.data());
  m.def("sin", &wf::sin, "arg"_a, docstrings::sin.data());
  m.def("tan", &wf::tan, "arg"_a, docstrings::tan.data());
  m.def("acos", &wf::acos, "arg"_a, docstrings::acos.data());
  m.def("asin", &wf::asin, "arg"_a, docstrings::asin.data());
  m.def("atan", &wf::atan, "arg"_a, docstrings::atan.data());
  m.def("cosh", &wf::cosh, "arg"_a, docstrings::cosh.data());
  m.def("sinh", &wf::sinh, "arg"_a, docstrings::sinh.data());
  m.def("tanh", &wf::tanh, "arg"_a, docstrings::tanh.data());
  m.def("acosh", &wf::acosh, "arg"_a, docstrings::acosh.data());
  m.def("asinh", &wf::asinh, "arg"_a, docstrings::asinh.data());
  m.def("atanh", &wf::atanh, "arg"_a, docstrings::atanh.data());
  m.def("sqrt", &wf::sqrt, "arg"_a, docstrings::sqrt.data());
  m.def("abs", static_cast<scalar_expr (*)(const scalar_expr&)>(&wf::abs), "arg"_a,
        docstrings::abs.data());
  m.def("sign", &wf::signum, "arg"_a, docstrings::sign.data());
  m.def("floor", &wf::floor, "arg"_a, docstrings::floor.data());
  m.def("atan2", &wf::atan2, "y"_a, "x"_a, docstrings::atan2.data());

  m.def("max", &wf::max, "a"_a, "b"_a, docstrings::max.data());
  m.def("min", &wf::min, "a"_a, "b"_a, docstrings::min.data());
  m.def("where",
        static_cast<scalar_expr (*)(const boolean_expr&, const scalar_expr&, const scalar_expr&)>(
            &wf::where),
        "c"_a, "a"_a, "b"_a, docstrings::where.data());

  // Relational operations:
  m.def("lt", static_cast<boolean_expr (*)(const scalar_expr&, const scalar_expr&)>(&operator<),
        "a"_a, "b"_a, docstrings::less_than.data());
  m.def("le", static_cast<boolean_expr (*)(const scalar_expr&, const scalar_expr&)>(&operator<=),
        "a"_a, "b"_a, docstrings::less_than_or_equal.data());
  m.def("gt", static_cast<boolean_expr (*)(const scalar_expr&, const scalar_expr&)>(&operator>),
        "a"_a, "b"_a, docstrings::greater_than.data());
  m.def("ge", static_cast<boolean_expr (*)(const scalar_expr&, const scalar_expr&)>(&operator>=),
        "a"_a, "b"_a, docstrings::greater_than_or_equal.data());
  m.def("eq", static_cast<boolean_expr (*)(const scalar_expr&, const scalar_expr&)>(&operator==),
        "a"_a, "b"_a, docstrings::equal.data());

  m.def("iverson", &wf::iverson, "arg"_a, docstrings::iverson.data());

  // Special constants:
  m.attr("E") = constants::euler;
  m.attr("pi") = constants::pi;
  m.attr("zoo") = constants::complex_infinity;
  m.attr("one") = constants::one;
  m.attr("zero") = constants::zero;
  m.attr("imaginary_unit") = constants::imaginary_unit;
  m.attr("I") = constants::imaginary_unit;
  m.attr("nan") = constants::undefined;

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
  wrap_sympy_conversion(m);

  auto m_geo = m.def_submodule("geometry", "Wrapped geometry methods.");
  wrap_geometry_operations(m_geo);

  auto m_ast = m.def_submodule("ast", "Wrapped AST types.");
  wrap_ast_operations(m_ast);

  auto m_codegen = m.def_submodule("codegen", "Wrapped code-generation types.");
  wrap_codegen_operations(m_codegen);
  wrap_code_formatting_operations(m_codegen);
}  // PYBIND11_MODULE
