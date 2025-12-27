// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <optional>
#include <vector>

#include <nanobind/nanobind.h>
#include <nanobind/operators.h>
#include <nanobind/stl/complex.h>
#include <nanobind/stl/function.h>
#include <nanobind/stl/string.h>
#include <nanobind/stl/vector.h>

#include "wf/collect.h"
#include "wf/constants.h"
#include "wf/cse.h"
#include "wf/expression.h"
#include "wf/expressions/addition.h"
#include "wf/expressions/derivative_expression.h"
#include "wf/expressions/function_expressions.h"
#include "wf/expressions/multiplication.h"
#include "wf/expressions/substitute_expression.h"
#include "wf/expressions/variable.h"
#include "wf/functions.h"
#include "wf/numerical_casts.h"
#include "wf/utility/error_types.h"
#include "wf/utility_visitors.h"

#include "args_visitor.h"
#include "docs/scalar_wrapper.h"
#include "visitor_wrappers.h"
#include "wrapper_utils.h"

namespace py = nanobind;
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
    return py::cast<scalar_expr>(variables[0]);
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
                   py::cast<std::variant<std::string, py::iterable>>(handle));
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
// We check for incompatible arrangements in this function.
static std::variant<scalar_expr, py::list> create_symbols_from_str_or_iterable(
    const std::variant<std::string, py::iterable>& arg, const bool real, const bool positive,
    const bool nonnegative, const bool complex) {
  return std::visit(
      [&](const auto& input) {
        return create_symbols_from_str(
            input, determine_set_from_flags(real, positive, nonnegative, complex));
      },
      arg);
}

static std::variant<scalar_expr, py::list> create_unique_variables(const std::size_t num,
                                                                   const bool real,
                                                                   const bool positive,
                                                                   const bool nonnegative,
                                                                   const bool complex) {
  if (num == 0) {
    throw invalid_argument_error("count must be >= 1");
  }
  const auto set = determine_set_from_flags(real, positive, nonnegative, complex);
  if (num == 1) {
    return make_unique_variable_symbol(set);
  } else {
    py::list result{};
    for (std::size_t i = 0; i < num; ++i) {
      result.append(make_unique_variable_symbol(set));
    }
    return result;
  }
}

static auto eval_wrapper(const scalar_expr& self) {
  if (const auto maybe_num = numerical_cast(self); maybe_num.has_value()) {
    return maybe_num;
  } else {
    throw wf::type_error("Expression of type `{}` is not coercible to a numeric value.",
                         self.type_name());
  }
}

// ReSharper disable CppIdenticalOperandsInBinaryExpression
void wrap_scalar_operations(py::module_& m) {
  // Primary expression type:
  wrap_class<scalar_expr>(m, "Expr")
      // Implicit construction from numerics:
      .def(py::init_implicit<std::int64_t>())
      .def(py::init_implicit<double>())
      // String conversion:
      .def("__repr__", &scalar_expr::to_string)
      .def("expression_tree_str", &scalar_expr::to_expression_tree_string,
           docstrings::scalar_expr_expression_tree_str.data())
      .def_prop_ro(
          "type_name", [](const scalar_expr& self) { return self.type_name(); },
          docstrings::scalar_expr_type_name.data())
      .def_prop_ro(
          "args", [](const scalar_expr& self) { return args_visitor{}(self); },
          "Arguments of ``self`` as a tuple.")
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
      .def("distribute", &scalar_expr::distribute, "See :func:`wrenfold.sym.distribute`.")
      .def("subs", &substitute_wrapper<scalar_expr>, py::arg("pairs"),
           "See :func:`wrenfold.sym.subs`.")
      .def("subs", make_substitute_wrapper_single<scalar_expr, scalar_expr>(), py::arg("target"),
           py::arg("substitute"),
           "Overload of ``subs`` that performs a single scalar-valued substitution.")
      .def("subs", make_substitute_wrapper_single<scalar_expr, boolean_expr>(), py::arg("target"),
           py::arg("substitute"),
           "Overload of ``subs`` that performs a single boolean-valued substitution.")
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
      .def("__abs__", [](const scalar_expr& self) { return abs(self); })
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
          "Coerce expression to bool.")
      .doc() = "A scalar-valued symbolic expression.";

  // py::implicitly_convertible<std::int64_t, scalar_expr>();
  // py::implicitly_convertible<double, scalar_expr>();

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
  m.def("unique_symbols", &create_unique_variables, py::arg("count"), py::arg("real") = false,
        py::arg("positive") = false, py::arg("nonnegative") = false, py::arg("complex") = false,
        docstrings::unique_symbols.data());

  m.def(
      "compare",
      [](const scalar_expr& a, const scalar_expr& b) {
        const relative_order order = order_by(a, b);
        return static_cast<int>(order);
      },
      "a"_a, "b"_a, docstrings::compare.data());

  // Built-in functions:
  m.def("log", &wf::log, "arg"_a, docstrings::log.data());
  m.def("exp", &wf::exp, "arg"_a, docstrings::exp.data());
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

  m.def("unevaluated", &wf::make_unevaluated, "arg"_a, docstrings::unevaluated.data());
  m.def("stop_derivative", &wf::stop_diff, "arg"_a, docstrings::stop_derivative.data());

  m.def(
      "eliminate_subexpressions",
      [](const scalar_expr& expr,
         std::optional<std::function<scalar_expr(std::size_t)>> make_variable,
         const std::size_t min_occurrences) {
        return eliminate_subexpressions(expr, std::move(make_variable).value_or(nullptr),
                                        min_occurrences);
      },
      "expr"_a, "make_variable"_a = py::none(), "min_occurrences"_a = 2,
      docstrings::eliminate_subexpressions.data());

  // Special constants:
  m.attr("E") = constants::euler;
  m.attr("pi") = constants::pi;
  m.attr("zoo") = constants::complex_infinity;
  m.attr("one") = constants::one;
  m.attr("zero") = constants::zero;
  m.attr("imaginary_unit") = constants::imaginary_unit;
  m.attr("I") = constants::imaginary_unit;
  m.attr("nan") = constants::undefined;

  // Direct constructors for addition+multiplication.
  m.def(
      "addition",
      [](const std::vector<scalar_expr>& args) { return addition::from_operands(args); },
      py::arg("args"), "Construct addition expression from provided operands.");
  m.def(
      "multiplication",
      [](const std::vector<scalar_expr>& args) { return multiplication::from_operands(args); },
      py::arg("args"), "Construct multiplication expression from provided operands.");

  wrap_class<symbolic_function>(m, "Function")
      .def(py::init<std::string>(), py::arg("name"),
           docstrings::symbolic_function_constructor.data())
      .def_prop_ro("name", &symbolic_function::name, "Name of the function.")
      .def("__repr__", &symbolic_function::name)
      .def(
          "__call__",
          [](const symbolic_function& self, const py::args& args) {
            return make_expr<symbolic_function_invocation>(
                self, transform_map<symbolic_function_invocation::container_type>(
                          args, [](const py::handle& x) { return py::cast<scalar_expr>(x); }));
          },
          "Invoke the symbolic function with the provided scalar expressions, and return a "
          "new scalar expression.")
      .doc() =
      "A scalar-valued symbolic function. Used to construct expressions of undefined functions.";

  m.def("substitution", &substitution::create, py::arg("input"), py::arg("target"),
        py::arg("replacement"), docstrings::substitution.data());
  m.def("derivative", &derivative::create, py::arg("function"), py::arg("arg"),
        py::arg("order") = 1, docstrings::derivative.data());

  m.def(
      "get_variables",
      [](const scalar_expr& expr) {
        return get_expressions_by_predicate(expr, [](const scalar_expr& x) {
          return x.is_type<variable, unique_variable, function_argument_variable>();
        });
      },
      py::arg("expr"), docstrings::get_variables.data());
}

}  // namespace wf
