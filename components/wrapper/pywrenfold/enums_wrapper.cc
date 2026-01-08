// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <nanobind/nanobind.h>
#include <nanobind/stl/string_view.h>

#include "wf/enumerations.h"

namespace py = nanobind;
using namespace py::literals;

namespace wf {

void wrap_enums(py::module_& m) {
  py::enum_<std_math_function>(m, "StdMathFunction")
      .value("Cos", std_math_function::cos, "Cosine")
      .value("Sin", std_math_function::sin, "Sine")
      .value("Tan", std_math_function::tan, "Tangent")
      .value("Acos", std_math_function::acos, "Arccosine")
      .value("Asin", std_math_function::asin, "Arcsine")
      .value("Atan", std_math_function::atan, "Arctangent")
      .value("Cosh", std_math_function::cosh, "Hyperbolic cosine")
      .value("Sinh", std_math_function::sinh, "Hyperbolic sine")
      .value("Tanh", std_math_function::tanh, "Hyperbolic tangent")
      .value("Acosh", std_math_function::acosh, "Inverse hyperbolic cosine")
      .value("Asinh", std_math_function::asinh, "Inverse hyperbolic sine")
      .value("Atanh", std_math_function::atanh, "Inverse hyperbolic tangent")
      .value("Log", std_math_function::log, "Natural logarithm")
      .value("Exp", std_math_function::exp, "Exponential (e**x)")
      .value("Sqrt", std_math_function::sqrt, "Square-root")
      .value("Abs", std_math_function::abs, "Absolute value")
      .value("Signum", std_math_function::signum, "Sign function")
      .value("Floor", std_math_function::floor, "Floor function")
      .value("Atan2", std_math_function::atan2, "Atan2")
      .value("Powi", std_math_function::powi, "Raised to integer power")
      .value("Powf", std_math_function::powf, "Rased to floating-point power")
      .def(
          "to_string",
          [](const std_math_function name) { return string_from_standard_library_function(name); },
          "Convert to string.")
      .doc() = "Standard-library math functions utilized in generated code.";

  py::enum_<relational_operation>(m, "RelationalOperation")
      .value("LessThan", relational_operation::less_than, "< operator")
      .value("LessThanOrEqual", relational_operation::less_than_or_equal, "<= operator")
      .value("Equal", relational_operation::equal, "== operator")
      .doc() =
      "Comparison operations are canonicalized in terms of these three comparison operators.";

  py::enum_<symbolic_constant_enum>(m, "SymbolicConstant")
      .value("Euler", symbolic_constant_enum::euler, "Euler's constant: e")
      .value("Pi", symbolic_constant_enum::pi, "Pi: π")
      .doc() = "Significant mathematical constants.";

  py::enum_<number_set>(m, "NumberSet")
      .value("RealPositive", number_set::real_positive, "Real and positive (> 0).")
      .value("RealNonNegative", number_set::real_non_negative, "Real and non-negative (>= 0).")
      .value("Real", number_set::real, "The set of real numbers.")
      .value("Complex", number_set::complex, "The set of complex numbers.")
      .value("Unknown", number_set::unknown,
             "No assumption is made about variables with unknown numeric set.")
      .doc() = "The classification or mathematical type of a symbol.";
}

}  // namespace wf
