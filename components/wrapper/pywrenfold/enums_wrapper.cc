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
      .value("Cos", std_math_function::cos)
      .value("Sin", std_math_function::sin)
      .value("Tan", std_math_function::tan)
      .value("Acos", std_math_function::acos)
      .value("Asin", std_math_function::asin)
      .value("Atan", std_math_function::atan)
      .value("Cosh", std_math_function::cosh)
      .value("Sinh", std_math_function::sinh)
      .value("Tanh", std_math_function::tanh)
      .value("Acosh", std_math_function::acosh)
      .value("Asinh", std_math_function::asinh)
      .value("Atanh", std_math_function::atanh)
      .value("Log", std_math_function::log)
      .value("Sqrt", std_math_function::sqrt)
      .value("Abs", std_math_function::abs)
      .value("Signum", std_math_function::signum)
      .value("Floor", std_math_function::floor)
      .value("Atan2", std_math_function::atan2)
      .value("Powi", std_math_function::powi)
      .value("Powf", std_math_function::powf)
      .def(
          "to_string",
          [](const std_math_function name) { return string_from_standard_library_function(name); },
          "Convert to string.");

  py::enum_<relational_operation>(m, "RelationalOperation")
      .value("LessThan", relational_operation::less_than)
      .value("LessThanOrEqual", relational_operation::less_than_or_equal)
      .value("Equal", relational_operation::equal);

  py::enum_<symbolic_constant_enum>(m, "SymbolicConstant")
      .value("Euler", symbolic_constant_enum::euler)
      .value("Pi", symbolic_constant_enum::pi);

  py::enum_<number_set>(m, "NumberSet")
      .value("RealPositive", number_set::real_positive)
      .value("RealNonNegative", number_set::real_non_negative)
      .value("Real", number_set::real)
      .value("Complex", number_set::complex)
      .value("Unknown", number_set::unknown);
}

}  // namespace wf
