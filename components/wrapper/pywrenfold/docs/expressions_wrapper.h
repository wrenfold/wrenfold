// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <string_view>

// Docstrings for the `expressions_wrapper.cc` file.
namespace wf::docstrings {

inline constexpr std::string_view variable_constructor = R"doc(
Construct a concrete ``Variable`` expression.

Args:
  name: String name of the variable.
  number_set: What numeric set to assume for this variable.

Examples:
  >>> from wrenfold import expressions, enumerations, sym
  >>> x = expressions.Variable('x', number_set=enumerations.NumberSet.RealPositive)
  >>> type(x)
  pywrenfold.expressions.Variable
  >>> sym.cos(x.to_expression())
  cos(x)
)doc";

}  // namespace wf::docstrings
