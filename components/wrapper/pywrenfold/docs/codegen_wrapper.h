// Copyright 2024 Gareth Cross
#pragma once
#include <string_view>

namespace wf::docstrings {

inline constexpr std::string_view function_description = R"doc(
Stores information required to emit the function signature, including:
  * The types of input and output values.
  * All the symbolic expressions required to compute the outputs.

``FunctionDescription`` may be passed to :func:`wrenfold.code_generation.transpile` in order to
create a syntax tree representation, which may then be converted into usable code.
)doc";

}  // namespace wf::docstrings
