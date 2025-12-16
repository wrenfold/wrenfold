// wrenfold symbolic code generator.
// Copyright (c) 2025 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <string_view>

namespace wf::docstrings {

inline constexpr std::string_view python_generator_constructor = R"doc(
Construct a python code generator.

Args:
  target: Which python API to utilize.
  float_width: Precision of floating point to enforce throughout generated code.
  indentation: Number of spaces of indentation to apply. Typically 2 or 4.
  use_output_arguments: By default, output arguments are emitted as return values when generating
    Python. When ``use_output_arguments=True``, matrix-type output arguments will become actual
    output arguments in the generated code. Optional outputs will have type ``np.ndarray | None``.
    This mode is only supported with ``target=NumPy``, and is untested in other configurations.
)doc";

}  // namespace wf::docstrings
