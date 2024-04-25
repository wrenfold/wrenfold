// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <string_view>

namespace wf::docstrings {

inline constexpr std::string_view to_sympy = R"doc(
Convert expression tree to `sympy <https://www.sympy.org/en/index.html>`_ expressions.

Args:
  expr: Scalar-valued expression.
  sp: The sympy module. If None, the ``sympy`` package will be imported.
  evaluate: Forwarded to sympy constructors as the ``evaluate`` argument. If true, sympy will
    canonicalize expressions.

Returns:
  Equivalent sympy expression.

Raises:
  wrenfold.sym.TypeError: If no equivalent expression type exists in sympy.
)doc";

}  // namespace wf::docstrings
