// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <string_view>

namespace wf::docstrings {

inline constexpr std::string_view distribute = R"doc(
Expand the mathematical expression. ``distribute`` will recursively traverse the expression tree and
multiply out any product of additions and subtractions. For example:

.. math::
  \left(x + y\right)\cdot(4 - y) \rightarrow 4 \cdot x + 4  \cdot y - x  \cdot y - y^{2}

Powers of the form :math:`f\left(x\right)^{\frac{n}{2}}` where :math:`n` is an integer are also
expanded:

.. math::
  \left(x + 2\right)^{\frac{3}{2}} \rightarrow x \cdot \left(x + 2\right)^{\frac{1}{2}} +
  2 \cdot \left(x + 2\right)^{\frac{1}{2}}

Returns:
  The input expression after expansion.

Examples:
  >>> x, y = sym.make_symbols('x', 'y')
  >>> f = (x - 2) * (y + x) * (y - 3)
  >>> f.distribute()
  6*x + 6*y - 5*x*y + x**2*y + x*y**2 - 3*x**2 - 2*y**2
)doc";

inline constexpr std::string_view subs = R"doc(
Traverse the expression tree and replace target expressions with corresponding substitutions. The
list of replacements is executed *in order*, such that substitutions that appear later in the list
of ``(target, replacement)`` pairs may leverage the result of earlier ones.

Args:
  pairs: A list of tuples. Each tuple contains a ``(target, replacement)`` pair, where the *target*
    is the expression to find and the *replacement* is the expression to substitute in its place.
    The pairs may be scalar-valued or boolean-valued expressions.

Returns:
  The input expression after performing replacements.

Examples:
  >>> x, y = sym.make_symbols('x', 'y')
  >>> sym.cos(x).subs([(x, y*2), (y, sym.pi)])
  1
  >>> (sym.pow(x, 2) * y).subs(x * y, 3)
  3*x
  >>> (x + 2*y - 5).subs(x + 2*y, y)
  -5 + y
)doc";

}  // namespace wf::docstrings
