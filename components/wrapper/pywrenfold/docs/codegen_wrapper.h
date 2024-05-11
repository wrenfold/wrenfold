// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <string_view>

namespace wf::docstrings {

inline constexpr std::string_view transpile = R"doc(
Given a :class:`wrenfold.code_generation.FunctionDescription` object, convert it to an abstract
syntax tree suitable for code generation. This operation incorporates three steps:

  #. The symbolic expression tree is converted to a flat intermediate representation. Common
     subexpression elimination is performed to minimize duplicated operations.
  #. A control flow graph (CFG) is generated.
  #. The CFG is then converted to an abstract syntax tree (AST) that can be emitted as usable code.
     See the :doc:`ast` module for a list of types used in the syntax tree.

The syntax tree can then be passed to a generator (for example,
:class:`wrenfold.code_generation.CppGenerator`) to emit compilable code that you can incorporate in
your project.

Args:
  desc: :class:`wrenfold.code_generation.FunctionDescription` object.

Returns:
  Instance of :class:`wrenfold.ast.FunctionDefinition`.
)doc";

inline constexpr std::string_view function_description = R"doc(
Stores information required to emit the function signature, including:
  * The types of input and output values.
  * All the symbolic expressions required to compute the outputs.

``FunctionDescription`` may be passed to :func:`wrenfold.code_generation.transpile` in order to
create a syntax tree representation, which may then be converted into usable code.
)doc";

inline constexpr std::string_view cse_function_description = R"doc(
Given a :class:`wrenfold.code_generation.FunctionDescription` object, run the code-generation CSE
and then convert the simplified result *back* to a list of symbolic expressions. This function will
apply all the simplifications and subexpression elimination steps normally applied during
code-generation.

The first returned object is a dict mapping from ``OutputKey`` to output expressions. The outputs
will be expressed as a function of variables ``[v0, v1, ... v{N}]``. The second returned object is a
list of tuples of the form ``[(v0, <v0 expr>), (v1, <v1 expr>), ...]`` - these are the eliminated
subexpressions required to evaluate the function.

Args:
  desc: :class:`wrenfold.code_generation.FunctionDescription` object.

Returns:
  A dict mapping from ``OutputKey`` to output expressions, and a list of intermediate subexpressions
  required to compute the function outputs.

Example:
  >>> from wrenfold import sym, code_generation, type_annotations
  >>> def func(x: type_annotations.RealScalar, y: type_annotations.RealScalar):
  >>>   return sym.abs(x * y) * sym.cos(x * y)
  >>> desc = code_generation.create_function_description(func)
  >>> outputs, intermediate_values = code_generation.cse_function_description(desc)
  >>> outputs
  {OutputKey(return_value): [v5]}
  >>> intermediate_values
  [(v0, $arg(0, 0)),
   (v1, $arg(1, 0)),
   (v2, v0*v1),
   (v3, abs(v2)),
   (v4, cos(v2)),
   (v5, v3*v4)]
)doc";

}  // namespace wf::docstrings
