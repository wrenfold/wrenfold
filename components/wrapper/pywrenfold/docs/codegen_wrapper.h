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

}  // namespace wf::docstrings
