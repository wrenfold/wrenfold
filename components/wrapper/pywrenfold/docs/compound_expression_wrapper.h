// Copyright 2024 Gareth Cross
#pragma once
#include <string_view>

namespace wf::docstrings {

inline constexpr std::string_view compound_expr = R"doc(
A compound expression is an instance of an aggregate type with members that have been initialized
from symbolic expressions. It is used to represent a user-provided struct in the symbolic expression
tree. This enables a couple of functionalities:

  * User types can be passed as input and output arguments from generated functions.
  * User types can be passed as inputs and outputs from external (non-generated) functions that are
    invoked from within a generated function.
)doc";

}  // namespace wf::docstrings
