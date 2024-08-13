// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/expressions/function_expressions.h"
#include "wf/functions.h"
#include "wf/utility/assertions.h"

namespace wf {

// Call the appropriate creation method for the specified enum value.
// We need this logic to support `map_children`.
scalar_expr built_in_function_invocation::create(const built_in_function name,
                                                 const absl::Span<const scalar_expr> args) {
  WF_ASSERT(!args.empty());
  switch (name) {
    case built_in_function::cos:
      return cos(args.front());
    case built_in_function::sin:
      return sin(args.front());
    case built_in_function::tan:
      return tan(args.front());
    case built_in_function::arccos:
      return acos(args.front());
    case built_in_function::arcsin:
      return asin(args.front());
    case built_in_function::arctan:
      return atan(args.front());
    case built_in_function::cosh:
      return cosh(args.front());
    case built_in_function::sinh:
      return sinh(args.front());
    case built_in_function::tanh:
      return tanh(args.front());
    case built_in_function::arccosh:
      return acosh(args.front());
    case built_in_function::arcsinh:
      return asinh(args.front());
    case built_in_function::arctanh:
      return atanh(args.front());
    case built_in_function::log:
      return log(args.front());
    case built_in_function::abs:
      return abs(args.front());
    case built_in_function::signum:
      return signum(args.front());
    case built_in_function::floor:
      return floor(args.front());
    case built_in_function::arctan2:
      return atan2(args[0], args[1]);
  }
  WF_ASSERT_ALWAYS("Invalid function name: {}", string_from_built_in_function(name));
}

symbolic_function::symbolic_function(std::string name)
    : impl_(std::make_shared<const impl>(impl{std::move(name)})) {
  if (impl_->name.empty()) {
    throw invalid_argument_error("Function name cannot be empty.");
  }
}

}  // namespace wf
