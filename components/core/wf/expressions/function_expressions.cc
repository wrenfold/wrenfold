// Copyright 2024 Gareth Cross
#include "wf/expressions/function_expressions.h"
#include "wf/assertions.h"
#include "wf/functions.h"

namespace wf {

// Call the appropriate creation method for the specified enum value.
// We need this logic to support `map_children`.
scalar_expr function::create(const built_in_function name,
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
    case built_in_function::ln:
      return log(args.front());
    case built_in_function::abs:
      return abs(args.front());
    case built_in_function::signum:
      return signum(args.front());
    case built_in_function::arctan2:
      return atan2(args[0], args[1]);
  }
  WF_ASSERT_ALWAYS("Invalid function name: {}", string_from_built_in_function(name));
}

}  // namespace wf