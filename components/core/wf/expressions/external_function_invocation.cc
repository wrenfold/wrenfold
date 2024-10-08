// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/expressions/external_function_invocation.h"

namespace wf {

external_function_invocation::external_function_invocation(external_function func,
                                                           container_type args)
    : function_(std::move(func)), args_(std::move(args)) {
  WF_ASSERT_EQ(
      function_.num_arguments(), args_.size(),
      "Mismatch in # of args between function spec and provided argument list. Function: {}",
      function_.name());
}

bool is_identical_struct<external_function_invocation>::operator()(
    const external_function_invocation& a, const external_function_invocation& b) const {
  return are_identical(a.function(), b.function()) && are_identical(a.children(), b.children());
}

}  // namespace wf
