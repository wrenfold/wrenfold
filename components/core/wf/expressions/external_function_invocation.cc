// Copyright 2023 Gareth Cross
#include "wf/expressions/external_function_invocation.h"

namespace wf {

external_function_invocation::external_function_invocation(external_function func,
                                                           container_type args)
    : function_(std::move(func)), args_(std::move(args)) {
  WF_ASSERT_EQUAL(
      function_.num_arguments(), args_.size(),
      "Mismatch in # of args between function spec and provided argument list. Function: {}",
      function_.name());
}

bool external_function_invocation::is_identical_to(
    const external_function_invocation& other) const {
  return are_identical(function_, other.function_) && all_identical(args_, other.args_);
}

}  // namespace wf
