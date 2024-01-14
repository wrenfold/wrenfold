#include "wf/expressions/custom_function_invocation.h"

namespace wf {

custom_function_invocation::custom_function_invocation(custom_function func, container_type args)
    : function_(std::move(func)), args_(std::move(args)) {
  WF_ASSERT_EQUAL(
      function_.num_arguments(), args_.size(),
      "Mismatch in # of args between function spec and provided argument list. Function: {}",
      function_.name());
}

bool custom_function_invocation::is_identical_to(const custom_function_invocation& other) const {
  return are_identical(function_, other.function_) && all_identical(args_, other.args_);
}

custom_type_construction::custom_type_construction(custom_type type, std::vector<Expr> args)
    : type_(std::move(type)), args_(std::move(args)) {
  WF_ASSERT_EQUAL(
      type_.total_size(), args_.size(),
      "Mismatch between size of custom type `{}` ({}) and the number of provided args ({}).",
      type_.name(), type_.total_size(), args_.size());
}

Expr compound_expression_element::create(compound_expr provenance, const std::size_t index) {
  if (const custom_type_construction* construct = cast_ptr<custom_type_construction>(provenance);
      construct != nullptr) {
    return construct->at(index);
  }
  return Expr(std::in_place_type_t<compound_expression_element>{}, std::move(provenance), index);
}

}  // namespace wf
