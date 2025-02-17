// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/function_description.h"

namespace wf {

function_description::function_description(std::string name) noexcept
    : impl_(std::make_shared<impl>(std::move(name))) {}

const any_expression& function_description::find_output_expressions(const output_key& key) const {
  const auto it = impl_->output_expressions.find(key);
  WF_ASSERT(it != impl_->output_expressions.end(), "Key missing: ({}, {})", key.name,
            string_from_expression_usage(key.usage));
  return it->second;
}

std::variant<scalar_expr, matrix_expr, compound_expr> function_description::add_input_argument(
    const std::string_view name, type_variant type) {
  const argument& arg = add_argument(name, std::move(type), argument_direction::input);
  return arg.create_symbolic_input();
}

void function_description::add_output_argument(const std::string_view name, type_variant type,
                                               const bool is_optional, any_expression expression) {
  add_argument(name, std::move(type),
               is_optional ? argument_direction::optional_output : argument_direction::output);

  impl_->output_expressions.emplace(
      output_key{is_optional ? expression_usage::optional_output_argument
                             : expression_usage::output_argument,
                 name},
      std::move(expression));
}

// ReSharper disable once CppMemberFunctionMayBeConst
void function_description::set_return_value(type_variant type, any_expression expression) {
  WF_ASSERT(!impl_->return_value_type.has_value(), "Return value on function `{}` already set.",
            name());
  impl_->return_value_type = std::move(type);
  impl_->output_expressions.emplace(output_key{expression_usage::return_value, ""},
                                    std::move(expression));
}

// ReSharper disable once CppMemberFunctionMayBeConst
const argument& function_description::add_argument(const std::string_view name, type_variant type,
                                                   const argument_direction direction) {
  if (any_of(impl_->arguments, [&name](const argument& arg) { return arg.name() == name; })) {
    throw invalid_argument_error("Argument with name `{}` already exists.", name);
  }
  impl_->arguments.emplace_back(name, std::move(type), direction, impl_->arguments.size());
  return impl_->arguments.back();
}

}  // namespace wf
