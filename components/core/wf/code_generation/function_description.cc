// Copyright 2023 Gareth Cross
#include "wf/code_generation/function_description.h"

namespace wf {

function_description::function_description(std::string name) noexcept
    : impl_(std::make_shared<impl>(std::move(name))) {}

function_description::function_description(std::string name) noexcept
    : impl_(std::make_shared<impl>(std::move(name))) {}

std::variant<Expr, MatrixExpr, std::vector<Expr>> function_description::add_input_argument(
    const std::string_view name, type_variant type) {
  const argument& arg = add_argument(name, std::move(type), argument_direction::input);

  using return_type = std::variant<Expr, MatrixExpr, std::vector<Expr>>;
  return std::visit(
      [&](const auto& type_concrete) -> return_type {
        return detail::create_function_input(type_concrete, arg.index());
      },
      arg.type());
}

void function_description::add_output_argument(const std::string_view name, type_variant type,
                                               const bool is_optional,
                                               std::vector<Expr> expressions) {
  add_argument(name, std::move(type),
               is_optional ? argument_direction::optional_output : argument_direction::output);

  output_key key{
      is_optional ? expression_usage::optional_output_argument : expression_usage::output_argument,
      name};
  impl_->output_expressions.emplace_back(std::move(expressions), std::move(key));
}

// ReSharper disable once CppMemberFunctionMayBeConst
void function_description::set_return_value(type_variant type, std::vector<Expr> expressions) {
  WF_ASSERT(!impl_->return_value_type.has_value(), "Return value on function `{}` already set.",
            name());
  impl_->return_value_type = std::move(type);
  impl_->output_expressions.emplace_back(std::move(expressions),
                                         output_key{expression_usage::return_value, ""});
}

// ReSharper disable once CppMemberFunctionMayBeConst
const argument& function_description::add_argument(const std::string_view name, type_variant type,
                                                   const argument_direction direction) {
  WF_ASSERT(!std::any_of(impl_->arguments.begin(), impl_->arguments.end(),
                         [&name](const argument& arg) { return arg.name() == name; }),
            "Argument with name `{}` already exists.", name);
  impl_->arguments.emplace_back(name, std::move(type), direction, impl_->arguments.size());
  return impl_->arguments.back();
}

// ReSharper disable once CppMemberFunctionMayBeConst
const argument& function_description::add_argument(const std::string_view name, type_variant type,
                                                   const argument_direction direction) {
  WF_ASSERT(!std::any_of(impl_->arguments.begin(), impl_->arguments.end(),
                         [&name](const argument& arg) { return arg.name() == name; }),
            "Argument with name `{}` already exists.", name);
  impl_->arguments.emplace_back(name, std::move(type), direction, impl_->arguments.size());
  return impl_->arguments.back();
}

}  // namespace wf
