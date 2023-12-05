// Copyright 2023 Gareth Cross
#include "wf/code_generation/function_description.h"

#include "wf/expressions/variable.h"

namespace wf {

void function_signature::add_argument(const std::string_view name, argument_type type,
                                      argument_direction direction) {
  auto it = std::find_if(arguments_.begin(), arguments_.end(),
                         [&name](const auto& arg) { return arg->name() == name; });
  WF_ASSERT(it == arguments_.end(), "Argument with name `{}` already exists.", name);
  arguments_.push_back(std::make_shared<const argument>(name, std::move(type), direction));
}

std::optional<std::shared_ptr<const argument>> function_signature::argument_by_name(
    std::string_view str) const {
  auto it = std::find_if(arguments_.begin(), arguments_.end(),
                         [&str](const auto& arg) { return arg->name() == str; });
  if (it == arguments_.end()) {
    return std::nullopt;
  }
  return *it;
}

bool function_signature::has_matrix_arguments() const noexcept {
  return std::any_of(arguments_.begin(), arguments_.end(),
                     [](const auto& arg) { return arg->is_matrix(); });
}

void function_signature::set_return_value_type(argument_type type) {
  return_value_type_ = std::move(type);
}

std::variant<Expr, MatrixExpr> function_description::add_input_argument(std::string_view name,
                                                                        argument_type type) {
  const std::size_t arg_index = signature_.num_arguments();
  signature_.add_argument(name, type, argument_direction::input);
  if (const scalar_type* s = std::get_if<scalar_type>(&type); s != nullptr) {
    return variable::create_function_argument(arg_index, 0);
  } else {
    const matrix_type* m = std::get_if<matrix_type>(&type);
    WF_ASSERT(m);

    std::vector<Expr> expressions{};
    expressions.reserve(m->size());
    for (std::size_t i = 0; i < m->size(); ++i) {
      expressions.push_back(variable::create_function_argument(arg_index, i));
    }
    return MatrixExpr::create(m->rows(), m->cols(), std::move(expressions));
  }
}

void function_description::add_output_argument(std::string_view name, argument_type type,
                                               bool is_optional, std::vector<Expr> expressions) {
  signature_.add_argument(
      name, type, is_optional ? argument_direction::optional_output : argument_direction::output);

  output_key key{
      is_optional ? expression_usage::optional_output_argument : expression_usage::output_argument,
      name};
  output_expressions_.emplace_back(std::move(expressions), std::move(key));
}

void function_description::set_return_value(argument_type type, std::vector<Expr> expressions) {
  WF_ASSERT(!signature_.has_return_value(), "Return value on function `{}` already set.",
            signature_.name());
  signature_.set_return_value_type(type);
  output_expressions_.emplace_back(std::move(expressions),
                                   output_key{expression_usage::return_value, ""});
}

}  // namespace wf
