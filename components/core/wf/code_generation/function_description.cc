// Copyright 2023 Gareth Cross
#include "wf/code_generation/function_description.h"

#include "wf/expressions/variable.h"
#include "wf/index_range.h"

namespace wf {

void function_signature::add_argument(const std::string_view name, type_variant type,
                                      argument_direction direction) {
  WF_ASSERT(!std::any_of(arguments_.begin(), arguments_.end(),
                         [&name](const argument& arg) { return arg.name() == name; }),
            "Argument with name `{}` already exists.", name);
  arguments_.emplace_back(name, std::move(type), direction);
}

std::optional<argument> function_signature::argument_by_name(std::string_view str) const {
  auto it = std::find_if(arguments_.begin(), arguments_.end(),
                         [&str](const auto& arg) { return arg.name() == str; });
  if (it == arguments_.end()) {
    return std::nullopt;
  }
  return *it;
}

bool function_signature::has_matrix_arguments() const noexcept {
  return std::any_of(arguments_.begin(), arguments_.end(),
                     [](const argument& arg) { return arg.is_matrix(); });
}

void function_signature::set_return_value_type(type_variant type) {
  return_value_type_ = std::move(type);
}

struct create_custom_type_inputs {
  static void append(const std::size_t arg_index, const std::size_t num_to_append,
                     std::vector<Expr>& output) {
    const std::size_t start_index = output.size();
    output.reserve(start_index + num_to_append);
    for (std::size_t element : make_range(start_index, start_index + num_to_append)) {
      output.push_back(variable::create_function_argument(arg_index, element));
    }
  }

  void operator()(const scalar_type&, const std::size_t arg_index,
                  std::vector<Expr>& output) const {
    append(arg_index, 1, output);
  }

  void operator()(const matrix_type& m, const std::size_t arg_index,
                  std::vector<Expr>& output) const {
    append(arg_index, m.size(), output);
  }

  void operator()(const custom_type& c, const std::size_t arg_index,
                  std::vector<Expr>& output) const {
    // Append every field on this type, and recurse as well into child custom types.
    for (const field& field : c.fields()) {
      std::visit([&](const auto& child) { operator()(child, arg_index, output); }, field.type());
    }
  }
};

std::variant<Expr, MatrixExpr, std::vector<Expr>> function_description::add_input_argument(
    std::string_view name, type_variant type) {
  const std::size_t arg_index = signature_.num_arguments();
  signature_.add_argument(name, type, argument_direction::input);

  if (const scalar_type* s = std::get_if<scalar_type>(&type); s != nullptr) {
    return variable::create_function_argument(arg_index, 0);
  } else if (const matrix_type* m = std::get_if<matrix_type>(&type); m != nullptr) {
    std::vector<Expr> expressions{};
    create_custom_type_inputs{}(*m, arg_index, expressions);
    return MatrixExpr::create(m->rows(), m->cols(), std::move(expressions));
  } else {
    WF_ASSERT(std::holds_alternative<custom_type>(type));
    // Traverse the type and instantiate expressions:
    std::vector<Expr> expressions{};
    create_custom_type_inputs{}(std::get<custom_type>(type), arg_index, expressions);
    return expressions;
  }
}

void function_description::add_output_argument(std::string_view name, type_variant type,
                                               bool is_optional, std::vector<Expr> expressions) {
  signature_.add_argument(
      name, std::move(type),
      is_optional ? argument_direction::optional_output : argument_direction::output);

  output_key key{
      is_optional ? expression_usage::optional_output_argument : expression_usage::output_argument,
      name};
  output_expressions_.emplace_back(std::move(expressions), std::move(key));
}

void function_description::set_return_value(type_variant type, std::vector<Expr> expressions) {
  WF_ASSERT(!signature_.has_return_value(), "Return value on function `{}` already set.",
            signature_.name());
  signature_.set_return_value_type(std::move(type));
  output_expressions_.emplace_back(std::move(expressions),
                                   output_key{expression_usage::return_value, ""});
}

}  // namespace wf
