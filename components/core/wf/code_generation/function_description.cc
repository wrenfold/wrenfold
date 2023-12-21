// Copyright 2023 Gareth Cross
#include "wf/code_generation/function_description.h"

#include "wf/expressions/variable.h"
#include "wf/index_range.h"

namespace wf {

struct create_custom_type_inputs {
  static void append(const std::size_t arg_index, const std::size_t num_to_append,
                     std::vector<Expr>& output) {
    const std::size_t start_index = output.size();
    output.reserve(start_index + num_to_append);
    for (const std::size_t element : make_range(start_index, start_index + num_to_append)) {
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
    const std::string_view name, type_variant type) {
  const argument& arg = add_argument(name, std::move(type), argument_direction::input);

  using return_type = std::variant<Expr, MatrixExpr, std::vector<Expr>>;
  return overloaded_visit(
      arg.type(),
      [&](const scalar_type) -> return_type {
        return variable::create_function_argument(arg.index(), 0);
      },
      [&](const matrix_type m) -> return_type {
        std::vector<Expr> expressions{};
        create_custom_type_inputs{}(m, arg.index(), expressions);
        return MatrixExpr::create(m.rows(), m.cols(), std::move(expressions));
      },
      [&](const custom_type& c) -> return_type {
        // Traverse the type and instantiate expressions:
        std::vector<Expr> expressions{};
        create_custom_type_inputs{}(c, arg.index(), expressions);
        return expressions;
      });
}

void function_description::add_output_argument(std::string_view name, type_variant type,
                                               bool is_optional, std::vector<Expr> expressions) {
  add_argument(name, std::move(type),
               is_optional ? argument_direction::optional_output : argument_direction::output);

  output_key key{
      is_optional ? expression_usage::optional_output_argument : expression_usage::output_argument,
      name};
  output_expressions_.emplace_back(std::move(expressions), std::move(key));
}

void function_description::set_return_value(type_variant type, std::vector<Expr> expressions) {
  WF_ASSERT(!return_value_type_.has_value(), "Return value on function `{}` already set.", name_);
  return_value_type_ = std::move(type);
  output_expressions_.emplace_back(std::move(expressions),
                                   output_key{expression_usage::return_value, ""});
}

const argument& function_description::add_argument(std::string_view name, type_variant type,
                                                   argument_direction direction) {
  WF_ASSERT(!std::any_of(arguments_.begin(), arguments_.end(),
                         [&name](const argument& arg) { return arg.name() == name; }),
            "Argument with name `{}` already exists.", name);
  arguments_.emplace_back(name, std::move(type), direction, arguments_.size());
  return arguments_.back();
}

}  // namespace wf
