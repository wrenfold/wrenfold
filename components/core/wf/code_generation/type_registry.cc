#include "wf/code_generation/type_registry.h"
#include "wf/expressions/variable.h"
#include "wf/index_range.h"

namespace wf::detail {

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
    for (const struct_field& field : c.fields()) {
      std::visit([&](const auto& child) { operator()(child, arg_index, output); }, field.type());
    }
  }
};

// TODO: Pass the numeric type information here.
Expr create_function_input(const scalar_type&, const std::size_t arg_index) {
  return variable::create_function_argument(arg_index, 0);
}

MatrixExpr create_function_input(const matrix_type& mat, const std::size_t arg_index) {
  std::vector<Expr> expressions{};
  create_custom_type_inputs{}(mat, arg_index, expressions);
  return MatrixExpr::create(mat.rows(), mat.cols(), std::move(expressions));
}

std::vector<Expr> create_function_input(const custom_type& custom, std::size_t arg_index) {
  std::vector<Expr> expressions{};
  create_custom_type_inputs{}(custom, arg_index, expressions);
  return expressions;
}

// TODO: The numeric type information needs to be propagate to the IR here.
std::vector<Expr> extract_function_output(const scalar_type&, const Expr& value) {
  return std::vector(1, value);
}

std::vector<Expr> extract_function_output(const matrix_type&, const MatrixExpr& value) {
  return value.to_vector();
}

}  // namespace wf::detail
