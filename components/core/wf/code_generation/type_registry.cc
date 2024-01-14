#include "wf/code_generation/type_registry.h"
#include "wf/expressions/variable.h"
#include "wf/index_range.h"

namespace wf::detail {

// TODO: Pass the numeric type information here.
Expr create_function_input(const scalar_type&, const std::size_t arg_index) {
  return variable::create_function_argument(arg_index, 0);
}

static std::vector<Expr> create_function_args(const std::size_t arg_index, const std::size_t size) {
  std::vector<Expr> expressions{};
  expressions.reserve(size);
  for (const std::size_t index : make_range(size)) {
    expressions.push_back(variable::create_function_argument(arg_index, index));
  }
  return expressions;
}

MatrixExpr create_function_input(const matrix_type& mat, const std::size_t arg_index) {
  return MatrixExpr::create(mat.rows(), mat.cols(), create_function_args(arg_index, mat.size()));
}

std::vector<Expr> create_function_input(const custom_type& custom, const std::size_t arg_index) {
  const compound_expr provenance = create_custom_type_argument(custom, arg_index);
  return create_expression_elements(provenance, custom.total_size());
}

// TODO: The numeric type information needs to be propagate to the IR here.
std::vector<Expr> extract_function_output(const scalar_type&, const Expr& value) {
  return std::vector(1, value);
}

std::vector<Expr> extract_function_output(const matrix_type&, const MatrixExpr& value) {
  return value.to_vector();
}

}  // namespace wf::detail
