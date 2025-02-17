// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/type_registry.h"
#include "wf/expressions/variable.h"
#include "wf/utility/index_range.h"

namespace wf::detail {

scalar_expr create_function_input(const scalar_type& scalar, const std::size_t arg_index) {
  return create_function_argument(arg_index, 0, scalar.numeric_type());
}

static std::vector<scalar_expr> create_function_args(const std::size_t arg_index,
                                                     const std::size_t size) {
  std::vector<scalar_expr> expressions{};
  expressions.reserve(size);
  for (const std::size_t index : make_range(size)) {
    expressions.push_back(
        create_function_argument(arg_index, index, numeric_primitive_type::floating_point));
  }
  return expressions;
}

matrix_expr create_function_input(const matrix_type& mat, const std::size_t arg_index) {
  return matrix_expr::create(mat.rows(), mat.cols(), create_function_args(arg_index, mat.size()));
}

}  // namespace wf::detail
