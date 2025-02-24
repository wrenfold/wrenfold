// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/type_registry.h"

#include <ranges>

#include "wf/code_generation/types.h"
#include "wf/expressions/variable.h"

namespace wf::detail {

scalar_expr create_function_input(const scalar_type& scalar, const std::string_view name) {
  return make_expr<function_argument_variable>(std::string{name}, scalar, 0);
}

matrix_expr create_function_input(const matrix_type& mat, const std::string_view name) {
  const std::size_t num_elements = mat.size();
  std::vector<scalar_expr> expressions{};
  expressions.reserve(num_elements);
  for (const std::size_t index : std::views::iota(num_elements)) {
    expressions.push_back(make_expr<function_argument_variable>(std::string{name}, mat, index));
  }
  return matrix_expr::create(mat.rows(), mat.cols(), std::move(expressions));
}

}  // namespace wf::detail
