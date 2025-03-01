// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/expressions/variable.h"

#include <atomic>

#include "wf/utility/assertions.h"
#include "wf/utility/overloaded_visit.h"

namespace wf {

std::size_t unique_variable::next_unique_variable_index() {
  // create thread-safe unique variable index:
  static std::atomic_size_t next_index{1};
  const std::size_t next = next_index.fetch_add(1);
  WF_ASSERT_NE(0, next);
  return next;
}

numeric_primitive_type function_argument_variable::primitive_type() const noexcept {
  return overloaded_visit(
      argument_type_, [](const scalar_type scalar) constexpr { return scalar.numeric_type(); },
      [](const matrix_type) constexpr { return numeric_primitive_type::floating_point; },
      [&](const custom_type& custom) -> numeric_primitive_type {
        return determine_member_type(custom, element_index_);
      });
}

}  // namespace wf
