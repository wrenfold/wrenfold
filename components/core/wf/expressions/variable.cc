// Copyright 2023 Gareth Cross
#include "wf/expressions/variable.h"

#include <atomic>

namespace math {

std::size_t UniqueVariable::next_unique_variable_index() noexcept {
  // create thread-safe unique variable index:
  static std::atomic_size_t next_index{1};
  const std::size_t next = next_index.fetch_add(1);
  WF_ASSERT_NOT_EQUAL(0, next);
  return next;
}

}  // namespace math
