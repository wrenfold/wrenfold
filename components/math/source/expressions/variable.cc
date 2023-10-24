// Copyright 2023 Gareth Cross
#include "expressions/variable.h"

#include <atomic>

namespace math {

std::size_t UniqueVariable::next_unique_variable_index() noexcept {
  // create thread-safe unique variable index:
  static std::atomic_size_t next_index{0};
  return next_index.fetch_add(1);
}

}  // namespace math
