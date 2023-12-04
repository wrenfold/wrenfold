// Copyright 2023 Gareth Cross
#include "wf/expressions/variable.h"

#include <atomic>

namespace math {

std::size_t unique_variable::next_unique_variable_index() {
  // create thread-safe unique variable index:
  static std::atomic_size_t next_index{1};
  const std::size_t next = next_index.fetch_add(1);
  WF_ASSERT_NOT_EQUAL(0, next);
  return next;
}

std::string variable::to_string() const {
  struct string_converter {
    std::string operator()(const named_variable& n) const { return n.name(); }
    std::string operator()(const function_argument_variable& f) const {
      return fmt::format("$arg({}, {})", f.arg_index(), f.element_index());
    }
    std::string operator()(const unique_variable& u) const {
      return fmt::format("$u_{}", u.index());
    }
  };
  return std::visit(string_converter{}, identifier_);
}

}  // namespace math
