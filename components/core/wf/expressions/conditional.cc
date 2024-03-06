// Copyright 2023 Gareth Cross
#include "wf/expressions/conditional.h"

#include "wf/constants.h"

namespace wf {

scalar_expr conditional::create(boolean_expr condition, scalar_expr if_branch,
                                scalar_expr else_branch) {
  if (condition.is_identical_to(constants::boolean_true)) {
    return if_branch;
  } else if (condition.is_identical_to(constants::boolean_false)) {
    return else_branch;
  } else if (if_branch.is_identical_to(else_branch)) {
    // If and else are the same anyway, so ignore the condition.
    return if_branch;
  }
  return scalar_expr(std::in_place_type_t<conditional>{}, std::move(condition),
                     std::move(if_branch), std::move(else_branch));
}

}  // namespace wf
