// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "args_visitor.h"

#include "wf/expression_visitor.h"

namespace py = nanobind;

namespace wf {

args_visitor::tuple args_visitor::operator()(const scalar_expr& input) const {
  return visit(input, *this);
}
args_visitor::tuple args_visitor::operator()(const boolean_expr& input) const {
  return visit(input, *this);
}

template <typename T, typename>
args_visitor::tuple args_visitor::operator()(const T& obj) const {
  if constexpr (T::is_leaf_node) {
    return py::make_tuple();
  } else {
    const auto& children = obj.children();
    using children_type = std::decay_t<decltype(children)>;
    if constexpr (is_tuple_v<children_type>) {
      return std::apply([](const auto&... child) { return py::make_tuple(child...); }, children);
    } else {
      py::list result{};
      for (const auto& element : children) {
        result.append(element);
      }
      return py::tuple(result);
    }
  }
}

}  // namespace wf
