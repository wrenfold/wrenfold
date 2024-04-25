// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "args_visitor.h"

#include "wf/expression_visitor.h"

namespace py = pybind11;
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
    if constexpr (std::is_same_v<T, conditional>) {
      return py::make_tuple(obj.condition(), obj.if_branch(), obj.else_branch());
    } else {
      py::tuple result{std::distance(obj.begin(), obj.end())};
      std::size_t index = 0;
      for (const auto& element : obj) {
        result[index++] = element;
      }
      return result;
    }
  }
}

}  // namespace wf
