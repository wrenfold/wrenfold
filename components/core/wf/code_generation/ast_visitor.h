// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/code_generation/ast.h"
#include "wf/utility/visit_switch.h"

namespace wf::ast {
namespace detail {

// Cast with no type-checking.
template <typename T>
const auto& cast_to_type(const ast_element& element) noexcept {
  const ast_element::model<T>* model =
      static_cast<const ast_element::model<T>*>(element.impl().get());
  return model->contents();
}

// Cast to const-reference of the type at index `I` in list `types`.
template <std::size_t I>
const auto& cast_to_index(const ast_element& element) noexcept {
  using types = ast_element::types;
  static_assert(I < type_list_size_v<types>, "Index exceeds number of types");
  return cast_to_type<const type_list_element_t<I, types>>(element);
}
}  // namespace detail

// Visit `ast_element` with `f`. The visitor is passed a const reference.
template <typename F>
auto visit(const ast_element& element, F&& f) {
  constexpr std::size_t num_types = type_list_size_v<ast_element::types>;
  return wf::detail::visit_switch<num_types>(element.index(), [&](const auto integral_constant) {
    constexpr std::size_t type_index = integral_constant();
    return f(detail::cast_to_index<type_index>(element));
  });
}

// If the underlying type is `T`, return a const pointer to it. Otherwise return nullptr.
template <typename T>
maybe_null<const T*> get_if(const ast_element& element) noexcept {
  using types = ast_element::types;
  static_assert(type_list_contains_v<T, types>);
  if (element.index() == type_list_index_v<T, types>) {
    return &detail::cast_to_type<T>(element);
  }
  return nullptr;
}

}  // namespace wf::ast
