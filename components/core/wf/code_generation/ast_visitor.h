#pragma once
#include "wf/code_generation/ast.h"

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

// If index `I` matches the internal index, call function `f` on it - otherwise recurse to the
// next index.
template <std::size_t I, typename F>
auto visit_recurse(const ast_element& element, F&& f) {
  using types = ast_element::types;
  if (element.index() == I) {
    return f(detail::cast_to_index<I>(element));
  } else if constexpr (I + 1 < type_list_size_v<types>) {
    return visit_recurse<I + 1>(element, std::forward<F>(f));
  } else {
    return f(detail::cast_to_index<type_list_size_v<types> - 1>(element));
  }
}

}  // namespace detail

// If index `I` matches the internal index, call function `f` on it - otherwise recurse to the
// next index.
template <typename F>
auto visit(const ast_element& element, F&& f) {
  return detail::visit_recurse<0>(element, std::forward<F>(f));
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
