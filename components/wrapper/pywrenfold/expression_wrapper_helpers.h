#pragma once
#include <functional>

#include "wf/utility/hashing.h"
#include "wf/utility/traits.h"
#include "wf/utility/type_list.h"

#include <nanobind/nanobind.h>

namespace wf {
namespace nb = nanobind;

namespace detail {
template <typename Self, typename F, typename ArgList, std::size_t... Is>
auto make_member_fn_wrapper(F&& f, ArgList, std::index_sequence<Is...>) {
  return [f = std::forward<F>(f)](const Self& self, type_list_element_t<Is, ArgList>... args) {
    return std::invoke(f, self.contents, std::forward<decltype(args)>(args)...);
  };
}

template <typename Self, typename F, typename ArgList, std::size_t I, std::size_t... Is>
auto make_wrapper(F&& f, ArgList, std::index_sequence<I, Is...>) {
  return [f = std::forward<F>(f)](const Self& self, type_list_element_t<Is, ArgList>... args) {
    return std::invoke(f, self.contents, std::forward<decltype(args)>(args)...);
  };
}
}  // namespace detail

template <typename... Ts>
struct class_ {
  template <typename... CtorArgs>
  explicit class_(CtorArgs&&... ctor_args) : klass_(std::forward<CtorArgs>(ctor_args)...) {}

  template <typename F, typename... Args>
  class_& def(std::string_view name, F&& f, const Args&... args) {
    using traits = function_traits<F>;
    using arg_list = typename traits::args_list;
    if constexpr (std::is_member_function_pointer_v<F>) {
      klass_.def(name.data(),
                 detail::make_member_fn_wrapper(std::forward<F>(f), arg_list{},
                                                std::make_index_sequence<traits::arity>{}),
                 args...);
    } else {
      klass_.def(name.data(),
                 detail::make_wrapper(std::forward<F>(f), arg_list{},
                                      std::make_index_sequence<traits::arity>{}),
                 args...);
    }
    return *this;
  }

  nb::class_<Ts...> klass_;
};

template <typename T>
wf::class_<T> wrap_expression_class(nb::module_& m, const std::string_view name) {
  wf::class_<T> klass(m, name.data());
  if constexpr (is_invocable_v<hash_struct<T>, const T&>) {
    klass.def("__hash__", &hash<T>, "Compute hash.");
  }
  if constexpr (is_invocable_v<is_identical_struct<T>, const T&, const T&>) {
    klass.def("is_identical_to", &are_identical<T>, nb::arg("other"),
              "Check for strict equality (identical expression trees). This is not the same as "
              "mathematical equivalence.");
    klass.def("__eq__", &are_identical<T>, nb::is_operator(), nb::arg("other"),
              "Check for strict equality (identical expression trees). This is not the same as "
              "mathematical equivalence.");
  }
  return klass;
}

// AbstractExpr will be scalar_expr, boolean_expr, etc.
template <typename AbstractXpr>
class expr_wrapper {
 public:
  virtual ~expr_wrapper() = default;

  // Copy or move construct from the abstract expression.
  explicit expr_wrapper(AbstractXpr contents) noexcept : contents(std::move(contents)) {}

  AbstractXpr contents;
};

// A child instance of `expr_wrapper` that carries additional type information about the underlying
// concrete expression type: `ConcreteXpr`. This is so that different underlying expression types
// can be visible in python.
template <typename ConcreteXpr, typename AbstractXpr>
class expr_wrapper_typed : public expr_wrapper<AbstractXpr> {
  using expr_wrapper<AbstractXpr>::expr_wrapper;  //  Inherit constructor.
};

}  // namespace wf
