#pragma once
#include <functional>

#include "wf/expression_variant.h"
#include "wf/utility/hashing.h"
#include "wf/utility/traits.h"
#include "wf/utility/type_list.h"

#include <nanobind/nanobind.h>
#include <nanobind/operators.h>

namespace wf {
namespace nb = nanobind;

template <typename T>
class expr_wrapper;

// ---

// Copy const and ref qualifiers from `Src` to `Dest`.
template <typename Src, typename Dest>
struct copy_const_ref_qualifiers {
  static_assert(!std::is_rvalue_reference_v<Src>);
  static constexpr bool is_ref_src = std::is_reference_v<Src>;
  static constexpr bool is_const_src = std::is_const_v<std::remove_reference_t<Src>>;

  using maybe_const_type = std::conditional_t<is_const_src, std::add_const_t<Dest>, Dest>;
  using type = std::conditional_t<is_ref_src, std::add_lvalue_reference_t<maybe_const_type>,
                                  maybe_const_type>;
};

template <typename Src, typename Dest>
using copy_const_ref_qualifiers_t = typename copy_const_ref_qualifiers<Src, Dest>::type;

static_assert(std::is_same_v<copy_const_ref_qualifiers_t<int, double>, double>);
static_assert(std::is_same_v<copy_const_ref_qualifiers_t<const int, double>, const double>);
static_assert(std::is_same_v<copy_const_ref_qualifiers_t<int&, double>, double&>);
static_assert(std::is_same_v<copy_const_ref_qualifiers_t<const int&, double>, const double&>);

//---

template <typename T, typename = void>
struct convert_expression_arg_type {
  using type = T;
};

template <typename T>
struct convert_expression_arg_type<T, enable_if_inherits_expression_base_t<std::decay_t<T>>> {
  using type = copy_const_ref_qualifiers_t<T, expr_wrapper<std::decay_t<T>>>;
};

template <typename T>
using convert_expression_arg_type_t = typename convert_expression_arg_type<T>::type;
namespace detail {

template <typename... Args>
std::tuple<convert_expression_arg_type_t<Args>...> convert_expression_args_to_wrapped_type(
    Args&&... args) {
  const auto convert = []<typename T>(T&& arg) {
    if constexpr (inherits_expression_base_v<std::decay_t<T>>) {
      using U = std::remove_cv_t<std::remove_reference_t<T>>;
      return expr_wrapper<U>(std::forward<T>(arg));
    } else {
      return std::forward<T>(arg);
    }
  };
  return std::forward_as_tuple(convert(std::forward<Args>(args))...);
}

template <typename Self, typename F>
auto make_expr_fn_wrapper(F&& f) {
  using traits = function_traits<F>;
  using arg_list = typename traits::args_list;

  const auto make = [&]<typename... Ts, std::size_t... Is>(type_list<Ts...> arg_list,
                                                           std::index_sequence<Is...>) constexpr {
    return [&f](const Self& self, type_list_element_t<Is, decltype(arg_list)>... args) {
      WF_ASSERT(self.contents.impl().has_value());
      return std::invoke(std::forward<F>(f), self.contents, std::forward<decltype(args)>(args)...);
    };
  };

  if constexpr (std::is_member_function_pointer_v<F>) {
    return make(arg_list{}, std::make_index_sequence<type_list_size_v<arg_list>>{});
  } else {
    return make(type_list_pop_front_t<arg_list>(),
                std::make_index_sequence<type_list_size_v<arg_list> - 1>{});
  }
}

}  // namespace detail

template <typename... Ts>
struct class_ {
  using wrapped_type = type_list_element_t<0, type_list<Ts...>>;

  template <typename... CtorArgs>
  explicit class_(CtorArgs&&... ctor_args) : klass_(std::forward<CtorArgs>(ctor_args)...) {}

  template <typename F, typename... Args>
  class_& def(std::string_view name, F&& f, const Args&... args) {
    klass_.def(name.data(), detail::make_expr_fn_wrapper<wrapped_type>(std::forward<F>(f)),
               args...);

    // using traits = function_traits<F>;
    // using arg_list = typename traits::args_list;
    // if constexpr (std::is_member_function_pointer_v<F>) {
    //   klass_.def(name.data(),
    //              detail::make_expr_member_fn_wrapper<wrapped_type>(
    //                  std::forward<F>(f), arg_list{},
    //                  std::make_index_sequence<traits::arity>{}),
    //              args...);
    // } else {
    //   klass_.def(name.data(),
    //              detail::make_expr_fn_wrapper<wrapped_type>(
    //                  std::forward<F>(f), arg_list{},
    //                  std::make_index_sequence<traits::arity>{}),
    //              args...);
    // }
    return *this;
  }

  template <typename Getter, typename... Args>
  class_& def_prop_ro(std::string_view name, Getter&& f, const Args&... args) {
    klass_.def_prop_ro(
        name.data(), detail::make_expr_fn_wrapper<wrapped_type>(std::forward<Getter>(f)), args...);

    return *this;
  }

  template <nb::detail::op_id id, nb::detail::op_type ot, typename L, typename R, typename... Extra>
  class_& def(const nb::detail::op_<id, ot, L, R>& op, const Extra&... extra) {
    // Copied from nb::op_::execute
    using Type = wrapped_type;
    using Lt = std::conditional_t<std::is_same_v<L, decltype(nb::self)>, Type, L>;
    using Rt = std::conditional_t<std::is_same_v<R, decltype(nb::self)>, Type, R>;
    using Op = nb::detail::op_impl<id, ot, Type, Lt, Rt>;
    klass_.def(Op::name(), &Op::execute, nb::is_operator(), Op::default_policy, extra...);
    return *this;
  }

  decltype(auto) doc() const { return klass_.doc(); }

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

  expr_wrapper(const expr_wrapper&) = default;
  expr_wrapper(expr_wrapper&&) = default;
  expr_wrapper& operator=(const expr_wrapper&) = default;
  expr_wrapper& operator=(expr_wrapper&&) = default;

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

template <typename Concrete, typename Abstract>
wf::class_<expr_wrapper_typed<Concrete, Abstract>, expr_wrapper<Abstract>>
wrap_typed_expression_class(nb::module_& m, const std::string_view name) {
  wf::class_<expr_wrapper_typed<Concrete, Abstract>, expr_wrapper<Abstract>> klass(m, name.data());
  static_assert(std::is_polymorphic_v<expr_wrapper_typed<Concrete, Abstract>>);
  return klass;
}

}  // namespace wf
