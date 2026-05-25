#pragma once
#include <nanobind/nanobind.h>

#include "wf/expression.h"
#include "wf/expressions/all_expressions.h"
#include "wf/utility/visit_switch.h"

#include "expression_wrapper_helpers.h"

namespace wf {
using scalar_expr_wrapper = expr_wrapper<scalar_expr>;
}  // namespace wf

namespace nanobind {

template <>
struct type_caster<wf::scalar_expr> {
  static constexpr bool something2 = true;

  using Value = wf::scalar_expr;
  static constexpr auto Name = const_name("scalar_expr");

  template <typename T_>
  using Cast = movable_cast_t<T_>;

  template <typename T_>
  constexpr bool can_cast() const noexcept {
    return true;
  }

  template <typename T_, enable_if_t<std::is_same_v<std::remove_cv_t<T_>, wf::scalar_expr>> = 0>
  static handle from_cpp(T_* p, rv_policy policy, cleanup_list* list) {
    if (!p) return none().release();
    return from_cpp(*p, policy, list);
  }

  operator wf::scalar_expr*() { return value ? &*value : nullptr; }

  operator wf::scalar_expr&() { return (wf::scalar_expr&)*value; }

  operator wf::scalar_expr&&() { return *std::move(value); }

  // Stored in std::optional because this cannot be default initialized.
  std::optional<wf::scalar_expr> value;

  // Convert scalar_expr_wrapper (and its derived types) to plain scalar_expr.
  bool from_python(handle src, uint8_t flags, cleanup_list* cleanup) noexcept {
    fmt::print("attempting cast from python!\n");
    std::fflush(nullptr);
    using Caster = make_caster<wf::scalar_expr_wrapper>;
    Caster caster;
    if (!caster.from_python(src, flags_for_local_caster<wf::scalar_expr_wrapper>(flags), cleanup)) {
      fmt::print("Cast failed!\n");
      std::fflush(nullptr);
      return false;  //  need to check if this is is working!!!!
    }
    value = std::move(caster.operator cast_t<wf::scalar_expr_wrapper>().contents);
    return true;
  }

  template <typename T_>
  static handle from_cpp(T_&& value, rv_policy policy, cleanup_list* cleanup) noexcept {
    static_assert(std::is_same_v<std::decay_t<T_>, wf::scalar_expr>);

    fmt::print("Attempting cast!\n");
    std::fflush(nullptr);

    using scalar_expr_types = typename wf::scalar_expr::types;
    return wf::detail::visit_switch<wf::type_list_size_v<scalar_expr_types>>(
        value.type_index(), [&]<std::size_t I>(std::integral_constant<std::size_t, I>) -> handle {
          using ith_type = wf::type_list_element_t<I, scalar_expr_types>;
          using wrapped_expr_type = wf::expr_wrapper_typed<ith_type, wf::scalar_expr>;
          using Caster = make_caster<wrapped_expr_type>;
          fmt::print("from_cpp on: {}\n", ith_type::name_str);
          handle result =
              Caster::from_cpp(wrapped_expr_type{std::forward<T_>(value)}, policy, cleanup);
          fmt::print("Done cast: {}\n", static_cast<bool>(result));
          return result;
        });
  }

  int something{3};
};

}  // namespace nanobind
