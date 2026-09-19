// wrenfold symbolic code generator.
// Copyright (c) 2026 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once

#include <cstdint>

#include <nanobind/nanobind.h>

#include "wf/expression.h"

namespace wf {

// Annotation-only marker used with nanobind::typed when a Python value may have any type.
struct typing_any {};

}  // namespace wf

namespace nanobind::detail {

// Keep this header included by every binding translation unit so type_caster resolves to the same
// specialization throughout the extension (see nanobind's ODR guidance for custom casters).

// scalar_expr can be constructed implicitly from Python integers and floats. Describe that
// conversion here so it is reflected automatically in function signatures, including when the
// type is nested inside a standard-library caster. Return values are always scalar expressions.
template <>
struct type_caster<wf::scalar_expr> : type_caster_base<wf::scalar_expr> {
  static constexpr auto Name =
      type_caster_base<wf::scalar_expr>::Name + io_name(" | int | float", "");
};

// Constructor detection requires the hidden `self` parameter to have an unmodified class-type
// descriptor. This is the same adapter nanobind provides for bound classes, with only its name
// restored to the underlying scalar_expr name.
template <>
struct type_caster<pointer_and_handle<wf::scalar_expr>> {
  using Caster = type_caster<wf::scalar_expr>;
  NB_TYPE_CASTER(pointer_and_handle<wf::scalar_expr>, type_caster_base<wf::scalar_expr>::Name)

  bool from_python(handle src, const std::uint8_t flags, cleanup_list* cleanup) noexcept {
    Caster caster;
    if (!caster.from_python(src, flags_for_local_caster<wf::scalar_expr*>(flags), cleanup) ||
        !caster.template can_cast<wf::scalar_expr*>()) {
      return false;
    }
    value.h = src;
    value.p = caster.operator wf::scalar_expr*();
    return true;
  }
};

template <>
struct type_caster<wf::typing_any> {
  NB_TYPE_CASTER(wf::typing_any, const_name("typing.Any"))
};

}  // namespace nanobind::detail
