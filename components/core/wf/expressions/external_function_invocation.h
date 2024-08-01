// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/compound_expression.h"
#include "wf/external_function.h"
#include "wf/utility/algorithms.h"

namespace wf {

// An invocation of a custom user-defined external function.
class external_function_invocation {
 public:
  static constexpr std::string_view name_str = "ExternalFunctionInvocation";
  static constexpr bool is_leaf_node = false;
  using container_type = std::vector<any_expression>;

  external_function_invocation(external_function func, container_type args);

  // The custom function being called.
  const external_function& function() const noexcept { return function_; }

  // Number of arguments.
  std::size_t size() const noexcept { return args_.size(); }

  // Iterators over argument.
  auto begin() const noexcept { return args_.begin(); }
  auto end() const noexcept { return args_.end(); }

  template <typename F>
  compound_expr map_children(F&& f) const {
    container_type args_out = transform_map<container_type>(
        args_, [&f](const any_expression& arg) -> any_expression { return f(arg); });
    return compound_expr(std::in_place_type_t<external_function_invocation>{}, function_,
                         std::move(args_out));
  }

  // The arguments to the function.
  constexpr const container_type& children() const noexcept { return args_; }

 private:
  external_function function_;
  container_type args_;
};

template <>
struct hash_struct<external_function_invocation> {
  std::size_t operator()(const external_function_invocation& func) const noexcept {
    return hash_all(func.function().hash(), func.children());
  }
};

template <>
struct is_identical_struct<external_function_invocation> {
  bool operator()(const external_function_invocation& a,
                  const external_function_invocation& b) const;
};

// To order function invocations, we need to be able to order types. This is tricky to do completely
// unambiguously because we can't meaningfully order python types. Two types could easily have the
// same name and members. We can do "best effort" by sorting by function name, and then by the
// argument content.
template <>
struct order_struct<external_function_invocation> {
  relative_order operator()(const external_function_invocation& a,
                            const external_function_invocation& b) const {
    return order_by(a.function().name(), b.function().name())
        .and_then_by(a.children(), b.children());
  }
};

}  // namespace wf
