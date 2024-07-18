// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/compound_expression.h"
#include "wf/external_function.h"
#include "wf/utility/algorithms.h"

namespace wf {

// An invocation of a custom user-defined external function.
template <typename Derived>
class external_function_invocation {
 public:
  static constexpr std::string_view name_str = "ExternalFunctionInvocation";
  static constexpr bool is_leaf_node = false;
  using container_type = std::vector<any_expression>;

  external_function_invocation(external_function func, container_type args)
      : function_(std::move(func)), args_(std::move(args)) {
    WF_ASSERT_EQ(
        function_.num_arguments(), args_.size(),
        "Mismatch in # of args between function spec and provided argument list. Function: {}",
        function_.name());
  }

  // The custom function being called.
  const external_function& function() const noexcept { return function_; }

  // The arguments to the function.
  const container_type& args() const noexcept { return args_; }

  // Number of arguments.
  std::size_t size() const noexcept { return args_.size(); }

  // Iterators over argument.
  auto begin() const noexcept { return args_.begin(); }
  auto end() const noexcept { return args_.end(); }

  template <typename F>
  auto map_children(F&& f) const {
    using ExpressionType = typename Derived::ExpressionType;
    container_type args_out = transform_map<container_type>(
        args_, [&f](const any_expression& arg) -> any_expression { return f(arg); });
    return ExpressionType(std::in_place_type_t<Derived>{}, function_, std::move(args_out));
  }

 private:
  external_function function_;
  container_type args_;
};

namespace detail {
// ReSharper disable CppFunctionIsNotImplemented
constexpr auto inherits_external_function_invocation_(...) -> std::false_type;
template <typename Derived>
constexpr auto inherits_external_function_invocation_(const external_function_invocation<Derived>&)
    -> std::true_type;
// ReSharper restore CppFunctionIsNotImplemented
}  // namespace detail

// Evaluates to std::true_type if `T` inherits from expression_base, otherwise std::false_type.
template <typename T>
using inherits_external_function_invocation =
    decltype(detail::inherits_external_function_invocation_(std::declval<const T>()));
template <typename T>
constexpr bool inherits_external_function_invocation_v =
    inherits_external_function_invocation<T>::value;

template <typename T>
struct hash_struct<T, std::enable_if_t<inherits_external_function_invocation_v<T>>> {
  std::size_t operator()(const T& func) const noexcept {
    return hash_all(func.function().hash(), func.args());
  }
};

template <typename T>
struct is_identical_struct<T, std::enable_if_t<inherits_external_function_invocation_v<T>>> {
  bool operator()(const T& a, const T& b) const {
    return are_identical(a.function(), b.function()) && are_identical(a.args(), b.args());
  }
};

// To order function invocations, we need to be able to order types. This is tricky to do completely
// unambiguously because we can't meaningfully order python types. Two types could easily have the
// same name and members. We can do "best effort" by sorting by function name, and then by the
// argument content.
template <typename T>
struct order_struct<T, std::enable_if_t<inherits_external_function_invocation_v<T>>> {
  relative_order operator()(const T& a, const T& b) const {
    return order_by(a.function().name(), b.function().name()).and_then_by(a.args(), b.args());
  }
};

class compound_valued_external_function_invocation
    : public external_function_invocation<compound_valued_external_function_invocation> {
 public:
  using ExpressionType = compound_expr;
  using external_function_invocation::external_function_invocation;
};

}  // namespace wf
