// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/external_function.h"
#include "wf/utility/algorithms.h"
#include "wf/utility/hashing.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/inlined_vector.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

// Store a built-in function call. Valid functions are enumerated in `built_in_function`.
class built_in_function_invocation {
 public:
  static constexpr std::string_view name_str = "BuiltInFunctionInvocation";
  static constexpr bool is_leaf_node = false;
  using container_type = absl::InlinedVector<scalar_expr, 2>;

  template <typename... Args>
  explicit built_in_function_invocation(const built_in_function func, Args&&... args)
      : func_(func), args_{std::forward<Args>(args)...} {}

  // Create a function. Examines `name`, and then invokes the correct function method.
  static scalar_expr create(built_in_function name, absl::Span<const scalar_expr> args);

  // Get the function name.
  constexpr built_in_function enum_value() const noexcept { return func_; }

  // Get name as a string.
  constexpr std::string_view function_name() const noexcept {
    return string_from_built_in_function(func_);
  }

  // Get the function argument.
  constexpr const auto& args() const noexcept { return args_; }

  // Number of arguments.
  std::size_t size() const noexcept { return args_.size(); }

  // Iterator over argument.
  auto begin() const noexcept { return args_.begin(); }
  auto end() const noexcept { return args_.end(); }

  // Implement ExpressionImpl::Map
  template <typename F>
  scalar_expr map_children(F&& f) const {
    return built_in_function_invocation::create(
        func_, transform_map<container_type>(args_, std::forward<F>(f)));
  }

  constexpr const auto& children() const noexcept { return args_; }

 protected:
  built_in_function func_;
  container_type args_;
};

template <>
struct hash_struct<built_in_function_invocation> {
  std::size_t operator()(const built_in_function_invocation& func) const {
    return hash_all(static_cast<std::size_t>(func.enum_value()), func.begin(), func.end());
  }
};

template <>
struct is_identical_struct<built_in_function_invocation> {
  bool operator()(const built_in_function_invocation& a,
                  const built_in_function_invocation& b) const {
    return a.function_name() == b.function_name() && a.size() == b.size() &&
           std::equal(a.begin(), a.end(), b.begin(), is_identical_struct<scalar_expr>{});
  }
};

template <>
struct order_struct<built_in_function_invocation> {
  relative_order operator()(const built_in_function_invocation& a,
                            const built_in_function_invocation& b) const {
    // First compare by name:
    if (const int name_comp = a.function_name().compare(b.function_name()); name_comp > 0) {
      return relative_order::greater_than;
    } else if (name_comp < 0) {
      return relative_order::less_than;
    }
    return lexicographical_order(a, b, order_struct<scalar_expr>{});
  }
};

// A user declared symbolic function, `f(x, y, ...)`.
// Unlike an external_function, we don't know anything about number of arguments or their types.
// TODO: Supporting adding other properties, like the number set.
class symbolic_function {
 public:
  explicit symbolic_function(std::string name);

  // Name of the function.
  const std::string& name() const noexcept { return impl_->name; }

 private:
  struct impl {
    std::string name;
  };
  std::shared_ptr<const impl> impl_;
};

template <>
struct hash_struct<symbolic_function> {
  std::size_t operator()(const symbolic_function& func) const noexcept {
    return hash_string_fnv(func.name());
  }
};

template <>
struct is_identical_struct<symbolic_function> {
  bool operator()(const symbolic_function& a, const symbolic_function& b) const noexcept {
    return a.name() == b.name();
  }
};

template <>
struct order_struct<symbolic_function> {
  relative_order operator()(const symbolic_function& a, const symbolic_function& b) const noexcept {
    return order_struct<std::string>{}(a.name(), b.name());
  }
};

// An invocation of a symbolic function.
class symbolic_function_invocation {
 public:
  static constexpr std::string_view name_str = "SymbolicFunctionInvocation";
  static constexpr bool is_leaf_node = false;
  using container_type = absl::InlinedVector<scalar_expr, 4>;

  template <typename... Args>
  explicit symbolic_function_invocation(symbolic_function function, Args&&... args)
      : function_(std::move(function)), args_{std::forward<Args>(args)...} {}

  constexpr const symbolic_function& function() const noexcept { return function_; }

  // Get the function arguments.
  constexpr const auto& args() const noexcept { return args_; }

  // Number of arguments.
  std::size_t size() const noexcept { return args_.size(); }

  // Iterator over arguments.
  auto begin() const noexcept { return args_.begin(); }
  auto end() const noexcept { return args_.end(); }

  template <typename F>
  scalar_expr map_children(F&& f) const {
    return scalar_expr{std::in_place_type_t<symbolic_function_invocation>{}, function_,
                       transform_map<container_type>(args_, std::forward<F>(f))};
  }

 private:
  symbolic_function function_;
  container_type args_;
};

template <>
struct hash_struct<symbolic_function_invocation> {
  std::size_t operator()(const symbolic_function_invocation& func) const noexcept {
    return hash_all(hash(func.function()), func.args());
  }
};

template <>
struct is_identical_struct<symbolic_function_invocation> {
  bool operator()(const symbolic_function_invocation& a,
                  const symbolic_function_invocation& b) const {
    return are_identical(a.function(), b.function()) && a.size() == b.size() &&
           std::equal(a.begin(), a.end(), b.begin(), is_identical_struct<scalar_expr>{});
  }
};

template <>
struct order_struct<symbolic_function_invocation> {
  relative_order operator()(const symbolic_function_invocation& a,
                            const symbolic_function_invocation& b) const {
    if (const relative_order order = determine_order(a.function(), b.function());
        order != relative_order::equal) {
      return order;
    }
    return lexicographical_order(a, b, order_struct<scalar_expr>{});
  }
};

}  // namespace wf
