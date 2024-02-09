// Copyright 2023 Gareth Cross
#pragma once
#include "wf/absl_imports.h"
#include "wf/algorithm_utils.h"
#include "wf/assertions.h"
#include "wf/constants.h"
#include "wf/external_function.h"
#include "wf/functions.h"
#include "wf/hashing.h"

namespace wf {

// Store a built-in function call. Valid functions are enumerated in `built_in_function`.
class function {
 public:
  static constexpr std::string_view name_str = "Function";
  static constexpr bool is_leaf_node = false;
  using container_type = absl::InlinedVector<scalar_expr, 2>;

  template <typename... Args>
  explicit function(const built_in_function func, Args&&... args)
      : func_(func), args_{std::forward<Args>(args)...} {}

  // Create a function. Examines `name`, and then invokes the correct function method.
  static scalar_expr create(built_in_function name, container_type&& container);

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
    return function::create(func_, transform_map<container_type>(args_, std::forward<F>(f)));
  }

 protected:
  built_in_function func_;
  container_type args_;
};

template <>
struct hash_struct<function> {
  std::size_t operator()(const function& func) const {
    return hash_all(static_cast<std::size_t>(func.enum_value()), func.begin(), func.end());
  }
};

template <>
struct is_identical_struct<function> {
  bool operator()(const function& a, const function& b) const {
    return a.function_name() == b.function_name() && a.size() == b.size() &&
           std::equal(a.begin(), a.end(), b.begin(), is_identical_struct<scalar_expr>{});
  }
};

// Call the appropriate creation method for the specified enum value.
// We need this logic to support `map_children`.
inline scalar_expr function::create(const built_in_function name,
                                    function::container_type&& container) {
  switch (name) {
    case built_in_function::cos:
      return cos(container.front());
    case built_in_function::sin:
      return sin(container.front());
    case built_in_function::tan:
      return tan(container.front());
    case built_in_function::arccos:
      return acos(container.front());
    case built_in_function::arcsin:
      return asin(container.front());
    case built_in_function::arctan:
      return atan(container.front());
    case built_in_function::ln:
      return log(container.front());
    case built_in_function::abs:
      return abs(container.front());
    case built_in_function::signum:
      return signum(container.front());
    case built_in_function::arctan2:
      return atan2(container[0], container[1]);
  }
  WF_ASSERT_ALWAYS("Invalid function name: {}", string_from_built_in_function(name));
}

}  // namespace wf
