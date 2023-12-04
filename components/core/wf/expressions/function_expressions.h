// Copyright 2023 Gareth Cross
#pragma once
#include "wf/absl_imports.h"
#include "wf/assertions.h"
#include "wf/constants.h"
#include "wf/functions.h"
#include "wf/hashing.h"

namespace math {

// Store a built-in function call. Valid functions are enumerated in `built_in_function`.
class function {
 public:
  static constexpr std::string_view name_str = "Function";
  static constexpr bool is_leaf_node = false;
  using container_type = absl::InlinedVector<Expr, 2>;

  template <typename... Args>
  function(built_in_function func, Args&&... args)
      : func_(func), args_{std::forward<Args>(args)...} {}

  // Create a function. Examines `name`, and then invokes the correct function method.
  static Expr create(built_in_function name, container_type&& container);

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

  // Function type and argument must match.
  bool is_identical_to(const function& other) const {
    return func_ == other.func_ && args_.size() == other.args_.size() &&
           std::equal(begin(), end(), other.begin(), is_identical_struct<Expr>{});
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void for_each(Operation&& operation) const {
    std::for_each(begin(), end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    container_type transformed{};
    transformed.reserve(args_.size());
    std::transform(begin(), end(), std::back_inserter(transformed),
                   std::forward<Operation>(operation));
    return function::create(func_, std::move(transformed));
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

// Call the appropriate creation method for the specified enum value.
// We need this logic because each type of function has simplifications it applies.
inline Expr function::create(built_in_function name, function::container_type&& container) {
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
  WF_ASSERT(false, "Invalid function name: {}", string_from_built_in_function(name));
  return constants::zero;  //  Unreachable.
}

}  // namespace math
