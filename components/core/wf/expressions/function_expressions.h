// Copyright 2023 Gareth Cross
#pragma once
#include "wf/absl_imports.h"
#include "wf/assertions.h"
#include "wf/constants.h"
#include "wf/functions.h"
#include "wf/hashing.h"

namespace math {

// Store a built-in function. Valid functions are enumerated in `BuiltInFunctionName`.
class Function {
 public:
  static constexpr std::string_view NameStr = "UnaryFunction";
  static constexpr bool IsLeafNode = false;
  using ContainerType = absl::InlinedVector<Expr, 2>;

  template <typename... Args>
  Function(BuiltInFunction func, Args&&... args)
      : func_(func), args_{std::forward<Args>(args)...} {}

  // Create a function. Examines `name`, and then invokes the correct function method.
  static Expr create(BuiltInFunction name, ContainerType&& container);

  // Get the function name.
  constexpr BuiltInFunction enum_value() const noexcept { return func_; }

  // Get name as a string.
  constexpr std::string_view function_name() const noexcept {
    return string_from_built_in_function(func_);
  }

  // Get the function argument.
  constexpr const auto& args() const noexcept { return args_; }

  // Number of arguments.
  std::size_t arity() const noexcept { return args_.size(); }

  // Iterator over argument.
  auto begin() const noexcept { return args_.begin(); }
  auto end() const noexcept { return args_.end(); }

  // Function type and argument must match.
  bool is_identical_to(const Function& other) const {
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
    ContainerType transformed{};
    transformed.reserve(args_.size());
    std::transform(begin(), end(), std::back_inserter(transformed),
                   std::forward<Operation>(operation));
    return Function::create(func_, std::move(transformed));
  }

 protected:
  BuiltInFunction func_;
  ContainerType args_;
};

template <>
struct hash_struct<Function> {
  std::size_t operator()(const Function& func) const {
    return hash_all(static_cast<std::size_t>(func.enum_value()), func.begin(), func.end());
  }
};

// Call the appropriate creation method for the specified enum value.
// We need this logic because each type of function has simplifications it applies.
inline Expr Function::create(BuiltInFunction name, Function::ContainerType&& container) {
  switch (name) {
    case BuiltInFunction::Cos:
      return cos(container.front());
    case BuiltInFunction::Sin:
      return sin(container.front());
    case BuiltInFunction::Tan:
      return tan(container.front());
    case BuiltInFunction::ArcCos:
      return acos(container.front());
    case BuiltInFunction::ArcSin:
      return asin(container.front());
    case BuiltInFunction::ArcTan:
      return atan(container.front());
    case BuiltInFunction::Log:
      return log(container.front());
    case BuiltInFunction::Abs:
      return abs(container.front());
    case BuiltInFunction::Signum:
      return signum(container.front());
    case BuiltInFunction::Arctan2:
      return atan2(container[0], container[1]);
  }
  WF_ASSERT(false, "Invalid function name: {}", string_from_built_in_function(name));
  return constants::zero;  //  Unreachable.
}

}  // namespace math
