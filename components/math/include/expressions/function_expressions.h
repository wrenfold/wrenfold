// Copyright 2023 Gareth Cross
#pragma once
#include "absl_imports.h"
#include "assertions.h"
#include "constants.h"
#include "functions.h"
#include "hashing.h"

namespace math {

// Store a built-in function. Valid functions are enumerated in `BuiltInFunctionName`.
class Function {
 public:
  static constexpr std::string_view NameStr = "UnaryFunction";
  static constexpr bool IsLeafNode = false;
  using ContainerType = absl::InlinedVector<Expr, 2>;

  template <typename... Args>
  Function(BuiltInFunctionName func, Args&&... args)
      : func_(func), args_{std::forward<Args>(args)...} {}

  // Create a function. Examines `name`, and then invokes the correct function method.
  static Expr create(BuiltInFunctionName name, ContainerType&& container);

  // Get the function name.
  constexpr BuiltInFunctionName enum_value() const noexcept { return func_; }

  // Get name as a string.
  constexpr std::string_view function_name() const noexcept { return ToString(func_); }

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
           std::equal(begin(), end(), other.begin(), IsIdenticalOperator<Expr>{});
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void iterate(Operation&& operation) const {
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
  BuiltInFunctionName func_;
  ContainerType args_;
};

template <>
struct Hash<Function> {
  std::size_t operator()(const Function& func) const {
    return HashAll(static_cast<std::size_t>(func.enum_value()), func.begin(), func.end());
  }
};

// Call the appropriate creation method for the specified enum value.
// We need this logic because each type of function has simplifications it applies.
inline Expr Function::create(BuiltInFunctionName name, Function::ContainerType&& container) {
  switch (name) {
    case BuiltInFunctionName::Cos:
      return cos(container.front());
    case BuiltInFunctionName::Sin:
      return sin(container.front());
    case BuiltInFunctionName::Tan:
      return tan(container.front());
    case BuiltInFunctionName::ArcCos:
      return acos(container.front());
    case BuiltInFunctionName::ArcSin:
      return asin(container.front());
    case BuiltInFunctionName::ArcTan:
      return atan(container.front());
    case BuiltInFunctionName::Log:
      return log(container.front());
    case BuiltInFunctionName::Sqrt:
      return sqrt(container.front());
    case BuiltInFunctionName::Abs:
      return abs(container.front());
    case BuiltInFunctionName::Signum:
      return signum(container.front());
    case BuiltInFunctionName::Arctan2:
      return atan2(container[0], container[1]);
    case BuiltInFunctionName::Pow:
      return pow(container[0], container[1]);
    case BuiltInFunctionName::ENUM_SIZE:
      break;
  }
  ASSERT(false, "Invalid function name: {}", ToString(name));
  return Constants::Zero;  //  Unreachable.
}

}  // namespace math
