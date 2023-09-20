// Copyright 2023 Gareth Cross
#pragma once
#include "assertions.h"
#include "constants.h"
#include "functions.h"
#include "hashing.h"

namespace math {

// Fwd declare.
Expr CreateUnaryFunction(const UnaryFunctionName name, const Expr& arg);

// Store a unary function. Built-in unary functions `f(x)` are described by the enum
// `UnaryFunctionName`.
class UnaryFunction {
 public:
  static constexpr std::string_view NameStr = "UnaryFunction";
  static constexpr bool IsLeafNode = false;

  UnaryFunction(UnaryFunctionName func, Expr arg) : func_(func), arg_(std::move(arg)) {}

  // Get the function name.
  constexpr UnaryFunctionName Func() const { return func_; }

  // Get name as a string.
  constexpr std::string_view Name() const { return ToString(func_); }

  // Get the function argument.
  const Expr& Arg() const { return arg_; }

  // Function type and argument must match.
  bool IsIdenticalTo(const UnaryFunction& other) const {
    return func_ == other.func_ && arg_.IsIdenticalTo(other.arg_);
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void Iterate(Operation operation) const {
    operation(arg_);
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr Map(Operation operation) const {
    return CreateUnaryFunction(func_, operation(arg_));
  }

 protected:
  UnaryFunctionName func_;
  Expr arg_;
};

template <>
struct Hash<UnaryFunction> {
  std::size_t operator()(const UnaryFunction& func) const {
    return HashArgs(static_cast<std::size_t>(func.Func()), func.Arg());
  }
};

class BinaryFunction {
 public:
  static constexpr std::string_view NameStr = "BinaryFunction";
  static constexpr bool IsLeafNode = false;

  BinaryFunction(BinaryFunctionName func, Expr first, Expr second)
      : func_(func), args_{std::move(first), std::move(second)} {}

  // Get the function name.
  constexpr BinaryFunctionName Func() const noexcept { return func_; }

  // Get the first function argument.
  constexpr const Expr& First() const noexcept { return args_[0]; }

  // Get the second function argument.
  constexpr const Expr& Second() const noexcept { return args_[1]; }

  constexpr auto begin() const noexcept { return args_.begin(); }
  constexpr auto end() const noexcept { return args_.end(); }

  // Function type and argument must match.
  bool IsIdenticalTo(const BinaryFunction& other) const {
    return func_ == other.func_ && args_[0].IsIdenticalTo(other.args_[0]) &&
           args_[1].IsIdenticalTo(other.args_[1]);
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void Iterate(Operation&& operation) const {
    std::for_each(args_.begin(), args_.end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  //  template <typename Operation>
  //  Expr Map(Operation operation) const {
  //    return CreateUnaryFunction(func_, operation(arg_));
  //  }

 protected:
  BinaryFunctionName func_;
  std::array<Expr, 2> args_;
};

template <>
struct Hash<BinaryFunction> {
  std::size_t operator()(const BinaryFunction& func) const {
    return HashArgs(static_cast<std::size_t>(func.Func()), func.First(), func.Second());
  }
};

// Call the appropriate creation method for the specified enum value.
// We need this logic because each type of unary has simplifications it applies.
inline Expr CreateUnaryFunction(const UnaryFunctionName name, const Expr& arg) {
  switch (name) {
    case UnaryFunctionName::Cos:
      return cos(arg);
    case UnaryFunctionName::Sin:
      return sin(arg);
    case UnaryFunctionName::Tan:
      return tan(arg);
    case UnaryFunctionName::ArcCos:
      return acos(arg);
    case UnaryFunctionName::ArcSin:
      return asin(arg);
    case UnaryFunctionName::ArcTan:
      return atan(arg);
    case UnaryFunctionName::Log:
      return log(arg);
    case UnaryFunctionName::Sqrt:
      return sqrt(arg);
    default:
      break;
  }
  ASSERT(false, "Invalid unary function name: {}", ToString(name));
  return Constants::Zero;  //  Unreachable.
}

}  // namespace math
