// Copyright 2023 Gareth Cross
#pragma once
#include "expression_impl.h"
#include "functions.h"
#include "hashing.h"

namespace math {

// clang-format off
enum class UnaryFunctionName {
  Cos = 0,
  Sin,
  Tan,
  ArcCos,
  ArcSin,
  ArcTan,
  Log,
  // Just to get the length of the enum:
  ENUM_SIZE,
};
// clang-format on

// Convert unary function enum to string.
constexpr std::string_view ToString(const UnaryFunctionName name) {
  switch (name) {
    case UnaryFunctionName::Cos:
      return "cos";
    case UnaryFunctionName::Sin:
      return "sin";
    case UnaryFunctionName::Tan:
      return "tan";
    case UnaryFunctionName::ArcCos:
      return "acos";
    case UnaryFunctionName::ArcSin:
      return "asin";
    case UnaryFunctionName::ArcTan:
      return "atan";
    case UnaryFunctionName::Log:
      return "ln";
    case UnaryFunctionName::ENUM_SIZE:
      return "<NOT A VALID ENUM VALUE>";
  }
}

template <typename Derived>
class BuiltInFunctionBase : public ExpressionImpl<Derived> {
 public:
};

// Store a unary function. Built-in unary functions `f(x)` are described by an enum indicating
class UnaryFunction : public BuiltInFunctionBase<UnaryFunction> {
 public:
  UnaryFunction(UnaryFunctionName func, Expr arg) : func_(func), arg_(std::move(arg)) {}

  // Get the function name.
  const UnaryFunctionName& Func() const { return func_; }

  // Get name as a string.
  std::string_view Name() const { return ToString(func_); }

  // Get the function argument.
  const Expr& Arg() const { return arg_; }

  // Function type and argument must match.
  bool IsIdenticalToImplTyped(const UnaryFunction& other) const {
    return func_ == other.func_ && arg_.IsIdenticalTo(other.arg_);
  }

 protected:
  UnaryFunctionName func_;
  Expr arg_;
};

// clang-format off
enum class BinaryFunctionName {
  Mod,
  // Just to get the length of the enum:
  ENUM_SIZE,
};
// clang-format on

#if 0
// Convert binary function enum to string.
constexpr std::string_view ToString(const BinaryFunctionName name) {
  switch (name) {
    case BinaryFunctionName::Mod:
      return "mod";
    case BinaryFunctionName::ENUM_SIZE:
      return "<NOT A VALID ENUM VALUE>";
  }
}

class BinaryFunction : public BuiltInFunctionBase<BinaryFunction> {
 public:
  BinaryFunction(BinaryFunctionName func, Expr first, Expr second)
      : func_(func), args_{std::move(first), std::move(second)} {}

  // Get the function name.
  const BinaryFunctionName& Func() const { return func_; }

  // Get the first function argument.
  const Expr& Arg0() const { return args_[0]; }

  // Get the second function argument.
  const Expr& Arg1() const { return args_[1]; }

  // Function type and argument must match.
  bool IsIdenticalToImplTyped(const BinaryFunction& other) const {
    return func_ == other.func_ && args_[0].IsIdenticalTo(other.args_[0]) &&
           args_[1].IsIdenticalTo(other.args_[1]);
  }

 protected:
  BinaryFunctionName func_;
  std::array<Expr, 2> args_;
};
#endif

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
    default:
      break;
  }
  ASSERT(false, "Invalid unary function name: {}", ToString(name));
  return Constants::Zero;  //  Unreachable.
}

}  // namespace math