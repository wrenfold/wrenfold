// Copyright 2023 Gareth Cross
#pragma once
#include <string>

#include "hashing.h"

namespace math {

enum class ExpressionUsage {
  // Expression is an optional output argument.
  OptionalOutputArgument,
  // Expression is an output argument.
  OutputArgument,
  // Expression is used as a return value.
  ReturnValue,
};

struct OutputKey {
  // How this block of expressions is used in the generated code.
  ExpressionUsage usage;

  // Position in the output. If this is a return value, this indicates whether it
  // is the first, second, ... return value in a tuple. If this is an output argument, then this
  // indicates the position in the argument list.
  std::size_t arg_position;

  constexpr OutputKey(ExpressionUsage usage, std::size_t arg_position)
      : usage(usage), arg_position(arg_position) {}

  // Equality test.
  constexpr bool operator==(const OutputKey& other) const {
    return usage == other.usage && arg_position == other.arg_position;
  }

  // Non-equality test.
  constexpr bool operator!=(const OutputKey& other) const {
    return usage != other.usage || arg_position != other.arg_position;
  }

  // Relative order (lexicographical order).
  constexpr bool operator<(const OutputKey& other) const {
    return std::make_pair(usage, arg_position) < std::make_pair(other.usage, other.arg_position);
  }
};

// Convert `ExpressionUsage` to a string.
inline constexpr std::string_view StringFromExpressionUsage(const ExpressionUsage usage) {
  switch (usage) {
    case ExpressionUsage::ReturnValue:
      return "ReturnValue";
    case ExpressionUsage::OutputArgument:
      return "OutputArgument";
    case ExpressionUsage::OptionalOutputArgument:
      return "OptionalOutputArgument";
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Hash `OutputKey` type.
struct OutputKeyHasher {
  constexpr std::size_t operator()(const OutputKey& k) const {
    return HashCombine(static_cast<std::size_t>(k.usage), k.arg_position);
  }
};

// Group together a set of expressions w/ key describing how they are relevant to a function
// we want to code-generate.
struct ExpressionGroup {
  // All the expressions in this group.
  std::vector<Expr> expressions;

  // Key describing how these expressions are used in a function we intend to code generate.
  OutputKey key;

  ExpressionGroup(std::vector<Expr>&& expressions, OutputKey key)
      : expressions(std::move(expressions)), key(key) {}
};

}  // namespace math
