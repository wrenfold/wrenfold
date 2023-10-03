// Copyright 2023 Gareth Cross
#pragma once
#include <string>
#include <string_view>
#include <vector>

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

  // Name of the output. If this is the return value, it will be empty.
  // If this is an output argument, this is the name of the argument.
  std::string name;

  // Construct w/ usage ane name.
  OutputKey(ExpressionUsage usage, std::string_view name) : usage(usage), name(name) {}

  // Equality test.
  constexpr bool operator==(const OutputKey& other) const {
    return usage == other.usage && name == other.name;
  }

  // Non-equality test.
  constexpr bool operator!=(const OutputKey& other) const { return !operator==(other); }
};

// Convert `ExpressionUsage` to a string.
inline constexpr std::string_view string_from_expression_usage(const ExpressionUsage usage) {
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
template <>
struct hash_struct<OutputKey> {
  std::size_t operator()(const OutputKey& k) const {
    return hash_combine(static_cast<std::size_t>(k.usage), hash_string_fnv(k.name));
  }
};

// Group together a set of expressions w/ key describing how they are relevant to a function
// we want to code-generate.
struct ExpressionGroup {
  // All the expressions in this group.
  std::vector<Expr> expressions;

  // Key describing how these expressions are used in a function we intend to code generate.
  OutputKey key;

  ExpressionGroup(std::vector<Expr> expressions, OutputKey key)
      : expressions(std::move(expressions)), key(std::move(key)) {}
};

}  // namespace math
