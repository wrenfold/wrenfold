// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <string>
#include <string_view>

#include "wf/utility/hashing.h"

namespace wf {

enum class expression_usage {
  // Expression is an optional output argument.
  optional_output_argument,
  // Expression is an output argument.
  output_argument,
  // Expression is used as a return value.
  return_value,
};

struct output_key {
  // How this block of expressions is used in the generated code.
  expression_usage usage;

  // Name of the output. If this is the return value, it will be empty.
  // If this is an output argument, this is the name of the argument.
  std::string name;

  // Construct w/ usage ane name.
  output_key(const expression_usage usage, const std::string_view name)
      : usage(usage), name(name) {}

  // Equality test.
  bool operator==(const output_key& other) const noexcept {
    return usage == other.usage && name == other.name;
  }

  // Non-equality test.
  bool operator!=(const output_key& other) const noexcept { return !operator==(other); }
};

// Convert `expression_usage` to a string.
constexpr std::string_view string_from_expression_usage(const expression_usage usage) noexcept {
  switch (usage) {
    case expression_usage::return_value:
      return "return_value";
    case expression_usage::output_argument:
      return "output_argument";
    case expression_usage::optional_output_argument:
      return "optional_output_argument";
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Hash `output_key` type.
template <>
struct hash_struct<output_key> {
  std::size_t operator()(const output_key& k) const noexcept {
    return hash_combine(static_cast<std::size_t>(k.usage), hash_string_fnv(k.name));
  }
};

// Test `output_key` for equality.
template <>
struct is_identical_struct<output_key> {
  bool operator()(const output_key& a, const output_key& b) const noexcept { return a == b; }
};

}  // namespace wf
