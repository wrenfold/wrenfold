// Copyright 2023 Gareth Cross
#pragma once
#include <limits>

namespace math {

// Index type for matrices/vectors.
using index_t = int;

// Mathematical precedence of operators.
enum class Precedence : int {
  Addition = 0,
  Multiplication,
  Power,
  None = std::numeric_limits<int>::max(),
};

// Describe the relative order of two expressions (a, b)
// This is for operations like sorting, not for expressing mathematical relations.
enum class RelativeOrder : int {
  // a < b
  LessThan = -1,
  // a == b
  Equal = 0,
  // a > b
  GreaterThan = 1,
};

// Types of built-in unary functions.
// clang-format off
enum class UnaryFunctionName {
  Cos = 0,
  Sin,
  Tan,
  ArcCos,
  ArcSin,
  ArcTan,
  Log,
  Sqrt,
  // Just to get the length of the enum:
  ENUM_SIZE,
};
// clang-format on

// clang-format off
enum class BinaryFunctionName {
  Mod,
  Pow,
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
    case UnaryFunctionName::Sqrt:
      return "sqrt";
    case UnaryFunctionName::ENUM_SIZE:
      return "<NOT A VALID ENUM VALUE>";
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Convert binary function enum to string.
constexpr std::string_view ToString(const BinaryFunctionName name) {
  switch (name) {
    case BinaryFunctionName::Mod:
      return "mod";
    case BinaryFunctionName::Pow:
      return "pow";
    case BinaryFunctionName::ENUM_SIZE:
      return "<NOT A VALID ENUM VALUE>";
  }
  return "<NOT A VALID ENUM VALUE>";
}

}  // namespace math
