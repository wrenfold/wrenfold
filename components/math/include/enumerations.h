// Copyright 2023 Gareth Cross
#pragma once
#include <limits>
#include <string_view>

namespace math {

// Index type for matrices/vectors.
using index_t = int;

// Mathematical precedence of operators.
enum class Precedence : int {
  Relational = 0,
  Addition,
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
enum class BuiltInFunctionName {
  // Unary functions.
  Cos = 0,
  Sin,
  Tan,
  ArcCos,
  ArcSin,
  ArcTan,
  Log,
  Sqrt,
  Abs,
  Signum,
  // Binary functions
  Arctan2,
  Pow,
  // Just to get the length of the enum:
  ENUM_SIZE,
};
// clang-format on

// Types of relations we can express:
enum class RelationalOperation {
  // a < b
  LessThan,
  // a <= b
  LessThanOrEqual,
  // a == b
  Equal,
};

// List of mathematical symbolic constants.
enum class SymbolicConstants : int {
  Euler,
  Pi,
  True,
  False,
};

// Types of numeric values (at code-generation time).
enum class NumericType : int {
  Bool = 0,
  Integer,
  Real,
  Complex,
};

// A tri-state value.
enum class TriState : uint8_t {
  // False
  False = 0,
  // True
  True = 1,
  // We cannot determine whether the outcome is true or false.
  Unknown = 2,
};

// Different sets of numbers that we can deal with.
enum class NumberSet : uint8_t {
  // Cannot determine the set.
  Unknown = 0,
  // On the real number line.
  Real,
  // On the real number line and >= 0
  RealNonNegative,
  // In the complex plane.
  Complex,
};

// Convert `RelativeOrder` to string view.
constexpr std::string_view string_from_relative_order(const RelativeOrder order) noexcept {
  switch (order) {
    case RelativeOrder::LessThan:
      return "LessThan";
    case RelativeOrder::Equal:
      return "Equal";
    case RelativeOrder::GreaterThan:
      return "GreaterThan";
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Convert unary function enum to string.
constexpr std::string_view to_string(const BuiltInFunctionName name) noexcept {
  switch (name) {
    case BuiltInFunctionName::Cos:
      return "cos";
    case BuiltInFunctionName::Sin:
      return "sin";
    case BuiltInFunctionName::Tan:
      return "tan";
    case BuiltInFunctionName::ArcCos:
      return "acos";
    case BuiltInFunctionName::ArcSin:
      return "asin";
    case BuiltInFunctionName::ArcTan:
      return "atan";
    case BuiltInFunctionName::Log:
      return "ln";
    case BuiltInFunctionName::Sqrt:
      return "sqrt";
    case BuiltInFunctionName::Abs:
      return "abs";
    case BuiltInFunctionName::Signum:
      return "signum";
    case BuiltInFunctionName::Arctan2:
      return "atan2";
    case BuiltInFunctionName::Pow:
      return "pow";
    default:
      break;
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Convert `RelationalOperation` to short string.
constexpr std::string_view string_from_relational_operation(const RelationalOperation op) noexcept {
  switch (op) {
    case RelationalOperation::LessThan:
      return "<";
    case RelationalOperation::LessThanOrEqual:
      return "<=";
    case RelationalOperation::Equal:
      return "==";
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Convert `NumericType` to string.
constexpr std::string_view string_from_numeric_type(const NumericType type) noexcept {
  switch (type) {
    case NumericType::Bool:
      return "Bool";
    case NumericType::Integer:
      return "Integer";
    case NumericType::Real:
      return "Real";
    case NumericType::Complex:
      return "Complex";
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Convert `NumberSet` to string.
constexpr inline std::string_view string_from_number_set(const NumberSet set) noexcept {
  switch (set) {
    case NumberSet::Unknown:
      return "Unknown";
    case NumberSet::Real:
      return "Real";
    case NumberSet::RealNonNegative:
      return "RealNonNegative";
    case NumberSet::Complex:
      return "Complex";
  }
  return "<NOT A VALID ENUM VALUE>";
}

}  // namespace math
