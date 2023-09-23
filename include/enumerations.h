// Copyright 2023 Gareth Cross
#pragma once
#include <limits>

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

// Types of numeric values.
enum class NumericType : int {
  Bool = 0,
  Integer,
  Real,
  Complex,
};

// Convert `RelativeOrder` to string view.
constexpr std::string_view StringFromRelativeOrder(const RelativeOrder order) {
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
constexpr std::string_view ToString(const BuiltInFunctionName name) {
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
constexpr std::string_view StringFromRelationalOperation(const RelationalOperation op) {
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

constexpr std::string_view StringFromNumericType(const NumericType type) {
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

// Convert `SymbolicConstants` to a floating point double.
constexpr double DoubleFromSymbolicConstant(SymbolicConstants constant) {
  switch (constant) {
    case SymbolicConstants::Euler:
      return M_E;
    case SymbolicConstants::Pi:
      return M_PI;
    case SymbolicConstants::True:
      return 1.0;
    case SymbolicConstants::False:
      return 0.0;
  }
  return std::numeric_limits<double>::quiet_NaN();
}

}  // namespace math
