// Copyright 2023 Gareth Cross
#pragma once
#include <limits>
#include <string_view>

namespace math {

// Index type for matrices/vectors.
using index_t = int;

// Mathematical precedence of operators.
enum class precedence : int {
  relational = 0,
  addition,
  multiplication,
  power,
  none = std::numeric_limits<int>::max(),
};

// Describe the relative order of two expressions (a, b)
// This is for operations like sorting, not for expressing mathematical relations.
enum class relative_order : int {
  // a < b
  less_than = -1,
  // a == b
  equal = 0,
  // a > b
  greater_than = 1,
};

// Types of mathematical functions.
// clang-format off
enum class built_in_function {
  // Unary functions.
  cos,
  sin,
  tan,
  arccos,
  arcsin,
  arctan,
  ln,
  abs,
  signum,
  // Binary functions
  arctan2,
};
// clang-format on

// Types of relations we can express:
enum class relational_operation {
  // a < b
  less_than,
  // a <= b
  less_than_or_equal,
  // a == b
  equal,
};

// List of mathematical symbolic constants.
enum class symbolic_constants : int {
  euler,
  pi,
  boolean_true,
  boolean_false,
};

// Types of numeric values (at code-generation time).
enum class code_numeric_type : int {
  boolean,
  integral,
  floating_point,
  complex,
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
  // On the real number line and > 0
  RealPositive,
  // On the real number line and >= 0
  RealNonNegative,
  // On the real number line.
  Real,
  // In the complex plane.
  Complex,
  // Cannot determine the set.
  Unknown,
};

// List of mathematical functions typically found in your standard library.
// This seems like a duplicate of `BuiltInFunction` at first, but they differ
// in that this list contains additional specialized cases. In the math
// expression tree powi, powf, and sqrt are all just instances of `Power`.
enum class StdMathFunction {
  Cos,
  Sin,
  Tan,
  ArcCos,
  ArcSin,
  ArcTan,
  Log,
  Sqrt,
  Abs,
  Signum,
  Arctan2,
  // Integral exponent power.
  Powi,
  // Floating point exponent power.
  Powf,
};

// True if the set is real numbers, or a constrained subset of the real numbers.
constexpr inline bool is_real_set(NumberSet set) noexcept {
  if (set == NumberSet::Real || set == NumberSet::RealNonNegative ||
      set == NumberSet::RealPositive) {
    return true;
  }
  return false;
}

// Convert `relative_order` to string view.
constexpr std::string_view string_from_relative_order(const relative_order order) noexcept {
  switch (order) {
    case relative_order::less_than:
      return "less_than";
    case relative_order::equal:
      return "equal";
    case relative_order::greater_than:
      return "greater_than";
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Convert unary function enum to string.
constexpr std::string_view string_from_built_in_function(const built_in_function name) noexcept {
  switch (name) {
    case built_in_function::cos:
      return "cos";
    case built_in_function::sin:
      return "sin";
    case built_in_function::tan:
      return "tan";
    case built_in_function::arccos:
      return "acos";
    case built_in_function::arcsin:
      return "asin";
    case built_in_function::arctan:
      return "atan";
    case built_in_function::ln:
      return "ln";
    case built_in_function::abs:
      return "abs";
    case built_in_function::signum:
      return "signum";
    case built_in_function::arctan2:
      return "atan2";
    default:
      break;
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Convert `RelationalOperation` to short string.
constexpr std::string_view string_from_relational_operation(
    const relational_operation op) noexcept {
  switch (op) {
    case relational_operation::less_than:
      return "<";
    case relational_operation::less_than_or_equal:
      return "<=";
    case relational_operation::equal:
      return "==";
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Convert symbolic constant enum to string constant.
inline constexpr std::string_view string_from_symbolic_constant(symbolic_constants value) noexcept {
  switch (value) {
    case symbolic_constants::euler:
      return "e";
    case symbolic_constants::pi:
      return "pi";
    case symbolic_constants::boolean_true:
      return "true";
    case symbolic_constants::boolean_false:
      return "false";
  }
  return "<INVALID ENUM VALUE>";
}

// Convert `NumericType` to string.
constexpr std::string_view string_from_numeric_type(const code_numeric_type type) noexcept {
  switch (type) {
    case code_numeric_type::boolean:
      return "Bool";
    case code_numeric_type::integral:
      return "Integer";
    case code_numeric_type::floating_point:
      return "Real";
    case code_numeric_type::complex:
      return "Complex";
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Convert `NumberSet` to string.
constexpr inline std::string_view string_from_number_set(const NumberSet set) noexcept {
  switch (set) {
    case NumberSet::RealPositive:
      return "RealPositive";
    case NumberSet::RealNonNegative:
      return "RealNonNegative";
    case NumberSet::Real:
      return "Real";
    case NumberSet::Complex:
      return "Complex";
    case NumberSet::Unknown:
      return "Unknown";
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Convert `StandardLibraryMathFunction` to string.
constexpr inline std::string_view string_from_standard_library_function(
    StdMathFunction name) noexcept {
  switch (name) {
    case StdMathFunction::Cos:
      return "cos";
    case StdMathFunction::Sin:
      return "sin";
    case StdMathFunction::Tan:
      return "tan";
    case StdMathFunction::ArcCos:
      return "acos";
    case StdMathFunction::ArcSin:
      return "asin";
    case StdMathFunction::ArcTan:
      return "atan";
    case StdMathFunction::Log:
      return "log";
    case StdMathFunction::Sqrt:
      return "sqrt";
    case StdMathFunction::Abs:
      return "abs";
    case StdMathFunction::Signum:
      return "sign";
    case StdMathFunction::Arctan2:
      return "atan2";
    case StdMathFunction::Powi:
      return "powi";
    case StdMathFunction::Powf:
      return "powf";
  }
  return "<NOT A VALID ENUM VALUE>";
}

}  // namespace math
