// Copyright 2023 Gareth Cross
#pragma once
#include <limits>
#include <string_view>

namespace wf {

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
enum class symbolic_constant_enum {
  euler,
  pi,
  boolean_true,
  boolean_false,
};

// Types of numeric values (at code-generation time).
enum class code_numeric_type {
  boolean,
  integral,
  floating_point,
};

// A tri-state value.
enum class tri_state : uint8_t {
  // False
  False = 0,
  // True
  True = 1,
  // We cannot determine whether the outcome is true or false.
  unknown = 2,
};

// Different sets of numbers that we can deal with.
enum class number_set : uint8_t {
  // On the real number line and > 0
  real_positive,
  // On the real number line and >= 0
  real_non_negative,
  // On the real number line.
  real,
  // In the complex plane.
  complex,
  // Cannot determine the set.
  unknown,
};

// List of mathematical functions typically found in your standard library.
// This seems like a duplicate of `BuiltInFunction` at first, but they differ
// in that this list contains additional specialized cases. In the math
// expression tree powi, powf, and sqrt are all just instances of `power`.
enum class std_math_function {
  cos,
  sin,
  tan,
  acos,
  asin,
  atan,
  log,
  sqrt,
  abs,
  signum,
  atan2,
  // Integral exponent power.
  powi,
  // Floating point exponent power.
  powf,
};

// True if the set is real numbers, or a constrained subset of the real numbers.
constexpr inline bool is_real_set(number_set set) noexcept {
  if (set == number_set::real || set == number_set::real_non_negative ||
      set == number_set::real_positive) {
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

// Convert `relational_operation` to short string.
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
inline constexpr std::string_view string_from_symbolic_constant(
    symbolic_constant_enum value) noexcept {
  switch (value) {
    case symbolic_constant_enum::euler:
      return "e";
    case symbolic_constant_enum::pi:
      return "pi";
    case symbolic_constant_enum::boolean_true:
      return "true";
    case symbolic_constant_enum::boolean_false:
      return "false";
  }
  return "<INVALID ENUM VALUE>";
}

// Convert `code_numeric_type` to string.
constexpr std::string_view string_from_code_numeric_type(const code_numeric_type type) noexcept {
  switch (type) {
    case code_numeric_type::boolean:
      return "boolean";
    case code_numeric_type::integral:
      return "integral";
    case code_numeric_type::floating_point:
      return "floating_point";
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Convert `number_set` to string.
constexpr inline std::string_view string_from_number_set(const number_set set) noexcept {
  switch (set) {
    case number_set::real_positive:
      return "real_positive";
    case number_set::real_non_negative:
      return "real_non_negative";
    case number_set::real:
      return "real";
    case number_set::complex:
      return "complex";
    case number_set::unknown:
      return "unknown";
  }
  return "<NOT A VALID ENUM VALUE>";
}

// Convert `std_math_function` to string.
constexpr inline std::string_view string_from_standard_library_function(
    std_math_function name) noexcept {
  switch (name) {
    case std_math_function::cos:
      return "cos";
    case std_math_function::sin:
      return "sin";
    case std_math_function::tan:
      return "tan";
    case std_math_function::acos:
      return "acos";
    case std_math_function::asin:
      return "asin";
    case std_math_function::atan:
      return "atan";
    case std_math_function::log:
      return "log";
    case std_math_function::sqrt:
      return "sqrt";
    case std_math_function::abs:
      return "abs";
    case std_math_function::signum:
      return "sign";
    case std_math_function::atan2:
      return "atan2";
    case std_math_function::powi:
      return "powi";
    case std_math_function::powf:
      return "powf";
  }
  return "<NOT A VALID ENUM VALUE>";
}

}  // namespace wf
