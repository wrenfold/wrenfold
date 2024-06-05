// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
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
  cosh,
  sinh,
  tanh,
  arccosh,
  arcsinh,
  arctanh,
  log,
  abs,
  signum,
  floor,
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
// This seems like a duplicate of `built_in_function` at first, but they differ in that this list
// contains additional specialized cases. In the math expression tree powi, powf, and sqrt are all
// just instances of `power`.
enum class std_math_function {
  cos,
  sin,
  tan,
  acos,
  asin,
  atan,
  cosh,
  sinh,
  tanh,
  acosh,
  asinh,
  atanh,
  log,
  sqrt,
  abs,
  signum,
  floor,
  atan2,
  // Integral exponent power.
  powi,
  // Floating point exponent power.
  powf,
};

// Govern behavior of the derivative visitor when we encounter non-differentiable functions.
// An example would be the round(x) function, which has derivatives:
//  - 0 for integer `x`.
//  - undefined everywhere else.
enum class non_differentiable_behavior {
  // Replace the derivative of non-differentiable functions with a suitable constant.
  // This is not strictly correct mathematically, but is more computationally useful in most cases.
  constant,
  // Insert abstract `derivative` expressions to represent something we don't know how to
  // differentiate: df(x)/dx --> derivative(f(x), x, 1)
  abstract,
};

// True if the set is real numbers, or a constrained subset of the real numbers.
constexpr bool is_real_set(const number_set set) noexcept {
  if (set == number_set::real || set == number_set::real_non_negative ||
      set == number_set::real_positive) {
    return true;
  }
  return false;
}

// Convert `precedence` to string view.
constexpr std::string_view string_from_precedence(const precedence p) noexcept {
  switch (p) {
    case precedence::relational:
      return "relational";
    case precedence::addition:
      return "addition";
    case precedence::multiplication:
      return "multiplication";
    case precedence::power:
      return "power";
    case precedence::none:
      return "none";
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
    case built_in_function::cosh:
      return "cosh";
    case built_in_function::sinh:
      return "sinh";
    case built_in_function::tanh:
      return "tanh";
    case built_in_function::arccosh:
      return "acosh";
    case built_in_function::arcsinh:
      return "asinh";
    case built_in_function::arctanh:
      return "atanh";
    case built_in_function::log:
      return "log";
    case built_in_function::abs:
      return "abs";
    case built_in_function::signum:
      return "sign";
    case built_in_function::floor:
      return "floor";
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
constexpr std::string_view string_from_symbolic_constant(
    const symbolic_constant_enum value) noexcept {
  switch (value) {
    case symbolic_constant_enum::euler:
      return "E";
    case symbolic_constant_enum::pi:
      return "pi";
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
constexpr std::string_view string_from_number_set(const number_set set) noexcept {
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
constexpr std::string_view string_from_standard_library_function(
    const std_math_function name) noexcept {
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
    case std_math_function::cosh:
      return "cosh";
    case std_math_function::sinh:
      return "sinh";
    case std_math_function::tanh:
      return "tanh";
    case std_math_function::acosh:
      return "acosh";
    case std_math_function::asinh:
      return "asinh";
    case std_math_function::atanh:
      return "atanh";
    case std_math_function::log:
      return "log";
    case std_math_function::sqrt:
      return "sqrt";
    case std_math_function::abs:
      return "abs";
    case std_math_function::signum:
      return "signum";
    case std_math_function::floor:
      return "floor";
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
