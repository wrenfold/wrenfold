// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <complex>
#include <optional>
#include <variant>

#include "wf/expression.h"

namespace wf {

// Try to cast an expression to a numerical type. The input must be one of:
// - integer_constant, float_constant
// - Complex expressions accepted by `complex_cast`.
// If the coersion is not possible, returns none.
std::optional<std::variant<std::int64_t, double, std::complex<double>>> numerical_cast(
    const scalar_expr& expr);

// Try to cast an expression to std::complex. The following are coerced to std::complex<double>:
// - float_constant
// - integer_constant (if allow_integer_conversion == true)
// - imaginary_unit
// - expressions of the form: a*i where `a` is float_constant
// - expressions for the form a + b where `a` and `b` are any of the three above.
std::optional<std::complex<double>> complex_cast(const scalar_expr& expr,
                                                 bool allow_integer_conversion = false);

}  // namespace wf
