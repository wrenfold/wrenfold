// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <variant>

#include "wf/compound_expression.h"
#include "wf/expression.h"
#include "wf/matrix_expression.h"

namespace wf {

// Variant of possible expression types.
using any_expression = std::variant<scalar_expr, matrix_expr, compound_expr, boolean_expr>;

template <>
struct hash_struct<any_expression> : hash_variant<any_expression> {};
template <>
struct is_identical_struct<any_expression> : is_identical_variant<any_expression> {};
template <>
struct order_struct<any_expression> : order_variant<any_expression> {};

template <typename T>
constexpr bool is_any_expression_v = std::is_same_v<T, any_expression>;

}  // namespace wf
