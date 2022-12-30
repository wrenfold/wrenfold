// Copyright 2022 Gareth Cross
#pragma once
#include <type_traits>

namespace math {

template <typename T, typename... Ts>
constexpr bool ContainsTypeHelper = std::disjunction_v<std::is_same<T, Ts>...>;

template <typename... Ts>
struct TypeList {};

template <typename T, typename U>
constexpr bool ContainsType = false;

template <typename T, typename... Ts>
constexpr bool ContainsType<T, TypeList<Ts...>> = ContainsTypeHelper<T, Ts...>;

}  // namespace math
