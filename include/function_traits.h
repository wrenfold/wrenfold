// Copyright 2022 Gareth Cross
#pragma once
#include <tuple>

namespace math {

template <typename T>
struct function_traits : public function_traits<decltype(&T::operator())> {};

template <typename ClassType, typename Ret, typename... Args>
struct function_traits<Ret (ClassType::*)(Args...) const> {
  constexpr static auto Arity = sizeof...(Args);

  // Return type of the function.
  using ReturnType = Ret;

  // Get the i'th argument type.
  // TODO: Seems like overkill to instantiate a tuple for this?
  template <std::size_t i>
  using ArgType = typename std::tuple_element<i, std::tuple<Args...>>::type;

  // Get the i'th argument type + decay it.
  template <std::size_t i>
  using DecayedArgType = std::decay_t<typename std::tuple_element<i, std::tuple<Args...>>::type>;
};

}  // namespace math
