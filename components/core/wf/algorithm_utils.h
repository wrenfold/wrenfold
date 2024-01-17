// Copyright 2024 Gareth Cross
#pragma once
#include <type_traits>

// Utility header for <algorithm>-like things.
namespace wf {

// Transform a vector-like container and return another container.
template <typename ContainerOut, typename ContainerIn, typename F>
ContainerOut transform_map(const ContainerIn& in,
                           F&& f) noexcept(std::is_nothrow_invocable_v<F, decltype(*in.begin())>) {
  ContainerOut result{};
  result.reserve(in.size());
  for (const auto& element : in) {
    result.push_back(f(element));
  }
  return result;
}

}  // namespace wf
