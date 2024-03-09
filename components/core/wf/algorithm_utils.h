// Copyright 2024 Gareth Cross
#pragma once
#include <type_traits>
#include <utility>

// Utility header for <algorithm>-like things.
namespace wf {
namespace detail {

// Check if type `T` supports reserve.
template <typename T, typename = void>
struct supports_reserve : std::false_type {};
template <typename T>
struct supports_reserve<T, decltype(std::declval<T>().reserve(static_cast<std::size_t>(0)), void())>
    : std::true_type {};

}  // namespace detail

// Transform a vector-like container and return another ordered container.
template <typename ContainerOut, typename ContainerIn, typename F>
ContainerOut transform_map(const ContainerIn& in, F&& f) {
  ContainerOut result{};
  if constexpr (detail::supports_reserve<ContainerOut>::value) {
    result.reserve(in.size());
  }
  for (const auto& element : in) {
    result.push_back(f(element));
  }
  return result;
}

// This version of `transform_map` deduces the template argument to the output container.
template <template <typename...> class ContainerOut, typename ContainerIn, typename F>
auto transform_map(const ContainerIn& in, F&& f) {
  using output_type = std::invoke_result_t<F, decltype(*in.begin())>;
  return transform_map<ContainerOut<output_type>>(in, std::forward<F>(f));
}

// Transform a vector-like container and return another ordered container. Passes iteration index to
// the callable object.
template <typename ContainerOut, typename ContainerIn, typename F>
ContainerOut transform_enumerate_map(const ContainerIn& in, F&& f) {
  ContainerOut result{};
  if constexpr (detail::supports_reserve<ContainerOut>::value) {
    result.reserve(in.size());
  }
  std::size_t index = 0;
  for (const auto& element : in) {
    result.push_back(f(index, element));
    ++index;
  }
  return result;
}

// This version of `transform_enumerate_map` deduces the template argument to the output container.
template <template <typename...> class ContainerOut, typename ContainerIn, typename F>
auto transform_enumerate_map(const ContainerIn& in, F&& f) {
  using output_type = std::invoke_result_t<F, std::size_t, decltype(*in.begin())>;
  return transform_enumerate_map<ContainerOut<output_type>>(in, std::forward<F>(f));
}

// True if any element of the container matches the predicate. False if no element matches, or if
// the container is empty.
template <typename Container, typename Predicate>
bool any_of(const Container& container, Predicate&& p) {
  for (const auto& element : container) {
    if (p(element)) {
      return true;
    }
  }
  return false;
}

}  // namespace wf
