// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <algorithm>
#include <type_traits>
#include <utility>

#include "wf/utility/traits.h"

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
  if constexpr (is_tuple_v<Container>) {
    return std::apply([&p](const auto&... element) { return (p(element) || ...); }, container);
  } else {
    return std::any_of(container.begin(), container.end(),
                       [&p](const auto& element) -> bool { return p(element); });
  }
}

// True if an element is found at least once in a container.
template <typename Container, typename T>
bool contains(const Container& container, const T& item) {
  return any_of(container, [&item](const auto& element) { return element == item; });
}

// True if all elements match the given predicate. Returns true if the container is empty.
template <typename Container, typename Predicate>
bool all_of(const Container& container, Predicate&& p) {
  return std::all_of(container.begin(), container.end(),
                     [&p](const auto& element) -> bool { return p(element); });
}

// True if none of the elements match the given predicate. Returns true if the container is empty.
template <typename Container, typename Predicate>
bool none_of(const Container& container, Predicate&& p) {
  return std::none_of(container.begin(), container.end(),
                      [&p](const auto& element) -> bool { return p(element); });
}

// Erase elements from a map-like container if they match predicate `p`.
// Can be replaced with std::erase_if when switching to c++20:
// https://en.cppreference.com/w/cpp/container/map/erase_if
template <typename Container, typename Predicate>
std::size_t map_erase_if(Container& container, Predicate&& p) {
  const std::size_t old_size = container.size();
  for (auto it = container.begin(); it != container.end();) {
    if (p(*it)) {
      it = container.erase(it);
    } else {
      ++it;
    }
  }
  return old_size - container.size();
}

// Traverse the container in order and remove elements that match `p`.
template <typename Container, typename Predicate>
void remove_if(Container& container, Predicate&& p) {
  container.erase(std::remove_if(container.begin(), container.end(),
                                 [&p](auto&& value) -> bool {
                                   return p(std::forward<decltype(value)>(value));
                                 }),
                  container.end());
}

// Traverse the container in reverse order, removing elements that match `p`.
template <typename Container, typename Predicate>
void reverse_remove_if(Container& container, Predicate&& p) {
  // This is reverse order, so we need to remove from the front.
  container.erase(container.rend().base(),
                  std::remove_if(container.rbegin(), container.rend(), [&p](auto&& value) -> bool {
                    return p(std::forward<decltype(value)>(value));
                  }).base());
}

// A variation on std::unique that extracts duplicates by moving them into another container.
// `output` should be an output iterator indicating where to store the duplicates.
template <typename Input, typename Output>
Input unique_and_extract_duplicates(Input first, Input last, Output output) {
  if (first == last) {
    return last;
  }
  Input result = first;
  for (++first; first != last; ++first) {
    if (*result == *first) {
      // This is a duplicate, move it to the output
      *output = std::move(*first);
      ++output;
    } else {
      // If the result is more than one space behind `first`, we need to move the unique elements
      // back.
      ++result;
      if (result != first) {
        *result = std::move(*first);
      }
    }
  }
  ++result;
  return result;
}

}  // namespace wf
