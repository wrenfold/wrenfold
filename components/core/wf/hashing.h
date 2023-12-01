// Copyright 2023 Gareth Cross
#pragma once
#include <array>

#include "wf/expression.h"

namespace math {

// Hash object T. Specializations are implemented elsewhere.
template <typename T>
struct hash_struct;

// constexpr FNV hash of string_view
inline constexpr std::size_t hash_string_fnv(const std::string_view& str) noexcept {
  constexpr std::size_t fnv_offset = 0xcbf29ce484222325;
  constexpr std::size_t fnv_prime = 0x100000001b3;
  std::size_t result = fnv_offset;
  for (const char c : str) {
    result = (result * fnv_prime) ^ static_cast<std::size_t>(c);
  }
  return result;
}

// Based on https://stackoverflow.com/questions/2590677/
// TODO: Investigate if there is something better than this.
// The special numeric value here is 2^64 divided by the golden ratio, as uint64_t.
inline constexpr std::size_t hash_combine(const std::size_t seed,
                                          const std::size_t new_hash) noexcept {
  static_assert(sizeof(std::size_t) == 8);
  return seed ^ (new_hash + 0x9e3779b97f4a7c15 + (seed << 6) + (seed >> 2));
}

// Hash a container of objects by traversing a range specified by iterators.
template <typename Iterator>
std::size_t hash_all(std::size_t seed, Iterator begin, Iterator end) {
  using T = std::decay_t<decltype(*begin)>;
  for (; begin != end; ++begin) {
    seed = hash_combine(seed, hash_struct<T>{}(*begin));
  }
  return seed;
}

// Hash a variadic list of arguments.
template <typename... Ts>
std::size_t hash_args(std::size_t seed, const Ts&... expressions) {
  const std::array<std::size_t, sizeof...(Ts)> hashes = {hash_struct<Ts>{}(expressions)...};
  for (std::size_t i = 0; i < hashes.size(); ++i) {
    seed = hash_combine(seed, hashes[i]);
  }
  return seed;
}

template <>
struct hash_struct<Expr> {
  std::size_t operator()(const Expr& expr) const { return expr.get_hash(); }
};

// Shorthand for invoking `hash_struct<T>` on an object.
template <typename T>
std::size_t hash(const T& object) noexcept(std::is_nothrow_invocable_v<hash_struct<T>, const T>) {
  return hash_struct<T>{}(object);
}

}  // namespace math
