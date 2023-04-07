// Copyright 2023 Gareth Cross
#pragma once
#include "expression.h"

namespace math {

// HashExpression expression.
std::size_t HashExpression(const Expr& x);

// Object to use as the map hasher type.
struct ExprHash {
  std::size_t operator()(const Expr& x) const { return HashExpression(x); }
};

// Check for equality. For use in hash-maps.
struct ExprEquality {
  bool operator()(const Expr& a, const Expr& b) const { return a.IsIdenticalTo(b); }
};

// constexpr FNV hash of string_view
inline constexpr std::size_t HashString(const std::string_view& str) {
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
// The special numeric value here is 2^64 divided by the golden ratio, as a uint64_t.
inline constexpr std::size_t HashCombine(const std::size_t seed, const std::size_t new_hash) {
  static_assert(sizeof(std::size_t) == 8);
  return seed ^ (new_hash + 0x9e3779b97f4a7c15 + (seed << 6) + (seed >> 2));
}

// Hash object T. Specializations are implemented elsewhere.
template <typename T>
struct Hash;

}  // namespace math
