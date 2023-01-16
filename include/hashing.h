// Copyright 2023 Gareth Cross
#pragma once
#include "expression.h"

namespace math {

// Hash expression.
std::size_t Hash(const Expr& x);

// Object for use in maps.
struct HashObject {
  std::size_t operator()(const Expr& x) const { return Hash(x); }
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

}  // namespace math
