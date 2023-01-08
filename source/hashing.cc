// Copyright 2023 Gareth Cross
#include "hashing.h"

#include <functional>

#include "assertions.h"
#include "expressions/all_expressions.h"
#include "visitor_impl.h"

namespace math {

// Based on https://stackoverflow.com/questions/2590677/
// TODO: Investigate if there is something better than this, see:
// https://stackoverflow.com/questions/664014/
// https://stackoverflow.com/questions/35985960/
// It is possible this is, in fact, not a very good hash combination.
inline constexpr std::size_t HashCombine(std::size_t seed, const std::size_t new_hash) {
  seed ^= new_hash + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  return seed;
}

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

// TODO: Hashes should be cached in containers like Addition/Multiplication.
struct HashVisitor {
  constexpr static VisitorPolicy Policy = VisitorPolicy::CompileError;
  using ReturnType = std::size_t;

  std::size_t HashAll(std::size_t seed, const std::vector<Expr>& expressions) const {
    for (const Expr& expr : expressions) {
      seed = HashCombine(seed, Hash(expr));
    }
    return seed;
  }

  std::size_t HashUnary(const std::size_t seed, const Expr& expr) const {
    return HashCombine(seed, Hash(expr));
  }

  std::size_t HashBinary(const std::size_t seed, const Expr& a, const Expr& b) const {
    return HashCombine(seed, HashCombine(Hash(a), Hash(b)));
  }

  std::size_t Apply(const Addition& a) const {
    constexpr std::size_t type_hash = HashString("Addition");
    return HashAll(type_hash, a.Args());
  }

  std::size_t Apply(const Constant& c) const {
    constexpr std::size_t type_hash = HashString("Constant");
    const auto enum_value = static_cast<std::uint64_t>(c.GetName());
    return HashCombine(type_hash, std::hash<std::uint64_t>{}(enum_value));
  }

  std::size_t Apply(const Float& f) const {
    constexpr std::size_t type_hash = HashString("Float");
    return HashCombine(type_hash, std::hash<Float::FloatType>{}(f.GetValue()));
  }

  std::size_t Apply(const Integer& i) const {
    constexpr std::size_t type_hash = HashString("Integer");
    return HashCombine(type_hash, std::hash<Integer::IntegralType>{}(i.GetValue()));
  }

  std::size_t Apply(const Multiplication& m) const {
    constexpr std::size_t type_hash = HashString("Multiplication");
    return HashAll(type_hash, m.Args());
  }

  std::size_t Apply(const NaturalLog& l) const {
    constexpr std::size_t type_hash = HashString("NaturalLog");
    return HashUnary(type_hash, l.Inner());
  }

  std::size_t Apply(const Power& p) const {
    constexpr std::size_t type_hash = HashString("Power");
    return HashBinary(type_hash, p.Base(), p.Exponent());
  }

  std::size_t Apply(const Rational& r) const {
    constexpr std::size_t type_hash = HashString("Rational");
    std::size_t result = type_hash;
    result = HashCombine(result, std::hash<Rational::IntegralType>{}(r.Numerator()));
    result = HashCombine(result, std::hash<Rational::IntegralType>{}(r.Denominator()));
    return result;
  }

  std::size_t Apply(const Variable& v) const {
    constexpr std::size_t type_hash = HashString("Variable");
    return HashCombine(type_hash, std::hash<std::string>{}(v.GetName()));
  }
};

std::size_t Hash(const Expr& x) {
  const std::optional<std::size_t> hash = VisitStruct(x, HashVisitor{});
  ASSERT(hash, "Failed to hash expression: {}", x.ToString());
  return *hash;
}

}  // namespace math
