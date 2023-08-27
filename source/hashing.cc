// Copyright 2023 Gareth Cross
#include "hashing.h"

#include <functional>

#include "assertions.h"
#include "expressions/all_expressions.h"
#include "visitor_impl.h"

namespace math {

template <typename Enum, typename Callable, typename T, T... ints>
constexpr auto EvaluateFuncOnEnum(Callable callable, std::integer_sequence<T, ints...>) {
  return std::array{callable(static_cast<Enum>(ints))...};
}

// Create hashes for unary function names at compile time.
constexpr auto MakeUnaryFunctionHashes() {
  constexpr std::size_t enum_length = static_cast<std::size_t>(UnaryFunctionName::ENUM_SIZE);
  return EvaluateFuncOnEnum<UnaryFunctionName>(
      [](UnaryFunctionName name) constexpr { return HashString(ToString(name)); },
      std::make_integer_sequence<std::size_t, enum_length>{});
}

// TODO: Hashes should be cached in containers like Addition/Multiplication.
struct HashVisitor {
  using ReturnType = std::size_t;

  template <typename Iterator>
  std::size_t HashAll(std::size_t seed, Iterator begin, Iterator end) const {
    for (; begin != end; ++begin) {
      seed = HashCombine(seed, HashExpression(*begin));
    }
    return seed;
  }

  template <typename... Ts>
  std::size_t HashExpressions(std::size_t seed, const Ts&... expressions) const {
    const std::array<std::size_t, sizeof...(Ts)> hashes = {HashExpression(expressions)...};
    for (std::size_t i = 0; i < hashes.size(); ++i) {
      seed = HashCombine(seed, hashes[i]);
    }
    return seed;
  }

  std::size_t Apply(const Addition& a) const {
    constexpr std::size_t type_hash = HashString("Addition");
    return HashAll(type_hash, a.begin(), a.end());
  }

  std::size_t Apply(const Conditional& c) const {
    constexpr std::size_t type_hash = HashString("Conditional");
    return HashExpressions(type_hash, c.Condition(), c.IfBranch(), c.ElseBranch());
  }

  std::size_t Apply(const Constant& c) const {
    constexpr std::size_t type_hash = HashString("Constant");
    const auto enum_value = static_cast<std::uint64_t>(c.GetName());
    return HashCombine(type_hash, std::hash<std::uint64_t>{}(enum_value));
  }

  std::size_t Apply(const Float& f) const {
    constexpr std::size_t type_hash = HashString("Float");
    return HashCombine(type_hash, Hash<Float>{}(f));
  }

  std::size_t Apply(const FunctionArgument& f) const {
    constexpr std::size_t type_hash = HashString("FunctionArgument");
    const std::size_t arg_hash = HashCombine(std::hash<std::size_t>{}(f.ArgIndex()),
                                             std::hash<std::size_t>{}(f.ElementIndex()));
    return HashCombine(type_hash, arg_hash);
  }

  constexpr std::size_t Apply(const Infinity&) const {
    constexpr std::size_t type_hash = HashString("Infinity");
    return type_hash;
  }

  std::size_t Apply(const Integer& i) const {
    constexpr std::size_t type_hash = HashString("Integer");
    return HashCombine(type_hash, Hash<Integer>{}(i));
  }

  std::size_t Apply(const Matrix& m) const {
    constexpr std::size_t type_hash = HashString("Matrix");
    std::size_t seed = type_hash;
    seed = HashCombine(seed, std::hash<std::size_t>{}(m.NumRows()));
    seed = HashCombine(seed, std::hash<std::size_t>{}(m.NumCols()));
    return HashAll(seed, m.begin(), m.end());
  }

  std::size_t Apply(const Multiplication& m) const {
    constexpr std::size_t type_hash = HashString("Multiplication");
    return HashAll(type_hash, m.begin(), m.end());
  }

  std::size_t Apply(const Power& p) const {
    constexpr std::size_t type_hash = HashString("Power");
    return HashExpressions(type_hash, p.Base(), p.Exponent());
  }

  std::size_t Apply(const Rational& r) const {
    constexpr std::size_t type_hash = HashString("Rational");
    return HashCombine(type_hash, Hash<Rational>{}(r));
  }

  std::size_t Apply(const Relational& r) const {
    constexpr std::size_t type_hash = HashString("Relational");
    return HashExpressions(type_hash, r.Left(), r.Right());
  }

  std::size_t Apply(const UnaryFunction& f) const {
    constexpr std::size_t type_hash = HashString("UnaryFunction");
    // Create a lookup table of hashes for each function name:
    constexpr static std::array lookup_table = MakeUnaryFunctionHashes();
    const std::size_t func_hash = lookup_table[static_cast<std::size_t>(f.Func())];
    return HashExpressions(HashCombine(type_hash, func_hash), f.Arg());
  }

  std::size_t Apply(const Variable& v) const {
    constexpr std::size_t type_hash = HashString("Variable");
    return HashCombine(type_hash, std::hash<std::string>{}(v.GetName()));
  }
};

std::size_t HashExpression(const Expr& x) { return VisitStruct(x, HashVisitor{}); }

}  // namespace math
