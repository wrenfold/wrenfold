// Copyright 2022 Gareth Cross
#pragma once
#include <unordered_map>

#include "constants.h"
#include "expressions/numeric_expressions.h"
#include "hashing.h"
#include "operation_bases.h"
#include "visitor_impl.h"

namespace math {

class Addition : public NAryOp<Addition> {
 public:
  static constexpr std::string_view NameStr = "Addition";

  // Do not call this - use `FromTwoOperands`.
  explicit Addition(std::vector<Expr> args);

  // ConstructMatrix from two operands.
  static Expr FromTwoOperands(const Expr& a, const Expr& b) { return FromOperands({a, b}); }

  // ConstructMatrix form a vector of operands.
  // The result is automatically simplified, and may not be an addition.
  static Expr FromOperands(const std::vector<Expr>& args);
};

template <>
struct Hash<Addition> {
  std::size_t operator()(const Addition& add) const { return HashAll(0, add.begin(), add.end()); }
};

// Helper object used to execute multiplications.
struct AdditionParts {
  AdditionParts() = default;
  explicit AdditionParts(std::size_t capacity) { terms.reserve(capacity); }

  // ConstructMatrix from existing multiplication.
  explicit AdditionParts(const Addition& add);

  // Rational coefficient.
  Rational rational_term{0, 1};
  // Floating point coefficient:
  std::optional<Float> float_term{};
  // Map from multiplicand to coefficient.
  std::unordered_map<Expr, Expr, Hash<Expr>, ExprsIdentical> terms{};

  // Update the internal representation by adding `arg`.
  void Add(const Expr& arg);

  // Nuke any terms w/ a zero exponent and normalize powers of integers.
  void Normalize();

  // Create the resulting multiplication. The provided storage is re-used.
  Expr CreateAddition(std::vector<Expr>&& args) const;
};

}  // namespace math
