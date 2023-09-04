// Copyright 2023 Gareth Cross
#pragma once
#include <unordered_map>
#include <variant>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4127)  // conditional expression is constant
#endif                           // _MSC_VER
#include <absl/container/inlined_vector.h>
#include <absl/types/span.h>
#ifdef _MSC_VER
#pragma warning(pop)
#endif  //  _MSC_VER

#include "constants.h"
#include "expressions/numeric_expressions.h"
#include "hashing.h"

namespace math {

// A multiplication of `N` terms.
class Multiplication {
 public:
  static constexpr std::string_view NameStr = "Multiplication";
  static constexpr bool IsLeafNode = false;
  using ContainerType = absl::InlinedVector<Expr, 16>;

  // Move-construct.
  explicit Multiplication(ContainerType&& terms) : terms_(std::move(terms)) {
    ASSERT_GREATER_OR_EQ(terms_.size(), 2);
  }

  // Construct from expressions:
  template <typename... Ts>
  explicit Multiplication(Ts&&... terms) : terms_() {
    static_assert(sizeof...(terms) >= 2);
    static_assert(std::conjunction_v<std::is_constructible<Expr, Ts>...>);
    terms_.reserve(sizeof...(terms));
    (terms_.emplace_back(std::forward<Ts>(terms)), ...);
    for (const auto& term : terms_) {
      ASSERT(!term.Is<Multiplication>(), "Multiplications should all be flattened: {}",
             term.ToString());
    }
  }

  // Access specific argument.
  const Expr& operator[](const std::size_t i) const { return terms_[i]; }

  // Number of arguments.
  std::size_t Arity() const { return terms_.size(); }

  // Iterators.
  ContainerType::const_iterator begin() const { return terms_.begin(); }
  ContainerType::const_iterator end() const { return terms_.end(); }

  // All terms must be identical.
  bool IsIdenticalTo(const Multiplication& other) const {
    if (Arity() != other.Arity()) {
      return false;
    }
    return std::equal(begin(), end(), other.begin(), IsIdenticalOperator<Expr>{});
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void Iterate(Operation&& operation) const {
    std::for_each(begin(), end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr Map(Operation&& operation) const {
    ContainerType transformed{};
    transformed.reserve(Arity());
    std::transform(begin(), end(), std::back_inserter(transformed),
                   std::forward<Operation>(operation));
    return Multiplication::FromOperands(transformed);
  }

  // Construct from a span of operands. Result is automatically simplified.
  static Expr FromOperands(absl::Span<const Expr> span);

 private:
  ContainerType terms_;
};

template <>
struct Hash<Multiplication> {
  std::size_t operator()(const Multiplication& mul) const {
    return HashAll(0, mul.begin(), mul.end());
  }
};

// Convert an expression into a coefficient and a multiplicand. This operation checks if
// expr is a multiplication. If it is, we extract all numeric constants and return them
// as the first value. The remaining terms form a new multiplication, which is returned as
// the second value.
std::pair<Expr, Expr> AsCoefficientAndMultiplicand(const Expr& expr);

// Helper object used to execute multiplications.
struct MultiplicationParts {
  MultiplicationParts() = default;
  explicit MultiplicationParts(std::size_t capacity) { terms.reserve(capacity); }

  // ConstructMatrix from existing multiplication.
  explicit MultiplicationParts(const Multiplication& mul, bool factorize_integers);

  // Rational coefficient.
  Rational rational_coeff{1, 1};
  // Floating point coefficient:
  std::optional<Float> float_coeff{};
  // Map from base to exponent.
  std::unordered_map<Expr, Expr, Hash<Expr>, IsIdenticalOperator<Expr>> terms{};

  // Update the internal product by multiplying on `arg`.
  void Multiply(const Expr& arg, bool factorize_integers = false);

  // Nuke any terms w/ a zero exponent and normalize powers of integers.
  void Normalize();

  // Create the resulting multiplication.
  Expr CreateMultiplication() const;
};

// A decomposition of `Multiplication` that is more convenient for printing.
// This is defined here and not in one particular formatter, since it is likely useful more than
// once.
struct MultiplicationFormattingInfo {
  struct BaseExp {
    Expr base;
    Expr exponent;
  };

  bool is_negative{false};
  std::vector<std::variant<Integer, Float, BaseExp>> numerator;
  std::vector<std::variant<Integer, Float, BaseExp>> denominator;
};

// Create `MultiplicationFormattingInfo` from a multiplication.
MultiplicationFormattingInfo GetFormattingInfo(const Multiplication& mul);

}  // namespace math
