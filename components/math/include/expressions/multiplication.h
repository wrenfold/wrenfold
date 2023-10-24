// Copyright 2023 Gareth Cross
#pragma once
#include <unordered_map>
#include <variant>

#include "absl_imports.h"
#include "constants.h"
#include "expressions/numeric_expressions.h"
#include "expressions/special_constants.h"
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
    ZEN_ASSERT_GREATER_OR_EQ(terms_.size(), 2);
    sort_terms();
  }

  // Construct from expressions:
  template <typename... Ts>
  explicit Multiplication(Ts&&... terms) : terms_() {
    static_assert(sizeof...(terms) >= 2);
    static_assert(std::conjunction_v<std::is_constructible<Expr, Ts>...>);
    terms_.reserve(sizeof...(terms));
    (terms_.emplace_back(std::forward<Ts>(terms)), ...);
    for (const auto& term : terms_) {
      ZEN_ASSERT(!term.is_type<Multiplication>(), "Multiplications should all be flattened: {}",
                 term.to_string());
    }
    sort_terms();
  }

  // Access specific argument.
  const Expr& operator[](const std::size_t i) const { return terms_[i]; }

  // Number of arguments.
  std::size_t arity() const { return terms_.size(); }

  // Iterators.
  ContainerType::const_iterator begin() const { return terms_.begin(); }
  ContainerType::const_iterator end() const { return terms_.end(); }

  // All terms must be identical.
  bool is_identical_to(const Multiplication& other) const {
    if (arity() != other.arity()) {
      return false;
    }
    return std::equal(begin(), end(), other.begin(), IsIdenticalOperator<Expr>{});
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void for_each(Operation&& operation) const {
    std::for_each(begin(), end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    ContainerType transformed{};
    transformed.reserve(arity());
    std::transform(begin(), end(), std::back_inserter(transformed),
                   std::forward<Operation>(operation));
    return Multiplication::from_operands(transformed);
  }

  // Construct from a span of operands. Result is automatically simplified.
  static Expr from_operands(absl::Span<const Expr> span);

 private:
  void sort_terms() {
    // We leave the integer/rational/float part in front.
    // TODO: Add a BinaryMul where the first term is always int/rational/float.
    const auto begin = std::find_if(terms_.begin(), terms_.end(), [](const Expr& term) {
      return !term.is_type<Integer, Rational, Float>();
    });
    std::sort(begin, terms_.end(), [](const Expr& a, const Expr& b) {
      if (a.get_hash() < b.get_hash()) {
        return true;
      } else if (a.get_hash() > b.get_hash()) {
        return false;
      } else {
        return ExpressionOrderPredicate{}(a, b);
      }
    });
  }

  ContainerType terms_;
};

template <>
struct hash_struct<Multiplication> {
  std::size_t operator()(const Multiplication& mul) const {
    return hash_all(0, mul.begin(), mul.end());
  }
};

// Convert an expression into a coefficient and a multiplicand. This operation checks if
// expr is a multiplication. If it is, we extract all numeric constants and return them
// as the first value. The remaining terms form a new multiplication, which is returned as
// the second value.
std::pair<Expr, Expr> as_coeff_and_mul(const Expr& expr);

// Helper object used to execute multiplications.
struct MultiplicationParts {
  MultiplicationParts() = default;
  explicit MultiplicationParts(std::size_t capacity) { terms.reserve(capacity); }

  // Construct from existing multiplication.
  explicit MultiplicationParts(const Multiplication& mul, bool factorize_integers);

  // Rational coefficient.
  Rational rational_coeff{1, 1};
  // Floating point coefficient:
  std::optional<Float> float_coeff{};
  // Map from base to exponent.
  std::unordered_map<Expr, Expr, hash_struct<Expr>, IsIdenticalOperator<Expr>> terms{};
  // Number of infinities.
  std::size_t num_infinities{0};

  // Update the internal product by multiplying on `arg`.
  void multiply_term(const Expr& arg, bool factorize_integers = false);

  // Nuke any terms w/ a zero exponent and normalize powers of integers.
  void normalize_coefficients();

  // Create the resulting multiplication.
  Expr create_multiplication() const;
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
MultiplicationFormattingInfo get_formatting_info(const Multiplication& mul);

}  // namespace math
