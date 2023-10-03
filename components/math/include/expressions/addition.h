// Copyright 2023 Gareth Cross
#pragma once
#include <unordered_map>

#include "absl_imports.h"
#include "constants.h"
#include "expressions/numeric_expressions.h"
#include "hashing.h"
#include "visitor_impl.h"

namespace math {

class Addition {
 public:
  static constexpr std::string_view NameStr = "Addition";
  static constexpr bool IsLeafNode = false;
  using ContainerType = absl::InlinedVector<Expr, 16>;

  // Move-construct.
  explicit Addition(ContainerType&& terms) : terms_(std::move(terms)) {
    ASSERT_GREATER_OR_EQ(terms_.size(), 2);
    // Place into a deterministic (but otherwise arbitrary) order.
    std::sort(terms_.begin(), terms_.end(), [](const Expr& a, const Expr& b) {
      if (a.get_hash() < b.get_hash()) {
        return true;
      } else if (a.get_hash() > b.get_hash()) {
        return false;
      } else {
        // There could be a collision, so we fall back to a slow path here.
        return ExpressionOrderPredicate{}(a, b);
      }
    });
  }

  // Access specific argument.
  const Expr& operator[](const std::size_t i) const { return terms_[i]; }

  // Number of arguments.
  std::size_t arity() const { return terms_.size(); }

  // Iterators.
  ContainerType::const_iterator begin() const { return terms_.begin(); }
  ContainerType::const_iterator end() const { return terms_.end(); }

  // All terms must be identical.
  bool is_identical_to(const Addition& other) const {
    if (arity() != other.arity()) {
      return false;
    }
    return std::equal(begin(), end(), other.begin(), IsIdenticalOperator<Expr>{});
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void iterate(Operation&& operation) const {
    std::for_each(begin(), end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    ContainerType transformed{};
    transformed.reserve(arity());
    std::transform(begin(), end(), std::back_inserter(transformed),
                   std::forward<Operation>(operation));
    return Addition::from_operands(transformed);
  }

  // Construct from a span of operands.
  // The result is automatically simplified, and may not be an addition.
  static Expr from_operands(absl::Span<const Expr> span);

 private:
  ContainerType terms_;
};

template <>
struct hash_struct<Addition> {
  std::size_t operator()(const Addition& add) const { return hash_all(0, add.begin(), add.end()); }
};

// Helper object used to manipulate additions.
struct AdditionParts {
  AdditionParts() = default;

  // Construct and reserve provided capacity.
  explicit AdditionParts(std::size_t capacity) { terms.reserve(capacity); }

  // Construct from existing addition.
  explicit AdditionParts(const Addition& add);

  // Rational coefficient.
  Rational rational_term{0, 1};

  // Floating point coefficient:
  std::optional<Float> float_term{};

  // Map from multiplicand to coefficient.
  std::unordered_map<Expr, Expr, hash_struct<Expr>, IsIdenticalOperator<Expr>> terms{};

  // Update the internal representation by adding `arg`.
  void add_terms(const Expr& arg);

  // Nuke any terms w/ a zero coefficient.
  void normalize_coefficients();

  // Create the resulting addition.
  Expr create_addition() const;
};

}  // namespace math
