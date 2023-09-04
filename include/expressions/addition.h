// Copyright 2023 Gareth Cross
#pragma once
#include <unordered_map>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4127)  // conditional expression is constant
#pragma warning(disable : 4100)  // unreferenced formal parameter
#pragma warning(disable : 4324)  // padded for alignment
#endif                           // _MSC_VER
#include <absl/container/inlined_vector.h>
#include <absl/types/span.h>
#ifdef _MSC_VER
#pragma warning(pop)
#endif  //  _MSC_VER

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
  }

  // Access specific argument.
  const Expr& operator[](const std::size_t i) const { return terms_[i]; }

  // Number of arguments.
  std::size_t Arity() const { return terms_.size(); }

  // Iterators.
  ContainerType::const_iterator begin() const { return terms_.begin(); }
  ContainerType::const_iterator end() const { return terms_.end(); }

  // All terms must be identical.
  bool IsIdenticalTo(const Addition& other) const {
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
    return Addition::FromOperands(transformed);
  }

  // Construct from a span of operands.
  // The result is automatically simplified, and may not be an addition.
  static Expr FromOperands(absl::Span<const Expr> span);

 private:
  ContainerType terms_;
};

template <>
struct Hash<Addition> {
  std::size_t operator()(const Addition& add) const { return HashAll(0, add.begin(), add.end()); }
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
  std::unordered_map<Expr, Expr, Hash<Expr>, IsIdenticalOperator<Expr>> terms{};

  // Update the internal representation by adding `arg`.
  void Add(const Expr& arg);

  // Nuke any terms w/ a zero exponent and normalize powers of integers.
  void Normalize();

  // Create the resulting addition.
  Expr CreateAddition() const;
};

}  // namespace math
