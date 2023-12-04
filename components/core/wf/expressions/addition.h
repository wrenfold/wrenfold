// Copyright 2023 Gareth Cross
#pragma once
#include <unordered_map>

#include "wf/absl_imports.h"
#include "wf/constants.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"
#include "wf/hashing.h"
#include "wf/visitor_impl.h"

namespace math {

class addition {
 public:
  static constexpr std::string_view name_str = "Addition";
  static constexpr bool is_leaf_node = false;
  using container_type = absl::InlinedVector<Expr, 16>;

  // Move-construct.
  explicit addition(container_type&& terms) : terms_(std::move(terms)) {
    WF_ASSERT_GREATER_OR_EQ(terms_.size(), 2);
    // Place into a deterministic (but otherwise mostly arbitrary) order.
    std::sort(terms_.begin(), terms_.end(), [](const Expr& a, const Expr& b) {
      if (a.get_hash() < b.get_hash()) {
        return true;
      } else if (a.get_hash() > b.get_hash()) {
        return false;
      } else {
        // There could be a collision, so we fall back to a slow path here.
        return expression_order_struct{}(a, b);
      }
    });
  }

  // Access specific argument.
  const Expr& operator[](const std::size_t i) const { return terms_[i]; }

  // Number of arguments.
  std::size_t size() const noexcept { return terms_.size(); }

  // Iterators.
  container_type::const_iterator begin() const noexcept { return terms_.begin(); }
  container_type::const_iterator end() const noexcept { return terms_.end(); }

  // All terms must be identical.
  bool is_identical_to(const addition& other) const {
    if (size() != other.size()) {
      return false;
    }
    return std::equal(begin(), end(), other.begin(), is_identical_struct<Expr>{});
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void for_each(Operation&& operation) const {
    std::for_each(begin(), end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    container_type transformed{};
    transformed.reserve(size());
    std::transform(begin(), end(), std::back_inserter(transformed),
                   std::forward<Operation>(operation));
    return addition::from_operands(transformed);
  }

  // Construct from a span of operands.
  // The result is automatically simplified, and may not be an addition.
  static Expr from_operands(absl::Span<const Expr> span);

 private:
  container_type terms_;
};

template <>
struct hash_struct<addition> {
  std::size_t operator()(const addition& add) const { return hash_all(0, add.begin(), add.end()); }
};

// Helper object used to manipulate additions.
struct addition_parts {
  addition_parts() = default;

  // Construct and reserve provided capacity.
  explicit addition_parts(std::size_t capacity) { terms.reserve(capacity); }

  // Construct from existing addition.
  explicit addition_parts(const addition& add);

  // Rational coefficient.
  Rational rational_term{0, 1};

  // Floating point coefficient:
  std::optional<Float> float_term{};

  // Map from multiplicand to coefficient.
  std::unordered_map<Expr, Expr, hash_struct<Expr>, is_identical_struct<Expr>> terms{};

  // Number of infinities.
  std::size_t num_infinities{0};

  // Update the internal representation by adding `arg`.
  void add_terms(const Expr& arg);

  // Nuke any terms w/ a zero coefficient.
  void normalize_coefficients();

  // Create the resulting addition.
  Expr create_addition() const;
};

}  // namespace math
