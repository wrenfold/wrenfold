// Copyright 2023 Gareth Cross
#pragma once
#include <unordered_map>

#include "wf/algorithm_utils.h"
#include "wf/constants.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"
#include "wf/hashing.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/inlined_vector.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

class addition {
 public:
  static constexpr std::string_view name_str = "Addition";
  static constexpr bool is_leaf_node = false;
  using container_type = absl::InlinedVector<scalar_expr, 16>;
  struct no_sort {};

  // Move-construct.
  explicit addition(container_type&& terms) : terms_(std::move(terms)) {
    WF_ASSERT_GREATER_OR_EQ(terms_.size(), 2);
    sort_terms();
  }

  // Move-construct and do not sort.
  explicit addition(no_sort, container_type&& terms) : terms_(std::move(terms)) {
    WF_ASSERT_GREATER_OR_EQ(terms_.size(), 2);
  }

  // Access specific argument.
  const scalar_expr& operator[](const std::size_t i) const {
    WF_ASSERT_LESS(i, terms_.size());
    return terms_[i];
  }

  // Number of arguments.
  std::size_t size() const noexcept { return terms_.size(); }

  // Iterators.
  container_type::const_iterator begin() const noexcept { return terms_.begin(); }
  container_type::const_iterator end() const noexcept { return terms_.end(); }

  // Get span over terms.
  absl::Span<const scalar_expr> as_span() const noexcept { return {terms_}; }

  // Get terms in the addition, sorted into canonical order.
  std::vector<scalar_expr> sorted_terms() const;

  // Implement ExpressionImpl::Map
  template <typename Operation>
  scalar_expr map_children(Operation&& operation) const {
    const container_type transformed =
        transform_map<container_type>(terms_, std::forward<Operation>(operation));
    return from_operands(transformed);
  }

  // Construct from a span of operands.
  // The result is automatically simplified, and may not be an addition.
  static scalar_expr from_operands(absl::Span<const scalar_expr> args);

 private:
  void sort_terms();

  container_type terms_;
};

template <>
struct hash_struct<addition> {
  std::size_t operator()(const addition& add) const { return hash_all(0, add.begin(), add.end()); }
};

template <>
struct is_identical_struct<addition> {
  bool operator()(const addition& a, const addition& b) const {
    return a.size() == b.size() &&
           std::equal(a.begin(), a.end(), b.begin(), is_identical_struct<scalar_expr>{});
  }
};

template <>
struct order_struct<addition> {
  relative_order operator()(const addition& a, const addition& b) const {
    return wf::lexicographical_order(a, b, order_struct<scalar_expr>{});
  }
};

// Helper object used to manipulate additions.
struct addition_parts {
  addition_parts() = default;

  // Construct and reserve provided capacity.
  explicit addition_parts(const std::size_t capacity) { terms.reserve(capacity); }

  // Construct from existing addition.
  explicit addition_parts(const addition& add);

  // Rational coefficient.
  rational_constant rational_term{0, 1};

  // Floating point coefficient:
  std::optional<float_constant> float_term{};

  // Map from multiplicand to coefficient.
  std::unordered_map<scalar_expr, scalar_expr, hash_struct<scalar_expr>,
                     is_identical_struct<scalar_expr>>
      terms{};

  // Number of infinities.
  std::size_t num_infinities{0};

  // Update the internal representation by adding `arg`.
  void add_terms(const scalar_expr& arg);

  // Nuke any terms w/ a zero coefficient.
  void normalize_coefficients();

  // Create the resulting addition.
  scalar_expr create_addition() const;
};

}  // namespace wf
