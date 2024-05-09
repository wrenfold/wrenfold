// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <bitset>
#include <vector>

#include "wf/assertions.h"
#include "wf/hashing.h"
#include "wf/third_party_imports.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/inlined_vector.h>
#include <absl/types/span.h>
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

constexpr std::size_t MAX_VARS_OR_TERMS = 64;

// Used as a mask to indicate which variables or terms are active in a particular factor.
using factor_bits = std::bitset<MAX_VARS_OR_TERMS>;

// Represents a factorization of a sum-of-products expression.
// Each "step" in the factorization extracts at least one (sometimes many) variables from a subset
// of the terms. Note that not all terms in the input sum will be represented by this object - only
// those that _could_ be partially factorized.
class factorization {
 public:
  factorization() = default;

  // Construct from initializer list of steps:
  explicit factorization(std::initializer_list<std::tuple<factor_bits, factor_bits>> steps);

  // Push a new step into the factorizations.
  // The variables represented by `vars` are factorized out of the terms indicated by `terms`.
  void push(factor_bits vars, factor_bits terms);

  // Number of multiplications eliminated by this factorization.
  std::size_t score() const noexcept;

  // Number of factorization steps.
  std::size_t size() const noexcept { return steps_.size(); }

  // Steps to perform in the factorization.
  constexpr const auto& steps() const noexcept { return steps_; }

  // Access i'th step of the factorization.
  const auto& operator[](const std::size_t i) const {
    WF_ASSERT_LESS(i, steps_.size());
    return steps_[i];
  }

  // Get terms that were not involved in the factorization.
  factor_bits unfactored_terms(std::size_t num_terms) const noexcept;

  // Convert to a string of integers for debugging.
  std::string to_string() const;

  // All steps must match exactly.
  bool operator==(const factorization& other) const noexcept {
    return steps_.size() == other.steps_.size() &&
           std::equal(other.steps_.begin(), other.steps_.end(), steps_.begin());
  }

  // Order lexicographically.
  bool operator<(const factorization& other) const noexcept;

 private:
  // First element of the tuple is the variables, second is the terms those variables
  // are being factorized out of.
  absl::InlinedVector<std::tuple<factor_bits, factor_bits>, 8> steps_{};
};

// Compute possible factorizations of the specified sum of products.
// The sum is provided as a span over terms, where each term can contain one or more variables.
std::vector<factorization> compute_factorizations(absl::Span<const factor_bits> terms,
                                                  std::size_t num_vars);

// Variation that accepts terms as a span-of-vectors-of-indices. For use in tests.
std::vector<factorization> compute_factorizations(
    absl::Span<const std::vector<std::uint32_t>> terms, std::size_t num_vars);

// Hash term in factorization by converting the bitmasks to std::size_t.
template <>
struct hash_struct<std::tuple<factor_bits, factor_bits>> {
  std::size_t operator()(const std::tuple<factor_bits, factor_bits>& f) const noexcept {
    const auto& [vars, terms] = f;
    return hash_combine(vars.to_ullong(), terms.to_ullong());
  }
};

// Hash all the steps in a factorization.
template <>
struct hash_struct<factorization> {
  std::size_t operator()(const factorization& f) const noexcept { return hash_all(0, f.steps()); }
};

// Iterate over positions of active bits in a bitset.
class bit_range {
 public:
  class const_iterator {
   public:
    using difference_type = std::ptrdiff_t;
    using value_type = std::size_t;
    using pointer = void;
    using reference = void;
    using iterator_category = std::input_iterator_tag;

    bool operator==(const const_iterator& other) const noexcept {
      return pos_ == other.pos_ && b_ == other.b_;
    }

    bool operator!=(const const_iterator& other) const noexcept { return !operator==(other); }

    const_iterator& operator++() noexcept {
      ++pos_;
      advance();
      return *this;
    }

    const_iterator operator++(int) noexcept {
      const const_iterator copy = *this;
      ++pos_;
      return copy;
    }

    constexpr std::size_t operator*() const noexcept { return pos_; }

    const_iterator(const factor_bits b, const std::size_t pos) noexcept : b_(b), pos_(pos) {
      advance();
    }

   private:
    void advance() {
      for (; pos_ < b_.size() && !b_[pos_]; ++pos_) {
      }
    }
    factor_bits b_;
    std::size_t pos_;
  };

  constexpr explicit bit_range(const factor_bits b) noexcept : b_(b) {}

  auto begin() const noexcept { return const_iterator{b_, 0}; }
  auto end() const noexcept { return const_iterator{b_, b_.size()}; }
  auto cbegin() const noexcept { return begin(); }
  auto cend() const noexcept { return end(); }

 private:
  factor_bits b_;
};

}  // namespace wf

template <>
struct fmt::formatter<wf::bit_range, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::bit_range& range, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "[{}]", fmt::join(range, ", "));
  }
};
