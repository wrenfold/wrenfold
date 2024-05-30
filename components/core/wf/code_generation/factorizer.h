// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <bitset>
#include <optional>
#include <vector>

#include "wf/code_generation/static_vector.h"
#include "wf/hashing.h"
#include "wf/third_party_imports.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/inlined_vector.h>
#include <absl/types/span.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {
namespace factorizer_params {
constexpr std::size_t MAX_VARS_OR_TERMS = 64;
constexpr std::size_t MAX_FACTORIZATION_STEPS = 8;
}  // namespace factorizer_params

// We use a fixed size bitset to indicate the presence or absence of a variable in a given term of
// a sum-of-products expression.
using factor_bits = std::bitset<factorizer_params::MAX_VARS_OR_TERMS>;

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
  constexpr std::size_t score() const noexcept { return score_; }

  // Number of factorization steps.
  std::size_t size() const noexcept { return steps_.size(); }

  // Steps to perform in the factorization.
  constexpr const auto& steps() const noexcept { return steps_; }

  // Access i'th step of the factorization.
  const auto& operator[](const std::size_t i) const { return steps_[i]; }

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
  // are being factorized out of. Using `static_vector` here offers a useful speedup
  // over absl::InlinedVector.
  static_vector<std::tuple<factor_bits, factor_bits>, factorizer_params::MAX_FACTORIZATION_STEPS>
      steps_{};

  // # Of multiplications eliminated by his factorization.
  std::size_t score_{0};
};

// Compute factorizations of a sum-of-products expression, and return them ordered by best to worst.
// The "best" factorization is the one that eliminates the largest number of multiplications with
// the smallest number of steps.
//
// The input span `terms` is indexed by the term in the sum. The stored bitsets indicate whether a
// given variable is active in that particular term.
std::vector<factorization> compute_ranked_factorizations(absl::Span<const factor_bits> terms,
                                                         std::size_t num_vars,
                                                         std::size_t branching_factor);

// For use with tests: Convert a list of terms (where each term is a vector of variable indices)
// into a vector of bitsets, where each bit indicates the presence of a variable.
std::tuple<std::vector<factor_bits>, std::size_t> create_term_bitsets(
    absl::Span<const std::vector<std::uint32_t>> terms);

// Hash `factor_bits`.
template <>
struct hash_struct<factor_bits> {
  std::size_t operator()(const factor_bits& b) const noexcept { return b.to_ullong(); }
};

// Hash term in factorization by converting the bitmasks to std::size_t.
template <>
struct hash_struct<std::tuple<factor_bits, factor_bits>> {
  std::size_t operator()(const std::tuple<factor_bits, factor_bits>& f) const noexcept {
    const auto& [vars, terms] = f;
    return hash_combine(hash(vars), hash(terms));
  }
};

// Hash all the steps in a factorization.
template <>
struct hash_struct<factorization> {
  std::size_t operator()(const factorization& f) const noexcept { return hash_all(0, f.steps()); }
};

}  // namespace wf
