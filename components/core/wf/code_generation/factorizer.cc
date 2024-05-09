// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/factorizer.h"

#include <numeric>
#include <unordered_set>

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/flat_hash_set.h>
WF_END_THIRD_PARTY_INCLUDES

#include "wf/algorithm_utils.h"
#include "wf/assertions.h"
#include "wf/string_utils.h"

namespace wf {

using factorization_set = absl::flat_hash_set<factorization, hash_struct<factorization>>;

// Sort tuples of [var, term] bitmasks by converting to tuples of unsigned integers.
struct order_bit_tuples {
  bool operator()(const std::tuple<factor_bits, factor_bits>& a,
                  const std::tuple<factor_bits, factor_bits>& b) const {
    const auto& [vars_a, terms_a] = a;
    const auto& [vars_b, terms_b] = b;
    return std::make_tuple(vars_a.to_ullong(), terms_a.to_ullong()) <
           std::make_tuple(vars_b.to_ullong(), terms_b.to_ullong());
  }
};

factorization::factorization(
    const std::initializer_list<std::tuple<factor_bits, factor_bits>> steps)
    : steps_(steps) {
  std::sort(steps_.begin(), steps_.end(), order_bit_tuples{});
}

void factorization::push(const factor_bits vars, const factor_bits terms) {
#if WF_DEBUG
  WF_ASSERT(vars.count() && terms.count(), "vars = {}, terms = {}", bit_range(vars),
            bit_range(terms));

  // Check invariant: The same variables cannot be factorized from the same term more
  // than once:
  for (const auto& [existing_vars, existing_terms] : steps_) {
    WF_ASSERT((existing_vars & vars).count() == 0 || (existing_terms & terms).count() == 0,
              "existing_vars = {}, existing_terms = {}, vars = {}, terms = {}",
              bit_range(existing_vars), bit_range(existing_terms), bit_range(vars),
              bit_range(terms));
  }
#endif

  // Insert and preserve order so we can easily compare factorizations for equality.
  const auto tuple = std::make_tuple(vars, terms);
  steps_.insert(std::lower_bound(steps_.begin(), steps_.end(), tuple, order_bit_tuples{}), tuple);

#if WF_DEBUG
  WF_ASSERT(std::is_sorted(steps_.begin(), steps_.end(), order_bit_tuples{}));
#endif
}

std::size_t factorization::score() const noexcept {
  return std::accumulate(
      steps_.begin(), steps_.end(), static_cast<std::size_t>(0),
      [](const std::size_t total, const std::tuple<factor_bits, factor_bits>& step) {
        const auto& [vars, terms] = step;
        return total + vars.count() * (terms.count() - 1);
      });
}

factor_bits factorization::unfactored_terms(const std::size_t num_terms) const noexcept {
  factor_bits result{};
  for (std::size_t i = 0; i < num_terms; ++i) {
    result.set(i);
  }
  for (const auto& [_, terms] : steps_) {
    result &= ~terms;
  }
  return result;
}

std::string factorization::to_string() const {
  return join(", ", steps(), [](const std::tuple<factor_bits, factor_bits>& step) {
    const auto& [vars, terms] = step;
    return fmt::format("({}, {})", bit_range(vars), bit_range(terms));
  });
}

bool factorization::operator<(const factorization& other) const noexcept {
  return std::lexicographical_compare(steps_.begin(), steps_.end(), other.steps_.begin(),
                                      other.steps_.end(), order_bit_tuples{});
}

inline factor_bits get_mask_for_row(const absl::Span<const factor_bits> table,
                                    const std::size_t term_index) {
  factor_bits output{};
  for (std::size_t j = 0; j < table.size(); ++j) {
    output[j] = table[j][term_index];
  }
  return output;
}

static bool generate_factorizations(const std::size_t var_index,
                                    const absl::Span<const factor_bits> table,
                                    const std::size_t num_terms, factor_bits remaining_vars,
                                    factor_bits remaining_terms, const factorization& parent,
                                    factorization_set& outputs) {
  WF_ASSERT_LESS(var_index, table.size());
  if (remaining_terms.none()) {
    return false;
  }

  // Identify terms that contain this variable:
  const factor_bits suitable_terms = remaining_terms & table[var_index];
  if (suitable_terms.count() <= 1) {
    return false;
  }

  // Identify all the variables that also appear in the varibles indicated by `suitable_terms`:
  factor_bits suitable_vars = remaining_vars;
  for (std::size_t i = 0; i < num_terms; ++i) {
    if (suitable_terms[i]) {
      suitable_vars &= get_mask_for_row(table, i);
    }
  }

  WF_ASSERT_GREATER_OR_EQ(suitable_vars.count(), 1);

  // Append this step to the factorization:
  factorization result{parent};
  result.push(suitable_vars, suitable_terms);

  // Clear the rows and variables we extracted:
  remaining_terms &= ~suitable_terms;
  remaining_vars.reset(var_index);

  if (remaining_terms.none()) {
    outputs.insert(std::move(result));
    return true;
  }

  // If a subset of the remaining variables appear in identical terms, we can drop the
  // "duplicate" variables and terminate early. For example, if all the remaining variables
  // appear in all the same terms, there is only one additional thing we can add to the
  // factorization.
  absl::InlinedVector<factor_bits, 16> exists{};
  factor_bits vars_to_recurse = remaining_vars;
  for (std::size_t j = 0; j < table.size(); ++j) {
    if (vars_to_recurse[j]) {
      // O(N^2) check here in the `exists` set, but it should be pretty small N.
      if (const factor_bits mask = table[j] & remaining_terms; contains(exists, mask)) {
        vars_to_recurse.reset(j);  //  Remove it from consideration.
      } else {
        exists.push_back(mask);
      }
    }
  }

  bool added_child = false;
  for (std::size_t j = 0; j < table.size(); ++j) {
    // Recurse on the remaining variables.
    if (vars_to_recurse[j]) {
      added_child |= generate_factorizations(j, table, num_terms, remaining_vars, remaining_terms,
                                             result, outputs);
    }
  }

  // If there were no further factorizations possible, we are a leaf node. We only want the leaf
  // nodes, since these represent the "most factorized terms".
  if (!added_child) {
    outputs.insert(std::move(result));
  }
  return true;
}

// Fill the first `num` bits.
inline factor_bits fill_bits(const std::size_t num) {
  factor_bits result{};
  for (std::size_t n = 0; n < num; ++n) {
    result.set(n, true);
  }
  return result;
}

std::vector<factorization> compute_factorizations(const absl::Span<const factor_bits> terms,
                                                  const std::size_t num_vars) {
  WF_ASSERT_LESS_OR_EQ(num_vars, MAX_VARS_OR_TERMS);
  WF_ASSERT_LESS_OR_EQ(terms.size(), MAX_VARS_OR_TERMS);

  if (terms.empty() || num_vars == 0) {
    return {};
  }

  // Transpose the input table.
  // `terms` is over terms in the sum (row-major).
  // `table` is over variables (column-major), the individual bitmasks are over terms:
  absl::InlinedVector<factor_bits, 16> table{};
  table.resize(num_vars, factor_bits());
  for (std::size_t i = 0; i < terms.size(); ++i) {
    for (std::size_t var = 0; var < num_vars; ++var) {
      if (terms[i][var]) {
        table[var].set(i);
      }
    }
  }

  // I tried the PMR set here with monotonic buffer, but it had no impact.
  factorization_set output_set{};
  output_set.reserve(32);

  for (std::size_t j = 0; j < num_vars; ++j) {
    generate_factorizations(j, table, terms.size(), fill_bits(num_vars), fill_bits(terms.size()),
                            factorization{}, output_set);
  }

  // Sort results by
  std::vector<factorization> ordered_results{std::make_move_iterator(output_set.begin()),
                                             std::make_move_iterator(output_set.end())};
  std::sort(ordered_results.begin(), ordered_results.end(),
            [](const factorization& a, const factorization& b) {
              // Prefer higher scores, then prefer lower number of factorizations.
              if (a.score() > b.score()) {
                return true;
              } else if (a.score() < b.score()) {
                return false;
              }
              if (a.size() < b.size()) {
                return true;
              } else if (a.size() > b.size()) {
                return false;
              }
              // Fall back to lexicographical order.
              return a < b;
            });
  return ordered_results;
}

std::vector<factorization> compute_factorizations(
    const absl::Span<const std::vector<std::uint32_t>> terms, const std::size_t num_vars) {
  absl::InlinedVector<factor_bits, 16> terms_bits{};
  terms_bits.resize(terms.size(), factor_bits());

  // Convert table format:
  for (std::size_t i = 0; i < terms.size(); ++i) {
    for (const std::size_t var_index : terms[i]) {
      terms_bits[i].set(var_index);
    }
  }

  return compute_factorizations(terms_bits, num_vars);
}

}  // namespace wf
