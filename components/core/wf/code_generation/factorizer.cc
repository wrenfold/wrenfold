// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/factorizer.h"

#include <numeric>

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/flat_hash_set.h>
WF_END_THIRD_PARTY_INCLUDES

#include "wf/assertions.h"
#include "wf/code_generation/bitset_range.h"
#include "wf/scoped_trace.h"
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
  score_ = std::accumulate(
      steps_.begin(), steps_.end(), static_cast<std::size_t>(0),
      [](const std::size_t total, const std::tuple<factor_bits, factor_bits>& step) {
        const auto& [vars, terms] = step;
        return total + vars.count() * (terms.count() - 1);
      });
}

void factorization::push(const factor_bits vars, const factor_bits terms) {
#ifdef WF_DEBUG
  WF_ASSERT(vars.count() && terms.count(), "vars = {}, terms = {}", bitset_range(vars),
            bitset_range(terms));

  // Check invariant: The same variables cannot be factorized from the same term more
  // than once:
  for (const auto& [existing_vars, existing_terms] : steps_) {
    WF_ASSERT((existing_vars & vars).count() == 0 || (existing_terms & terms).count() == 0,
              "existing_vars = {}, existing_terms = {}, vars = {}, terms = {}",
              bitset_range(existing_vars), bitset_range(existing_terms), bitset_range(vars),
              bitset_range(terms));
  }
#endif

  score_ += vars.count() * (terms.count() - 1);

  // Insert and preserve order so we can easily compare factorizations for equality.
  const auto tuple = std::make_tuple(vars, terms);
  steps_.insert(std::lower_bound(steps_.begin(), steps_.end(), tuple, order_bit_tuples{}), tuple);
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
    return fmt::format("({}, {})", bitset_range(vars), bitset_range(terms));
  });
}

bool factorization::operator<(const factorization& other) const noexcept {
  return std::lexicographical_compare(steps_.begin(), steps_.end(), other.steps_.begin(),
                                      other.steps_.end(), order_bit_tuples{});
}

// Store the table mapping from variables -> terms, and its transpose: terms -> variable.
class factors_table {
 public:
  factors_table(const absl::Span<const factor_bits> vars_to_terms,
                const absl::Span<const factor_bits> terms_to_vars) noexcept
      : vars_to_terms_(vars_to_terms), terms_to_vars_(terms_to_vars) {}

  constexpr std::size_t num_vars() const noexcept { return vars_to_terms_.size(); }
  constexpr std::size_t num_terms() const noexcept { return terms_to_vars_.size(); }

  // Get the column associated with a variable.
  constexpr const factor_bits& get_var(const std::size_t pos) const { return vars_to_terms_[pos]; }

  // Get the row associated with a term.
  constexpr const factor_bits& get_term(const std::size_t pos) const { return terms_to_vars_[pos]; }

 private:
  absl::Span<const factor_bits> vars_to_terms_;
  absl::Span<const factor_bits> terms_to_vars_;
};

// Of the remaining variables in `remaining_vars`, compute the subset that appear in
// the same terms as `var_index`. We only consider terms in `remaining_terms`.
// If the chosen variable appears in only one term, we return an empty set of bits.
inline std::tuple<factor_bits, factor_bits> find_jointly_factorizable_variables(
    const std::size_t var_index, const factors_table& table, const factor_bits remaining_terms,
    const factor_bits remaining_vars) {
  WF_ASSERT_LESS(var_index, table.num_vars());

  // Identify terms that contain this variable:
  const factor_bits suitable_terms = remaining_terms & table.get_var(var_index);
  if (suitable_terms.count() <= 1) {
    return std::make_tuple(factor_bits(), suitable_terms);
  }

  // Identify all the variables that also appear in the varibles indicated by `suitable_terms`:
  factor_bits suitable_vars = remaining_vars;
  for (std::size_t i = 0; i < table.num_terms(); ++i) {
    if (suitable_terms[i]) {
      suitable_vars &= table.get_term(i);
    }
  }
  return std::make_tuple(suitable_vars, suitable_terms);
}

struct scored_factorization_step {
  std::uint32_t variable_index;
  std::uint32_t score;
  std::tuple<factor_bits, factor_bits> step;

  constexpr const factor_bits& factorized_vars() const noexcept { return std::get<0>(step); }
  constexpr const factor_bits& factorized_terms() const noexcept { return std::get<1>(step); }
};

// Given the set of remaining variables, extract the top `max_to_return` factorization steps.
// Steps are ranked by the # of multiplications they eliminate.
inline auto rank_factorization_steps(const factors_table& table, const factor_bits& remaining_vars,
                                     const factor_bits& remaining_terms,
                                     const std::size_t max_to_return) {
  absl::InlinedVector<scored_factorization_step, 8> ranked{};
  ranked.reserve(std::min(remaining_vars.count(), max_to_return));

  for (std::size_t v = 0; v < table.num_vars(); ++v) {
    if (!remaining_vars[v]) {
      continue;
    }

    const auto [factored_vars, factored_terms] =
        find_jointly_factorizable_variables(v, table, remaining_terms, remaining_vars);
    if (factored_vars.none()) {
      continue;
    }

    // Total number of multiplications saved by this step of the factorization:
    const std::size_t score = factored_vars.count() * (factored_terms.count() - 1);

    // Insert in order:
    const auto it = std::lower_bound(
        ranked.begin(), ranked.end(), score,
        [](const scored_factorization_step& step, const std::size_t s) { return step.score > s; });

    if (it != ranked.end() && std::get<0>(it->step) == factored_vars) {
      // Aready have one with the same set of factored variables - don't add another.
      // This can occur if two variables always occur together.
      continue;
    }

    ranked.insert(it, scored_factorization_step{static_cast<std::uint32_t>(v),
                                                static_cast<std::uint32_t>(score),
                                                std::make_tuple(factored_vars, factored_terms)});
    if (ranked.size() > max_to_return) {
      ranked.resize(max_to_return);
    }
  }

  return ranked;
}

static void factorize_and_recurse(const scored_factorization_step& step, const factors_table& table,
                                  factor_bits remaining_vars, factor_bits remaining_terms,
                                  const factorization& parent, const std::size_t branching_factor,
                                  factorization_set& visited, std::vector<factorization>& outputs) {
  factorization result{parent};
  result.push(step.factorized_vars(), step.factorized_terms());

  if (const auto [_, was_inserted] = visited.insert(result); !was_inserted) {
    // Already visited.
    return;
  }

  // Clear the rows and variables we extracted:
  remaining_terms &= ~step.factorized_terms();
  remaining_vars.reset(step.variable_index);

  if (remaining_terms.none() || result.size() == MAX_FACTORIZATION_STEPS) {
    // This is a leaf node.
    outputs.push_back(std::move(result));
    return;
  }

  const auto ranked_steps =
      rank_factorization_steps(table, remaining_vars, remaining_terms, branching_factor);

  if (ranked_steps.empty()) {
    outputs.push_back(std::move(result));  //  Leaf node.
    return;
  }

  for (const auto& next_step : ranked_steps) {
    factorize_and_recurse(next_step, table, remaining_vars, remaining_terms, result,
                          branching_factor, visited, outputs);
  }
}

// Fill the first `num` bits.
inline factor_bits fill_bits(const std::size_t num) {
  factor_bits result{};
  for (std::size_t n = 0; n < num; ++n) {
    result.set(n, true);
  }
  return result;
}

std::vector<factorization> compute_ranked_factorizations(const absl::Span<const factor_bits> terms,
                                                         const std::size_t num_vars,
                                                         const std::size_t branching_factor) {
  WF_FUNCTION_TRACE();
  WF_ASSERT_LESS_OR_EQ(num_vars, MAX_VARS_OR_TERMS);
  WF_ASSERT_LESS_OR_EQ(terms.size(), MAX_VARS_OR_TERMS);

  std::vector<factorization> result{};
  if (terms.empty() || num_vars == 0) {
    return result;
  }

  // Transpose the input table.
  // `terms` is over terms in the sum (row-major).
  // `vars_to_terms` is over variables (column-major), the individual bitmasks are over terms:
  absl::InlinedVector<factor_bits, 16> vars_to_terms{};
  vars_to_terms.resize(num_vars, factor_bits());
  for (std::size_t i = 0; i < terms.size(); ++i) {
    for (std::size_t var = 0; var < num_vars; ++var) {
      if (terms[i][var]) {
        vars_to_terms[var].set(i);
      }
    }
  }

  const factors_table table{vars_to_terms, terms};

  const auto remaining_vars = fill_bits(vars_to_terms.size());
  const auto remaining_terms = fill_bits(terms.size());
  const auto ranked_steps =
      rank_factorization_steps(table, remaining_vars, remaining_terms, branching_factor);

  if (ranked_steps.empty()) {
    return result;
  }

  // I tried the PMR set here with monotonic buffer, but it had no impact.
  factorization_set visited{};
  visited.reserve(32);

  result.reserve(32);
  for (const auto& next_step : ranked_steps) {
    factorize_and_recurse(next_step, table, remaining_vars, remaining_terms, factorization{},
                          branching_factor, visited, result);
  }

  std::stable_sort(result.begin(), result.end(),
                   [&](const factorization& a, const factorization& b) {
                     // Prefer higher scores, then prefer lower number of factorizations.
                     if (a.score() > b.score()) {
                       return true;
                     } else if (a.score() < b.score()) {
                       return false;
                     }
                     if (a.size() < b.size()) {
                       return true;
                     }
                     return false;
                   });
  return result;
}

std::tuple<std::vector<factor_bits>, std::size_t> create_term_bitsets(
    const absl::Span<const std::vector<std::uint32_t>> terms) {
  std::uint32_t max_var{0};
  for (const auto& vec : terms) {
    WF_ASSERT(!vec.empty());
    max_var = std::max(max_var, *std::max_element(vec.begin(), vec.end()));
  }

  std::vector<factor_bits> terms_bits{};
  terms_bits.resize(terms.size(), factor_bits());

  // Convert table format:
  for (std::size_t i = 0; i < terms.size(); ++i) {
    for (const std::size_t var_index : terms[i]) {
      terms_bits[i].set(var_index);
    }
  }

  // +1 because we want the count, not the max.
  return std::make_tuple(std::move(terms_bits), max_var + 1);
}

}  // namespace wf
