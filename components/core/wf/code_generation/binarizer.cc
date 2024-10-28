// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/binarizer.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/flat_hash_set.h>
#include <fmt/ranges.h>
WF_END_THIRD_PARTY_INCLUDES

#include "wf/utility/scoped_trace.h"

namespace wf {

using value_pair = std::tuple<ir::value_ptr, ir::value_ptr>;

inline auto make_value_pair(ir::value_ptr a, ir::value_ptr b) {
  if (a->name() < b->name()) {
    return std::make_tuple(a, b);
  } else {
    return std::make_tuple(b, a);
  }
}

inline bool value_pair_ordered_by_name(const value_pair& a, const value_pair& b) noexcept {
  return std::make_tuple(std::get<0>(a)->name(), std::get<1>(a)->name()) <
         std::make_tuple(std::get<0>(b)->name(), std::get<1>(b)->name());
}

template <>
struct hash_struct<value_pair> {
  constexpr std::size_t operator()(const value_pair& p) const noexcept {
    return static_cast<std::size_t>(std::get<0>(p)->name()) << 32 |
           static_cast<std::size_t>(std::get<1>(p)->name());
  }
};

// Determine the unique values in sorted container `operands`, and write them to output iterator
// `output`.
template <typename Container, typename OutputContainer>
static void count_operand_multiplicity(const Container& operands, OutputContainer& output) {
  WF_ASSERT(std::is_sorted(operands.begin(), operands.end(),
                           [](auto a, auto b) { return a->name() < b->name(); }),
            "operands: [{}]", fmt::join(operands, ", "));

  output.clear();
  for (auto it = operands.begin(); it != operands.end();) {
    const ir::value_ptr val = *it;

    // Find the next unique operand:
    const auto next =
        std::find_if(it, operands.end(), [val](const ir::const_value_ptr v) { return v != val; });
    const std::size_t count = static_cast<std::size_t>(std::distance(it, next));

    // Save the operand and its count:
    output.emplace_back(val, count);
    it = next;
  }
}

using pair_table =
    std::unordered_map<value_pair, vector_of_multiplicities, hash_struct<value_pair>>;

// TODO: This method of counting over counts some times of pairs.
// For example, in the multiplication:
//  v0 = x*x*x*x*y
// There are two instances of x*x, and one instance of x*y.
// We should not double count all the permutations of `x`, but we presently do.
template <typename Container>
static void count_operand_pairs(const Container& container, const binarize_type operation,
                                const std::unordered_set<std::uint32_t>& active_values,
                                pair_table& table) {
  table.reserve(container.size());

  std::vector<std::tuple<ir::value_ptr, std::size_t>> operands_with_multiplicities{};
  for (const ir::value_ptr val : container) {
    const bool is_suitable_operation = (operation == binarize_type::mul && val->is_op<ir::mul>()) ||
                                       (operation == binarize_type::add && val->is_op<ir::add>());
    if (!is_suitable_operation || val->is_unused()) {
      // We only binarize multiplications and additions. Unused values are discarded later.
      continue;
    }
    if (active_values.count(val->name()) == 0) {
      // This value was considered on a previous invocation of binarize_operations and does not
      // need to be recounted.
      continue;
    }

    // Count multiplicities of all arguments to this
    count_operand_multiplicity(val->operands(), operands_with_multiplicities);

    for (std::size_t i = 0; i < operands_with_multiplicities.size(); ++i) {
      const auto [a, a_count] = operands_with_multiplicities[i];
      if (a_count > 1) {
        // For example: x*x*x*x (count of 4) admits two unique x*x multiplications: (x*x)*(x*x)
        // x*x*x*x*x*x (count of 6) admits three: (x*x)*(x*x)*(x*x)
        // x*x*x (count of three) admits only one: (x*x)*x
        table[make_value_pair(a, a)].emplace_back(val, a_count / 2);
      }

      for (std::size_t j = i + 1; j < operands_with_multiplicities.size(); ++j) {
        const auto [b, b_count] = operands_with_multiplicities[j];
        WF_ASSERT(a != b);  //  These should be unique after `count_operand_multiplicity`.

        // Take the min. For example: x*x*y can form one x*y multiplication, not two.
        // x*x*y*y --> min(2, 2) --> 2
        table[make_value_pair(a, b)].emplace_back(val, std::min(a_count, b_count));
      }
    }
  }
}

inline std::size_t total_of_multiplicities(const vector_of_multiplicities& vec) noexcept {
  return std::accumulate(vec.begin(), vec.end(), std::size_t{0},
                         [](const std::size_t total, const value_multiplicity& mul) {
                           return total + mul.multiplicity;
                         });
}

// NOTE: This algorithm is approximate because we count all the pairs once at the start, and then
// edit the graph. Pair counts aren't changed as we edit the graph of operations. So some pairs will
// not apply by the time we reach them.
std::vector<value_pair_with_total_multiplicity> identify_operations_to_binarize(
    const absl::Span<const ir::value_ptr> operations,
    const std::unordered_set<std::uint32_t>& active_values, const binarize_type operation) {
  WF_FUNCTION_TRACE();

  // Build a map from pairs: (a, b) --> a vector of every downstream value that consumes them.
  // So if `a * b` appears in values v3, v4, we'll insert [v3, v4] as values for key (a, b).
  pair_table table;
  table.reserve(operations.size());
  count_operand_pairs(operations, operation, active_values, table);

  // Order the table by highest to lowest occurrence:
  std::vector<value_pair_with_total_multiplicity> sorted_table{};
  sorted_table.reserve(table.size());
  for (auto& [key, consumers] : table) {
    // A pair has to show up > 1 time to be worth binarizing individually.
    if (const std::size_t total = total_of_multiplicities(consumers); total > 1) {
      // We won't touch `table` again, so move out of it:
      sorted_table.emplace_back(key, std::move(consumers), total);
    }
  }

  // Sort by total multiplicity descending:
  std::sort(
      sorted_table.begin(), sorted_table.end(),
      [](const value_pair_with_total_multiplicity& a, const value_pair_with_total_multiplicity& b) {
        if (a.total_multiplicity > b.total_multiplicity) {
          return true;
        } else if (b.total_multiplicity > a.total_multiplicity) {
          return false;
        }
        return value_pair_ordered_by_name(a.pair, b.pair);
      });

  return sorted_table;
}

}  // namespace wf
