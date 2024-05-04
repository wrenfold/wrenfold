// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/operation_counts.h"

#include <algorithm>
#include <numeric>

namespace wf {

operation_counts::operation_counts() noexcept { std::fill_n(counts_.data(), counts_.size(), 0); }

void operation_counts::increment(const operation_counts& other) noexcept {
  std::transform(counts_.cbegin(), counts_.cend(), other.counts_.cbegin(), counts_.begin(),
                 std::plus<std::size_t>{});
}

operation_counts operation_counts::max(const operation_counts& other) const noexcept {
  operation_counts result{};
  std::transform(counts_.cbegin(), counts_.cend(), other.counts_.cbegin(), result.counts_.begin(),
                 [](std::size_t a, std::size_t b) { return std::max(a, b); });
  return result;
}

std::array<std::tuple<operation_count_label, std::size_t>, operation_counts::num_counters>
operation_counts::labels_and_counts() const noexcept {
  std::array<std::tuple<operation_count_label, std::size_t>, num_counters> out{};
  for (std::size_t i = 0; i < num_counters; ++i) {
    out[i] = std::make_tuple(static_cast<operation_count_label>(i), counts_[i]);
  }
  return out;
}

std::size_t operation_counts::total() const noexcept {
  return std::accumulate(counts_.cbegin(), counts_.cend(), static_cast<std::size_t>(0));
}

}  // namespace wf
