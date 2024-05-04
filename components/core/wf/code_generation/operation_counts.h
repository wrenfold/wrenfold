// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <array>
#include <string_view>

#include "wf/assertions.h"

namespace wf {

// Different categories of operations we count in order to generate summaries for the user.
enum class operation_count_label {
  add = 0,
  branch,
  call,
  compare,
  divide,
  multiply,
  negate,
  MAX_VALUE,
};

// Store a count of operations in an array.
class operation_counts {
 public:
  static constexpr std::size_t num_counters =
      static_cast<std::size_t>(operation_count_label::MAX_VALUE);

  // Zero initialize counts.
  operation_counts() noexcept;

  // Increment all counters by the values in `other`.
  void increment(const operation_counts& other) noexcept;

  // Take the maximum of self and another set of counts.
  operation_counts max(const operation_counts& other) const noexcept;

  // Const access to a counter.
  constexpr const std::size_t& operator[](const operation_count_label label) const {
    WF_ASSERT_LESS(static_cast<std::size_t>(label), counts_.size());
    return counts_[static_cast<std::size_t>(label)];
  }

  // Access a counter to update it.
  constexpr std::size_t& operator[](const operation_count_label label) {
    WF_ASSERT_LESS(static_cast<std::size_t>(label), counts_.size());
    return counts_[static_cast<std::size_t>(label)];
  }

  // Get array of labels and counts.
  std::array<std::tuple<operation_count_label, std::size_t>, num_counters> labels_and_counts()
      const noexcept;

  // Total of all the counters.
  std::size_t total() const noexcept;

 private:
  std::array<std::size_t, num_counters> counts_;
};

// Convert operation_count_label to string.
inline constexpr std::string_view string_from_operation_count_label(
    const operation_count_label label) noexcept {
  switch (label) {
    case operation_count_label::add:
      return "add";
    case operation_count_label::branch:
      return "branch";
    case operation_count_label::call:
      return "call";
    case operation_count_label::compare:
      return "compare";
    case operation_count_label::divide:
      return "divide";
    case operation_count_label::multiply:
      return "multiply";
    case operation_count_label::negate:
      return "negate";
    default:
      break;
  }
  return "<NOT A VALID ENUM VALUE>";
}

}  // namespace wf
