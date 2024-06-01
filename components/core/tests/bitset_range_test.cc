// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <gtest/gtest.h>

#include "wf/utility/bitset_range.h"

// Tests for the bitset_range type.
namespace wf {

// AppleClang won't accept assign(begin, end) here for some reason.
// I don't care enough to understand this right now - gcc and msvc are fine with it.
template <std::size_t N>
auto vector_from_range(bitset_range<N> range) {
  std::vector<std::size_t> values{};
  for (auto value : range) {
    values.push_back(value);
  }
  return values;
}

TEST(BitsetRangeTest, TestEmpty) {
  const std::bitset<32> zeros{};
  const auto zero_range = bitset_range(zeros);
  ASSERT_EQ(std::vector<std::size_t>(), vector_from_range(zero_range));
}

TEST(BitsetRangeTest, TestSomeFilled) {
  std::bitset<32> bits{};
  bits.set(3);
  bits.set(22);
  bits.set(30);
  const auto range = bitset_range(bits);
  const std::vector<std::size_t> expected{3, 22, 30};
  ASSERT_EQ(expected, vector_from_range(range));
}

TEST(BitsetRangeTest, TestFull) {
  std::bitset<12> bits{};
  bits.set();
  const auto range = bitset_range(bits);
  const std::vector<std::size_t> expected{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
  ASSERT_EQ(expected, vector_from_range(range));
}

}  // namespace wf
