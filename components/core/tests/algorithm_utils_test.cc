#include <deque>
#include <list>
#include <vector>

#include <gtest/gtest.h>

#include "wf/absl_imports.h"
#include "wf/algorithm_utils.h"
#include "wf/fmt_imports.h"

// Tests for functions in `algorithm_utils`.
namespace wf {

TEST(AlgorithmUtilsTest, TestTransformMap) {
  const std::vector<int> values_in = {1, 2, 3};

  static_assert(detail::supports_reserve<std::vector<int>>::value);
  static_assert(!detail::supports_reserve<std::deque<int>>::value);

  const auto values_out_1 = transform_map<std::vector<int>>(values_in, [](int x) { return x * 2; });
  ASSERT_EQ(std::vector<int>({2, 4, 6}), values_out_1);

  // Try a type that does not support reserve, should still compile:
  const auto values_out_2 = transform_map<std::deque<int>>(values_in, [](int x) { return x + 5; });
  ASSERT_EQ(std::deque<int>({6, 7, 8}), values_out_2);

  // transform a span
  const auto values_out_3 = transform_map<std::vector>(absl::Span<const int>(values_in),
                                                       [](int x) { return std::to_string(x); });
  ASSERT_EQ(std::vector<std::string>({"1", "2", "3"}), values_out_3);
}

TEST(AlgoritmUtilsTest, TestTransformEnumerateMap) {
  const std::vector<int> values_in = {-1, 3, -2, 5};

  const auto values_out_1 = transform_enumerate_map<std::vector<int>>(
      values_in, [](std::size_t index, int value) { return static_cast<int>(index) + value; });
  ASSERT_EQ(std::vector<int>({-1, 4, 0, 8}), values_out_1);

  const auto values_out_2 = transform_enumerate_map<std::list>(
      values_in, [](std::size_t index, int value) { return fmt::format("{}_{}", index, value); });
  ASSERT_EQ(std::list<std::string>({"0_-1", "1_3", "2_-2", "3_5"}), values_out_2);
}

}  // namespace wf
