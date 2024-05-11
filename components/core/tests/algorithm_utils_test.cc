// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <deque>
#include <list>
#include <vector>

#include <gtest/gtest.h>

#include "wf/algorithm_utils.h"
#include "wf/third_party_imports.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/types/span.h>
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

// Tests for functions in `algorithm_utils`.
namespace wf {

TEST(AlgorithmUtilsTest, TestTransformMap) {
  const std::vector<int> values_in = {1, 2, 3};

  static_assert(detail::supports_reserve<std::vector<int>>::value);
  static_assert(!detail::supports_reserve<std::deque<int>>::value);

  const auto values_out_1 = transform_map<std::vector<int>>(values_in, [](int x) { return x * 2; });
  ASSERT_EQ(std::vector({2, 4, 6}), values_out_1);

  // Try a type that does not support reserve, should still compile:
  const auto values_out_2 = transform_map<std::deque<int>>(values_in, [](int x) { return x + 5; });
  ASSERT_EQ(std::deque({6, 7, 8}), values_out_2);

  // transform a span
  const auto values_out_3 = transform_map<std::vector>(absl::Span<const int>(values_in),
                                                       [](int x) { return std::to_string(x); });
  ASSERT_EQ(std::vector<std::string>({"1", "2", "3"}), values_out_3);
}

TEST(AlgoritmUtilsTest, TestTransformEnumerateMap) {
  const std::vector<int> values_in = {-1, 3, -2, 5};

  const auto values_out_1 = transform_enumerate_map<std::vector<int>>(
      values_in, [](std::size_t index, int value) { return static_cast<int>(index) + value; });
  ASSERT_EQ(std::vector({-1, 4, 0, 8}), values_out_1);

  const auto values_out_2 = transform_enumerate_map<std::list>(
      values_in, [](std::size_t index, int value) { return fmt::format("{}_{}", index, value); });
  ASSERT_EQ(std::list<std::string>({"0_-1", "1_3", "2_-2", "3_5"}), values_out_2);
}

TEST(AlgorithmUtilsTest, TestAnyOf) {
  const std::vector<int> values = {1, 3, 5};
  ASSERT_TRUE(any_of(values, [](int x) { return x == 5; }));
  ASSERT_TRUE(any_of(values, [](int x) { return x > 0; }));
  ASSERT_FALSE(any_of(values, [](int x) { return x < 0; }));
  ASSERT_FALSE(any_of(std::vector<int>(), [](int x) { return x > 0; }));
}

TEST(AlgorithmUtilsTest, TestContains) {
  const std::vector<std::string_view> values = {"alea", "iacta", "est"};
  ASSERT_TRUE(contains(values, "iacta"));
  ASSERT_TRUE(contains(values, "est"));
  ASSERT_FALSE(contains(values, "carthago"));
  ASSERT_FALSE(contains(std::vector<int>{}, 5));
}

TEST(AlgorithmUtilsTest, TestMapEraseIf) {
  std::unordered_map<int, std::string> map = {
      {1, "chicken"}, {2, "cow"}, {5, "horse"}, {6, "mule"}};

  const auto num_erased = map_erase_if(
      map, [](const auto& pair) { return pair.second.find("o") != std::string::npos; });
  ASSERT_EQ(2, num_erased);
  ASSERT_TRUE(map.count(1));
  ASSERT_TRUE(map.count(6));
  ASSERT_FALSE(map.count(2));
  ASSERT_FALSE(map.count(5));

  const auto num_erased_2 = map_erase_if(map, [](const auto& pair) { return pair.first == 1; });
  ASSERT_EQ(1, num_erased_2);
  ASSERT_EQ(1, map.size());
  ASSERT_TRUE(map.count(6));

  const auto num_erased_3 =
      map_erase_if(map, [](const auto& pair) { return pair.second == "missing"; });
  ASSERT_EQ(0, num_erased_3);
}

}  // namespace wf
