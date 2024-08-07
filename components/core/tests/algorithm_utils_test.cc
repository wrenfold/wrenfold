// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <deque>
#include <list>
#include <vector>

#include <gtest/gtest.h>

#include "wf/utility/algorithms.h"
#include "wf/utility/third_party_imports.h"

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

  // Test tuple version:
  constexpr std::tuple<int, float, unsigned int> values_tuple{1, -3.1f, 5};
  ASSERT_TRUE(any_of(values_tuple, [](auto v) { return v < 0; }));
  ASSERT_FALSE(any_of(values_tuple, [](auto v) { return v == 2; }));
}

TEST(AlgorithmUtilsTest, TestContains) {
  const std::vector<std::string_view> values = {"alea", "iacta", "est"};
  ASSERT_TRUE(contains(values, "iacta"));
  ASSERT_TRUE(contains(values, "est"));
  ASSERT_FALSE(contains(values, "carthago"));
  ASSERT_FALSE(contains(std::vector<int>{}, 5));
}

TEST(AlgorithmUtilsTest, TestAllOf) {
  const std::vector<int> values = {3, 6, 8, 9, 22};
  ASSERT_TRUE(all_of(values, [](int x) { return x > 2; }));
  ASSERT_TRUE(all_of(values, [](int x) { return x < 30; }));
  ASSERT_FALSE(all_of(values, [](int x) { return !(x & 1); }));

  const std::vector<std::string_view> empty{};
  ASSERT_TRUE(all_of(empty, [](auto) { return false; }));
}

TEST(AlgorithmUtilsTest, TestNoneOf) {
  const std::vector<int> values = {4, 6, 8, 10};
  ASSERT_TRUE(none_of(values, [](int x) { return x < 0; }));
  ASSERT_TRUE(none_of(values, [](int x) { return x == 12; }));
  ASSERT_FALSE(none_of(values, [](int x) { return !(x & 1); }));

  const std::vector<std::string_view> empty{};
  ASSERT_TRUE(none_of(empty, [](auto) { return true; }));
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

template <typename T, typename... Ts>
auto make_vector(Ts... vals) {
  return std::vector<T>{vals...};
}

TEST(AlgorithmUtilsTest, TestRemoveIf) {
  // Remove all the negatives:
  std::vector<int> values = {4, -3, 78, -9, 0, -3, 8, 13};
  remove_if(values, [](const int x) { return x < 0; });
  ASSERT_EQ(make_vector<int>(4, 78, 0, 8, 13), values);

  // Make sure values are passed by reference correctly:
  remove_if(values, [](int& x) {
    if (x == 0) {
      return true;
    } else if (!(x & 1)) {
      x *= 2;
    }
    return false;
  });
  ASSERT_EQ(make_vector<int>(8, 156, 16, 13), values);
}

TEST(AlgorithmUtilsTest, TestReverseRemoveIf) {
  std::vector<int> values = {4, 7, -2, 5, 6, 8};
  std::vector<int> traversal_order{};
  reverse_remove_if(values, [&](const int x) {
    traversal_order.push_back(x);
    return x & 1;
  });
  ASSERT_EQ(make_vector<int>(8, 6, 5, -2, 7, 4), traversal_order);
  ASSERT_EQ(make_vector<int>(4, -2, 6, 8), values);

  // Remove nothing:
  traversal_order.clear();
  reverse_remove_if(values, [&](const int x) {
    traversal_order.push_back(x);
    return false;
  });
  ASSERT_EQ(4, values.size());
  ASSERT_EQ(make_vector<int>(8, 6, -2, 4), traversal_order);
}

TEST(AlgorithmUtilsTest, TestUniqueAndExtractDuplicates) {
  std::vector<int> values = {0, 1, 1, 2, 3, 3, 5};
  std::vector<int> removed{};
  values.erase(
      unique_and_extract_duplicates(values.begin(), values.end(), std::back_inserter(removed)),
      values.end());

  ASSERT_EQ(make_vector<int>(0, 1, 2, 3, 5), values);
  ASSERT_EQ(make_vector<int>(1, 3), removed);

  values = {0, 0, 13, 14, 21, 76, 76, 76, 76, 78};
  removed.clear();
  values.erase(
      unique_and_extract_duplicates(values.begin(), values.end(), std::back_inserter(removed)),
      values.end());
  ASSERT_EQ(make_vector<int>(0, 13, 14, 21, 76, 78), values);
  ASSERT_EQ(make_vector<int>(0, 76, 76, 76), removed);

  // all duplicated:
  values = {13, 13, 13, 13, 13};
  removed.clear();
  values.erase(
      unique_and_extract_duplicates(values.begin(), values.end(), std::back_inserter(removed)),
      values.end());
  ASSERT_EQ(make_vector<int>(13), values);
  ASSERT_EQ(make_vector<int>(13, 13, 13, 13), removed);

  // Check empty
  values.clear();
  removed.clear();
  ASSERT_TRUE(values.begin() == unique_and_extract_duplicates(values.begin(), values.end(),
                                                              std::back_inserter(removed)));
  ASSERT_TRUE(removed.empty());
}

}  // namespace wf
