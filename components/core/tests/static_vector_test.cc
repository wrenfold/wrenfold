// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <gtest/gtest.h>

#include "wf/utility/static_vector.h"
#include "wf/utility/third_party_imports.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/ranges.h>
WF_END_THIRD_PARTY_INCLUDES

// Tests for the static_vector type.
namespace wf {

// ostream support
template <typename T, std::size_t N>
std::ostream& operator<<(std::ostream& stream, const static_vector<T, N>& v) {
  stream << fmt::format("[{}]", fmt::join(v, ", "));
  return stream;
}

template <typename T, typename... Ts>
auto make_static_vector(T x, Ts... xs) {
  return static_vector<T, sizeof...(xs) + 1>{x, xs...};
}

TEST(StaticVectorTest, TestConstructEmpty) {
  const static_vector<int, 8> vals{};
  ASSERT_EQ(0, vals.size());
  ASSERT_EQ(8, vals.max_size());
  ASSERT_TRUE(vals.empty());
  ASSERT_FALSE(vals.full());
}

TEST(StaticVectorTest, TestSubscript) {
  const static_vector<int, 5> vals = {-8, 15, 6};
  ASSERT_EQ(-8, vals[0]);
  ASSERT_EQ(15, vals[1]);
  ASSERT_EQ(6, vals[2]);
}

TEST(StaticVectorTest, TestPushBack) {
  static_vector<int, 4> vals{};

  vals.push_back(2);
  ASSERT_EQ(1, vals.size());
  ASSERT_EQ(1, std::distance(vals.begin(), vals.end()));
  ASSERT_EQ(2, *vals.begin());

  vals.push_back(17);
  ASSERT_EQ(2, vals.size());
  ASSERT_EQ(make_static_vector(2, 17), vals);
  ASSERT_FALSE(vals.full());

  vals.push_back(33);
  ASSERT_EQ(3, vals.size());
  ASSERT_EQ(make_static_vector(2, 17, 33), vals);
  ASSERT_FALSE(vals.full());

  vals.push_back(-1);
  ASSERT_EQ(4, vals.size());
  ASSERT_EQ(make_static_vector(2, 17, 33, -1), vals);
  ASSERT_TRUE(vals.full());
}

TEST(StaticVectorTest, TestIterators) {
  const static_vector<int, 22> vals = {0, 2, -5, 13, 6, 83, -82, 19, 4};

  ASSERT_EQ(9, std::distance(vals.begin(), vals.end()));
  ASSERT_EQ(9, std::distance(vals.cbegin(), vals.cend()));
  ASSERT_EQ(-5, *(vals.begin() + 2));

  static_vector<int, 16> outputs{};
  for (const int x : vals) {
    outputs.push_back(x * 2);
  }
  ASSERT_EQ(vals.size(), outputs.size());
  ASSERT_TRUE(std::equal(vals.begin(), vals.end(), outputs.begin(),
                         [](int a, int b) { return a * 2 == b; }));
}

TEST(StaticVectorTest, TestInsert) {
  static_vector<int, 8> vals = {-2, 5, 3, 8, 9};
  ASSERT_EQ(5, vals.size());

  vals.insert(vals.end(), 10);
  ASSERT_EQ(make_static_vector(-2, 5, 3, 8, 9, 10), vals);

  vals.insert(vals.begin(), 0);
  ASSERT_EQ(make_static_vector(0, -2, 5, 3, 8, 9, 10), vals);

  vals.insert(vals.begin() + 3, 19);
  ASSERT_EQ(make_static_vector(0, -2, 5, 19, 3, 8, 9, 10), vals);
}

}  // namespace wf
