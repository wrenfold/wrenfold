// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <gtest/gtest.h>

#include "wf/string_utils.h"

// Tests for functions in `string_utils`.
namespace wf {

TEST(StringUtilsTest, TestFmtView) {
  // Formatters that transform values:
  EXPECT_EQ(
      "6, 12, 13_x",
      fmt::format("{}, {}, {}", make_fmt_view([](auto x) { return std::to_string(x * 3); }, 2),
                  make_fmt_view([](auto x) { return std::to_string(x + 5); }, 7),
                  make_fmt_view([](auto x) { return std::to_string(x) + "_x"; }, 13)));

  struct type_printer {
    std::string operator()(int x) const { return fmt::format("int: {}", x); }
    std::string operator()(double x) const { return fmt::format("double: {}", x); }
  };
  EXPECT_EQ("int: 5, double: 22", fmt::format("{}, {}", make_fmt_view(type_printer{}, 5),
                                              make_fmt_view(type_printer{}, 22.0)));

  // Test that we can store vector in the view.
  struct vector_printer {
    std::string operator()(const std::vector<int>& data) const {
      return fmt::format("vector of size {}, first = {}", data.size(), data.front());
    }
  };
  EXPECT_EQ("vector of size 3, first = 89",
            fmt::format("{}", make_fmt_view(vector_printer{}, std::vector{89, 64, 123})));
}

TEST(StringUtilsTest, TestJoin) {
  EXPECT_EQ("", join(", ", std::vector<int>{}, [](auto x) { return std::to_string(x); }));
  EXPECT_EQ("alone", join(", ", std::vector<std::string_view>{"alone"}, [](auto x) { return x; }));
  EXPECT_EQ("1, 2, 3", join(", ", std::vector{1, 2, 3}, [](auto x) { return std::to_string(x); }));
  EXPECT_EQ("llo; rld; iend", join("; ", std::vector<std::string_view>{"hello", "world", "friend"},
                                   [](auto x) { return x.substr(2); }));
}

TEST(StringUtilsTest, TestJoinEnumerate) {
  const std::vector<std::string_view> elements{"alea", "iacta", "est"};
  EXPECT_EQ("0: alea, 1: iacta, 2: est",
            join_enumerate(", ", elements, [](std::size_t index, auto element) {
              return fmt::format("{}: {}", index, element);
            }));
}

}  // namespace wf
