// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <optional>

#include <gtest/gtest.h>

#include "wf/utility/type_list.h"

// Test of `type_list<>` utilities.
// These are all compile-time tests, but I break them into test cases for organization.
namespace wf {

TEST(TypeListTest, TestSize) {
  static_assert(0 == type_list_size_v<type_list<>>);
  static_assert(2 == type_list_size_v<type_list<int, float>>);
  static_assert(3 == type_list_size_v<type_list<int, float, std::string>>);
}

TEST(TypeListTest, TestContains) {
  using list = type_list<int, float, std::string>;
  static_assert(type_list_contains_v<int, list>);
  static_assert(type_list_contains_v<float, list>);
  static_assert(type_list_contains_v<std::string, list>);
  static_assert(!type_list_contains_v<int&, list>);
  static_assert(!type_list_contains_v<const std::string&, list>);
  static_assert(!type_list_contains_v<float&&, list>);
}

TEST(TypeListTest, TestPushFront) {
  using list = type_list<>;
  static_assert(std::is_same_v<type_list_push_front_t<int, list>, type_list<int>>);
  static_assert(
      std::is_same_v<type_list_push_front_t<float, type_list<int>>, type_list<float, int>>);
}

TEST(TypeListTest, TestFront) {
  static_assert(std::is_same_v<int, type_list_front_t<type_list<int>>>);
  static_assert(std::is_same_v<std::string&, type_list_front_t<type_list<std::string&, int>>>);
}

TEST(TypeListTest, TestConcatenate) {
  using list1 = type_list<int, float, std::string>;
  using list2 = type_list<double, char>;
  static_assert(std::is_same_v<type_list<int, float, std::string, double, char>,
                               type_list_concatenate_t<list1, list2>>);
}

TEST(TypeListTest, TestIndex) {
  using list = type_list<int, int&, float, float&&, std::string, const std::string>;
  static_assert(0 == type_list_index_v<int, list>);
  static_assert(1 == type_list_index_v<int&, list>);
  static_assert(2 == type_list_index_v<float, list>);
  static_assert(3 == type_list_index_v<float&&, list>);
  static_assert(4 == type_list_index_v<std::string, list>);
  static_assert(5 == type_list_index_v<const std::string, list>);
}

TEST(TypeListTest, TestElement) {
  using list = type_list<int, float, double>;
  static_assert(std::is_same_v<int, type_list_element_t<0, list>>);
  static_assert(std::is_same_v<float, type_list_element_t<1, list>>);
  static_assert(std::is_same_v<double, type_list_element_t<2, list>>);
}

TEST(TypeListTest, TestMap) {
  using list = type_list<int&, float&, std::string>;
  static_assert(std::is_same_v<type_list<int, float, std::string>,
                               type_list_map_t<std::remove_reference_t, list>>);
  static_assert(std::is_same_v<type_list<std::optional<int>, std::optional<float>>,
                               type_list_map_t<std::optional, type_list<int, float>>>);
}

TEST(TypeListTest, TestFilter) {
  static_assert(std::is_same_v<
                type_list<int, long, char>,
                type_list_filter_t<std::is_integral, type_list<int, float, long, double, char>>>);
}

TEST(TypeListTest, TestFilterSequence) {
  using list1 = type_list<int, float, long, double, char>;
  constexpr auto seq_ints = filter_type_sequence<std::is_integral>(list1{});
  static_assert(std::is_same_v<const std::index_sequence<0, 2, 4>, decltype(seq_ints)>);

  using list2 = type_list<std::string, std::string&, int, int&>;
  constexpr auto seq_ref = filter_type_sequence<std::is_reference>(list2{});
  static_assert(std::is_same_v<const std::index_sequence<1, 3>, decltype(seq_ref)>);
}

}  // namespace wf
