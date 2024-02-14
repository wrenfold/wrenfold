// Copyright 2024 Gareth Cross
#include <gtest/gtest.h>

#include <numeric>

#include "wf/checked_int.h"

namespace wf {

// ostream support for gtest
inline std::ostream& operator<<(std::ostream& stream, const checked_int x) {
  stream << x.value();
  return stream;
}

static_assert(std::is_trivially_copyable_v<checked_int>);
static_assert(std::is_trivially_move_assignable_v<checked_int>);
static_assert(std::is_trivially_move_constructible_v<checked_int>);

TEST(CheckedIntTest, TestConstruct) {
  checked_int a = 13;
  ASSERT_EQ(13, a.value());
  ASSERT_EQ(13, static_cast<checked_int::value_type>(a));

  a = checked_int::min();
  ASSERT_EQ(checked_int::min(), a);
  a = checked_int::max();
  ASSERT_EQ(checked_int::max(), a);

  // Test which conversions work:
  static_assert(std::is_constructible_v<checked_int, std::int8_t>);
  static_assert(std::is_constructible_v<checked_int, std::int16_t>);
  static_assert(std::is_constructible_v<checked_int, std::int32_t>);
  static_assert(std::is_constructible_v<checked_int, std::int64_t>);

  // And don't work
  static_assert(!std::is_constructible_v<checked_int, std::uint8_t>);
  static_assert(!std::is_constructible_v<checked_int, std::uint16_t>);
  static_assert(!std::is_constructible_v<checked_int, std::uint32_t>);
  static_assert(!std::is_constructible_v<checked_int, std::uint64_t>);
  static_assert(!std::is_constructible_v<checked_int, double>);
}

TEST(CheckedIntTest, TestCompare) {
  ASSERT_EQ(0, 0_chk);
  ASSERT_NE(1, 0_chk);
  ASSERT_GT(1, 0_chk);
  ASSERT_LT(-1, 0_chk);
  ASSERT_GT(checked_int::max(), 0_chk);
  ASSERT_LT(checked_int::min(), 0_chk);

  ASSERT_LT(checked_int::min(), checked_int::max());
  ASSERT_GT(checked_int::max(), checked_int::min());

  ASSERT_EQ(2_chk, 2);
  ASSERT_NE(10_chk, -10);
  ASSERT_LT(-13_chk, -6);
  ASSERT_GT(-100_chk, -128);
  ASSERT_LT(900_chk, 901);
  ASSERT_GT(22_chk, 13);

  ASSERT_LE(0_chk, 0);
  ASSERT_LE(0_chk, 7);
  ASSERT_GE(32_chk, 32);
  ASSERT_GE(32_chk, 31);
}

TEST(CheckedIntTest, TestNegation) {
  ASSERT_EQ(0_chk, -0_chk);
  ASSERT_EQ(-1_chk, -checked_int(1));
  ASSERT_EQ(checked_int::max(), -(checked_int::min() + 1));
  ASSERT_THROW(-checked_int::min(), arithmetic_error);

  ASSERT_EQ(1, abs(-1_chk));
  ASSERT_EQ(checked_int::max(), abs(checked_int::min() + 1));
  ASSERT_THROW(abs(-checked_int::min()), arithmetic_error);
}

TEST(CheckedIntTest, TestMultiplication) {
  // Some valid cases:
  ASSERT_EQ(0_chk, 0_chk * 0);
  ASSERT_EQ(0_chk, -33 * 0_chk);
  ASSERT_EQ(28_chk, 7_chk * 4);
  ASSERT_EQ(-81_chk, 9_chk * -9);
  ASSERT_EQ(-1, 1_chk * -1);
  ASSERT_EQ(-78, 78_chk * -1);
  ASSERT_EQ(checked_int::min(), checked_int::min() * 1);
  ASSERT_EQ(checked_int::max(), checked_int::max() * 1);

  const auto fold_multiply = [](auto... values) {
    const std::array<checked_int, sizeof...(values)> array = {values...};
    return std::accumulate(array.begin(), array.end(), 1_chk, std::multiplies<checked_int>{});
  };

  // Factors of checked_int::max
  ASSERT_EQ(checked_int::max(), fold_multiply(7, 7, 73, 127, 337, 92737, 649657));
  ASSERT_EQ(checked_int::max(), fold_multiply(649657, 92737, 337, 127, 73, 7, 7));
  ASSERT_EQ(-checked_int::max(), fold_multiply(337, 127, -7, 649657, 7, 92737, 73));

  // Some factorizations of checked_int::min
  for (std::size_t i = 0; i < 62; ++i) {
    auto lhs = static_cast<std::uint64_t>(2) << i;
    auto rhs = static_cast<std::uint64_t>(2) << (62 - i - 1);
    if (lhs > rhs) {
      std::swap(lhs, rhs);
    }
    ASSERT_EQ(checked_int::min(),
              checked_int(-static_cast<std::int64_t>(lhs)) * static_cast<std::int64_t>(rhs));
  }

  ASSERT_EQ(9223372036854775806_chk, fold_multiply(2, 3, 715827883, 2147483647));
  ASSERT_EQ(9223372036854775805_chk, fold_multiply(5, 23, 53301701, 1504703107));
  ASSERT_EQ(-9223372036854775803_chk, fold_multiply(3, 71, -42013, 1030686124187));

  // Some cases that overflow:
  ASSERT_THROW(checked_int::max() * 2, arithmetic_error);
  ASSERT_THROW(checked_int::min() * -1, arithmetic_error);

  for (int i = 2; i <= 10; ++i) {
    ASSERT_THROW((checked_int::max() / i + 1) * i, arithmetic_error);
  }
  ASSERT_THROW(14197294936951_chk * 649658, arithmetic_error);
  ASSERT_THROW(14197294936952 * 649657_chk, arithmetic_error);
  ASSERT_THROW(2 * 4611686018427387904_chk, arithmetic_error);  //  2^63
  ASSERT_THROW(4294967296_chk * 2147483648_chk, arithmetic_error);

  // On the negative side:
  ASSERT_THROW(checked_int::min() * 2, arithmetic_error);
  for (int i = 2; i <= 10; ++i) {
    ASSERT_THROW((checked_int::min() / i - 1) * i, arithmetic_error);
    ASSERT_THROW(abs(checked_int::min() / i - 1) * -i, arithmetic_error);
  }

  ASSERT_THROW(119537721_chk * -77158673929, arithmetic_error);
  ASSERT_THROW(-77158673929_chk * 119537721, arithmetic_error);
  ASSERT_THROW(73787_chk * -124999959841907_chk, arithmetic_error);
  ASSERT_THROW(-124999959841907_chk * 73787_chk, arithmetic_error);

  // Check that *= works:
  ASSERT_EQ(42, 6_chk *= 7);
  ASSERT_EQ(-221, 13_chk *= -17);
}

TEST(CheckedIntTest, TestDivision) {
  ASSERT_EQ(1, 1_chk / 1);
  ASSERT_EQ(0, 1_chk / 2);
  ASSERT_EQ(-1, 1_chk / -1);
  ASSERT_EQ(1, checked_int::max() / checked_int::max());
  ASSERT_EQ(1, checked_int::min() / checked_int::min());
  ASSERT_EQ(0, checked_int::max() / checked_int::min());
  ASSERT_EQ(9, 81_chk / 9);
  ASSERT_EQ(649657, checked_int::max() / 14197294936951_chk);
  ASSERT_EQ(-649657, checked_int::max() / -14197294936951_chk);

  // Cannot divide by zero:
  ASSERT_THROW(1_chk / 0, arithmetic_error);
  ASSERT_THROW(0_chk / 0, arithmetic_error);

  // Cannot divide `min` by -1, because it exceeds `max`.
  ASSERT_THROW(checked_int::min() / -1, arithmetic_error);

  // Check that /= works
  ASSERT_EQ(11, 143_chk /= 13);
  ASSERT_EQ(-173, 2941_chk /= -17);
}

TEST(CheckedIntTest, TestModulo) {
  ASSERT_EQ(0, 1_chk % 1);
  ASSERT_EQ(1, 10_chk % 3);
  ASSERT_EQ(-3, -15_chk % 4);
  ASSERT_EQ(0, checked_int::max() % checked_int::max());
  ASSERT_EQ(0, checked_int::min() % checked_int::min());

  // Cannot divide by zero:
  ASSERT_THROW(1_chk % 0, arithmetic_error);
  ASSERT_THROW(100_chk % 0, arithmetic_error);

  // Cannot mod `min` by -1, because it exceeds `max`.
  ASSERT_THROW(checked_int::min() % -1, arithmetic_error);
}

TEST(CheckedIntTest, TestAddition) {
  ASSERT_EQ(0, 0_chk + 0);
  ASSERT_EQ(0, 1_chk + -1);
  ASSERT_EQ(13, 10_chk + 3);
  ASSERT_EQ(checked_int::max(), 9223372036854775806_chk + 1);
  ASSERT_EQ(checked_int::min(), -9223372036854775807_chk + -1);
  ASSERT_EQ(checked_int::max(), 4611686018427387904_chk + 4611686018427387903_chk);
  ASSERT_EQ(-1_chk, checked_int::max() + checked_int::min());
  ASSERT_EQ(-1_chk, checked_int::min() + checked_int::max());

  // Check some additions that overflow.
  for (int i = 1; i <= 10; ++i) {
    ASSERT_THROW(checked_int(checked_int::max().value() - i) + i + 1, arithmetic_error);
    ASSERT_THROW(checked_int(checked_int::min().value() + i) + (-i - 1), arithmetic_error);
  }

  ASSERT_THROW(4611686018427387904_chk + 4611686018427387904_chk, arithmetic_error);

  ASSERT_EQ(22_chk, 20_chk += 2);
  ASSERT_EQ(0_chk, -14_chk += 14);
}

TEST(CheckedIntTest, TestSubtraction) {
  ASSERT_EQ(0, 1_chk - 1);
  ASSERT_EQ(-6, 7_chk - 13);
  ASSERT_EQ(0, checked_int::max() - checked_int::max());
  ASSERT_EQ(-1_chk, checked_int::min() - (checked_int::min() + 1));
  ASSERT_EQ(checked_int::min(), -1 - checked_int::max());
  ASSERT_EQ(checked_int::min(), -4611686018427387904_chk - 4611686018427387904_chk);
  ASSERT_EQ(checked_int::max(), 4611686018427387904_chk - -4611686018427387903_chk);

  // Check some subtractions that overflow.
  for (int i = 1; i <= 10; ++i) {
    ASSERT_THROW(checked_int(checked_int::max().value() - i) + i + 1, arithmetic_error);
    ASSERT_THROW(checked_int(checked_int::min().value() + i) + (-i - 1), arithmetic_error);
  }

  ASSERT_THROW(-4611686018427387904_chk - 4611686018427387905_chk, arithmetic_error);

  ASSERT_EQ(0, 13_chk -= 13);
  ASSERT_EQ(-17, -13_chk -= 4);
}

}  // namespace wf
