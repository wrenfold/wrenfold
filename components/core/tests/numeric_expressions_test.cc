// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <array>

#include <gtest/gtest.h>

#include "wf/expressions/numeric_expressions.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

std::ostream& operator<<(std::ostream& stream, const wf::integer_constant& i) {
  stream << fmt::format("Integer({})", i.value());
  return stream;
}

std::ostream& operator<<(std::ostream& stream, const wf::rational_constant& r) {
  stream << fmt::format("Rational({} / {})", r.numerator(), r.denominator());
  return stream;
}

TEST(NumericExpressionsTest, TestFormatters) {
  ASSERT_EQ("33", fmt::format("{}", integer_constant{33}));
  ASSERT_EQ("-5", fmt::format("{}", integer_constant{-5}));
  ASSERT_EQ("(2 / 3)", fmt::format("{}", rational_constant{2, 3}));
  ASSERT_EQ("(-1 / 10)", fmt::format("{}", rational_constant{-1, 10}));
  ASSERT_EQ(fmt::format("{}", 2.3123), fmt::format("{}", float_constant{2.3123}));
}

TEST(NumericExpressionsTest, TestFloatSignedZeroHash) {
  const float_constant negative_zero{-0.0};
  const float_constant positive_zero{0.0};
  ASSERT_TRUE(are_identical(negative_zero, positive_zero));
  ASSERT_EQ(hash(negative_zero), hash(positive_zero));
}

TEST(NumericExpressionsTest, TestRational) {
  const rational_constant a{15, 60};
  ASSERT_EQ(a.numerator(), 1);
  ASSERT_EQ(a.denominator(), 4);
  ASSERT_FALSE(a.is_zero());
  ASSERT_FALSE(a.is_one());
  ASSERT_TRUE(are_identical(a, {1, 4}));
  ASSERT_TRUE(are_identical(a, {10, 40}));
  ASSERT_FALSE(are_identical(a, {1, 3}));
  ASSERT_FALSE(a.try_convert_to_integer());
  ASSERT_EQ(a, rational_constant(16, 64));
  ASSERT_EQ(0.25, static_cast<float_constant>(a).value());
  ASSERT_NE(a, rational_constant(-3, 8));

  const rational_constant b{30, 10};
  ASSERT_TRUE(b.try_convert_to_integer());
  ASSERT_EQ(integer_constant{3}, *b.try_convert_to_integer());

  // Numerator should carry sign information:
  const rational_constant c{2, -3};
  ASSERT_EQ(c.numerator(), -2);
  ASSERT_EQ(c.denominator(), 3);

  // Common denominator under composition:
  ASSERT_EQ(rational_constant(-1, 6), a * c);
  ASSERT_EQ(rational_constant(-3, 8), a / c);
  ASSERT_EQ(rational_constant(-5, 12), a + c);
  ASSERT_EQ(rational_constant(11, 12), a - c);

  const rational_constant d{3, 7};
  ASSERT_EQ(rational_constant(3, 28), a * d);
  ASSERT_EQ(rational_constant(7, 12), a / d);
  ASSERT_EQ(rational_constant(19, 28), a + d);
  ASSERT_EQ(rational_constant(-5, 28), a - d);

  // Check comparisons:
  ASSERT_LT(rational_constant(1, 3), rational_constant(1, 2));
  ASSERT_LT(rational_constant(7, 10), rational_constant(4, 5));
  ASSERT_LT(rational_constant(-1, 3), rational_constant(1, 5));
  ASSERT_LT(rational_constant(-5, 2), rational_constant(-3, 2));
  ASSERT_GT(rational_constant(9, 11), rational_constant(1, 3));
  ASSERT_GT(rational_constant(10, 24), rational_constant(2, 5));
  ASSERT_GT(rational_constant(-6, 7), rational_constant(12, -6));
}

TEST(NumericExpressionsTest, TestLargeRationalComparison) {
  static_assert(rational_constant{4000000000LL, 4000000001LL} <
                rational_constant{4000000001LL, 4000000000LL});
  constexpr auto max = checked_int::max();
  // Consecutive Fibonacci ratios require many reciprocal comparisons.
  constexpr auto f90 = 2880067194370816120_chk;
  constexpr auto f91 = 4660046610375530309_chk;
  constexpr auto f92 = 7540113804746346429_chk;
  // These values are in strictly increasing order. Several pairs have cross products
  // that exceed int64, including distinct fractions that round to the same double.
  const std::array<rational_constant, 21> values{
      rational_constant{integer_constant{checked_int::min()}},
      rational_constant{-max, 2},
      rational_constant{-4000000001LL, 4000000000LL},
      rational_constant{-max, max - 1},
      rational_constant{-1, 1},
      rational_constant{-(max - 1), max},
      rational_constant{-4000000000LL, 4000000001LL},
      rational_constant{-f91, f92},
      rational_constant{-f90, f91},
      rational_constant{-1, max},
      rational_constant{0, 1},
      rational_constant{1, max},
      rational_constant{f90, f91},
      rational_constant{f91, f92},
      rational_constant{4000000000LL, 4000000001LL},
      rational_constant{max - 1, max},
      rational_constant{1, 1},
      rational_constant{max, max - 1},
      rational_constant{4000000001LL, 4000000000LL},
      rational_constant{max, 2},
      rational_constant{max, 1},
  };
  for (std::size_t i = 0; i < values.size(); ++i) {
    for (std::size_t j = 0; j < values.size(); ++j) {
      SCOPED_TRACE(fmt::format("a = {}, b = {}", values[i], values[j]));
      ASSERT_EQ(order_by_comparison(i, j), determine_order(values[i], values[j]));
      ASSERT_EQ(i < j, values[i] < values[j]);
      ASSERT_EQ(i > j, values[i] > values[j]);
      ASSERT_EQ(i == j, values[i] == values[j]);
    }
  }
}

TEST(NumericExpressionsTest, TestSmallRationalComparison) {
  // For small inputs, cross multiplication provides an independent exact reference.
  for (int n1 = -8; n1 <= 8; ++n1) {
    for (int d1 = 1; d1 <= 8; ++d1) {
      for (int n2 = -8; n2 <= 8; ++n2) {
        for (int d2 = 1; d2 <= 8; ++d2) {
          const rational_constant a{n1, d1};
          const rational_constant b{n2, d2};
          ASSERT_EQ(order_by_comparison(n1 * d2, n2 * d1), determine_order(a, b))
              << fmt::format("a = {}, b = {}", a, b);
        }
      }
    }
  }
}

TEST(NumericExpressionsTest, TestRationalModulo) {
  // Positives:
  ASSERT_EQ(rational_constant(1, 3), rational_constant(2, 3) % rational_constant(1, 2));
  ASSERT_EQ(rational_constant(25, 28), rational_constant(5, 7) % rational_constant(4, 5));
  ASSERT_EQ(rational_constant(2, 3), rational_constant(11, 6) % rational_constant(1, 2));
  ASSERT_EQ(rational_constant(9, 40), rational_constant(3, 8) % rational_constant(5, 3));
  ASSERT_EQ(rational_constant(1, 2), rational_constant(3, 4) % rational_constant(1, 2));

  // Negatives:
  ASSERT_EQ(rational_constant(-3, 5), rational_constant(-4, 5) % rational_constant(1, 2));
  ASSERT_EQ(rational_constant(-1, 22), rational_constant(9, -11) % rational_constant(2, 5));
  ASSERT_EQ(rational_constant(-1, 4), rational_constant(-3, 4) % rational_constant(1, 3));
  ASSERT_EQ(rational_constant(0, 1), rational_constant(2, 3) % rational_constant(4, -6));
}

TEST(NumericExpressionsTest, TestRationalNormalize) {
  // Rationals that are already normalized:
  ASSERT_EQ(std::make_pair(integer_constant(0), rational_constant(1, 4)),
            rational_constant(1, 4).normalized());
  ASSERT_EQ(std::make_pair(integer_constant(0), rational_constant(2, -3)),
            rational_constant(2, -3).normalized());

  // Non-normalized whole integers:
  ASSERT_EQ(std::make_pair(integer_constant(1), rational_constant(0, 1)),
            rational_constant(382, 382).normalized());
  ASSERT_EQ(std::make_pair(integer_constant(-1), rational_constant(0, 1)),
            rational_constant(3, -3).normalized());

  // Some with fractional parts:
  ASSERT_EQ(std::make_pair(integer_constant(2), rational_constant(1, 2)),
            rational_constant(5, 2).normalized());
  ASSERT_EQ(std::make_pair(integer_constant(-2), rational_constant(-2, 7)),
            rational_constant(-16, 7).normalized());
  ASSERT_EQ(std::make_pair(integer_constant(-11), rational_constant(-6, 15)),
            rational_constant(-171, 15).normalized());

  ASSERT_TRUE(rational_constant(1, 2).is_proper());
  ASSERT_TRUE(rational_constant(-5, 7).is_proper());
  ASSERT_FALSE(rational_constant(4, 4).is_proper());
  ASSERT_FALSE(rational_constant(-7, 5).is_proper());
}

TEST(NumericExpressionsTest, TestModPiRational) {
  ASSERT_EQ(rational_constant(1, 3), mod_pi_rational({1, 3}));
  ASSERT_EQ(rational_constant(-4, 5), mod_pi_rational({-4, 5}));

  // Multiples of pi/2:
  for (int i = 0; i < 20; ++i) {
    // Even -> 1/2
    // Odd -> -1/2
    ASSERT_EQ(rational_constant(i & 1 ? -1 : 1, 2), mod_pi_rational({(2 * i + 1), 2}));
  }

  // Even multiples of pi or -pi:
  for (int i = 0; i < 10; ++i) {
    ASSERT_EQ(rational_constant(0, 1), mod_pi_rational({2 * i, 1}));
    ASSERT_EQ(rational_constant(0, 1), mod_pi_rational({-2 * i, 1}));
  }

  // Odd multiples of pi or negative pi:
  for (int i = 0; i < 10; ++i) {
    ASSERT_EQ(rational_constant(1, 1), mod_pi_rational({2 * i + 1, 1}));
    ASSERT_EQ(rational_constant(1, 1), mod_pi_rational({-2 * i - 1, 1}));
  }

  // Odds and ends:
  for (const int m : {2, 4, 6}) {
    ASSERT_EQ(rational_constant(-3, 4), mod_pi_rational({5 + 4 * m, 4}));
    ASSERT_EQ(rational_constant(3, 4), mod_pi_rational({-5 - 4 * m, 4}));
  }
  for (const int m : {2, 4, 6}) {
    ASSERT_EQ(rational_constant(-5, 6), mod_pi_rational({7 + 6 * m, 6}));
    ASSERT_EQ(rational_constant(5, 6), mod_pi_rational({-7 - 6 * m, 6}));
  }
}

}  // namespace wf
