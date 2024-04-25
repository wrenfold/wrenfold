// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/expressions/numeric_expressions.h"

#include "wf_test_support/test_macros.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {
using namespace wf::custom_literals;

std::ostream& operator<<(std::ostream& stream, const wf::integer_constant& i) {
  stream << fmt::format("Integer({})", i.value());
  return stream;
}

std::ostream& operator<<(std::ostream& stream, const wf::rational_constant& r) {
  stream << fmt::format("Rational({} / {})", r.numerator(), r.denominator());
  return stream;
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
