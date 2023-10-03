// Copyright 2023 Gareth Cross
#include "expressions/numeric_expressions.h"

#include "fmt_imports.h"
#include "test_helpers.h"

namespace math {
using namespace math::custom_literals;

std::ostream& operator<<(std::ostream& stream, const math::Integer& i) {
  stream << fmt::format("Integer({})", i.get_value());
  return stream;
}

std::ostream& operator<<(std::ostream& stream, const math::Rational& r) {
  stream << fmt::format("Rational({} / {})", r.numerator(), r.denominator());
  return stream;
}

TEST(NumericExpressionsTest, TestRational) {
  const Rational a{15, 60};
  ASSERT_EQ(a.numerator(), 1);
  ASSERT_EQ(a.denominator(), 4);
  ASSERT_FALSE(a.is_zero());
  ASSERT_FALSE(a.is_one());
  ASSERT_TRUE(a.is_identical_to({1, 4}));
  ASSERT_TRUE(a.is_identical_to({10, 40}));
  ASSERT_FALSE(a.is_identical_to({1, 3}));
  ASSERT_FALSE(a.try_convert_to_integer());
  ASSERT_EQ(a, Rational(16, 64));
  ASSERT_EQ(0.25, static_cast<Float>(a).get_value());

  const Rational b{30, 10};
  ASSERT_TRUE(b.try_convert_to_integer());
  ASSERT_EQ(Integer{3}, *b.try_convert_to_integer());

  // Numerator should carry sign information:
  const Rational c{2, -3};
  ASSERT_EQ(c.numerator(), -2);
  ASSERT_EQ(c.denominator(), 3);

  // Common denominator under composition:
  ASSERT_EQ(Rational(-1, 6), a * c);
  ASSERT_EQ(Rational(-3, 8), a / c);
  ASSERT_EQ(Rational(-5, 12), a + c);
  ASSERT_EQ(Rational(11, 12), a - c);

  const Rational d{3, 7};
  ASSERT_EQ(Rational(3, 28), a * d);
  ASSERT_EQ(Rational(7, 12), a / d);
  ASSERT_EQ(Rational(19, 28), a + d);
  ASSERT_EQ(Rational(-5, 28), a - d);

  // Check comparisons:
  ASSERT_LT(Rational(1, 3), Rational(1, 2));
  ASSERT_LT(Rational(7, 10), Rational(4, 5));
  ASSERT_LT(Rational(-1, 3), Rational(1, 5));
  ASSERT_LT(Rational(-5, 2), Rational(-3, 2));
  ASSERT_GT(Rational(9, 11), Rational(1, 3));
  ASSERT_GT(Rational(10, 24), Rational(2, 5));
  ASSERT_GT(Rational(-6, 7), Rational(12, -6));
}

TEST(NumericExpressionsTest, TestRationalModulo) {
  // Positives:
  ASSERT_EQ(Rational(1, 3), Rational(2, 3) % Rational(1, 2));
  ASSERT_EQ(Rational(25, 28), Rational(5, 7) % Rational(4, 5));
  ASSERT_EQ(Rational(2, 3), Rational(11, 6) % Rational(1, 2));
  ASSERT_EQ(Rational(9, 40), Rational(3, 8) % Rational(5, 3));
  ASSERT_EQ(Rational(1, 2), Rational(3, 4) % Rational(1, 2));

  // Negatives:
  ASSERT_EQ(Rational(-3, 5), Rational(-4, 5) % Rational(1, 2));
  ASSERT_EQ(Rational(-1, 22), Rational(9, -11) % Rational(2, 5));
  ASSERT_EQ(Rational(-1, 4), Rational(-3, 4) % Rational(1, 3));
  ASSERT_EQ(Rational(0, 1), Rational(2, 3) % Rational(4, -6));
}

TEST(NumericExpressionsTest, TestRationalNormalize) {
  // Rationals that are already normalized:
  ASSERT_EQ(std::make_pair(Integer(0), Rational(1, 4)), Rational(1, 4).normalized());
  ASSERT_EQ(std::make_pair(Integer(0), Rational(2, -3)), Rational(2, -3).normalized());

  // Non-normalized whole integers:
  ASSERT_EQ(std::make_pair(Integer(1), Rational(0, 1)), Rational(382, 382).normalized());
  ASSERT_EQ(std::make_pair(Integer(-1), Rational(0, 1)), Rational(3, -3).normalized());

  // Some with fractional parts:
  ASSERT_EQ(std::make_pair(Integer(2), Rational(1, 2)), Rational(5, 2).normalized());
  ASSERT_EQ(std::make_pair(Integer(-2), Rational(-2, 7)), Rational(-16, 7).normalized());
  ASSERT_EQ(std::make_pair(Integer(-11), Rational(-6, 15)), Rational(-171, 15).normalized());

  ASSERT_TRUE(Rational(1, 2).is_normalized());
  ASSERT_TRUE(Rational(-5, 7).is_normalized());
  ASSERT_FALSE(Rational(4, 4).is_normalized());
  ASSERT_FALSE(Rational(-7, 5).is_normalized());
}

TEST(NumericExpressionsTest, TestModPiRational) {
  ASSERT_EQ(Rational(1, 3), mod_pi_rational({1, 3}));
  ASSERT_EQ(Rational(-4, 5), mod_pi_rational({-4, 5}));

  // Multiples of pi/2:
  for (int i = 0; i < 20; ++i) {
    // Even -> 1/2
    // Odd -> -1/2
    ASSERT_EQ(Rational(i & 1 ? -1 : 1, 2), mod_pi_rational({(2 * i + 1), 2}));
  }

  // Even multiples of pi or -pi:
  for (int i = 0; i < 10; ++i) {
    ASSERT_EQ(Rational(0, 1), mod_pi_rational({2 * i, 1}));
    ASSERT_EQ(Rational(0, 1), mod_pi_rational({-2 * i, 1}));
  }

  // Odd multiples of pi or negative pi:
  for (int i = 0; i < 10; ++i) {
    ASSERT_EQ(Rational(1, 1), mod_pi_rational({2 * i + 1, 1}));
    ASSERT_EQ(Rational(1, 1), mod_pi_rational({-2 * i - 1, 1}));
  }

  // Odds and ends:
  for (const int m : {2, 4, 6}) {
    ASSERT_EQ(Rational(-3, 4), mod_pi_rational({5 + 4 * m, 4}));
    ASSERT_EQ(Rational(3, 4), mod_pi_rational({-5 - 4 * m, 4}));
  }
  for (const int m : {2, 4, 6}) {
    ASSERT_EQ(Rational(-5, 6), mod_pi_rational({7 + 6 * m, 6}));
    ASSERT_EQ(Rational(5, 6), mod_pi_rational({-7 - 6 * m, 6}));
  }
}

}  // namespace math
