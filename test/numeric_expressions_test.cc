// Copyright 2023 Gareth Cross
#include "expressions/numeric_expressions.h"

#include <fmt/format.h>

#include "test_helpers.h"

namespace math {

std::ostream& operator<<(std::ostream& stream, const math::Integer& i) {
  stream << fmt::format("Integer({})", i.GetValue());
  return stream;
}

std::ostream& operator<<(std::ostream& stream, const math::Rational& r) {
  stream << fmt::format("Rational({} / {})", r.Numerator(), r.Denominator());
  return stream;
}

TEST(NumericExpressionsTest, TestRational) {
  const Rational a{15, 60};
  ASSERT_EQ(a.Numerator(), 1);
  ASSERT_EQ(a.Denominator(), 4);
  ASSERT_FALSE(a.IsZero());
  ASSERT_FALSE(a.IsOne());
  ASSERT_TRUE(a.IsIdenticalToImplTyped({1, 4}));
  ASSERT_TRUE(a.IsIdenticalToImplTyped({10, 40}));
  ASSERT_FALSE(a.IsIdenticalToImplTyped({1, 3}));
  ASSERT_FALSE(a.TryConvertToInteger());
  ASSERT_EQ(a, Rational(16, 64));
  ASSERT_EQ(0.25, static_cast<Float>(a).GetValue());

  const Rational b{30, 10};
  ASSERT_TRUE(b.TryConvertToInteger());
  ASSERT_EQ(Integer{3}, *b.TryConvertToInteger());

  // Numerator should carry sign information:
  const Rational c{2, -3};
  ASSERT_EQ(c.Numerator(), -2);
  ASSERT_EQ(c.Denominator(), 3);

  // Common denominator under composition:
  ASSERT_EQ(-1, (a * c).Numerator());
  ASSERT_EQ(6, (a * c).Denominator());
  ASSERT_EQ(-5, (a + c).Numerator());
  ASSERT_EQ(12, (a + c).Denominator());
  ASSERT_EQ(11, (a - c).Numerator());
  ASSERT_EQ(12, (a - c).Denominator());

  // Rationals that are already normalized:
  ASSERT_EQ(Integer(0), a.Normalize().first);
  ASSERT_EQ(a, a.Normalize().second);
  ASSERT_EQ(Integer(0), c.Normalize().first);
  ASSERT_EQ(c, c.Normalize().second);

  // Non-normalized whole integers:
  ASSERT_EQ(Integer(1), Rational(382, 382).Normalize().first);
  ASSERT_EQ(Rational(0, 1), Rational(382, 382).Normalize().second);
  ASSERT_EQ(Integer(-1), Rational(3, -3).Normalize().first);
  ASSERT_EQ(Rational(0, 1), Rational(3, -3).Normalize().second);

  // Some with fractional parts:
  ASSERT_EQ(Integer(2), Rational(5, 2).Normalize().first);
  ASSERT_EQ(Rational(1, 2), Rational(5, 2).Normalize().second);
  ASSERT_EQ(Integer(-2), Rational(16, -7).Normalize().first);
  ASSERT_EQ(Rational(-2, 7), Rational(16, -7).Normalize().second);
  ASSERT_EQ(Integer(-11), Rational(-171, 15).Normalize().first);
  ASSERT_EQ(Rational(-6, 15), Rational(-171, 15).Normalize().second);

  ASSERT_TRUE(Rational(1, 2).IsNormalized());
  ASSERT_TRUE(Rational(-5, 7).IsNormalized());
  ASSERT_FALSE(Rational(4, 4).IsNormalized());
  ASSERT_FALSE(Rational(-7, 5).IsNormalized());
}

TEST(NumericExpressionsTest, TestModPiRational) {
  ASSERT_EQ(Rational(1, 3), ModPiRational({1, 3}));
  ASSERT_EQ(Rational(-4, 5), ModPiRational({-4, 5}));

  // Multiples of pi/2:
  for (int i = 0; i < 20; ++i) {
    // Even -> 1/2
    // Odd -> -1/2
    ASSERT_EQ(Rational(i & 1 ? -1 : 1, 2), ModPiRational({(2 * i + 1), 2}));
  }

  // Even multiples of pi or -pi:
  for (int i = 0; i < 10; ++i) {
    ASSERT_EQ(Rational(0, 1), ModPiRational({2 * i, 1}));
    ASSERT_EQ(Rational(0, 1), ModPiRational({-2 * i, 1}));
  }

  // Odd multiples of pi or negative pi:
  for (int i = 0; i < 10; ++i) {
    ASSERT_EQ(Rational(1, 1), ModPiRational({2 * i + 1, 1}));
    ASSERT_EQ(Rational(1, 1), ModPiRational({-2 * i - 1, 1}));
  }

  // Odds and ends:
  for (const int m : {2, 4, 6}) {
    ASSERT_EQ(Rational(-3, 4), ModPiRational({5 + 4 * m, 4}));
    ASSERT_EQ(Rational(3, 4), ModPiRational({-5 - 4 * m, 4}));
  }
  for (const int m : {2, 4, 6}) {
    ASSERT_EQ(Rational(-5, 6), ModPiRational({7 + 6 * m, 6}));
    ASSERT_EQ(Rational(5, 6), ModPiRational({-7 - 6 * m, 6}));
  }
}

}  // namespace math
