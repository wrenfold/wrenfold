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
}

}  // namespace math
