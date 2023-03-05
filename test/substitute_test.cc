// Copyright 2023 Gareth Cross
#include "substitute.h"
#include "constants.h"
#include "error_types.h"
#include "functions.h"
#include "matrix_functions.h"
#include "test_helpers.h"

namespace math {
using namespace math::custom_literals;

TEST(SubstituteTest, TestVariables) {
  const auto [x, y, z] = Symbols("x", "y", "z");

  ASSERT_IDENTICAL(x, x.Subs(x, x));
  ASSERT_IDENTICAL(y, x.Subs(x, y));
  ASSERT_IDENTICAL(z, z.Subs(x, y));
  ASSERT_IDENTICAL(5, z.Subs(z, 5));
  ASSERT_IDENTICAL(Constants::Pi * z, y.Subs(y, Constants::Pi * z));

  // Cannot replace a literal numeric:
  ASSERT_THROW(x.Subs(5, y), TypeError);
  ASSERT_THROW(x.Subs(2.3, y), TypeError);
  ASSERT_THROW(x.Subs(3_s / 5, y), TypeError);
}

TEST(SubstituteTest, TestFunctions) {
  const auto [x, y, z] = Symbols("x", "y", "z");

  ASSERT_IDENTICAL(cos(x), cos(z).Subs(z, x));
  ASSERT_IDENTICAL(cos(sin(y) * log(z)), cos(z).Subs(z, sin(y) * log(z)));
  ASSERT_IDENTICAL(0, cos(x).Subs(x, Constants::Pi / 2));
  ASSERT_IDENTICAL(Constants::Infinity, tan(z).Subs(z, Constants::Pi / 2));

  // Replacing function expressions:
  ASSERT_IDENTICAL(z + y, cos(x).Subs(cos(x), z + y));
  ASSERT_IDENTICAL(log(x), log(sin(5 * z)).Subs(sin(5 * z), x));
}

TEST(SubstituteTest, TestPower) {
  const auto [w, x, y, z] = Symbols("w", "x", "y", "z");

  ASSERT_IDENTICAL(z, pow(w, y).Subs(pow(w, y), z));
  ASSERT_IDENTICAL(w + y, (pow(x, z) + y).Subs(pow(x, z), w));

  // If an integer multiple of the exponent is found, replace it:
  ASSERT_IDENTICAL(pow(y, 2), pow(x, z * 2).Subs(pow(x, z), y));
  ASSERT_IDENTICAL(pow(y, 4), pow(x, z * 4).Subs(pow(x, z), y));
  ASSERT_IDENTICAL(x * pow(y, 2), x * pow(x, cos(z) * 1_s / 2 * Constants::Pi)
                                          .Subs(pow(x, cos(z) * 1_s / 4 * Constants::Pi), y));
  ASSERT_IDENTICAL(pow(x * y, 18),
                   pow(sin(z), log(z) * z * 3_s).Subs(pow(sin(z), log(z) * z * 1_s / 6), x * y));

  // Don't replace non-integer multiples:
  ASSERT_IDENTICAL(pow(x, Constants::Pi * z), pow(x, Constants::Pi * z).Subs(pow(x, z), y));
  ASSERT_IDENTICAL(pow(x, tan(y) * z), pow(x, tan(y) * z).Subs(pow(x, z), y));
  ASSERT_IDENTICAL(pow(x, 2.2 * y), pow(x, 2.2 * y).Subs(pow(x, y), z));
  ASSERT_IDENTICAL(pow(x, y / 2), pow(x, y / 2).Subs(pow(x, y), z));

  // Powers where the exponent is an addition:
  ASSERT_IDENTICAL(w * pow(x, y), pow(x, y + x).Subs(pow(x, x), w));
  ASSERT_IDENTICAL(w * pow(x, 1_s / 2), pow(x, y + 1_s / 2).Subs(pow(x, y), w));
  ASSERT_IDENTICAL(pow(w, 5) * pow(x, 2_s / 3), pow(x, 5 * y + 2_s / 3).Subs(pow(x, y), w));
  ASSERT_IDENTICAL(w * pow(x, sin(y)), pow(x, cos(y) + sin(y)).Subs(pow(x, cos(y)), w));
  ASSERT_IDENTICAL(pow(w, 3) * pow(x, 5), pow(x, 3 * z + 5).Subs(pow(x, z), w));

  // If there is a fraction in the power that can divide w/ an integer part:
  ASSERT_IDENTICAL(w * pow(x, 2_s / 3 * z + 2), pow(x, 5_s / 3 * z + 2).Subs(pow(x, z), w));
  ASSERT_IDENTICAL((1 / w) * pow(x, -1_s / 5 * (z + y) + w),
                   pow(x, -11_s / 5 * (z + y) + w).Subs(pow(x, 2 * (z + y)), w));
  ASSERT_IDENTICAL(pow(w, -3) * pow(log(x * 2), -2_s / 7 * z),
                   pow(log(x * 2), -23_s / 7 * z).Subs(pow(log(x * 2), z), w));

  // Don't replace if the addition requires matching multiple terms:
  ASSERT_IDENTICAL(pow(x, y + z + 1_s / 2), pow(x, y + z + 1_s / 2).Subs(pow(x, y + z), w));
}

TEST(SubstituteTest, TestAdditions) {
  const auto [w, x, y, z] = Symbols("w", "x", "y", "z");

  // Exact matches:
  ASSERT_IDENTICAL(x + z, (x + y).Subs(y, z));
  ASSERT_IDENTICAL(y, (x + z).Subs(x + z, y));
  ASSERT_IDENTICAL(y, (x + 5).Subs(x + 5, y));
  ASSERT_IDENTICAL(w + y, (x + log(y) - 32 * z).Subs(x + log(y) - 32 * z, w + y));
  ASSERT_IDENTICAL(cos(x) + 2, (2 + cos(2 * z + w)).Subs(2 * z + w, x));

  // Partial matches:
  ASSERT_IDENTICAL(x + z, (x + y + 2).Subs(y + 2, z));
  ASSERT_IDENTICAL(x + z, (x + y + 6.12).Subs(y + 6.12, z));
  ASSERT_IDENTICAL(cos(w) + w, (w - sin(y) * cos(z) + 2).Subs(2 - sin(y) * cos(z), cos(w)));

  // Don't replace inexact float matches:
  ASSERT_IDENTICAL(0.5 + x * log(y) - z, (0.5 + x * log(y) - z).Subs(1.221 + x * log(y), w));

  // Nested in another expression
  ASSERT_IDENTICAL(cos(w) * log(w - 2), (cos(x + y - 3) * log(x + y - 5)).Subs(x + y - 3, w));
}

TEST(SubstituteTest, TestMultiplications) {
  const auto [w, x, y, z] = Symbols("w", "x", "y", "z");

  // Replace whole multiplication, or subset of terms:
  ASSERT_IDENTICAL(y, (x * w).Subs(x * w, y));
  ASSERT_IDENTICAL(x * 5, (x * w).Subs(w, 5));
  ASSERT_IDENTICAL(y * z, (5 * x * y).Subs(5 * x, z));
  ASSERT_IDENTICAL(2 * x, (w * 2_s / 5).Subs(w / 5, x));
  ASSERT_IDENTICAL(y, (sqrt(x) * z).Subs(sqrt(x) * z, y));
  ASSERT_IDENTICAL(9 * pow(x, 2) + 3 * x + y, (z * z + z + y).Subs(z, x * 3));
  ASSERT_IDENTICAL(pow(y, 2), (x * w * y).Subs(x * w, y));
  ASSERT_IDENTICAL(pow(x, 2) * y, (x * w * z * y).Subs(w * z, x));

  // Substitute higher powers:
  ASSERT_IDENTICAL(1, (x * x * x * y * y * z).Subs(x * x * x, 1 / (y * y * z)));
  ASSERT_IDENTICAL(pow(z, 3) / pow(y, 2), (x * x * x * x * z).Subs(x * x, z / y));
  ASSERT_IDENTICAL(w * w, (cos(x) * cos(x) * sin(y) * sin(y)).Subs(cos(x) * sin(y), w));
  ASSERT_IDENTICAL(w * w * sin(y),
                   (cos(x) * cos(x) * sin(y) * sin(y) * sin(y)).Subs(cos(x) * sin(y), w));
  ASSERT_IDENTICAL(log(Constants::Pi * pow(w, 4) * y),
                   log(x * x * x * x * Constants::Pi * y * y * y * y * y).Subs(x * y, w));

  // Allow replacing part of a power (part of the exponent is subtracted):
  ASSERT_IDENTICAL(x * x * pow(y, 3_s / 2), (x * pow(y, 2) * w).Subs(pow(y, 1_s / 2) * w, x));
  ASSERT_IDENTICAL(pow(w, 5) * x * y * y, (pow(x, 6) * pow(y, 7) * 32).Subs(2 * x * y, w));

  // Allow replacing part of the exponent, leaving behind fractional powers:
  ASSERT_IDENTICAL(45 * w * pow(x, 1_s / 4) * pow(y, 1_s / 7),
                   (pow(x, 9 / 4_s) * pow(y, 15 / 7_s) * 45).Subs(pow(x * y, 2), w));

  // Including negative powers:
  ASSERT_IDENTICAL(1 / (w * w * x * y * y), (pow(x, -5_s) * pow(y, -6_s)).Subs(x * x * y * y, w));
  ASSERT_IDENTICAL((1 / w) * pow(x, -3_s / 4) * pow(y, -2_s / 7) * log(z),
                   (pow(x, -11_s / 4) * pow(y, -16_s / 7) * log(z)).Subs(pow(x * y, 2), w));

  // Don't do replacement if the candidate powers are less than the target:
  ASSERT_IDENTICAL(pow(x, 2_s / 3), pow(x, 2_s / 3).Subs(x * x, y));
  ASSERT_IDENTICAL(pow(x, -7_s / 6), pow(x, -7_s / 6).Subs(x * x, y));
  ASSERT_IDENTICAL(pow(x, -35_s / 17), pow(x, -35_s / 17).Subs(x * x * x, z));
  ASSERT_IDENTICAL(pow(x, 3 / 4_s) * pow(y, 2 / 7_s),
                   (pow(x, 3 / 4_s) * pow(y, 2 / 7_s)).Subs(pow(x * y, 2), w));

  // Don't do anything w/ floats, unless the coefficient matches exactly:
  ASSERT_IDENTICAL(x * 2.2, (x * 2.2).Subs(x * 1.5, y));
  ASSERT_IDENTICAL(x * w, (x * z * 5.1).Subs(z * 5.1, w));
  ASSERT_IDENTICAL(y * pow(x, 1_s / 4) * pow(w, 1_s / 3),
                   (1.125 * pow(x, 5_s / 4) * pow(w, 4_s / 3)).Subs(1.125 * x * w, y));

  // Check recursion behavior - this should not collapse to x:
  ASSERT_IDENTICAL(log(x * x), log(x * log(x * x)).Subs(log(x * x), x));
}

TEST(SubstituteTest, TestMatrix) {
  const auto [a, b, c] = Symbols("a", "b", "c");
  const MatrixExpr m0 = Vector(a + b, c + sin(b), 22 + c);
  ASSERT_IDENTICAL(Vector(a - log(c), c - sin(log(c)), 22 + c), m0.Subs(b, -log(c)));

  // Replace a whole matrix at once:
  // TODO: Does it make sense to allow this?
  const MatrixExpr m1 = CreateMatrix(2, 2, a * b, c - Constants::Pi, cos(b), atan(b));
  ASSERT_IDENTICAL(Identity(2), m1.Subs(m1, Identity(2)));
}

}  // namespace math
