// Copyright 2023 Gareth Cross
#include "constants.h"
#include "error_types.h"
#include "functions.h"
#include "matrix_functions.h"
#include "operations.h"
#include "test_helpers.h"

namespace math {
using namespace math::custom_literals;

TEST(SubstituteTest, TestVariables) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  ASSERT_IDENTICAL(x, x.subs(x, x));
  ASSERT_IDENTICAL(y, x.subs(x, y));
  ASSERT_IDENTICAL(z, z.subs(x, y));
  ASSERT_IDENTICAL(5, z.subs(z, 5));
  ASSERT_IDENTICAL(Constants::Pi * z, y.subs(y, Constants::Pi * z));

  // Cannot replace a literal numeric:
  ASSERT_THROW(x.subs(5, y), TypeError);
  ASSERT_THROW(x.subs(2.3, y), TypeError);
  ASSERT_THROW(x.subs(3_s / 5, y), TypeError);
}

TEST(SubstituteTest, TestFunctions) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  ASSERT_IDENTICAL(cos(x), cos(z).subs(z, x));
  ASSERT_IDENTICAL(cos(sin(y) * log(z)), cos(z).subs(z, sin(y) * log(z)));
  ASSERT_IDENTICAL(0, cos(x).subs(x, Constants::Pi / 2));
  ASSERT_IDENTICAL(Constants::Infinity, tan(z).subs(z, Constants::Pi / 2));

  // Replacing function expressions:
  ASSERT_IDENTICAL(z + y, cos(x).subs(cos(x), z + y));
  ASSERT_IDENTICAL(log(x), log(sin(5 * z)).subs(sin(5 * z), x));
}

TEST(SubstituteTest, TestPower) {
  const auto [w, x, y, z] = make_symbols("w", "x", "y", "z");

  ASSERT_IDENTICAL(z, pow(w, y).subs(pow(w, y), z));
  ASSERT_IDENTICAL(w + y, (pow(x, z) + y).subs(pow(x, z), w));

  // If an integer multiple of the exponent is found, replace it:
  ASSERT_IDENTICAL(pow(y, 2), pow(x, z * 2).subs(pow(x, z), y));
  ASSERT_IDENTICAL(pow(y, 4), pow(x, z * 4).subs(pow(x, z), y));
  ASSERT_IDENTICAL(x * pow(y, 2), x * pow(x, cos(z) * 1_s / 2 * Constants::Pi)
                                          .subs(pow(x, cos(z) * 1_s / 4 * Constants::Pi), y));
  ASSERT_IDENTICAL(pow(x * y, 18),
                   pow(sin(z), log(z) * z * 3_s).subs(pow(sin(z), log(z) * z * 1_s / 6), x * y));

  // Don't replace non-integer multiples:
  ASSERT_IDENTICAL(pow(x, Constants::Pi * z), pow(x, Constants::Pi * z).subs(pow(x, z), y));
  ASSERT_IDENTICAL(pow(x, tan(y) * z), pow(x, tan(y) * z).subs(pow(x, z), y));
  ASSERT_IDENTICAL(pow(x, 2.2 * y), pow(x, 2.2 * y).subs(pow(x, y), z));
  ASSERT_IDENTICAL(pow(x, y / 2), pow(x, y / 2).subs(pow(x, y), z));

  // TODO: There is an argument to be made this _should_ be the behavior, but it currently is not.
#if 0
  ASSERT_IDENTICAL(pow(x, 5 * z / 3), pow(x, 5 * z / 3).Subs(pow(x, z), w));
#endif

  // Powers where the exponent is an addition:
  ASSERT_IDENTICAL(w * pow(x, y), pow(x, y + x).subs(pow(x, x), w));
  ASSERT_IDENTICAL(w * pow(x, 1_s / 2), pow(x, y + 1_s / 2).subs(pow(x, y), w));
  ASSERT_IDENTICAL(pow(w, 5) * pow(x, 2_s / 3), pow(x, 5 * y + 2_s / 3).subs(pow(x, y), w));
  ASSERT_IDENTICAL(w * pow(x, sin(y)), pow(x, cos(y) + sin(y)).subs(pow(x, cos(y)), w));
  ASSERT_IDENTICAL(pow(w, 3) * pow(x, 5), pow(x, 3 * z + 5).subs(pow(x, z), w));

  // If there is a fraction in the power that can divide w/ an integer part:
  ASSERT_IDENTICAL(w * pow(x, 2_s / 3 * z + 2), pow(x, 5_s / 3 * z + 2).subs(pow(x, z), w));

  // TODO: this simplification seems perhaps not worthwhile and overly compliated?
  ASSERT_IDENTICAL(pow(w, -3) * pow(log(x * 2), -2_s / 7 * z),
                   pow(log(x * 2), -23_s / 7 * z).subs(pow(log(x * 2), z), w));

  // Don't replace if the addition requires matching multiple terms:
  ASSERT_IDENTICAL(pow(x, y + z + 1_s / 2), pow(x, y + z + 1_s / 2).subs(pow(x, y + z), w));
}

TEST(SubstituteTest, TestAdditions) {
  const auto [w, x, y, z] = make_symbols("w", "x", "y", "z");

  // Exact matches:
  ASSERT_IDENTICAL(x + z, (x + y).subs(y, z));
  ASSERT_IDENTICAL(y, (x + z).subs(x + z, y));
  ASSERT_IDENTICAL(y, (x + 5).subs(x + 5, y));
  ASSERT_IDENTICAL(w + y, (x + log(y) - 32 * z).subs(x + log(y) - 32 * z, w + y));
  ASSERT_IDENTICAL(cos(x) + 2, (2 + cos(2 * z + w)).subs(2 * z + w, x));

  // Partial matches:
  ASSERT_IDENTICAL(x + z, (x + y + 2).subs(y + 2, z));
  ASSERT_IDENTICAL(x + z, (x + y + 6.12).subs(y + 6.12, z));
  ASSERT_IDENTICAL(cos(w) + w, (w - sin(y) * cos(z) + 2).subs(2 - sin(y) * cos(z), cos(w)));

  // Don't replace inexact float matches:
  ASSERT_IDENTICAL(0.5 + x * log(y) - z, (0.5 + x * log(y) - z).subs(1.221 + x * log(y), w));

  // Nested in another expression
  ASSERT_IDENTICAL(cos(w) * log(w - 2), (cos(x + y - 3) * log(x + y - 5)).subs(x + y - 3, w));
}

TEST(SubstituteTest, TestMultiplications) {
  const auto [w, x, y, z] = make_symbols("w", "x", "y", "z");

  // Replace whole multiplication, or subset of terms:
  ASSERT_IDENTICAL(y, (x * w).subs(x * w, y));
  ASSERT_IDENTICAL(x * 5, (x * w).subs(w, 5));
  ASSERT_IDENTICAL(y * z, (5 * x * y).subs(5 * x, z));
  ASSERT_IDENTICAL(2 * x, (w * 2_s / 5).subs(w / 5, x));
  ASSERT_IDENTICAL(y, (sqrt(x) * z).subs(sqrt(x) * z, y));
  ASSERT_IDENTICAL(9 * pow(x, 2) + 3 * x + y, (z * z + z + y).subs(z, x * 3));
  ASSERT_IDENTICAL(pow(y, 2), (x * w * y).subs(x * w, y));
  ASSERT_IDENTICAL(pow(x, 2) * y, (x * w * z * y).subs(w * z, x));

  // Substitute higher powers:
  ASSERT_IDENTICAL(1, (x * x * x * y * y * z).subs(x * x * x, 1 / (y * y * z)));
  ASSERT_IDENTICAL(pow(z, 3) / pow(y, 2), (x * x * x * x * z).subs(x * x, z / y));
  ASSERT_IDENTICAL(w * w, (cos(x) * cos(x) * sin(y) * sin(y)).subs(cos(x) * sin(y), w));
  ASSERT_IDENTICAL(w * w * sin(y),
                   (cos(x) * cos(x) * sin(y) * sin(y) * sin(y)).subs(cos(x) * sin(y), w));
  ASSERT_IDENTICAL(log(Constants::Pi * pow(w, 4) * y),
                   log(x * x * x * x * Constants::Pi * y * y * y * y * y).subs(x * y, w));

  // Allow replacing part of a power (part of the exponent is subtracted):
  ASSERT_IDENTICAL(x * x * pow(y, 3 / 2_s), (x * pow(y, 2) * w).subs(pow(y, 1 / 2_s) * w, x));
  ASSERT_IDENTICAL(pow(w, 5) * x * y * y, (pow(x, 6) * pow(y, 7) * 32).subs(2 * x * y, w));

  // Allow replacing part of the exponent, leaving behind fractional powers:
  ASSERT_IDENTICAL(45 * w * pow(x, 1 / 4_s) * pow(y, 1 / 7_s),
                   (pow(x, 9 / 4_s) * pow(y, 15 / 7_s) * 45).subs(pow(x * y, 2), w));

  // Including negative powers:
  ASSERT_IDENTICAL(1 / (w * w * x * y * y), (pow(x, -5_s) * pow(y, -6_s)).subs(x * x * y * y, w));
  ASSERT_IDENTICAL((1 / w) * pow(x, -3_s / 4) * pow(y, -2_s / 7) * log(z),
                   (pow(x, -11_s / 4) * pow(y, -16_s / 7) * log(z)).subs(pow(x * y, 2), w));

  // Don't do replacement if the candidate powers are less than the target:
  ASSERT_IDENTICAL(pow(x, 2 / 3_s), pow(x, 2 / 3_s).subs(x * x, y));
  ASSERT_IDENTICAL(pow(x, -7 / 6_s), pow(x, -7 / 6_s).subs(x * x, y));
  ASSERT_IDENTICAL(pow(x, -35 / 17_s), pow(x, -35 / 17_s).subs(x * x * x, z));
  ASSERT_IDENTICAL(pow(x, 3 / 4_s) * pow(y, 2 / 7_s),
                   (pow(x, 3 / 4_s) * pow(y, 2 / 7_s)).subs(pow(x * y, 2), w));

  // Don't do anything w/ floats, unless the coefficient matches exactly:
  ASSERT_IDENTICAL(x * 2.2, (x * 2.2).subs(x * 1.5, y));
  ASSERT_IDENTICAL(x * w, (x * z * 5.1).subs(z * 5.1, w));
  ASSERT_IDENTICAL(y * pow(x, 1_s / 4) * pow(w, 1_s / 3),
                   (1.125 * pow(x, 5_s / 4) * pow(w, 4_s / 3)).subs(1.125 * x * w, y));

  // Check recursion behavior - this should not collapse to x:
  ASSERT_IDENTICAL(log(x * x), log(x * log(x * x)).subs(log(x * x), x));
}

TEST(SubstituteTest, TestMatrix) {
  const auto [a, b, c] = make_symbols("a", "b", "c");
  const MatrixExpr m0 = make_vector(a + b, c + sin(b), 22 + c);
  ASSERT_IDENTICAL(make_vector(a - log(c), c - sin(log(c)), 22 + c), m0.subs(b, -log(c)));
}

TEST(SubstituteTest, TestRelational) {
  const auto [a, b, c] = make_symbols("a", "b", "c");
  ASSERT_IDENTICAL(b < c, (a < c).subs(a, b));
  ASSERT_IDENTICAL(Constants::True, (a == 5).subs(a, 5));
  ASSERT_IDENTICAL(Constants::True, (b + c > 2).subs(b, 5 / 4_s).subs(c, 1));
  ASSERT_IDENTICAL(Constants::False, (b - c >= 2).subs(b, 2).subs(c, 1));
}

}  // namespace math
