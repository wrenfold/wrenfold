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

TEST(SubstituteTest, TestAdditions) {
  const auto [w, x, y, z] = Symbols("w", "x", "y", "z");

  ASSERT_IDENTICAL(x + z, (x + y).Subs(y, z));
  ASSERT_IDENTICAL(y, (x + z).Subs(x + z, y));
  ASSERT_IDENTICAL(y, (x + 5).Subs(x + 5, y));
  ASSERT_IDENTICAL(x + y, (x + y).Subs(x + y, x + y));

  // Replacing parts of an addition:
  ASSERT_IDENTICAL(z + x, (y + 5 + x).Subs(y + 5, z));
  ASSERT_IDENTICAL(z * 3 - log(y) + x, (w - 5_s / 3 - log(y) + x).Subs(w - 5_s / 3, z * 3));
  ASSERT_IDENTICAL(0, (z + y + w).Subs(w, -z - y));

  // Additions inside functions:
  ASSERT_IDENTICAL(cos(2 * w - z + cos(w * 2 - z)),
                   cos(x + y - z + cos(x + y - z)).Subs(x + y, w * 2));
  ASSERT_IDENTICAL(0, tan(z + z * cos(y - z + Constants::Pi / 2)).Subs(-z + y, Constants::Pi / 2));

  // Additions inside powers:
  ASSERT_IDENTICAL(pow(z, 5_s / 2_s), pow(x - y + z, 5_s / 2_s).Subs(-y + x, 0));
  ASSERT_IDENTICAL(pow(z, 4), (pow(x + y, 2) * pow(x - y, 2)).Subs(x + y, z).Subs(x - y, z));

  // Test that we don't recurse in unexpected ways:
  ASSERT_IDENTICAL(x + 2 * y, (x + 2 * (x + 2 * y)).Subs(x + 2 * y, y));
}

TEST(SubstituteTest, TestMultiplications) {
  const auto [w, x, y, z] = Symbols("w", "x", "y", "z");

  ASSERT_IDENTICAL(y, (x * w).Subs(x * w, y));
  ASSERT_IDENTICAL(x * 5, (x * w).Subs(w, 5));
  ASSERT_IDENTICAL(y * z, (5 * x * y).Subs(5 * x, z));
  ASSERT_IDENTICAL(y, (sqrt(x) * z).Subs(sqrt(x) * z, y));
  ASSERT_IDENTICAL(y * z, (sqrt(x) * z).Subs(sqrt(x), y));
  ASSERT_IDENTICAL(9 * pow(x, 2) + 3 * x + y, (z * z + z + y).Subs(z, x * 3));

  ASSERT_IDENTICAL(pow(y, 2), (x * w * y).Subs(x * w, y));
  ASSERT_IDENTICAL(pow(x, 2) * y, (x * w * z * y).Subs(w * z, x));
  ASSERT_IDENTICAL(pow(cos(x), y) * pow(sin(y), 2),
                   (pow(cos(x), y + 2) * z * sin(y)).Subs(z * pow(cos(x), 2), sin(y)));

  ASSERT_IDENTICAL(1, (x * x * x * y * y * z).Subs(x * x * x, 1 / (y * y * z)));
  ASSERT_IDENTICAL(0, cos(x * x * Constants::Pi).Subs(x, sqrt(1_s / 2)));

  // Should not replace x^2 here:
  ASSERT_IDENTICAL(pow(x, 2_s / 3), pow(x, 2_s / 3).Subs(x * x, y));

  // Some composites of additions/multiplications:
  ASSERT_IDENTICAL(0, (pow(1 + x, 3) * pow(y + 1 + x, 2)).Subs(1 + x, -y));
  ASSERT_IDENTICAL(5 * pow(5 + x, w - 1) * pow(y, z - 1),
                   (pow(5 + x, w) * pow(y, z)).Subs(pow(5 + x, 1) * y, 5));
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
