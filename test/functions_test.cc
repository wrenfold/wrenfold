#include "functions.h"
#include "constants.h"
#include "expressions/function_expressions.h"
#include "expressions/numeric_expressions.h"
#include "test_helpers.h"

namespace math {
using namespace math::custom_literals;

TEST(FunctionsTest, TestLog) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(log(x), log(x));
  ASSERT_NOT_IDENTICAL(log(x), log(y));
  ASSERT_IDENTICAL(Constants::One, log(Constants::Euler));
  ASSERT_IDENTICAL(Constants::Zero, log(1_s));
}

TEST(FunctionsTest, TestCosine) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(cos(x), cos(x));
  ASSERT_NOT_IDENTICAL(cos(x), cos(y));

  ASSERT_IDENTICAL(Constants::One, cos(0_s));
  for (int i = -15; i < 15; ++i) {
    // Even and odd multiples of pi.
    ASSERT_IDENTICAL(Constants::One, cos(Integer::Create(i * 2) * Constants::Pi));
    ASSERT_IDENTICAL(Constants::NegativeOne, cos(Integer::Create(i * 2 + 1) * Constants::Pi));
  }

  ASSERT_IDENTICAL(Constants::Zero, cos(Constants::Pi / 2_s));
  ASSERT_IDENTICAL(Constants::Zero, cos(-Constants::Pi / 2_s));
  for (int i = -15; i < 15; ++i) {
    // Multiples of pi/2
    ASSERT_IDENTICAL(Constants::Zero,
                     cos(Constants::Pi / 2_s + Integer::Create(i) * Constants::Pi));
  }

  // Modulo:
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(cos(Constants::Pi * 3_s / 5_s),
                     cos(Constants::Pi * 3_s / 5_s + Integer::Create(2 * i) * Constants::Pi));
    ASSERT_IDENTICAL(cos(Constants::Pi * -2_s / 7_s),
                     cos(Constants::Pi * -2_s / 7_s + Integer::Create(2 * i) * Constants::Pi));
  }

  // Sign adjustment
  ASSERT_IDENTICAL(cos(x), cos(-x));
  ASSERT_IDENTICAL(cos(5_s * x * y), cos(-y * 5_s * x));
  ASSERT_IDENTICAL(cos(3_s), cos(-3_s));

  // Evaluation on floats:
  for (double v : {-0.51, 0.78, 1.8, -2.1}) {
    ASSERT_IDENTICAL(Float::Create(std::cos(v)), cos(v));
  }
}

TEST(FunctionsTest, TestSine) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(sin(x), sin(x));
  ASSERT_NOT_IDENTICAL(sin(x), sin(y));

  ASSERT_IDENTICAL(Constants::Zero, sin(0_s));
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(Constants::Zero, sin(Integer::Create(i * 2) * Constants::Pi));
    ASSERT_IDENTICAL(Constants::Zero, sin(Integer::Create(i * 2 + 1) * Constants::Pi));
  }

  ASSERT_IDENTICAL(Constants::One, sin(Constants::Pi / 2_s));
  ASSERT_IDENTICAL(Constants::NegativeOne, sin(-Constants::Pi / 2_s));
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(Constants::One,
                     sin(Constants::Pi / 2_s + Integer::Create(i * 2) * Constants::Pi));
    ASSERT_IDENTICAL(Constants::NegativeOne,
                     sin(-Constants::Pi / 2_s + Integer::Create(i * 2) * Constants::Pi));
  }

  // Modulo:
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(sin(Constants::Pi * 2_s / 11_s),
                     sin(Constants::Pi * 2_s / 11_s + Integer::Create(2 * i) * Constants::Pi));
    ASSERT_IDENTICAL(sin(Constants::Pi * -6_s / 13_s),
                     sin(Constants::Pi * -6_s / 13_s + Integer::Create(2 * i) * Constants::Pi));
  }

  ASSERT_IDENTICAL(-sin(x), sin(-x));
  ASSERT_IDENTICAL(-sin(y / x), sin(y / -x));
  ASSERT_IDENTICAL(-sin(3_s / 5_s), sin(-3_s / 5_s));

  for (double v : {6.0, 0.112, -0.65, 0.22}) {
    ASSERT_IDENTICAL(Float::Create(std::sin(v)), sin(v));
  }
}

TEST(FunctionsTest, TestTan) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(tan(x), tan(x));
  ASSERT_NOT_IDENTICAL(tan(x), tan(y));

  // Zero at multiples of pi.
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(Constants::Zero, tan(Integer::Create(i) * Constants::Pi));
  }

  // Infinity at odd multiples of pi/2:
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(Constants::Infinity, tan(Integer::Create(i * 2 + 1) * Constants::Pi / 2_s));
  }

  // Modulo pi:
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(tan(Constants::Pi * 4_s / 11_s),
                     tan(Constants::Pi * 4_s / 11_s + Integer::Create(i) * Constants::Pi));
    ASSERT_IDENTICAL(tan(Constants::Pi * -3_s / 13_s),
                     tan(Constants::Pi * -3_s / 13_s + Integer::Create(i) * Constants::Pi));
  }

  ASSERT_IDENTICAL(-tan(x), tan(-x));
  ASSERT_IDENTICAL(-tan(x * y), tan(-x * y));

  for (double v : {-4.0, -0.132, 1.6, 6.8}) {
    ASSERT_IDENTICAL(Float::Create(std::tan(v)), tan(v));
  }
}

TEST(FunctionsTest, TestArccos) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(acos(x), acos(x));
  ASSERT_NOT_IDENTICAL(acos(x), acos(y));
  ASSERT_IDENTICAL(Constants::Zero, acos(1_s));
  ASSERT_IDENTICAL(Constants::Pi, acos(-1_s));
  ASSERT_IDENTICAL(Constants::Pi / 2_s, acos(Constants::Zero));
}

TEST(FunctionsTest, TestArcsin) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(asin(x), asin(x));
  ASSERT_NOT_IDENTICAL(asin(x), asin(y));
  ASSERT_IDENTICAL(-asin(x), asin(-x));
  ASSERT_IDENTICAL(-asin(x * y * 3_s), asin(-x * 3_s * y));

  ASSERT_IDENTICAL(Constants::Zero, asin(0_s));
  ASSERT_IDENTICAL(Constants::Pi / 2_s, asin(1_s));
  ASSERT_IDENTICAL(-Constants::Pi / 2_s, asin(-1_s));
}

TEST(FunctionsTest, TestArctan) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(atan(x), atan(x));
  ASSERT_NOT_IDENTICAL(atan(x), atan(y));
  ASSERT_IDENTICAL(-atan(x), atan(-x));

  ASSERT_IDENTICAL(Constants::Zero, atan(0_s));
  ASSERT_IDENTICAL(Constants::Pi / 4_s, atan(1_s));
  ASSERT_IDENTICAL(-Constants::Pi / 4_s, atan(-1_s));
}

TEST(FunctionsTest, TestArctan2) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(atan2(y, x), atan2(y, x));
  ASSERT_NOT_IDENTICAL(atan2(y, x), atan2(x, y));
  ASSERT_IDENTICAL(0, atan2(0, 1));
  ASSERT_IDENTICAL(Constants::Pi / 2, atan2(1, 0));
  ASSERT_IDENTICAL(Constants::Pi, atan2(y, x).Subs(y, 0).Subs(x, -1));
  ASSERT_IDENTICAL(-Constants::Pi / 2, atan2(y, x).Subs(x, 0).Subs(y, -1));

  ASSERT_IDENTICAL(Constants::Pi / 4, atan2(5, 5));
  ASSERT_IDENTICAL(3 * Constants::Pi / 4, atan2(8, -8));
  ASSERT_IDENTICAL(-Constants::Pi / 4, atan2(-2, 2));
  ASSERT_IDENTICAL(-3 * Constants::Pi / 4, atan2(-4, -4));

  // floating point inputs should be evaluated immediately
  ASSERT_IDENTICAL(std::atan2(0.1, -0.6), atan2(0.1, -0.6));
  ASSERT_IDENTICAL(std::atan2(-1.2, 0.2), atan2(-1.2, 0.2));
}

TEST(FunctionsTest, TestAbs) {
  const Expr x{"x"};
  const Expr y{"y"};
  ASSERT_IDENTICAL(abs(x), abs(x));
  ASSERT_NOT_IDENTICAL(abs(x), abs(y));
  ASSERT_IDENTICAL(abs(x), abs(abs(x)));
  ASSERT_IDENTICAL(abs(y - 3), abs(abs(abs(y - 3))));
  ASSERT_IDENTICAL(3, abs(3));
  ASSERT_IDENTICAL(7, abs(-7));
  ASSERT_IDENTICAL(3 / 2_s, abs(-3 / 2_s));
  ASSERT_IDENTICAL(0.1, abs(-0.1));
  ASSERT_IDENTICAL(Constants::Pi, abs(Constants::Pi));
}

TEST(FunctionTest, TestMinMax) {
  const auto [x, y, z] = Symbols("x", "y", "z");
  ASSERT_NOT_IDENTICAL(max(x, y), max(y, x));  //  order matters
  ASSERT_NOT_IDENTICAL(max(x, y), max(x, z));
  ASSERT_IDENTICAL(max(x, y), where(x < y, y, x));

  ASSERT_IDENTICAL(2, max(1, 2));
  ASSERT_IDENTICAL(0, max(-5, 0));
  ASSERT_IDENTICAL(Constants::Pi, max(3, Constants::Pi));
  ASSERT_IDENTICAL(6.02, max(5, 6.02));
  ASSERT_IDENTICAL(Constants::Pi, max(Constants::Euler, Constants::Pi));
  ASSERT_IDENTICAL(3_s / 2, max(-1, 3_s / 2));

  ASSERT_NOT_IDENTICAL(min(x, y), min(y, x));  //  order matters
  ASSERT_NOT_IDENTICAL(min(x, y), min(x, z));
  ASSERT_IDENTICAL(min(x, y), where(y < x, y, x));

  ASSERT_IDENTICAL(0, min(1, 0));
  ASSERT_IDENTICAL(1.4123, min(1.4123, 10));
  ASSERT_IDENTICAL(Constants::False, min(Constants::False, Constants::True));
  ASSERT_IDENTICAL(-10, min(-10, Constants::Euler));

  ASSERT_IDENTICAL(x, max(x, x));
  ASSERT_IDENTICAL(z + 2, min(z + 2, z + 2));
}

}  // namespace math
