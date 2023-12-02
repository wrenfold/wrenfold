#include "wf/functions.h"
#include "wf/constants.h"
#include "wf/expressions/function_expressions.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf_test_support/test_macros.h"

namespace math {
using namespace math::custom_literals;

TEST(FunctionsTest, TestLog) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(log(x), log(x));
  ASSERT_NOT_IDENTICAL(log(x), log(y));
  ASSERT_IDENTICAL(Constants::One, log(Constants::Euler));
  ASSERT_IDENTICAL(Constants::Zero, log(1_s));
}

TEST(FunctionsTest, TestCosine) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(cos(x), cos(x));
  ASSERT_NOT_IDENTICAL(cos(x), cos(y));

  ASSERT_IDENTICAL(Constants::One, cos(0_s));
  for (int i = -15; i < 15; ++i) {
    // Even and odd multiples of pi.
    ASSERT_IDENTICAL(Constants::One, cos(Integer::create(i * 2) * Constants::Pi));
    ASSERT_IDENTICAL(Constants::NegativeOne, cos(Integer::create(i * 2 + 1) * Constants::Pi));
  }

  ASSERT_IDENTICAL(Constants::Zero, cos(Constants::Pi / 2_s));
  ASSERT_IDENTICAL(Constants::Zero, cos(-Constants::Pi / 2_s));
  for (int i = -15; i < 15; ++i) {
    // Multiples of pi/2
    ASSERT_IDENTICAL(Constants::Zero,
                     cos(Constants::Pi / 2_s + Integer::create(i) * Constants::Pi));
  }

  // Modulo:
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(cos(Constants::Pi * 3_s / 5_s),
                     cos(Constants::Pi * 3_s / 5_s + Integer::create(2 * i) * Constants::Pi));
    ASSERT_IDENTICAL(cos(Constants::Pi * -2_s / 7_s),
                     cos(Constants::Pi * -2_s / 7_s + Integer::create(2 * i) * Constants::Pi));
  }

  // Sign adjustment
  ASSERT_IDENTICAL(cos(x), cos(-x));
  ASSERT_IDENTICAL(cos(5_s * x * y), cos(-y * 5_s * x));
  ASSERT_IDENTICAL(cos(3_s), cos(-3_s));

  // Evaluation on floats:
  for (double v : {-0.51, 0.78, 1.8, -2.1}) {
    ASSERT_IDENTICAL(Float::create(std::cos(v)), cos(v));
  }

  ASSERT_IDENTICAL(Constants::Undefined, cos(Constants::ComplexInfinity));
  ASSERT_IDENTICAL(Constants::Undefined, cos(Constants::Undefined));
}

TEST(FunctionsTest, TestSine) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(sin(x), sin(x));
  ASSERT_NOT_IDENTICAL(sin(x), sin(y));

  ASSERT_IDENTICAL(Constants::Zero, sin(0_s));
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(Constants::Zero, sin(Integer::create(i * 2) * Constants::Pi));
    ASSERT_IDENTICAL(Constants::Zero, sin(Integer::create(i * 2 + 1) * Constants::Pi));
  }

  ASSERT_IDENTICAL(Constants::One, sin(Constants::Pi / 2_s));
  ASSERT_IDENTICAL(Constants::NegativeOne, sin(-Constants::Pi / 2_s));
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(Constants::One,
                     sin(Constants::Pi / 2_s + Integer::create(i * 2) * Constants::Pi));
    ASSERT_IDENTICAL(Constants::NegativeOne,
                     sin(-Constants::Pi / 2_s + Integer::create(i * 2) * Constants::Pi));
  }

  // Modulo:
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(sin(Constants::Pi * 2_s / 11_s),
                     sin(Constants::Pi * 2_s / 11_s + Integer::create(2 * i) * Constants::Pi));
    ASSERT_IDENTICAL(sin(Constants::Pi * -6_s / 13_s),
                     sin(Constants::Pi * -6_s / 13_s + Integer::create(2 * i) * Constants::Pi));
  }

  ASSERT_IDENTICAL(-sin(x), sin(-x));
  ASSERT_IDENTICAL(-sin(y / x), sin(y / -x));
  ASSERT_IDENTICAL(-sin(3_s / 5_s), sin(-3_s / 5_s));

  for (double v : {6.0, 0.112, -0.65, 0.22}) {
    ASSERT_IDENTICAL(Float::create(std::sin(v)), sin(v));
  }

  ASSERT_IDENTICAL(Constants::Undefined, sin(Constants::ComplexInfinity));
  ASSERT_IDENTICAL(Constants::Undefined, sin(Constants::Undefined));
}

TEST(FunctionsTest, TestTan) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(tan(x), tan(x));
  ASSERT_NOT_IDENTICAL(tan(x), tan(y));

  // Zero at multiples of pi.
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(Constants::Zero, tan(Integer::create(i) * Constants::Pi));
  }

  // Infinity at odd multiples of pi/2:
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(Constants::ComplexInfinity,
                     tan(Integer::create(i * 2 + 1) * Constants::Pi / 2_s));
  }

  // Modulo pi:
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(tan(Constants::Pi * 4_s / 11_s),
                     tan(Constants::Pi * 4_s / 11_s + Integer::create(i) * Constants::Pi));
    ASSERT_IDENTICAL(tan(Constants::Pi * -3_s / 13_s),
                     tan(Constants::Pi * -3_s / 13_s + Integer::create(i) * Constants::Pi));
  }

  ASSERT_IDENTICAL(-tan(x), tan(-x));
  ASSERT_IDENTICAL(-tan(x * y), tan(-x * y));

  for (double v : {-4.0, -0.132, 1.6, 6.8}) {
    ASSERT_IDENTICAL(Float::create(std::tan(v)), tan(v));
  }

  ASSERT_IDENTICAL(Constants::Undefined, tan(Constants::ComplexInfinity));
  ASSERT_IDENTICAL(Constants::Undefined, tan(Constants::Undefined));
}

TEST(FunctionsTest, TestArccos) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(acos(x), acos(x));
  ASSERT_NOT_IDENTICAL(acos(x), acos(y));

  ASSERT_IDENTICAL(Constants::Zero, acos(1_s));
  ASSERT_IDENTICAL(Constants::Pi, acos(-1_s));
  ASSERT_IDENTICAL(Constants::Pi / 2_s, acos(Constants::Zero));

  ASSERT_IDENTICAL(Constants::Undefined, acos(Constants::Undefined));
  ASSERT_IDENTICAL(Constants::Undefined, acos(Constants::ComplexInfinity));
}

TEST(FunctionsTest, TestArcsin) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(asin(x), asin(x));
  ASSERT_NOT_IDENTICAL(asin(x), asin(y));
  ASSERT_IDENTICAL(-asin(x), asin(-x));
  ASSERT_IDENTICAL(-asin(x * y * 3_s), asin(-x * 3_s * y));

  ASSERT_IDENTICAL(Constants::Zero, asin(0_s));
  ASSERT_IDENTICAL(Constants::Pi / 2_s, asin(1_s));
  ASSERT_IDENTICAL(-Constants::Pi / 2_s, asin(-1_s));

  ASSERT_IDENTICAL(Constants::Undefined, asin(Constants::Undefined));
  ASSERT_IDENTICAL(Constants::Undefined, asin(Constants::ComplexInfinity));
}

TEST(FunctionsTest, TestArctan) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(atan(x), atan(x));
  ASSERT_NOT_IDENTICAL(atan(x), atan(y));
  ASSERT_IDENTICAL(-atan(x), atan(-x));

  ASSERT_IDENTICAL(Constants::Zero, atan(0_s));
  ASSERT_IDENTICAL(Constants::Pi / 4_s, atan(1_s));
  ASSERT_IDENTICAL(-Constants::Pi / 4_s, atan(-1_s));

  ASSERT_IDENTICAL(Constants::Undefined, atan(Constants::Undefined));
  ASSERT_IDENTICAL(Constants::Undefined, atan(Constants::ComplexInfinity));
}

TEST(FunctionsTest, TestArctan2) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(atan2(y, x), atan2(y, x));
  ASSERT_NOT_IDENTICAL(atan2(y, x), atan2(x, y));
  ASSERT_IDENTICAL(0, atan2(0, 1));
  ASSERT_IDENTICAL(Constants::Pi / 2, atan2(1, 0));
  ASSERT_IDENTICAL(Constants::Pi, atan2(y, x).subs(y, 0).subs(x, -1));
  ASSERT_IDENTICAL(-Constants::Pi / 2, atan2(y, x).subs(x, 0).subs(y, -1));

  ASSERT_IDENTICAL(Constants::Pi / 4, atan2(5, 5));
  ASSERT_IDENTICAL(3 * Constants::Pi / 4, atan2(8, -8));
  ASSERT_IDENTICAL(-Constants::Pi / 4, atan2(-2, 2));
  ASSERT_IDENTICAL(-3 * Constants::Pi / 4, atan2(-4, -4));

  // floating point inputs should be evaluated immediately
  ASSERT_IDENTICAL(std::atan2(0.1, -0.6), atan2(0.1, -0.6));
  ASSERT_IDENTICAL(std::atan2(-1.2, 0.2), atan2(-1.2, 0.2));

  // undefined and infinity
  ASSERT_IDENTICAL(Constants::Undefined, atan2(y, Constants::Undefined));
  ASSERT_IDENTICAL(Constants::Undefined, atan2(y, Constants::ComplexInfinity));
  ASSERT_IDENTICAL(Constants::Undefined, atan2(Constants::Undefined, x));
  ASSERT_IDENTICAL(Constants::Undefined, atan2(Constants::ComplexInfinity, x));
}

TEST(FunctionsTest, TestAbs) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(abs(x), abs(x));
  ASSERT_NOT_IDENTICAL(abs(x), abs(y));
  ASSERT_IDENTICAL(abs(x), abs(abs(x)));
  ASSERT_IDENTICAL(abs(y - 3), abs(abs(abs(y - 3))));
  ASSERT_IDENTICAL(3, abs(3));
  ASSERT_IDENTICAL(7, abs(-7));
  ASSERT_IDENTICAL(3 / 2_s, abs(-3 / 2_s));
  ASSERT_IDENTICAL(0.1, abs(-0.1));
  ASSERT_IDENTICAL(Constants::Pi, abs(Constants::Pi));
  ASSERT_IDENTICAL(Constants::Undefined, abs(Constants::ComplexInfinity));
  ASSERT_IDENTICAL(Constants::Undefined, abs(Constants::Undefined));
}

TEST(FunctionsTest, TestSignum) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(signum(x), signum(x));
  ASSERT_NOT_IDENTICAL(signum(x), signum(y));

  ASSERT_IDENTICAL(signum(x), signum(signum(x)));

  ASSERT_IDENTICAL(0, signum(0));
  ASSERT_IDENTICAL(1, signum(10));
  ASSERT_IDENTICAL(-1, signum(-82));

  ASSERT_IDENTICAL(0, signum(0.0));
  ASSERT_IDENTICAL(1, signum(8.911));
  ASSERT_IDENTICAL(-1, signum(-111.0));

  ASSERT_IDENTICAL(1, signum(Constants::Pi));
  ASSERT_IDENTICAL(1, signum(Constants::Euler));

  ASSERT_IDENTICAL(Constants::Undefined, signum(Constants::Undefined));
}

TEST(FunctionTest, TestMinMax) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
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
