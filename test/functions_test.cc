#include "functions.h"
#include "constants.h"
#include "expressions/numeric_expressions.h"
#include "test_helpers.h"

namespace math {

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
    ASSERT_IDENTICAL(Float::Create(std::cos(v)), cos(Expr{v}));
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
    ASSERT_IDENTICAL(Float::Create(std::sin(v)), sin(Expr{v}));
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
    ASSERT_IDENTICAL(Float::Create(std::tan(v)), tan(Expr{v}));
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

}  // namespace math
