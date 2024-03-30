// Copyright 2024 Gareth Cross
#include "wf/functions.h"
#include "wf/constants.h"
#include "wf/expressions/function_expressions.h"

#include "wf_test_support/test_macros.h"

namespace wf {
using namespace wf::custom_literals;

using complex_double = std::complex<double>;

TEST(FunctionsTest, TestLog) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(log(x), log(x));
  ASSERT_NOT_IDENTICAL(log(x), log(y));
  ASSERT_IDENTICAL(constants::one, log(constants::euler));
  ASSERT_IDENTICAL(constants::zero, log(1_s));

  ASSERT_COMPLEX_NEAR(std::log(complex_double(2.4345)), log(2.4345_s), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::log(complex_double(0.411)), log(0.411_s), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::log(complex_double(-0.8)), log(-0.8_s), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::log(complex_double(1.2, 9.8)), log(scalar_expr::from_complex(1.2, 9.8)),
                      1.0e-15);
}

TEST(FunctionsTest, TestCosine) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(cos(x), cos(x));
  ASSERT_NOT_IDENTICAL(cos(x), cos(y));

  ASSERT_IDENTICAL(constants::one, cos(0_s));
  for (int i = -15; i < 15; ++i) {
    // Even and odd multiples of pi.
    ASSERT_IDENTICAL(constants::one, cos(scalar_expr(i * 2) * constants::pi));
    ASSERT_IDENTICAL(constants::negative_one, cos(scalar_expr(i * 2 + 1) * constants::pi));
  }

  ASSERT_IDENTICAL(constants::zero, cos(constants::pi / 2_s));
  ASSERT_IDENTICAL(constants::zero, cos(-constants::pi / 2_s));
  for (int i = -15; i < 15; ++i) {
    // Multiples of pi/2
    ASSERT_IDENTICAL(constants::zero, cos(constants::pi / 2_s + scalar_expr(i) * constants::pi));
  }

  // Modulo:
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(cos(constants::pi * 3_s / 5_s),
                     cos(constants::pi * 3_s / 5_s + scalar_expr(2 * i) * constants::pi));
    ASSERT_IDENTICAL(cos(constants::pi * -2_s / 7_s),
                     cos(constants::pi * -2_s / 7_s + scalar_expr(2 * i) * constants::pi));
  }

  ASSERT_IDENTICAL(make_expr<function>(built_in_function::cos, constants::pi / 4),
                   cos(constants::pi / 4));
  ASSERT_IDENTICAL(make_expr<function>(built_in_function::cos, constants::pi / 6),
                   cos(constants::pi / 6));
  ASSERT_IDENTICAL(make_expr<function>(built_in_function::cos, -constants::pi / 12),
                   cos(-constants::pi / 12));

  // Sign adjustment
  ASSERT_IDENTICAL(cos(x), cos(-x));
  ASSERT_IDENTICAL(cos(5_s * x * y), cos(-y * 5_s * x));
  ASSERT_IDENTICAL(cos(3_s), cos(-3_s));

  // Evaluation on floats:
  for (const double v : {-0.51, 0.78, 1.8, -2.1}) {
    ASSERT_IDENTICAL(scalar_expr(std::cos(v)), cos(v));
  }

  ASSERT_IDENTICAL(constants::undefined, cos(constants::complex_infinity));
  ASSERT_IDENTICAL(constants::undefined, cos(constants::undefined));

  ASSERT_IDENTICAL(cosh(x), cos(x * constants::imaginary_unit));
  ASSERT_IDENTICAL(cosh(1), cos(constants::imaginary_unit));
}

TEST(FunctionsTest, TestSine) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(sin(x), sin(x));
  ASSERT_NOT_IDENTICAL(sin(x), sin(y));

  ASSERT_IDENTICAL(constants::zero, sin(0_s));
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(constants::zero, sin(scalar_expr(i * 2) * constants::pi));
    ASSERT_IDENTICAL(constants::zero, sin(scalar_expr(i * 2 + 1) * constants::pi));
  }

  ASSERT_IDENTICAL(constants::one, sin(constants::pi / 2_s));
  ASSERT_IDENTICAL(constants::negative_one, sin(-constants::pi / 2_s));
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(constants::one, sin(constants::pi / 2_s + scalar_expr(i * 2) * constants::pi));
    ASSERT_IDENTICAL(constants::negative_one,
                     sin(-constants::pi / 2_s + scalar_expr(i * 2) * constants::pi));
  }

  // Modulo:
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(sin(constants::pi * 2_s / 11_s),
                     sin(constants::pi * 2_s / 11_s + scalar_expr(2 * i) * constants::pi));
    ASSERT_IDENTICAL(sin(constants::pi * -6_s / 13_s),
                     sin(constants::pi * -6_s / 13_s + scalar_expr(2 * i) * constants::pi));
  }

  ASSERT_IDENTICAL(make_expr<function>(built_in_function::sin, constants::pi / 4),
                   sin(constants::pi / 4));
  ASSERT_IDENTICAL(make_expr<function>(built_in_function::sin, constants::pi / 6),
                   sin(constants::pi / 6));
  ASSERT_IDENTICAL(make_expr<function>(built_in_function::sin, -constants::pi / 12),
                   sin(-constants::pi / 12));

  ASSERT_IDENTICAL(-sin(x), sin(-x));
  ASSERT_IDENTICAL(-sin(y / x), sin(y / -x));
  ASSERT_IDENTICAL(-sin(3_s / 5_s), sin(-3_s / 5_s));

  for (double v : {6.0, 0.112, -0.65, 0.22}) {
    ASSERT_IDENTICAL(scalar_expr(std::sin(v)), sin(v));
  }

  ASSERT_IDENTICAL(constants::undefined, sin(constants::complex_infinity));
  ASSERT_IDENTICAL(constants::undefined, sin(constants::undefined));

  ASSERT_IDENTICAL(sinh(x) * constants::imaginary_unit, sin(x * constants::imaginary_unit));
  ASSERT_IDENTICAL(sinh(1) * constants::imaginary_unit, sin(constants::imaginary_unit));
}

TEST(FunctionsTest, TestTan) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(tan(x), tan(x));
  ASSERT_NOT_IDENTICAL(tan(x), tan(y));

  // Zero at multiples of pi.
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(constants::zero, tan(scalar_expr(i) * constants::pi));
  }

  // complex_infinity at odd multiples of pi/2:
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(constants::complex_infinity,
                     tan(scalar_expr(i * 2 + 1) * constants::pi / 2_s));
  }

  // Modulo pi:
  for (int i = -15; i < 15; ++i) {
    ASSERT_IDENTICAL(tan(constants::pi * 4_s / 11_s),
                     tan(constants::pi * 4_s / 11_s + scalar_expr(i) * constants::pi));
    ASSERT_IDENTICAL(tan(constants::pi * -3_s / 13_s),
                     tan(constants::pi * -3_s / 13_s + scalar_expr(i) * constants::pi));
  }

  ASSERT_IDENTICAL(make_expr<function>(built_in_function::tan, constants::pi / 4),
                   tan(constants::pi / 4));
  ASSERT_IDENTICAL(make_expr<function>(built_in_function::tan, constants::pi / 6),
                   tan(constants::pi / 6));
  ASSERT_IDENTICAL(make_expr<function>(built_in_function::tan, -constants::pi / 12),
                   tan(-constants::pi / 12));

  ASSERT_IDENTICAL(-tan(x), tan(-x));
  ASSERT_IDENTICAL(-tan(x * y), tan(-x * y));

  for (const double v : {-4.0, -0.132, 1.6, 6.8}) {
    ASSERT_COMPLEX_NEAR(std::tan(complex_double(v)), tan(v), 1.0e-15);
  }

  ASSERT_IDENTICAL(constants::undefined, tan(constants::complex_infinity));
  ASSERT_IDENTICAL(constants::undefined, tan(constants::undefined));

  ASSERT_IDENTICAL(tanh(x) * constants::imaginary_unit, tan(x * constants::imaginary_unit));
  ASSERT_IDENTICAL(tanh(1) * constants::imaginary_unit, tan(constants::imaginary_unit));
}

TEST(FunctionsTest, TestArccos) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(acos(x), acos(x));
  ASSERT_NOT_IDENTICAL(acos(x), acos(y));

  ASSERT_IDENTICAL(constants::zero, acos(1_s));
  ASSERT_IDENTICAL(constants::pi, acos(-1_s));
  ASSERT_IDENTICAL(constants::pi / 2_s, acos(constants::zero));

  ASSERT_COMPLEX_NEAR(std::acos(complex_double(0.67)), acos(0.67), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::acos(complex_double{-0.4, 0.78}),
                      acos(scalar_expr::from_complex(-0.4, 0.78)), 1.0e-15);

  ASSERT_IDENTICAL(constants::undefined, acos(constants::undefined));
  ASSERT_IDENTICAL(constants::undefined, acos(constants::complex_infinity));
}

TEST(FunctionsTest, TestArcsin) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(asin(x), asin(x));
  ASSERT_NOT_IDENTICAL(asin(x), asin(y));
  ASSERT_IDENTICAL(-asin(x), asin(-x));
  ASSERT_IDENTICAL(-asin(x * y * 3_s), asin(-x * 3_s * y));

  ASSERT_IDENTICAL(constants::zero, asin(0_s));
  ASSERT_IDENTICAL(constants::pi / 2_s, asin(1_s));
  ASSERT_IDENTICAL(-constants::pi / 2_s, asin(-1_s));

  ASSERT_COMPLEX_NEAR(std::asin(complex_double(0.67)), asin(0.67), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::asin(complex_double{-0.4, 0.78}),
                      asin(scalar_expr::from_complex(-0.4, 0.78)), 1.0e-15);

  ASSERT_IDENTICAL(constants::undefined, asin(constants::undefined));
  ASSERT_IDENTICAL(constants::undefined, asin(constants::complex_infinity));
}

TEST(FunctionsTest, TestArctan) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(atan(x), atan(x));
  ASSERT_NOT_IDENTICAL(atan(x), atan(y));
  ASSERT_IDENTICAL(-atan(x), atan(-x));

  ASSERT_IDENTICAL(constants::zero, atan(0_s));
  ASSERT_IDENTICAL(constants::pi / 4_s, atan(1_s));
  ASSERT_IDENTICAL(-constants::pi / 4_s, atan(-1_s));

  ASSERT_COMPLEX_NEAR(std::atan(complex_double(0.67)), atan(0.67), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::atan(complex_double{-0.4, 0.78}),
                      atan(scalar_expr::from_complex(-0.4, 0.78)), 1.0e-15);

  ASSERT_IDENTICAL(constants::undefined, atan(constants::undefined));
  ASSERT_IDENTICAL(constants::undefined, atan(constants::complex_infinity));
}

TEST(FunctionsTest, TestCosh) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_NOT_IDENTICAL(cosh(x), cosh(y));
  ASSERT_IDENTICAL(1, cosh(0));

  ASSERT_COMPLEX_NEAR(std::cosh(complex_double(1.321)), cosh(1.321), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::cosh(complex_double(1.2, -1.4)),
                      cosh(scalar_expr::from_complex(1.2, -1.4)), 1.0e-15);

  ASSERT_IDENTICAL(cosh(x), cosh(-x));
  ASSERT_IDENTICAL(cosh(78 / y), cosh(-78 / y));

  // conversion of cosh -> cos
  ASSERT_IDENTICAL(cos(x), cosh(x * constants::imaginary_unit));
  ASSERT_IDENTICAL(cos(22 * y), cosh(22 * y * constants::imaginary_unit));
  ASSERT_IDENTICAL(cos(1), cosh(constants::imaginary_unit));

  ASSERT_IDENTICAL(constants::undefined, cosh(constants::undefined));
  ASSERT_IDENTICAL(constants::undefined, cosh(constants::complex_infinity));

  ASSERT_IDENTICAL(x, cosh(acosh(x)));
  ASSERT_IDENTICAL(x * y, cosh(acosh(x * y)));
}

TEST(FunctionsTest, TestSinh) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_NOT_IDENTICAL(sinh(x), sinh(y));
  ASSERT_IDENTICAL(0, sinh(0));

  ASSERT_COMPLEX_NEAR(std::sinh(complex_double(-0.3231)), sinh(-0.3231), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::sinh(complex_double(1.2, -1.4)),
                      sinh(scalar_expr::from_complex(1.2, -1.4)), 1.0e-15);

  ASSERT_IDENTICAL(-sinh(x), sinh(-x));
  ASSERT_IDENTICAL(-sinh(constants::pi * 33 * y / x), sinh(-constants::pi * 33 * y / x));

  // conversion of sinh -> sin
  ASSERT_IDENTICAL(constants::imaginary_unit * sin(x), sinh(x * constants::imaginary_unit));
  ASSERT_IDENTICAL(constants::imaginary_unit * -sin(5 * y),
                   sinh(y * -constants::imaginary_unit * 5));
  ASSERT_IDENTICAL(constants::imaginary_unit * sin(1), sinh(constants::imaginary_unit));

  ASSERT_IDENTICAL(constants::undefined, sinh(constants::undefined));
  ASSERT_IDENTICAL(constants::undefined, sinh(constants::complex_infinity));

  ASSERT_IDENTICAL(x, sinh(asinh(x)));
  ASSERT_IDENTICAL(x * y, sinh(asinh(x * y)));
}

TEST(FunctionsTest, TestTanh) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_NOT_IDENTICAL(tanh(x), tanh(y));
  ASSERT_IDENTICAL(0, tanh(0));

  ASSERT_COMPLEX_NEAR(std::tanh(complex_double(9.23)), tanh(9.23), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::tanh(complex_double(0.52, -0.78)),
                      tanh(scalar_expr::from_complex(0.52, -0.78)), 1.0e-15);

  ASSERT_IDENTICAL(-tanh(x), tanh(-x));
  ASSERT_IDENTICAL(-tanh(x * y * 3_s / 2), tanh(-x * y * 3_s / 2));

  ASSERT_IDENTICAL(constants::imaginary_unit * tan(x), tanh(x * constants::imaginary_unit));
  ASSERT_IDENTICAL(constants::imaginary_unit * -tan(x * y),
                   tanh(x * constants::imaginary_unit * -y));
  ASSERT_IDENTICAL(constants::imaginary_unit * tan(1), tanh(constants::imaginary_unit));

  ASSERT_IDENTICAL(constants::undefined, tanh(constants::undefined));
  ASSERT_IDENTICAL(constants::undefined, tanh(constants::complex_infinity));

  ASSERT_IDENTICAL(x, tanh(atanh(x)));
  ASSERT_IDENTICAL(x * y, tanh(atanh(x * y)));
}

TEST(FunctionsTest, TestArccosh) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_NOT_IDENTICAL(acosh(x), acosh(y));
  ASSERT_TRUE(acosh(3).is_type<function>());  //  Does not simplify.

  ASSERT_IDENTICAL(constants::pi * constants::imaginary_unit / 2, acosh(0));

  // Check that we call the complex version of acosh()
  ASSERT_COMPLEX_NEAR(std::acosh(complex_double(1.23123)), acosh(1.23123_s), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::acosh(complex_double(1.0)), acosh(1.0), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::acosh(complex_double(0.8508)), acosh(0.8508_s), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::acosh(complex_double(-0.645)), acosh(-0.645_s), 1.0e-15);

  ASSERT_IDENTICAL(constants::undefined, acosh(constants::undefined));
  ASSERT_IDENTICAL(constants::undefined, acosh(constants::complex_infinity));
}

TEST(FunctionsTest, TestArcsinh) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_NOT_IDENTICAL(asinh(x), asinh(y));
  ASSERT_TRUE(asinh(2).is_type<function>());

  ASSERT_IDENTICAL(0, asinh(0));

  ASSERT_COMPLEX_NEAR(std::asinh(complex_double(0.76)), asinh(0.76_s), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::asinh(complex_double(0.0)), asinh(0.0_s), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::asinh(complex_double(0.115)), asinh(0.115), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::asinh(complex_double(-1.8)), asinh(-1.8_s), 1.0e-15);

  ASSERT_IDENTICAL(constants::undefined, asinh(constants::undefined));
  ASSERT_IDENTICAL(constants::undefined, asinh(constants::complex_infinity));
}

TEST(FunctionsTest, TestArctanh) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_NOT_IDENTICAL(atanh(x), atanh(y));
  ASSERT_TRUE(atanh(2).is_type<function>());

  ASSERT_IDENTICAL(0, atanh(0));

  ASSERT_COMPLEX_NEAR(std::atanh(complex_double(0.45)), atanh(0.45_s), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::atanh(complex_double(-0.23)), atanh(-0.23_s), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::atanh(complex_double(1.254)), atanh(1.254), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::atanh(complex_double(-1.78)), atanh(-1.78_s), 1.0e-15);

  ASSERT_IDENTICAL(constants::undefined, atanh(constants::undefined));
  ASSERT_IDENTICAL(constants::undefined, atanh(constants::complex_infinity));
}

TEST(FunctionsTest, TestArctan2) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(atan2(y, x), atan2(y, x));
  ASSERT_NOT_IDENTICAL(atan2(y, x), atan2(x, y));
  ASSERT_IDENTICAL(0, atan2(0, 1));
  ASSERT_IDENTICAL(constants::pi / 2, atan2(1, 0));
  ASSERT_IDENTICAL(constants::pi, atan2(y, x).subs(y, 0).subs(x, -1));
  ASSERT_IDENTICAL(-constants::pi / 2, atan2(y, x).subs(x, 0).subs(y, -1));

  ASSERT_IDENTICAL(constants::pi / 4, atan2(5, 5));
  ASSERT_IDENTICAL(3 * constants::pi / 4, atan2(8, -8));
  ASSERT_IDENTICAL(-constants::pi / 4, atan2(-2, 2));
  ASSERT_IDENTICAL(-3 * constants::pi / 4, atan2(-4, -4));

  // floating point inputs should be evaluated immediately
  ASSERT_IDENTICAL(std::atan2(0.1, -0.6), atan2(0.1, -0.6));
  ASSERT_IDENTICAL(std::atan2(-1.2, 0.2), atan2(-1.2, 0.2));

  // undefined and infinity
  ASSERT_IDENTICAL(constants::undefined, atan2(y, constants::undefined));
  ASSERT_IDENTICAL(constants::undefined, atan2(y, constants::complex_infinity));
  ASSERT_IDENTICAL(constants::undefined, atan2(constants::undefined, x));
  ASSERT_IDENTICAL(constants::undefined, atan2(constants::complex_infinity, x));
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
  ASSERT_COMPLEX_NEAR(std::abs(complex_double(-0.8)), abs(-0.8_s), 1.0e-15);
  ASSERT_COMPLEX_NEAR(std::abs(complex_double(1.2, 9.8)), abs(scalar_expr::from_complex(1.2, 9.8)),
                      1.0e-15);

  ASSERT_IDENTICAL(constants::pi, abs(constants::pi));
  ASSERT_IDENTICAL(constants::undefined, abs(constants::complex_infinity));
  ASSERT_IDENTICAL(constants::undefined, abs(constants::undefined));
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

  ASSERT_IDENTICAL(1, signum(constants::pi));
  ASSERT_IDENTICAL(1, signum(constants::euler));

  ASSERT_IDENTICAL(constants::undefined, signum(constants::undefined));
}

TEST(FunctionsTest, TestFloor) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(floor(x), floor(x));
  ASSERT_NOT_IDENTICAL(floor(x), floor(y));

  // operation on integers
  ASSERT_IDENTICAL(0, floor(0_s));
  ASSERT_IDENTICAL(13, floor(13_s));
  ASSERT_IDENTICAL(-28, floor(-28_s));

  // rationals
  ASSERT_IDENTICAL(0, floor(7_s / 9));
  ASSERT_IDENTICAL(1, floor(3_s / 2));
  ASSERT_IDENTICAL(5, floor(23_s / 4));
  ASSERT_IDENTICAL(-16, floor(-78_s / 5));
  ASSERT_IDENTICAL(-8, floor(-103_s / 13));
  ASSERT_IDENTICAL(-1, floor(-1_s / 3));

  // floats
  ASSERT_IDENTICAL(0, floor(0.0_s));
  ASSERT_IDENTICAL(1, floor(1.0_s));
  ASSERT_IDENTICAL(3, floor(3.9999_s));
  ASSERT_IDENTICAL(-1, floor(-1.0_s));
  ASSERT_IDENTICAL(-2, floor(-1.01_s));

  // special constants
  ASSERT_IDENTICAL(2, floor(constants::euler));
  ASSERT_IDENTICAL(3, floor(constants::pi));
  ASSERT_IDENTICAL(constants::undefined, floor(constants::undefined));
  ASSERT_IDENTICAL(constants::complex_infinity, floor(constants::complex_infinity));

  // Simplify nested call:
  ASSERT_IDENTICAL(floor(x), floor(floor(x)));
}

TEST(FunctionTest, TestMinMax) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  ASSERT_NOT_IDENTICAL(max(x, y), max(y, x));  //  order matters
  ASSERT_NOT_IDENTICAL(max(x, y), max(x, z));
  ASSERT_IDENTICAL(max(x, y), where(x < y, y, x));

  ASSERT_IDENTICAL(2, max(1, 2));
  ASSERT_IDENTICAL(0, max(-5, 0));
  ASSERT_IDENTICAL(constants::pi, max(3, constants::pi));
  ASSERT_IDENTICAL(6.02, max(5, 6.02));
  ASSERT_IDENTICAL(constants::pi, max(constants::euler, constants::pi));
  ASSERT_IDENTICAL(3_s / 2, max(-1, 3_s / 2));

  ASSERT_NOT_IDENTICAL(min(x, y), min(y, x));  //  order matters
  ASSERT_NOT_IDENTICAL(min(x, y), min(x, z));
  ASSERT_IDENTICAL(min(x, y), where(y < x, y, x));

  ASSERT_IDENTICAL(0, min(1, 0));
  ASSERT_IDENTICAL(1.4123, min(1.4123, 10));
  ASSERT_IDENTICAL(-10, min(-10, constants::euler));

  ASSERT_IDENTICAL(x, max(x, x));
  ASSERT_IDENTICAL(z + 2, min(z + 2, z + 2));
}

}  // namespace wf
