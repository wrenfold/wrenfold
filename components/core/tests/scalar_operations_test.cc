// Copyright 2024 Gareth Cross
#include "wf/constants.h"
#include "wf/expressions/addition.h"
#include "wf/expressions/conditional.h"
#include "wf/expressions/multiplication.h"
#include "wf/expressions/power.h"
#include "wf/functions.h"

#include "wf_test_support/test_macros.h"

// Test basic composition of scalars and functions of scalars.
namespace wf {
using namespace wf::custom_literals;

static_assert(std::is_nothrow_move_constructible_v<scalar_expr> &&
                  std::is_nothrow_move_assignable_v<scalar_expr>,
              "Should be movable");

// Make power (no simplifications allowed).
template <typename... Args>
auto make_pow(Args&&... args) {
  return make_expr<power>(std::forward<Args>(args)...);
}

// Make multiplication.
template <typename... Args>
auto make_mul(Args&&... args) {
  return make_expr<multiplication>(multiplication::container_type{std::forward<Args>(args)...});
}

TEST(ScalarOperationsTest, TestNumericConstructors) {
  ASSERT_IDENTICAL(constants::one, scalar_expr{1});
  ASSERT_TRUE(constants::one.has_same_address(scalar_expr{integer_constant{1}}));
  ASSERT_TRUE(constants::zero.has_same_address(scalar_expr{integer_constant{0}}));

  ASSERT_TRUE(scalar_expr{1.0}.is_type<float_constant>());
  ASSERT_TRUE(scalar_expr{0.0}.is_type<float_constant>());

  ASSERT_IDENTICAL(constants::undefined, scalar_expr{std::numeric_limits<double>::quiet_NaN()});
  ASSERT_IDENTICAL(constants::complex_infinity,
                   scalar_expr{std::numeric_limits<double>::infinity()});
  ASSERT_IDENTICAL(constants::complex_infinity,
                   scalar_expr{-std::numeric_limits<double>::infinity()});
}

TEST(ScalarOperationsTest, TestAddition) {
  const auto [w, x, y, z] = make_symbols("w", "x", "y", "z");
  ASSERT_TRUE((x + y).is_type<addition>());
  ASSERT_TRUE((x - y).is_type<addition>());
  ASSERT_IDENTICAL(x + y, x + y);
  ASSERT_IDENTICAL(x + y, y + x);
  ASSERT_NOT_IDENTICAL(x + w, x + y);
  ASSERT_NOT_IDENTICAL(x - w, z + y);
  ASSERT_EQ("Addition", (x + y).type_name());
  ASSERT_EQ(precedence::addition, get_precedence(x + y));
  ASSERT_EQ(precedence::addition, get_precedence(x - y));

  // Canonicalization of order:
  ASSERT_IDENTICAL(w + x + y, y + x + w);
  ASSERT_IDENTICAL(w + x + y, x + w + y);
  ASSERT_IDENTICAL(1 + w, w + 1);
  ASSERT_IDENTICAL(5_s / 7 + x, x + 5 / 7_s);
  ASSERT_IDENTICAL(x * w + x * z, x * z + x * w);
  ASSERT_IDENTICAL(w * x + x * z, x * z + x * w);

  // Collapsing of numerics:
  ASSERT_IDENTICAL(x, x + 0);
  ASSERT_IDENTICAL(x, 0 + x);
  ASSERT_IDENTICAL(w + 19 / 7_s, w + 5 / 7_s + 2);
  ASSERT_IDENTICAL(w + y + 1, y + 1 / 2_s + w + 1 / 2_s);
  ASSERT_IDENTICAL(2 * z, 3 + z - 2 + z - 1);
  ASSERT_IDENTICAL(0_s, 3_s + -4_s + 1_s);
  ASSERT_IDENTICAL(0.5_s + x, x + 0.25_s + 1_s / 4_s);
  ASSERT_IDENTICAL(1.0_s, 3_s / 2 + y - 0.5_s - y);

  // Collection of identical terms:
  ASSERT_IDENTICAL(2 * x, x + x);
  ASSERT_IDENTICAL(-3 * x, x - 4_s * x);
  ASSERT_IDENTICAL(3 * pow(x, 2) - 4 * y, pow(x, 2) - 2 * y + 2 * pow(x, 2) - 2 * y);
  ASSERT_IDENTICAL(-2 * log(x) + 2 * pow(y, 2), -log(x) + -log(x) + y * y + y * y);
  ASSERT_IDENTICAL(z * x, z * x / 3_s + 2 * z * x / 3_s);
}

TEST(ScalarOperationsTest, TestAdditionNumericalConstants) {
  ASSERT_IDENTICAL(5, 2_s + 3_s);
  ASSERT_IDENTICAL(0, 1_s - 1_s);
  ASSERT_IDENTICAL(7, addition::from_operands({2_s, 1_s, 4_s}));
  ASSERT_IDENTICAL(0, addition::from_operands({2_s, 3_s, -2_s, -3_s}));
  // int plus rational
  ASSERT_IDENTICAL(23_s / 4, 5_s + 3_s / 4_s);
  ASSERT_IDENTICAL(-1_s / 2, 1_s / 2 - 1_s);
  ASSERT_IDENTICAL(18_s / 11, addition::from_operands({2_s, 7_s / 11, -1_s}));
  ASSERT_IDENTICAL(1_s, addition::from_operands({2_s / 3, -2_s / 3, 1_s, 0_s}));
  // int plus float:
  ASSERT_IDENTICAL(4.21, 4_s + 0.21);
  ASSERT_IDENTICAL(1.62, -0.38_s + 2);
  ASSERT_IDENTICAL(0, 2_s - 2.0);
  ASSERT_IDENTICAL(0, 3.0_s - 3);
  ASSERT_IDENTICAL(6.0, addition::from_operands({4.2_s, -0.2_s, 3_s, -1_s}));
  ASSERT_IDENTICAL(0, addition::from_operands({-0.4_s, 3_s, 0.4_s, -2_s, -1_s}));
  // rational plus float:
  ASSERT_IDENTICAL(4.2, 4.0_s + 1_s / 5);
  ASSERT_IDENTICAL(-1.0, -1_s / 2 - 0.5);
  ASSERT_IDENTICAL(0, -1_s / 2 + 0.5);
  ASSERT_IDENTICAL(0, 0.125 - 1_s / 8);
}

TEST(ScalarOperationsTest, TestAdditionInfinities) {
  // Test handling of infinities under addition/subtraction:
  auto [x, y] = make_symbols("x", "y");
  const auto z_inf = constants::complex_infinity;
  ASSERT_IDENTICAL(x + z_inf, x + 3 + z_inf);
  ASSERT_IDENTICAL(x + z_inf, x - z_inf);
  ASSERT_IDENTICAL(y - 0.1231 + z_inf, y + z_inf);
  ASSERT_IDENTICAL(z_inf, 2_s / 3 + z_inf);
  ASSERT_IDENTICAL(constants::undefined, z_inf + z_inf);
  ASSERT_IDENTICAL(constants::undefined, addition::from_operands({y, x, z_inf, z_inf}));
}

TEST(ScalarOperationsTest, TestAdditionUndefined) {
  auto [x, y] = make_symbols("x", "y");
  const auto& undef = constants::undefined;
  const auto& z_inf = constants::complex_infinity;
  ASSERT_IDENTICAL(undef, 0 + undef);
  ASSERT_IDENTICAL(undef, 3 + undef);
  ASSERT_IDENTICAL(undef, 3.14 + undef);
  ASSERT_IDENTICAL(undef, 5_s / 3 + undef);
  ASSERT_IDENTICAL(undef, undef + undef);
  ASSERT_IDENTICAL(undef, undef - undef);
  ASSERT_IDENTICAL(undef, (x + y - 3) + undef);
  ASSERT_IDENTICAL(undef, undef + z_inf);
}

TEST(ScalarOperationsTest, TestMultiplication) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  ASSERT_TRUE((x * y).is_type<multiplication>());
  ASSERT_IDENTICAL(make_mul(x, y), x * y);
  ASSERT_IDENTICAL(make_mul(x, y, z), x * y * z);
  ASSERT_EQ("Multiplication", (x * y).type_name());
  ASSERT_EQ(precedence::multiplication, get_precedence(x * y));

  // Canonicalization of order for a simple case:
  ASSERT_IDENTICAL(y * x, x * y);
  ASSERT_IDENTICAL(x * y * z, y * z * x);
  ASSERT_IDENTICAL(x * y * z, z * y * x);
  ASSERT_IDENTICAL(x * y * z, x * z * y);

  // Multiplication by zero:
  ASSERT_IDENTICAL(0, x * 0);
  ASSERT_IDENTICAL(0, 0 * z);
  ASSERT_IDENTICAL(0, 0 * x * y);

  // Collapsing of numeric terms:
  ASSERT_IDENTICAL(1_s, 1_s * 1 * 1);
  ASSERT_IDENTICAL(x, 1 * x);
  ASSERT_IDENTICAL(y, y * 2 * (1 / 2_s));
  ASSERT_IDENTICAL(y * z, (y * 3) * 2 * (1 / 6_s) * z);
  ASSERT_IDENTICAL(z * (10_s / 21), (5 / 3_s) * z * (2 / 7_s));

  // Collapsing with floats:
  ASSERT_IDENTICAL(x * 0.5_s, (x * 4.0_s) * 0.125_s);
  ASSERT_IDENTICAL(x * 1.25_s, x * 5_s * 0.125_s * 2.0_s);
  ASSERT_IDENTICAL(x * 0.0625_s, (x * 2.0_s) * (1_s / 32_s));

  // Automatic distribution of numeric constants into additions:
  ASSERT_IDENTICAL(3 * x + 3 * y, 3 * (x + y));
  ASSERT_IDENTICAL(-5_s / 6 * z - 5_s / 6 * constants::pi, -5_s / 6 * (z + constants::pi));
  ASSERT_IDENTICAL(x / 3 - y, (x - 3 * y) / 3);
  ASSERT_IDENTICAL(3.2 * x - 6.4 * y, (x - 2 * y) * 3.2);

  // Collections of powers:
  ASSERT_IDENTICAL(pow(x, 2), x * x);
  ASSERT_IDENTICAL(pow(x, 3), x * x * x);
  ASSERT_IDENTICAL(pow(x, 2) * pow(y, 2), x * y * x * y);
  ASSERT_IDENTICAL(1, pow(x, 2) * pow(x, -2));
  ASSERT_IDENTICAL(x * pow(y, 2) * pow(log(z), 3), log(z) * y * x * log(z) * y * log(z));
  ASSERT_IDENTICAL(x * make_pow(33, 2_s / 3) / 33, pow(33, -2 / 3_s) * pow(33, 1 / 3_s) * x);
  ASSERT_IDENTICAL(x, sqrt(x) * sqrt(x));
  ASSERT_IDENTICAL(make_pow(x, 3 / 2_s), sqrt(x) * sqrt(x) * sqrt(x));
  ASSERT_IDENTICAL(12 * x * y, 12_s / 5 * sqrt(x * 5) * y * sqrt(x * 5));

  // Multiplication becomes an addition times a constant:
  ASSERT_IDENTICAL(5 * x - 15, sqrt(x - 3) * 5 * sqrt(x - 3));
  ASSERT_IDENTICAL(10 * pow(x, 2) + 60 * x - 220, pow(pow(x, 2) + 6 * x - 22, 1_s / 3) * 2 *
                                                      pow(pow(x, 2) + 6 * x - 22, 1_s / 3) * 5 *
                                                      pow(pow(x, 2) + 6 * x - 22, 1_s / 3));

  // Normalization of powers of integers:
  ASSERT_IDENTICAL(2 * pow(2, 1 / 7_s), pow(2, 3 / 7_s) * pow(2, 5 / 7_s));
  ASSERT_IDENTICAL(pow(5, 10 / 11_s) / 25, pow(5, -5 / 11_s) * pow(5, -7 / 11_s));

  // Including symbolics constants:
  ASSERT_IDENTICAL(pow(constants::pi, 3), constants::pi * constants::pi * constants::pi);
  ASSERT_IDENTICAL(pow(constants::euler, 2) * x, constants::euler * x * constants::euler);

  // Collections of powers of functions:
  ASSERT_IDENTICAL(pow(cos(x), 2), cos(x) * cos(x));
  ASSERT_IDENTICAL(pow(cos(x), 2) * pow(tan(y), 3 / 5_s),
                   cos(x) * cos(x) * pow(tan(y), 1 / 5_s) * pow(tan(y), 2 / 5_s));
  ASSERT_IDENTICAL(pow(sin(x), 2) * pow(log(z * y), constants::negative_one),
                   sin(x) * sin(x) / log(z * y));
}

TEST(ScalarOperationsTest, TestMultiplicationImaginaryUnit) {
  // Test multiplications involving `i`:
  const auto [x, y] = make_symbols("x", "y");
  const auto i = constants::imaginary_unit;
  ASSERT_IDENTICAL(-1, i * i);
  ASSERT_IDENTICAL(-i, i * i * i);
  ASSERT_IDENTICAL(2, i * 2 / i);
  ASSERT_IDENTICAL(-2, i * 2 * i);
  ASSERT_IDENTICAL(-12 * i, i * 3 * i * 4 * i);
  ASSERT_IDENTICAL(5_s / 6, (5 * i) / (6 * i));
  ASSERT_IDENTICAL(x * 2 * i / y, (x * i) / (y * i) * (2 * i));
  ASSERT_IDENTICAL(-1, sqrt(i) * sqrt(i) * i);
  ASSERT_IDENTICAL(22 * sqrt(i), 22 * pow(i, -1_s / 2) * pow(i, -3));
}

TEST(ScalarOperationsTest, TestMultiplicationNumericalConstants) {
  ASSERT_IDENTICAL(6, 2_s * 3_s);
  ASSERT_IDENTICAL(0, 0_s * 1_s);
  ASSERT_IDENTICAL(-1_s, multiplication::from_operands({-1_s, -1_s, 1_s, -1_s}));
  // int times rational
  ASSERT_IDENTICAL(8_s / 3, 2_s / 3 * 4);
  ASSERT_IDENTICAL(-24_s / 13, -3_s * (8_s / 13));
  ASSERT_IDENTICAL(14_s / 3, multiplication::from_operands({7_s, 2_s / 3, 1_s}));
  // int times float:
  ASSERT_IDENTICAL(0.84, 4_s * 0.21);
  ASSERT_IDENTICAL(-0.76, -0.38_s * 2);
  ASSERT_IDENTICAL(0, 4_s * 0.0);
  ASSERT_IDENTICAL(0, 0.0_s * 9);
  ASSERT_IDENTICAL(-24.0_s, multiplication::from_operands({4_s, -12_s, 0.5_s}));
  ASSERT_IDENTICAL(0, multiplication::from_operands({0.0_s, 17_s, -0.0_s, 1.4_s}));
  // rational times float:
  ASSERT_IDENTICAL(0.8, 4.0_s * (1_s / 5));
  ASSERT_IDENTICAL(-1.0, (-1_s / 2) * 2.0);
  ASSERT_IDENTICAL(0, -1_s / 2 * 0.0);
  ASSERT_IDENTICAL(0, 0.0 * 1_s / 7);
}

TEST(ScalarOperationsTest, TestMultiplicationInfinities) {
  auto [x, y] = make_symbols("x", "y");
  const auto z_inf = constants::complex_infinity;
  ASSERT_IDENTICAL(constants::undefined, 0 * z_inf);
  ASSERT_IDENTICAL(z_inf, 5 * z_inf);
  ASSERT_IDENTICAL(z_inf, 7_s / 11 * z_inf);
  ASSERT_IDENTICAL(z_inf, -1.231 * z_inf);
  ASSERT_IDENTICAL(z_inf, z_inf * z_inf);
  ASSERT_IDENTICAL(constants::undefined, z_inf / z_inf);

  ASSERT_IDENTICAL(x * z_inf, x * z_inf);
  ASSERT_IDENTICAL(x * z_inf, x * 5 * z_inf);
  ASSERT_IDENTICAL(y * z_inf, y * -1 * z_inf);
  ASSERT_IDENTICAL(z_inf, -z_inf);
  ASSERT_IDENTICAL(0, 22 * x / z_inf);
  ASSERT_IDENTICAL(0, constants::pi / z_inf);
  ASSERT_IDENTICAL(0, (y * x) / z_inf);

  ASSERT_IDENTICAL(z_inf, multiplication::from_operands({z_inf, z_inf, 22, -1.02}));
}

TEST(ScalarOperationsTest, TestMultiplicationUndefined) {
  const auto [x, y] = make_symbols("x", "y");
  const auto& undef = constants::undefined;
  ASSERT_IDENTICAL(undef, undef * 5);
  ASSERT_IDENTICAL(undef, undef * (x * y) * 3_s / 7);
  ASSERT_IDENTICAL(undef, (1.23 * y) * undef);
  ASSERT_IDENTICAL(undef, undef * undef);
  ASSERT_IDENTICAL(undef, -undef);
}

TEST(ScalarOperationsTest, TestNegation) {
  const scalar_expr x{"x"};
  ASSERT_IDENTICAL(make_mul(-1, x), -x);
  ASSERT_TRUE((-x).is_type<multiplication>());
  ASSERT_IDENTICAL(-(-x), x);
  ASSERT_IDENTICAL(-(-(-x)), -x);
}

TEST(ScalarOperationsTest, TestDivision) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  ASSERT_IDENTICAL(make_mul(x, make_pow(y, -1)), x / y);
  ASSERT_TRUE((x / y).is_type<multiplication>());
  ASSERT_EQ(precedence::multiplication, get_precedence(x / y));
  ASSERT_NOT_IDENTICAL(y / x, x / y);
  ASSERT_IDENTICAL(x / y / z, (x / y) / z);
  ASSERT_IDENTICAL(constants::zero, 0 / x);
  ASSERT_IDENTICAL(x, x / 1);
  ASSERT_IDENTICAL(1_s, x / x);
  ASSERT_IDENTICAL(x / y / z, x * pow(y, -1) * pow(z, -1_s));
  ASSERT_IDENTICAL(z / (y * x), z * pow(y, -1) * pow(x, -1_s));
  ASSERT_IDENTICAL(z / (y * z), 1_s / y);

  // Cancellation of powers:
  ASSERT_IDENTICAL(x, pow(x, 3) / pow(x, 2));
  ASSERT_IDENTICAL(constants::one, pow(x, 3) / (x * x * x));
  ASSERT_IDENTICAL(x * y * pow(z, 1_s / 2_s),
                   pow(x, 5) * pow(y, 3) * pow(z, 3_s / 2) / (x * x * x * x * y * y * z));
}

TEST(ScalarOperationsTest, TestAsCoeffAndMultiplicand) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  const scalar_expr z{"z"};

  ASSERT_IDENTICAL(constants::zero, as_coeff_and_mul(0).first);
  ASSERT_IDENTICAL(constants::one, as_coeff_and_mul(0).second);

  ASSERT_IDENTICAL(2_s, as_coeff_and_mul(2).first);
  ASSERT_IDENTICAL(constants::one, as_coeff_and_mul(2).second);

  ASSERT_IDENTICAL(constants::one, as_coeff_and_mul(x).first);
  ASSERT_IDENTICAL(x, as_coeff_and_mul(x).second);

  ASSERT_IDENTICAL(constants::one, as_coeff_and_mul(x / y).first);
  ASSERT_IDENTICAL(x / y, as_coeff_and_mul(x / y).second);

  // Special constants are symbols, and do not go in the multiplicand:
  ASSERT_IDENTICAL(3_s, as_coeff_and_mul(3 * constants::pi).first);
  ASSERT_IDENTICAL(constants::pi, as_coeff_and_mul(3 * constants::pi).second);

  ASSERT_IDENTICAL(5_s / 7, as_coeff_and_mul(constants::pi * z * 5_s / 7).first);
  ASSERT_IDENTICAL(constants::pi * z, as_coeff_and_mul(constants::pi * z).second);

  // Include some functions:
  ASSERT_IDENTICAL(1.22_s, as_coeff_and_mul(1.22 * sin(x) * cos(y)).first);
  ASSERT_IDENTICAL(cos(y) * sin(x), as_coeff_and_mul(1.22 * sin(x) * cos(y)).second);
}

TEST(ScalarOperationsTest, TestPower) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  ASSERT_IDENTICAL(make_pow(x, y), pow(x, y));
  ASSERT_NOT_IDENTICAL(pow(x, y), pow(y, x));
  ASSERT_IDENTICAL(as_base_and_exp(pow(x, y)).first, x);
  ASSERT_IDENTICAL(as_base_and_exp(pow(x, y)).second, y);
  ASSERT_EQ("Power", pow(x, y).type_name());
  ASSERT_EQ(precedence::power, get_precedence(pow(x, y)));

  // Powers don't get combined automatically (for variable exponents):
  ASSERT_IDENTICAL(as_base_and_exp(pow(pow(x, y), z)).first, pow(x, y));
  ASSERT_IDENTICAL(as_base_and_exp(pow(pow(x, y), z)).second, z);

  // Raised to power 0 or 1:
  ASSERT_IDENTICAL(1, pow(x * y, 0));
  ASSERT_IDENTICAL(x + y, pow(x + y, 1));

  // Should not get simplified, because we can't make assumptions about y+z.
  ASSERT_IDENTICAL(make_pow(0, y + z), pow(0, y + z));
}

TEST(ScalarOperationsTest, TestPowerDistribution) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  const scalar_expr w{"w", number_set::real_non_negative};
  const scalar_expr u{"u", number_set::real_positive};
  const scalar_expr v{"v", number_set::real};
  const scalar_expr a{"a", number_set::real};
  const scalar_expr b{"b", number_set::real};
  const scalar_expr c{"c", number_set::real};

  // Distribute over integer powers:
  ASSERT_IDENTICAL(pow(3, 2) * pow(x, 2), pow(3 * x, 2));
  ASSERT_IDENTICAL(pow(x, 3) * pow(y, 3) * pow(z, 3), pow(x * y * z, 3));
  ASSERT_IDENTICAL(pow(x, -2) * pow(z, -2), pow(x * z, -2));
  ASSERT_IDENTICAL(pow(constants::pi, 4) * pow(sin(z), 4), pow(constants::pi * sin(z), 4));

  // Do not distribute over non-integer powers:
  ASSERT_IDENTICAL(x, pow(sqrt(x), 2));
  ASSERT_IDENTICAL(make_pow(3 * z, x), pow(3 * z, x));
  ASSERT_IDENTICAL(make_pow(x * z, 2_s / 3), pow(x * z, 2_s / 3));
  ASSERT_IDENTICAL(make_pow(x * z, -0.276_s), pow(x * z, -0.276));
  ASSERT_IDENTICAL(make_pow(x * z, sin(z)), pow(x * z, sin(z)));
  ASSERT_IDENTICAL(make_pow(x * z, -constants::pi), pow(x * z, -constants::pi));

  // Allow distribution with rationals when terms are non-negative:
  ASSERT_IDENTICAL(abs(v) * make_pow(y, 1_s / 2), sqrt(abs(v) * abs(v) * y));
  ASSERT_IDENTICAL(w * 3 * make_pow(y, 1_s / 2), sqrt(w * w * y * 9));
  ASSERT_IDENTICAL(u * pow(0.5, 1_s / 4) * make_pow(y, 1_s / 4),
                   pow(u * u * u * u * y * 0.5, 1_s / 4));
  ASSERT_IDENTICAL(sqrt(5) * make_pow(y, 1_s / 2), sqrt(5 * y));
  ASSERT_IDENTICAL(sqrt(w) * sqrt(u) * sqrt(x), sqrt(w * u * x));
  ASSERT_IDENTICAL(sqrt(w) * sqrt(u) * sqrt(-x), sqrt(w * u * -x));
  ASSERT_IDENTICAL(pow(2, 3_s / 5) * pow(w, 3_s / 5) * pow(x * y, 3_s / 5),
                   pow(w * x * y * 2, 3_s / 5));

  ASSERT_IDENTICAL(sqrt(a * a + b * b + c * c) * sqrt(pow(v, -2)),
                   pow((a * a + b * b + c * c) / (v * v), 1_s / 2));
}

// Test combining powers when outer exponent is an integer.
TEST(ScalarOperationsTest, TestPowerCombinationInt) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  ASSERT_IDENTICAL(make_pow(x, y * 2), pow(pow(x, y), 2));
  ASSERT_IDENTICAL(make_pow(x, y * 27), pow(pow(x, y * 3), 9));
  ASSERT_IDENTICAL(make_pow(x, 35_s / 6), pow(pow(x, 7 / 6_s), 5));
  ASSERT_IDENTICAL(make_pow(x, 5.01 * 4), pow(pow(x, 5.01), 4));
  ASSERT_IDENTICAL(make_pow(x, 3 * y + 3 * z), pow(pow(x, y + z), 3));
  ASSERT_IDENTICAL(make_pow(x, -y), 1 / pow(x, y));
}

// Inner exponent is a proper rational:
TEST(ScalarOperationsTest, TestPowerProperRational) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  ASSERT_IDENTICAL(make_pow(x, 2 / 5_s), pow(pow(x, 1 / 5_s), 2));
  ASSERT_IDENTICAL(make_pow(x, 6 / 55_s), pow(pow(x, 2 / 11_s), 3_s / 5));
  ASSERT_IDENTICAL(make_pow(x, 3 / 4_s * z), pow(pow(x, 3 / 4_s), z));
  ASSERT_IDENTICAL(make_pow(x, -1 / 2_s), 1 / sqrt(x));
  ASSERT_IDENTICAL(make_pow(x, z / 3 + 2 / 3_s), pow(pow(x, 1 / 3_s), z + 2));
}

TEST(ScalarOperationsTest, TestPowerCombinationFloat) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  // Inner exponent is a float <= 1:
  ASSERT_IDENTICAL(make_pow(x, 0.25), pow(pow(x, 1 / 2_s), 0.5));
  ASSERT_IDENTICAL(make_pow(x, 3.0), pow(pow(x, 1.0), 3));
  ASSERT_IDENTICAL(make_pow(x, -0.3412 * 4), pow(pow(x, -0.3412), 4));
  ASSERT_IDENTICAL(make_pow(x, 0.42 * y), pow(pow(x, 0.42), y));
  ASSERT_IDENTICAL(make_pow(x, 0.77 * z - 0.77 * 4), pow(pow(x, 0.77), z - 4));

  // Cannot simplify if the inner power is >= 1
  ASSERT_IDENTICAL(make_pow(make_pow(x, 2), 1 / 2_s), pow(pow(x, 2), 1 / 2_s));
  ASSERT_IDENTICAL(make_pow(make_pow(x, 4), 1 / 4_s), pow(pow(x, 4), 1 / 4_s));
  ASSERT_IDENTICAL(make_pow(make_pow(x, 14), -1 / 7_s), pow(pow(x, 14), -1 / 7_s));
  ASSERT_IDENTICAL(make_pow(make_pow(x, 2), z), pow(pow(x, 2), z));
  ASSERT_IDENTICAL(make_pow(make_pow(x, 1.52), 2.0), pow(pow(x, 1.52), 2.0));
  ASSERT_IDENTICAL(make_pow(make_pow(x, -1.01), 5 / 7_s), pow(pow(x, -1.01), 5 / 7_s));
}

// Inner power is negative
TEST(ScalarOperationsTest, TestPowerCombinationNegativeInnerPower) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(make_pow(make_pow(x, -1), 1 / 2_s), sqrt(1 / x));
  ASSERT_IDENTICAL(make_pow(make_mul(x, make_pow(y, -1)), 1 / 2_s), sqrt(x / y));
  ASSERT_IDENTICAL(make_pow(make_pow(y, -3), 1_s / 5), pow(1 / pow(y, 3), 1_s / 5));
}

TEST(ScalarOperationsTest, TestPowerCombinationNonConstant) {
  const auto [x, y] = make_symbols("x", "y");

  // Cannot simplify if the inner exponent is not a constant:
  ASSERT_IDENTICAL(make_pow(make_pow(x, y * 2 / 7_s), 1 / 2_s), pow(pow(x, y * 2 / 7_s), 1 / 2_s));
  ASSERT_IDENTICAL(make_pow(make_pow(x, y / 2), 1 / 2_s), pow(pow(x, y / 2), 1 / 2_s));
  ASSERT_IDENTICAL(make_pow(make_pow(x, y), 1 / 4_s), pow(pow(x, y), 1 / 4_s));
}

TEST(ScalarOperationsTest, TestPowerCombinationInnerNonNegative) {
  const auto [x] = make_symbols("x");
  const scalar_expr w{"w", number_set::real_non_negative};
  const scalar_expr u{"u", number_set::real_positive};
  const scalar_expr v{"v", number_set::real};

  // Simplifications when the inner value is non-negative:
  for (const scalar_expr& s : {w, u}) {
    ASSERT_IDENTICAL(s, pow(pow(s, 2), 1 / 2_s));
    ASSERT_IDENTICAL(s, pow(pow(s, 3), 1 / 3_s));
    ASSERT_IDENTICAL(pow(s, 12 / 7_s), pow(pow(s, 2), 6 / 7_s));
    ASSERT_IDENTICAL(pow(s + 3, x / 3), pow(pow(s + 3, x), 1 / 3_s));
  }
  ASSERT_IDENTICAL(pow(abs(v), 5 * x / 3), pow(pow(abs(v), 5 / 3_s), x));
  ASSERT_IDENTICAL(abs(v), pow(pow(abs(v), 2), 1_s / 2));
}

// Test: int**int
TEST(ScalarOperationsTest, TestPowerIntToInt) {
  ASSERT_IDENTICAL(1, pow(1, 1));
  ASSERT_IDENTICAL(8, pow(2, 3));
  ASSERT_IDENTICAL(-243_s, pow(-3, 5));
  ASSERT_IDENTICAL(0, pow(0, 10));
  ASSERT_IDENTICAL(1, pow(-5, 0));
  ASSERT_IDENTICAL(1 / 8_s, pow(2, -3));
  ASSERT_IDENTICAL(1 / 5_s, pow(5, -1));
  ASSERT_IDENTICAL(constants::complex_infinity, pow(0, -1));
  ASSERT_IDENTICAL(constants::complex_infinity, pow(0, -2));
  ASSERT_IDENTICAL(constants::undefined, pow(0, 0));
}

// Test: rational**int
TEST(ScalarOperationsTest, TestPowerRationalToInt) {
  ASSERT_IDENTICAL(25 / 64_s, pow(5 / 8_s, 2));
  ASSERT_IDENTICAL(343 / 729_s, pow(9 / 7_s, -3));
  ASSERT_IDENTICAL(1, pow(-2_s / 3, 0));
}

// Test: float**numerical or numerical**float
TEST(ScalarOperationsTest, TestPowerFloats) {
  // If either quantity is a float, a floating point pow is performed.
  ASSERT_IDENTICAL(std::pow(2.0, 4.5), pow(2, 4.5));
  ASSERT_IDENTICAL(std::pow(-3.0, 5.0), pow(-3.0, 5));
  ASSERT_IDENTICAL(std::pow(1.122, 6.0), pow(1.122, 6));
  ASSERT_IDENTICAL(std::pow(6.7, -0.5), pow(6.7, -0.5));
  ASSERT_IDENTICAL(0.0, pow(0, 1.3));
  ASSERT_IDENTICAL(1.0, pow(1.5, 0));
  ASSERT_IDENTICAL(std::pow(0.5, 3.4), pow(1_s / 2, 3.4));
  ASSERT_IDENTICAL(std::pow(2.2, 0.75), pow(2.2, 3_s / 4));
  ASSERT_IDENTICAL(constants::complex_infinity, pow(0.0, -1));
}

// Test: int**rational
TEST(ScalarOperationsTest, TestPowerIntToRational) {
  const auto& i = constants::imaginary_unit;

  ASSERT_IDENTICAL(0, pow(0, 6_s / 11));
  ASSERT_IDENTICAL(1, pow(1, 7_s / 9));
  ASSERT_IDENTICAL(1, pow(1, -3_s / 22));
  ASSERT_IDENTICAL(3, as_base_and_exp(pow(3, 3 / 4_s)).first);
  ASSERT_IDENTICAL(3 * pow(3, 1 / 3_s), pow(3, 4 / 3_s));
  ASSERT_IDENTICAL(3 * pow(3, 1 / 3_s), pow(9, 2 / 3_s));
  ASSERT_IDENTICAL(make_pow(42, 7_s / 9), pow(42, 7 / 9_s));
  ASSERT_IDENTICAL(make_pow(-42, 7_s / 9), pow(-42, 7 / 9_s));
  ASSERT_IDENTICAL(177147 * make_pow(3, 1_s / 4), pow(243, 9_s / 4));
  ASSERT_IDENTICAL(177147 * make_pow(-3, 1_s / 4), pow(-243, 9_s / 4));

  // Positive rational powers that involve factorization:
  ASSERT_IDENTICAL(make_pow(77, 1_s / 8), pow(77, 1 / 8_s));
  ASSERT_IDENTICAL(make_pow(77, 3_s / 8), pow(77, 3 / 8_s));
  ASSERT_IDENTICAL(5929 * make_pow(77, 1_s / 8), pow(77, 17 / 8_s));

  ASSERT_IDENTICAL(2 * sqrt(3) * make_pow(5, 1_s / 6), pow(8640, 1_s / 6));
  ASSERT_IDENTICAL(288 * sqrt(3) * make_pow(5, 5_s / 6), pow(8640, 5_s / 6));
  ASSERT_IDENTICAL(17280 * sqrt(3) * make_pow(5, 1_s / 6), pow(8640, 7_s / 6));

  ASSERT_IDENTICAL(make_pow(735, 1_s / 5), pow(735, 1_s / 5));
  ASSERT_IDENTICAL(make_pow(-735, 1_s / 5), pow(-735, 1_s / 5));
  ASSERT_IDENTICAL(7 * make_pow(-1, 4_s / 5) * make_pow(17364375, 1_s / 5), pow(-735, 4_s / 5));
  ASSERT_IDENTICAL(-735 * make_pow(-735, 1_s / 5), pow(-735, 6_s / 5));
  ASSERT_IDENTICAL(540225 * make_pow(-735, 1_s / 5), pow(-735, 11_s / 5));

  ASSERT_IDENTICAL(sqrt(11_s) * make_pow(5, 1_s / 6) * make_pow(1183, 1_s / 3),
                   pow(5 * pow(7_s, 2) * pow(11_s, 3) * pow(13_s, 4), 1_s / 6));
  ASSERT_IDENTICAL(sqrt(11_s) * make_pow(-5, 1_s / 6) * make_pow(1183, 1_s / 3),
                   pow(-5 * pow(7_s, 2) * pow(11_s, 3) * pow(13_s, 4), 1_s / 6));

  ASSERT_IDENTICAL(2 * sqrt(3) * make_pow(84035, 1_s / 6),
                   pow(pow(3, 3) * 5 * pow(7, 5) * 64, 1_s / 6));
  ASSERT_IDENTICAL(2 * sqrt(3) * make_pow(-84035, 1_s / 6),
                   pow(pow(3, 3) * 5 * pow(-7, 5) * 64, 1_s / 6));

  ASSERT_IDENTICAL(make_pow(18, 2_s / 9), pow(pow(3_s, 4) * 4, 1_s / 9));
  ASSERT_IDENTICAL(12 * make_pow(6, 2_s / 9), pow(pow(3, 11_s) * pow(4, 10_s), 1_s / 9));
  ASSERT_IDENTICAL(12 * make_pow(-1, 1_s / 9) * make_pow(6, 2_s / 9),
                   pow(-pow(3, 11_s) * pow(4, 10_s), 1_s / 9));

  // Negative rational powers:
  ASSERT_IDENTICAL(pow(2, -4 / 5_s), pow(2, -4 / 5_s));
  ASSERT_IDENTICAL(pow(2, -2 / 5_s) / 2, pow(2, -7 / 5_s));
  ASSERT_IDENTICAL(pow(5, -1 / 11_s) / 5, pow(5, -12 / 11_s));
  ASSERT_IDENTICAL(make_pow(91, 2 / 3_s) / 8281, pow(91, -4 / 3_s));

  ASSERT_IDENTICAL(sqrt(3) * make_pow(5, 5_s / 6) / 30, pow(8640, -1_s / 6));
  ASSERT_IDENTICAL(sqrt(3) * make_pow(5, 1_s / 6) / 4320, pow(8640, -5_s / 6));
  ASSERT_IDENTICAL(sqrt(3) * make_pow(5, 1_s / 6) / 37324800, pow(8640, -11_s / 6));

  ASSERT_IDENTICAL(make_pow(17364375, 1_s / 5) / 105, pow(735, -1_s / 5));
  ASSERT_IDENTICAL(-make_pow(-1, 4_s / 5) * make_pow(17364375, 1_s / 5) / 105, pow(-735, -1_s / 5));
  ASSERT_IDENTICAL(-make_pow(-735, 1_s / 5) / 735, pow(-735, -4_s / 5));
  ASSERT_IDENTICAL(make_pow(-1, 4_s / 5) * make_pow(17364375, 1_s / 5) / 77175,
                   pow(-735, -6_s / 5));
  ASSERT_IDENTICAL(-make_pow(-1, 4_s / 5) * make_pow(17364375, 1_s / 5) / 56723625,
                   pow(-735, -11_s / 5));
  ASSERT_IDENTICAL(-make_pow(-1, 3_s / 5) * make_pow(23625, 1_s / 5) / 56723625,
                   pow(-735, -12_s / 5));

  ASSERT_IDENTICAL(sqrt(11_s) * make_pow(5, 5_s / 6) * make_pow(637, 1_s / 3) / 5005,
                   pow(5 * pow(7_s, 2) * pow(11_s, 3) * pow(13_s, 4), -1_s / 6));
  ASSERT_IDENTICAL(sqrt(11_s) * make_pow(5, 1_s / 6) * make_pow(1183, 1_s / 3) / 9313599295,
                   pow(5 * pow(7_s, 2) * pow(11_s, 3) * pow(13_s, 4), -5_s / 6));
  ASSERT_IDENTICAL(sqrt(11_s) * make_pow(5, 5_s / 6) * make_pow(637, 1_s / 3) / 46614564471475,
                   pow(5 * pow(7_s, 2) * pow(11_s, 3) * pow(13_s, 4), -7_s / 6));

  // -1 raised to the power of n/2:
  ASSERT_IDENTICAL(i, sqrt(-1_s));
  ASSERT_IDENTICAL(-i, pow(-1, 3_s / 2));
  ASSERT_IDENTICAL(i, pow(-1, 5_s / 2));
  ASSERT_IDENTICAL(-i, pow(-1, 7_s / 2));
  ASSERT_IDENTICAL(-i, pow(-1, -1_s / 2));
  ASSERT_IDENTICAL(i, pow(-1, -3_s / 2));
  ASSERT_IDENTICAL(-i, pow(-1, -5_s / 2));
  ASSERT_IDENTICAL(i, pow(-1, -7_s / 2));
  ASSERT_IDENTICAL(-i, pow(-1, -9_s / 2));

  // Negative integer raised to the power of n/2:
  ASSERT_IDENTICAL(sqrt(2) * i, sqrt(-2));
  ASSERT_IDENTICAL(-2 * sqrt(2) * i, pow(-2, 3_s / 2));
  ASSERT_IDENTICAL(4 * sqrt(2) * i, pow(-2, 5_s / 2));
  ASSERT_IDENTICAL(sqrt(2) * i / 4, pow(-2, -3_s / 2));
  ASSERT_IDENTICAL(-sqrt(2) * i / 8, pow(-2, -5_s / 2));

  ASSERT_IDENTICAL(-24 * sqrt(3) * i, pow(-12, 3_s / 2));
  ASSERT_IDENTICAL(288 * sqrt(3) * i, pow(-12, 5_s / 2));
  ASSERT_IDENTICAL(sqrt(3) * i / 72, pow(-12, -3_s / 2));
  ASSERT_IDENTICAL(-sqrt(3) * i / 864, pow(-12, -5_s / 2));
  ASSERT_IDENTICAL(-120 * sqrt(15) * i, pow(-60, 3_s / 2));
  ASSERT_IDENTICAL(25920000 * sqrt(15) * i, pow(-60, 9_s / 2));
  ASSERT_IDENTICAL(sqrt(15) * i / 1800, pow(-60, -3_s / 2));
  ASSERT_IDENTICAL(-sqrt(15) * i / 388800000, pow(-60, -9_s / 2));

  // -1 raised to the power of of a non-n/2 rational:
  ASSERT_IDENTICAL(make_pow(-1, 1_s / 3), pow(-1, 1_s / 3));
  ASSERT_IDENTICAL(make_pow(-1, 2_s / 5), pow(-1, 2_s / 5));
  ASSERT_IDENTICAL(-make_pow(-1, 1_s / 5), pow(-1, 6_s / 5));

  // 0 raised to power of a negative rational
  ASSERT_IDENTICAL(constants::complex_infinity, pow(0, -2_s / 3));
}

// Test powers that produce the imaginary constant.
TEST(ScalarOperationsTest, TestPowerImaginaryUnit) {
  const auto [x] = make_symbols("x");
  const auto& i = constants::imaginary_unit;

  // Powers of `i`:
  ASSERT_IDENTICAL(1, pow(i, 0));
  ASSERT_IDENTICAL(i, pow(i, 1));
  ASSERT_IDENTICAL(-1, pow(i, 2));
  ASSERT_IDENTICAL(-i, pow(i, 3));
  ASSERT_IDENTICAL(1, pow(i, 4));
  ASSERT_IDENTICAL(i, pow(i, 5));
  ASSERT_IDENTICAL(-1, pow(i, 6));
  ASSERT_IDENTICAL(-i, pow(i, -1));
  ASSERT_IDENTICAL(-1, pow(i, -2));
  ASSERT_IDENTICAL(i, pow(i, -3));
  ASSERT_IDENTICAL(1, pow(i, -4));
  ASSERT_IDENTICAL(-i, pow(i, -5));
  ASSERT_IDENTICAL(-1, i * i);

  // Rational powers of `i`:
  ASSERT_IDENTICAL(make_pow(i, 1_s / 2), pow(i, 1_s / 2));
  ASSERT_IDENTICAL(-sqrt(i), pow(i, 5_s / 2));
  ASSERT_IDENTICAL(-pow(i, 3_s / 2), pow(i, 7_s / 2));
  ASSERT_IDENTICAL(sqrt(i), pow(i, 9_s / 2));
  ASSERT_IDENTICAL(sqrt(i), pow(i, 9_s / 2));
  ASSERT_IDENTICAL(pow(i, 3_s / 2), pow(i, 11_s / 2));
  ASSERT_IDENTICAL(-sqrt(i), pow(i, 13_s / 2));

  ASSERT_IDENTICAL(pow(i, 3_s / 2), pow(i, -5_s / 2));
  ASSERT_IDENTICAL(sqrt(i), pow(i, -7_s / 2));
  ASSERT_IDENTICAL(-pow(i, 3_s / 2), pow(i, -9_s / 2));
  ASSERT_IDENTICAL(-sqrt(i), pow(i, -11_s / 2));
  ASSERT_IDENTICAL(pow(i, 3_s / 2), pow(i, -13_s / 2));

  ASSERT_IDENTICAL(-pow(i, 1_s / 3), pow(i, 7_s / 3));
  ASSERT_IDENTICAL(-pow(i, 2_s / 3), pow(i, 8_s / 3));
  ASSERT_IDENTICAL(-pow(i, 5_s / 3), pow(i, 11_s / 3));
  ASSERT_IDENTICAL(pow(i, 1_s / 3), pow(i, 13_s / 3));
  ASSERT_IDENTICAL(pow(i, 2_s / 3), pow(i, 14_s / 3));

  ASSERT_IDENTICAL(pow(i, 5_s / 3), pow(i, -7_s / 3));
  ASSERT_IDENTICAL(pow(i, 4_s / 3), pow(i, -8_s / 3));
  ASSERT_IDENTICAL(pow(i, 1_s / 3), pow(i, -11_s / 3));
  ASSERT_IDENTICAL(-pow(i, 5_s / 3), pow(i, -13_s / 3));
  ASSERT_IDENTICAL(-pow(i, 4_s / 3), pow(i, -14_s / 3));

  ASSERT_IDENTICAL(i * sqrt(21), sqrt(-21));
}

TEST(ScalarOperationsTest, TestPowerInfinities) {
  const auto z_inf = constants::complex_infinity;
  ASSERT_IDENTICAL(constants::undefined, pow(z_inf, 0));
  ASSERT_IDENTICAL(constants::undefined, pow(0, z_inf));

  ASSERT_IDENTICAL(constants::undefined, pow(1, z_inf));
  ASSERT_IDENTICAL(constants::undefined, pow(-1, z_inf));

  ASSERT_IDENTICAL(z_inf, pow(0_s, -1));
  ASSERT_IDENTICAL(z_inf, pow(0_s, -3_s / 5));
  ASSERT_IDENTICAL(z_inf, pow(0_s, -5.2));
  ASSERT_IDENTICAL(z_inf, pow(0_s, -0.12));

  ASSERT_IDENTICAL(z_inf, pow(z_inf, 1_s / 2));
  ASSERT_IDENTICAL(0, pow(z_inf, -1));
  ASSERT_IDENTICAL(0, pow(z_inf, -1.234123));
  ASSERT_IDENTICAL(0, pow(z_inf, -9_s / 13));
  ASSERT_IDENTICAL(z_inf, pow(z_inf, 0.1234));
}

TEST(ScalarOperationsTest, TestPowerUndefined) {
  const auto [x, y] = make_symbols("x", "y");
  const auto& undef = constants::undefined;
  ASSERT_IDENTICAL(undef, pow(undef, undef));
  ASSERT_IDENTICAL(undef, pow(undef, x));
  ASSERT_IDENTICAL(undef, pow(y, undef));
  ASSERT_IDENTICAL(undef, pow(2, undef));
  ASSERT_IDENTICAL(undef, pow(undef, -5));
  ASSERT_IDENTICAL(undef, pow(2_s / 3, undef));
  ASSERT_IDENTICAL(undef, pow(undef, 7_s / 11));
  ASSERT_IDENTICAL(undef, pow(2.123, undef));
  ASSERT_IDENTICAL(undef, pow(undef, -0.1231));
}

// Test creating relational expressions
TEST(ScalarOperationsTest, TestRelationals) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  ASSERT_TRUE((x > y).is_type<relational>());
  ASSERT_TRUE((x == y).is_type<relational>());
  ASSERT_IDENTICAL(x > y, x > y);
  ASSERT_IDENTICAL(x > y, y < x);
  ASSERT_IDENTICAL(z <= x, x >= z);
  ASSERT_IDENTICAL(x == y, y == x);
  ASSERT_NOT_IDENTICAL(x < y, x <= y);

  // Simplification of numerical cases:
  ASSERT_IDENTICAL(constants::boolean_true, 3 > 2_s);
  ASSERT_IDENTICAL(constants::boolean_true, 3 >= 2_s);
  ASSERT_IDENTICAL(constants::boolean_false, 3 == 2_s);
  ASSERT_IDENTICAL(constants::boolean_true, 4 == 4_s);
  ASSERT_IDENTICAL(constants::boolean_false, -4 > 0_s);
  ASSERT_IDENTICAL(constants::boolean_false, -4 >= 0_s);
  ASSERT_IDENTICAL(constants::boolean_true, 0 < 1_s);
  ASSERT_IDENTICAL(constants::boolean_true, 2 <= 2_s);

  ASSERT_IDENTICAL(constants::boolean_true, 3 / 2_s > 1 / 5_s);
  ASSERT_IDENTICAL(constants::boolean_true, 7 / 9_s > 3 / 6_s);
  ASSERT_IDENTICAL(constants::boolean_true, 45 / 11_s > 4);
  ASSERT_IDENTICAL(constants::boolean_true, 9 > 116 / 13_s);
  ASSERT_IDENTICAL(constants::boolean_false, 9 < 116 / 13_s);
  ASSERT_IDENTICAL(constants::boolean_false, 9 <= 116 / 13_s);
  ASSERT_IDENTICAL(constants::boolean_true, 9 < 128 / 13_s);
  ASSERT_IDENTICAL(constants::boolean_false, 9 > 128 / 13_s);
  ASSERT_IDENTICAL(constants::boolean_false, 6 / 7_s == 2 / 3_s);
  ASSERT_IDENTICAL(constants::boolean_true, 6 / 7_s == 12 / 14_s);

  // Float comparisons:
  ASSERT_IDENTICAL(constants::boolean_true, 1.2_s == 1.2);
  ASSERT_IDENTICAL(constants::boolean_true, 0.5_s < 0.6);
  ASSERT_IDENTICAL(constants::boolean_false, 2.45_s < 1.999);
  ASSERT_IDENTICAL(constants::boolean_true, 7.7_s >= 7.7);
  ASSERT_IDENTICAL(constants::boolean_false, 0.6667_s >= 10.0);

  // Float to integer comparison (mostly tested in integer_utils):
  ASSERT_IDENTICAL(constants::boolean_true, 18.2_s > 10);
  ASSERT_IDENTICAL(constants::boolean_true, 10.0_s == 10);
  ASSERT_IDENTICAL(constants::boolean_true, 109.2_s < 110);
  ASSERT_IDENTICAL(constants::boolean_true, 22.0_s <= 22);

  // Constant to constant comparison:
  ASSERT_IDENTICAL(constants::boolean_true, constants::pi > constants::euler);
  ASSERT_IDENTICAL(constants::boolean_true, constants::euler == constants::euler);
  ASSERT_IDENTICAL(constants::boolean_false, constants::euler >= constants::pi);

  // Constant to integer comparison is simplified
  ASSERT_IDENTICAL(constants::boolean_true, constants::pi > 3);
  ASSERT_IDENTICAL(constants::boolean_true, constants::pi >= 3);
  ASSERT_IDENTICAL(constants::boolean_false, constants::pi > 4);
  ASSERT_IDENTICAL(constants::boolean_false, constants::pi == 3);
  ASSERT_IDENTICAL(constants::boolean_true, constants::pi < 4);
  ASSERT_IDENTICAL(constants::boolean_true, constants::euler > 2);
  ASSERT_IDENTICAL(constants::boolean_false, constants::euler > 3);
  ASSERT_IDENTICAL(constants::boolean_true, constants::euler < 3);

  ASSERT_THROW(3 < constants::complex_infinity, type_error);
  ASSERT_THROW(-0.4 > constants::complex_infinity, type_error);
  ASSERT_THROW(constants::complex_infinity == z, type_error);
  ASSERT_THROW(constants::undefined > x, type_error);
  ASSERT_THROW(3_s / 5 == constants::undefined, type_error);
}

TEST(ScalarOperationsTest, TestIverson) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_TRUE(iverson(x < y).is_type<iverson_bracket>());
  ASSERT_IDENTICAL(iverson(x < y), iverson(x < y));
  ASSERT_NOT_IDENTICAL(iverson(x < y), iverson(x == y));

  ASSERT_IDENTICAL(1, iverson(constants::boolean_true));
  ASSERT_IDENTICAL(0, iverson(constants::boolean_false));
}

TEST(ScalarOperationsTest, TestConditional) {
  const auto [w, x, y, z] = make_symbols("w", "x", "y", "z");

  ASSERT_TRUE(where(x > 0, y, z).is_type<conditional>());
  ASSERT_IDENTICAL(x, where(constants::boolean_true, x, z));
  ASSERT_IDENTICAL(z, where(constants::boolean_false, x, z));

  // Identical branches simplify:
  ASSERT_IDENTICAL(w + 3, where(x > 0, 3 + w, w + 3));
  ASSERT_IDENTICAL(23 * z, where(y == 0, 23 * z, 23 * z));
  ASSERT_IDENTICAL(abs(x) + z * 5, where(x - y < z, abs(x) + z * 5, abs(x) + z * 5));

  // Nested conditionals don't simplify:
  const scalar_expr nested = where(x < 0, where(x < 0, cos(x), sin(x)), log(z));
  ASSERT_IDENTICAL(get<const conditional>(nested).if_branch(), where(x < 0, cos(x), sin(x)));
  ASSERT_IDENTICAL(get<const conditional>(nested).else_branch(), log(z));
}

TEST(ScalarOperationsTest, TestDistribute) {
  const auto [w, x, y, z, p, q, v] = make_symbols("w", "x", "y", "z", "p", "q", "v");

  ASSERT_IDENTICAL(x, x.distribute());
  ASSERT_IDENTICAL(x + y, (x + y).distribute());
  ASSERT_IDENTICAL(6 + 3 * x, (3 * (x + 2)).distribute());
  ASSERT_IDENTICAL(w * x + w * y, (w * (x + y)).distribute());
  ASSERT_IDENTICAL(x * x * y + x * x * 5 / 3_s, (x * x * (y + 5 / 3_s)).distribute());
  ASSERT_IDENTICAL(x * log(y) - x * w, ((log(y) - w) * x).distribute());

  // Multiple distributions:
  ASSERT_IDENTICAL(w * y + w * z + x * y + x * z, ((w + x) * (y + z)).distribute());
  ASSERT_IDENTICAL(w * w - 16, ((w - 4) * (w + 4)).distribute());
  ASSERT_IDENTICAL((p * w * y + p * w * z - p * x * y - p * x * z) +
                       (q * w * y + q * w * z - q * x * y - q * x * z) +
                       (v * w * y + v * w * z - v * x * y - v * x * z),
                   ((w - x) * (p + q + v) * (y + z)).distribute());
  // Recursive distributions:
  ASSERT_IDENTICAL((2 * p * q * w * x) - (2_s * p * q * w * y) + (p * v * w * x) - (p * v * w * y),
                   (w * (x - y) * (p * (v + q * 2_s))).distribute());

  // Distribute powers:
  ASSERT_IDENTICAL(1 + 2 * x + pow(x, 2), pow(1 + x, 2).distribute());
  ASSERT_IDENTICAL(pow(x, 3) - 4 * pow(x, 2) / 3 + 16 * x / 27 - 64_s / 729,
                   pow(x - 4_s / 9, 3).distribute());
  ASSERT_IDENTICAL(sin(pow(x, 2) * pow(y, 4) + 4 * pow(x, 2) * pow(y, 3) +
                       6 * pow(x, 2) * pow(y, 2) + 4 * pow(x, 2) * y + pow(x, 2) -
                       6 * x * pow(y, 4) - 24 * x * pow(y, 3) - 36 * x * pow(y, 2) - 24 * x * y -
                       6 * x + 9 * pow(y, 4) + 36 * pow(y, 3) + 54 * pow(y, 2) + 36 * y + 9),
                   sin(pow(x - 3, 2) * pow(y + 1, 4)).distribute());

  // Negative integer powers:
  ASSERT_IDENTICAL(pow(x * x - 4 * x / 3 + 4_s / 9, -1), pow(2_s / 3 - x, -2).distribute());

  // Rational powers that contain sqrt:
  ASSERT_IDENTICAL(-x * sqrt(4 - x) + 4 * sqrt(4 - x), pow(4 - x, 3_s / 2).distribute());
  ASSERT_IDENTICAL(-pow(x, 3) * sqrt(1 - x * x * x) + sqrt(1 - x * x * x),
                   pow(1 - x * x * x, 3_s / 2).distribute());
  ASSERT_IDENTICAL(
      pow(x, 6) * sqrt(x * x / 3 - x + 4) / 27 - pow(x, 5) * sqrt(x * x / 3 - x + 4) / 3 +
          7 * pow(x, 4) * sqrt(x * x / 3 - x + 4) / 3 - 9 * pow(x, 3) * sqrt(x * x / 3 - x + 4) +
          28 * x * x * sqrt(x * x / 3 - x + 4) - 48 * x * sqrt(x * x / 3 - x + 4) +
          64 * sqrt(x * x / 3 - x + 4),
      pow(4 - x + x * x / 3, 7_s / 2).distribute());

  // Expression where distribution of two factors produces an existing factor:
  ASSERT_IDENTICAL(pow(x, 4) - 2 * pow(x, 2) + 1,
                   ((1 - x) * (1 + x) * (1 - pow(x, 2))).distribute());
  ASSERT_IDENTICAL(-pow(x, 4) - pow(x, 3) * sqrt(pow(x, 2) + 4 * x - 1) - 6 * pow(x, 3) -
                       2 * pow(x, 2) * sqrt(pow(x, 2) + 4 * x - 1) - 6 * pow(x, 2) +
                       9 * x * sqrt(pow(x, 2) + 4 * x - 1) + 6 * x -
                       2 * sqrt(pow(x, 2) + 4 * x - 1) - 1,
                   ((2 - sqrt(pow(x, 2) + 4 * x - 1)) * (x + sqrt(pow(x, 2) + 4 * x - 1)) *
                    (pow(x, 2) + 4 * x - 1))
                       .distribute());

  // This case doesn't work yet, because we don't group terms under equivalent exponents.
  // If we did: sqrt(f) * sqrt(g) --> sqrt(f * g), we can then expand under the sqrt(...).
#if 0
  ASSERT_IDENTICAL(
      x * x - 6 * x + 2_s / 3,
      (sqrt((x - 3 - 5 * sqrt(3) / 3) * (x - 3 + 5 * sqrt(3) / 3)) * sqrt(x * x - 6 * x + 2_s / 3))
          .distribute());
#endif

  // Negative rational powers:
  ASSERT_IDENTICAL(
      1 / (pow(x, 6) * sqrt(x * x / 3 - x + 4) / 27 - pow(x, 5) * sqrt(x * x / 3 - x + 4) / 3 +
           7 * pow(x, 4) * sqrt(x * x / 3 - x + 4) / 3 - 9 * pow(x, 3) * sqrt(x * x / 3 - x + 4) +
           28 * x * x * sqrt(x * x / 3 - x + 4) - 48 * x * sqrt(x * x / 3 - x + 4) +
           64 * sqrt(x * x / 3 - x + 4)),
      pow(4 - x + x * x / 3, -7_s / 2).distribute());

  // Distribute through relational:
  ASSERT_IDENTICAL(iverson(-8 + 2 * x + pow(x, 2) < q * x + q * y - v * x - v * y),
                   iverson((x + y) * (q - v) > (x - 2) * (x + 4)).distribute());
  ASSERT_IDENTICAL(iverson(x * y + 2 * y == sin(p)), iverson((x + 2) * y == sin(p)).distribute());
}

TEST(ScalarOperationsTest, TestCollect) {
  auto [x, y, z, w] = make_symbols("x", "y", "z", "w");

  // No relevant terms:
  ASSERT_IDENTICAL(5, collect(5, x));
  ASSERT_IDENTICAL(constants::pi + y, collect(constants::pi + y, x));

  // Single term that does not need collection:
  ASSERT_IDENTICAL(x * 3, collect(x * 3, x));
  ASSERT_IDENTICAL(pow(x, y), collect(pow(x, y), x));

  // Collecting polynomial terms:
  ASSERT_IDENTICAL(x * (y + 2), collect(x * y + 2 * x, x));
  ASSERT_IDENTICAL(x * (y + z) + pow(x, 2) * (5 + z),
                   collect(x * y + x * z + pow(x, 2) * 5 + pow(x, 2) * z, x));

  // Recursive collection, w/ cancelling terms:
  // clang-format off
  auto f1 = pow(x, z) * constants::pi +
                 pow(x, z) * sin(y) +
                 log(x * 5 + x * y) +
                 pow(x, 2) * z +
                 pow(x, 2) * -z;
  // clang-format on
  ASSERT_IDENTICAL(pow(x, z) * (constants::pi + sin(y)) + log(x * (5 + y)), collect(f1, x));

  // Many recursions for one term:
  // clang-format off
  auto f2 =
    pow(x, 2) * log(pow(x, y) * sin(z) +
    cos(y) * pow(x, y) -
    x * 5 +
    x * abs(pow(x, 3) * z + pow(x, 3) * -3 + y)) -
    pow(x, 2) * z + x * 3 +
    sin(abs(z) - x) * x +
    constants::pi;
  // clang-format on
  ASSERT_IDENTICAL(pow(x, 2) * (-z + log(pow(x, y) * (sin(z) + cos(y)) +
                                         x * (-5 + abs(x * x * x * (z - 3) + y)))) +
                       x * (3 + sin(abs(z) - x)) + constants::pi,
                   collect(f2, x));

  // power with non-trivial exponent:
  // clang-format off
  auto f3 = pow(x, y) * pow(x, 2) * 5 +
                 pow(x, 3) * constants::pi +
                 pow(x, 2) * sin(y) +
                 pow(x, y + 2) * -log(z) +
                 22 +
                 pow(x, 3) * z;
  // clang-format on
  ASSERT_IDENTICAL(
      22 + pow(x, y + 2) * (5 - log(z)) + pow(x, 3) * (constants::pi + z) + pow(x, 2) * sin(y),
      collect(f3, x));

  // collected base is a function:
  // clang-format off
  auto f4 = cos(x) * cos(x) * 5 +
                 cos(x) * cos(x) * (y - 17) +
                 cos(x) * sin(cos(x) * 4 -
                 cos(x) * log(z)) -
                 cos(x) * tan(z);
  // clang-format on
  ASSERT_IDENTICAL(pow(cos(x), 2) * (y - 12) + cos(x) * (sin(cos(x) * (4 - log(z))) - tan(z)),
                   collect(f4, cos(x)));

  // A power of a sum: 1 / (pow(x, 2) + pow(y, 2))
  // Currently this only works if the sum appears as one cohesive term in a multiplication.
  auto denominator = pow(x, 2) + pow(y, 2);
  auto f5 = x / denominator - y / denominator;
  ASSERT_IDENTICAL((x - y) / denominator, collect(f5, denominator));

  // Try with: 1 / sqrt(pow(x, 2) + pow(y, 2))
  auto f6 = x / sqrt(denominator) - 5 * y / sqrt(denominator);
  ASSERT_IDENTICAL((x - 5 * y) / sqrt(denominator), collect(f6, denominator));
}

TEST(ScalarOperationsTest, TestCollectMany) {
  auto [x, y, z, w] = make_symbols("x", "y", "z", "w");

  // Collecting multiple variables:
  // clang-format off
  auto f1 = x * y +
                 x * -5 -
                 x * y * log(z);
  // clang-format on
  ASSERT_IDENTICAL(x * (-5 + y * (1 - log(z))), collect_many(f1, {x, y}));
  ASSERT_IDENTICAL(y * x * (1 - log(z)) - x * 5, collect_many(f1, {y, x}));

  // clang-format off
  auto f2 = x * x * y * y * y +
            x * x * 2 * y * y * y -
            x * x * y * y * sin(z) -
            x * x * y * y +
            x * x + x * x * 4;
  // clang-format on
  ASSERT_IDENTICAL(x * x * (y * y * y * 3 + y * y * (-sin(z) - 1) + 5), collect_many(f2, {x, y}));
  ASSERT_IDENTICAL(y * y * y * x * x * 3 + y * y * x * x * (-sin(z) - 1) + pow(x, 2) * 5,
                   collect_many(f2, {y, x}));

  // Collecting many functions:
  // clang-format off
  auto f3 = log(y) * z * -cos(x) * cos(x) +
                 cos(x) * cos(x) * 16 -
                 log(y) / tan(z) * cos(x) * cos(x) +
                 log(y) * (z * z) +
                 log(y) * -3;
  // clang-format on
  ASSERT_IDENTICAL(cos(x) * cos(x) * (16 + log(y) * (-z - 1 / tan(z))) + log(y) * (z * z - 3),
                   collect_many(f3, {cos(x), log(y)}));
  ASSERT_IDENTICAL(log(y) * (pow(cos(x), 2) * (-z - 1 / tan(z)) + z * z - 3) + pow(cos(x), 2) * 16,
                   collect_many(f3, {log(y), cos(x)}));

  // Three layers of nesting
  // clang-format off
  auto f4 =
    pow(x, 2) * (pow(y, 2) * (pow(z, 2) - 4 * z + 8) - 2 * y + 10) +
    x * (pow(y, 3) * z * (8 - sin(y)) + pow(y, 2) * z * log(x)) +
    pow(y, 2) * (pow(z, 2) * (5 + constants::euler) - z * 8 + 1) + y * (pow(z, 5) - 22);
  // clang-format on
  ASSERT_IDENTICAL(f4, collect_many(f4.distribute(), {x, y, z}));

  // Generate a polynomial in four variables:
  // Use std::function, so that we can recurse this lambda.
  std::function<scalar_expr(absl::Span<const scalar_expr>)> make_poly;
  make_poly = [&make_poly](absl::Span<const scalar_expr> vars) -> scalar_expr {
    if (vars.size() == 1) {
      return pow(vars[0], 2) + vars[0] + 1;
    } else {
      return pow(vars[0], 2) * make_poly(vars.subspan(1)) + vars[0] * make_poly(vars.subspan(1)) +
             1;
    }
  };

  const auto f5 = make_poly({x, y, z, w});
  ASSERT_IDENTICAL(f5, collect_many(f5.distribute(), {x, y, z, w}));
}

TEST(ScalarOperationsTest, TestNumericSetsVariables) {
  ASSERT_EQ(number_set::unknown, determine_numeric_set(scalar_expr{"x"}));
  ASSERT_EQ(number_set::real, determine_numeric_set(scalar_expr{"x", number_set::real}));
  ASSERT_EQ(number_set::complex, determine_numeric_set(scalar_expr{"x", number_set::complex}));

  // Identical must consider the numeric set of the variable.
  ASSERT_IDENTICAL(scalar_expr("x"), scalar_expr("x", number_set::unknown));
  ASSERT_IDENTICAL(scalar_expr("x", number_set::real), scalar_expr("x", number_set::real));
  ASSERT_NOT_IDENTICAL(scalar_expr("x", number_set::real), scalar_expr("x", number_set::complex));
  ASSERT_NOT_IDENTICAL(scalar_expr("x", number_set::real),
                       scalar_expr("x", number_set::real_non_negative));
}

TEST(ScalarOperationsTest, TestNumericSetsNumericalValues) {
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(0));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(3));
  ASSERT_EQ(number_set::real, determine_numeric_set(-10));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(11_s / 13));
  ASSERT_EQ(number_set::real, determine_numeric_set(-3_s / 2));
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(0.0));
}

TEST(ScalarOperationsTest, TestNumericSetsAddMul) {
  // make unique symbols so additions can't collapse into multiplications:
  const auto real = []() { return make_unique_variable_symbol(number_set::real); };
  const auto real_non_negative = []() {
    return make_unique_variable_symbol(number_set::real_non_negative);
  };
  const auto real_positive = []() {
    return make_unique_variable_symbol(number_set::real_positive);
  };
  const auto complex = []() { return make_unique_variable_symbol(number_set::complex); };
  const auto unknown = []() { return make_unique_variable_symbol(number_set::unknown); };

  ASSERT_EQ(number_set::real, determine_numeric_set(real() + real()));
  ASSERT_EQ(number_set::real, determine_numeric_set(real() + real_non_negative()));
  ASSERT_EQ(number_set::real, determine_numeric_set(real() + real_positive()));
  ASSERT_EQ(number_set::complex, determine_numeric_set(real() + complex()));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(real() + unknown()));

  ASSERT_EQ(number_set::real_positive, determine_numeric_set(real_positive() + real_positive()));
  ASSERT_EQ(number_set::real_positive,
            determine_numeric_set(real_positive() + real_non_negative()));
  ASSERT_EQ(number_set::complex, determine_numeric_set(real_positive() + complex()));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(real_positive() + unknown()));

  ASSERT_EQ(number_set::real_non_negative,
            determine_numeric_set(real_non_negative() + real_non_negative()));
  ASSERT_EQ(number_set::complex, determine_numeric_set(real_non_negative() + complex()));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(real_non_negative() + unknown()));

  ASSERT_EQ(number_set::complex, determine_numeric_set(complex() + complex()));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(complex() + unknown()));

  ASSERT_EQ(number_set::real, determine_numeric_set(real() * real()));
  ASSERT_EQ(number_set::real, determine_numeric_set(real() * real_non_negative()));
  ASSERT_EQ(number_set::real, determine_numeric_set(real() * real_positive()));
  ASSERT_EQ(number_set::complex, determine_numeric_set(real() * complex()));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(real() * unknown()));

  ASSERT_EQ(number_set::real_positive, determine_numeric_set(real_positive() * real_positive()));
  ASSERT_EQ(number_set::real_non_negative,
            determine_numeric_set(real_positive() * real_non_negative()));
  ASSERT_EQ(number_set::complex, determine_numeric_set(real_positive() * complex()));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(real_positive() * unknown()));

  ASSERT_EQ(number_set::real_non_negative,
            determine_numeric_set(real_non_negative() * real_non_negative()));
  ASSERT_EQ(number_set::complex, determine_numeric_set(real_non_negative() * complex()));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(real_non_negative() * unknown()));

  ASSERT_EQ(number_set::complex, determine_numeric_set(complex() * complex()));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(complex() * unknown()));

  // subtraction is both addition and multiplication:
  ASSERT_EQ(number_set::real, determine_numeric_set(real_positive() - real_positive()));
  ASSERT_EQ(number_set::real, determine_numeric_set(real_positive() - real_non_negative()));
  ASSERT_EQ(number_set::complex, determine_numeric_set(real_positive() - complex()));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(real_positive() - unknown()));

  ASSERT_EQ(number_set::real, determine_numeric_set(real_non_negative() - real_non_negative()));
  ASSERT_EQ(number_set::complex, determine_numeric_set(real_non_negative() - complex()));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(real_non_negative() - unknown()));
}

TEST(ScalarOperationsTest, TestNumericSetsPow) {
  const scalar_expr real = make_unique_variable_symbol(number_set::real);
  const scalar_expr real_non_negative = make_unique_variable_symbol(number_set::real_non_negative);
  const scalar_expr real_positive = make_unique_variable_symbol(number_set::real_positive);
  const scalar_expr complex{"z", number_set::complex};
  const scalar_expr unknown{"v", number_set::unknown};

  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(real * real));
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(pow(real, 4)));
  ASSERT_EQ(number_set::real, determine_numeric_set(pow(real, 3)));
  ASSERT_EQ(number_set::real, determine_numeric_set(pow(real, real_positive)));
  ASSERT_EQ(number_set::real, determine_numeric_set(pow(real, real_non_negative)));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(pow(real, real)));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(pow(real, complex)));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(pow(real, unknown)));

  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(pow(real_non_negative, 2)));
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(pow(real_non_negative, 3)));
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(pow(real_non_negative, 2.1231)));
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(pow(real_non_negative, 3_s / 5)));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(pow(real_non_negative, real)));
  ASSERT_EQ(number_set::real_non_negative,
            determine_numeric_set(pow(real_non_negative, real_non_negative)));
  ASSERT_EQ(number_set::real_non_negative,
            determine_numeric_set(pow(real_non_negative, real_positive)));

  ASSERT_EQ(number_set::real_positive, determine_numeric_set(pow(real_positive, 4)));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(pow(real_positive, 3)));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(pow(real_positive, 2.1231)));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(pow(real_positive, 3_s / 5)));
  ASSERT_EQ(number_set::real, determine_numeric_set(pow(real_positive, real)));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(pow(real_positive, real_positive)));
  ASSERT_EQ(number_set::real_positive,
            determine_numeric_set(pow(real_positive, real_non_negative)));

  ASSERT_EQ(number_set::unknown, determine_numeric_set(pow(complex, complex)));
}

TEST(ScalarOperationsTest, TestNumericSetsFunctions) {
  const scalar_expr real{"x", number_set::real};
  const scalar_expr real_non_negative{"y", number_set::real_non_negative};
  const scalar_expr real_positive{"w", number_set::real_positive};
  const scalar_expr complex{"z", number_set::complex};

  ASSERT_EQ(number_set::real, determine_numeric_set(cos(real)));
  ASSERT_EQ(number_set::real, determine_numeric_set(sin(real)));
  ASSERT_EQ(number_set::real, determine_numeric_set(cos(real_non_negative)));
  ASSERT_EQ(number_set::real, determine_numeric_set(sin(real_non_negative)));
  ASSERT_EQ(number_set::complex, determine_numeric_set(sin(complex)));
  ASSERT_EQ(number_set::complex, determine_numeric_set(cos(complex)));

  ASSERT_EQ(number_set::unknown, determine_numeric_set(tan(real)));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(tan(complex)));

  // inverse trig not implement
  for (const auto& val : {real, real_non_negative, real_non_negative, complex}) {
    ASSERT_EQ(number_set::unknown, determine_numeric_set(acos(val)));
    ASSERT_EQ(number_set::unknown, determine_numeric_set(asin(val)));
    ASSERT_EQ(number_set::unknown, determine_numeric_set(atan(val)));
  }

  ASSERT_EQ(number_set::unknown, determine_numeric_set(acos(real)));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(acos(real_non_negative)));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(acos(complex)));

  // hyperbolic functions:
  ASSERT_EQ(number_set::unknown, determine_numeric_set(cosh(complex)));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(cosh(real)));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(cosh(real_non_negative)));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(cosh(real_positive)));

  ASSERT_EQ(number_set::unknown, determine_numeric_set(sinh(complex)));
  ASSERT_EQ(number_set::real, determine_numeric_set(sinh(real)));
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(sinh(real_non_negative)));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(sinh(real_positive)));

  ASSERT_EQ(number_set::unknown, determine_numeric_set(tanh(complex)));
  ASSERT_EQ(number_set::real, determine_numeric_set(tanh(real)));
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(tanh(real_non_negative)));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(tanh(real_positive)));

  // inverse hyperbolic trig not implemented:
  for (const auto& val : {real, real_non_negative, real_non_negative, complex}) {
    ASSERT_EQ(number_set::unknown, determine_numeric_set(acosh(val)));
    ASSERT_EQ(number_set::unknown, determine_numeric_set(asinh(val)));
    ASSERT_EQ(number_set::unknown, determine_numeric_set(atanh(val)));
  }

  // not implemented
  ASSERT_EQ(number_set::unknown, determine_numeric_set(log(real)));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(log(real_non_negative)));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(log(complex)));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(log(real_positive)));

  ASSERT_EQ(number_set::real_positive, determine_numeric_set(abs(real_positive)));
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(abs(real)));
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(abs(real_non_negative)));
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(abs(complex)));

  ASSERT_EQ(number_set::real, determine_numeric_set(signum(real)));
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(signum(real_non_negative)));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(signum(real_positive)));

  ASSERT_EQ(number_set::real, determine_numeric_set(floor(real)));
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(floor(real_non_negative)));
  ASSERT_EQ(number_set::real_non_negative, determine_numeric_set(floor(real_positive)));
  ASSERT_EQ(number_set::complex, determine_numeric_set(floor(complex)));
}

TEST(ScalarOperationsTest, TestNumericSetsSpecialValues) {
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(constants::euler));
  ASSERT_EQ(number_set::real_positive, determine_numeric_set(constants::pi));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(constants::complex_infinity));
  ASSERT_EQ(number_set::unknown, determine_numeric_set(constants::undefined));
  ASSERT_EQ(number_set::complex, determine_numeric_set(constants::imaginary_unit));
}

TEST(ScalarOperationsTest, TestNumericSetsConditional) {
  const scalar_expr real{"x", number_set::real};
  const scalar_expr real_non_negative{"y", number_set::real_non_negative};
  const scalar_expr real_positive{"w", number_set::real_positive};
  const scalar_expr complex{"z", number_set::complex};

  ASSERT_EQ(number_set::real_non_negative,
            determine_numeric_set(iverson(real > 0)));  //  TODO: Should be boolean.
  ASSERT_EQ(number_set::real, determine_numeric_set(where(real > 0, real, real_non_negative)));
  ASSERT_EQ(number_set::real_non_negative,
            determine_numeric_set(where(real > 0, real_positive, real_non_negative)));
  ASSERT_EQ(number_set::complex, determine_numeric_set(where(real > 0, real_positive, complex)));
}

}  // namespace wf
