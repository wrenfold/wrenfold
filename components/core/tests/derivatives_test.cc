// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.

#include "wf/constants.h"
#include "wf/expressions/derivative_expression.h"
#include "wf/expressions/function_expressions.h"
#include "wf/expressions/substitute_expression.h"
#include "wf/functions.h"
#include "wf/matrix_functions.h"
#include "wf/number_set.h"
#include "wf/utility/error_types.h"
#include "wf/utility_visitors.h"

#include "wf_test_support/test_macros.h"

// Test `diff` operation.
namespace wf {
using namespace wf::custom_literals;

TEST(DerivativesTest, TestConstants) {
  const scalar_expr x{"x"};
  ASSERT_IDENTICAL(constants::zero, (5_s).diff(x));
  ASSERT_IDENTICAL(constants::zero, (22.5_s).diff(x, 4));
  ASSERT_IDENTICAL(constants::zero, constants::pi.diff(x));
  ASSERT_IDENTICAL(constants::zero, constants::euler.diff(x));
  ASSERT_IDENTICAL(constants::zero, constants::complex_infinity.diff(x));
  ASSERT_IDENTICAL(constants::zero, constants::imaginary_unit.diff(x));
  ASSERT_THROW(x.diff(5), type_error);
  ASSERT_THROW(x.diff(constants::pi), type_error);
  ASSERT_THROW(x.diff(constants::imaginary_unit), type_error);
}

TEST(DerivativesTest, TestAdditionAndSubtraction) {
  const scalar_expr w{"w"};
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  ASSERT_IDENTICAL(constants::zero, (x + y).diff(w));
  ASSERT_IDENTICAL(constants::zero, (x - y).diff(w, 2));
  ASSERT_IDENTICAL(constants::one, (x + y).diff(y));
  ASSERT_IDENTICAL(-1, (x - y).diff(y));
  ASSERT_IDENTICAL(-5 / 7_s, ((x - y * 5) / 7_s).diff(y));
  ASSERT_IDENTICAL(constants::zero, (x - y).diff(y, 2));
  ASSERT_IDENTICAL(2, (x + y + x).diff(x));
}

TEST(DerivativesTest, TestMultiplication) {
  const auto [w, x, y] = make_symbols("w", "x", "y");

  ASSERT_IDENTICAL(0, (x * y).diff(w));
  ASSERT_IDENTICAL(y, (x * y).diff(x));
  ASSERT_IDENTICAL(x, (x * y).diff(y));
  ASSERT_IDENTICAL(3, (x * 3).diff(x));
  ASSERT_IDENTICAL(0, (x * y).diff(x, 2));
  ASSERT_IDENTICAL(2 * x, (x * x).diff(x));
  ASSERT_IDENTICAL(2, (x * x).diff(x, 2));
  ASSERT_IDENTICAL(3 * pow(x, 2), (x * x * x).diff(x));
  ASSERT_IDENTICAL(2 * x * y, (x * y * x).diff(x));
  ASSERT_IDENTICAL(2 * y, (x * y * x).diff(x, 2));
  ASSERT_IDENTICAL(5 * pow(y, 4), pow(y, 5).diff(y));
  ASSERT_IDENTICAL(20 * pow(y, 3), pow(y, 5).diff(y, 2));
  ASSERT_IDENTICAL(1 / y, (x / y).diff(x));
  ASSERT_IDENTICAL(-y / (x * x), (y / x).diff(x));
  ASSERT_IDENTICAL(2 * y / pow(x, 3), (y / x).diff(x, 2));
  ASSERT_IDENTICAL(x * y + sin(w) * y - 3 * log(x) * pow(w, 2),
                   (w * x * y - cos(w) * y - log(x) * pow(w, 3)).diff(w));
  ASSERT_IDENTICAL(5 * cos(x) * sin(w) * sin(x) / x + 5 * pow(cos(x), 2) * log(x) * sin(w) -
                       5 * log(x) * sin(w) * pow(sin(x), 2),
                   (cos(x) * sin(x) * log(x) * 5 * sin(w)).diff(x));
  ASSERT_IDENTICAL(w * pow(x, 2) / pow(cos(x), 2) + 2 * w * x * tan(x),
                   (tan(x) * pow(x, 2) * w).diff(x));
}

TEST(DerivativesTest, TestPower) {
  const scalar_expr w{"w"};
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  ASSERT_IDENTICAL(constants::zero, pow(x, y).diff(w));

  ASSERT_IDENTICAL(y * pow(x, y - 1), pow(x, y).diff(x));
  ASSERT_IDENTICAL(y * (y - 1) * pow(x, y - 2), pow(x, y).diff(x, 2));
  ASSERT_IDENTICAL(y * (y - 1) * (y - 2) * pow(x, y - 3), pow(x, y).diff(x, 3));

  ASSERT_IDENTICAL(pow(y, w) * log(y), pow(y, w).diff(w));
  ASSERT_IDENTICAL(pow(y, w) * pow(log(y), 2), pow(y, w).diff(w, 2));
  ASSERT_IDENTICAL(pow(y, w) * pow(log(y), 3), pow(y, w).diff(w, 3));
  ASSERT_IDENTICAL(pow(x, x) * log(x) + pow(x, x), pow(x, x).diff(x));
  ASSERT_IDENTICAL(
      sin(x) * pow(cos(x), sin(x) - 1) * -sin(x) + pow(cos(x), sin(x)) * log(cos(x)) * cos(x),
      pow(cos(x), sin(x)).diff(x));
}

TEST(DerivativesTest, TestLog) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  ASSERT_IDENTICAL(1 / x, log(x).diff(x));
  ASSERT_IDENTICAL(0, log(x).diff(y));
  ASSERT_IDENTICAL(1 / y, log(x * y).diff(y));
  ASSERT_IDENTICAL(1 / (2 * x), log(sqrt(x)).diff(x));
}

TEST(DerivativesTest, TestTrig) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  ASSERT_IDENTICAL(cos(x), sin(x).diff(x));
  ASSERT_IDENTICAL(-sin(x), cos(x).diff(x));
  ASSERT_IDENTICAL(-cos(y) * cos(cos(sin(y))) * sin(sin(y)), sin(cos(sin(y))).diff(y));
  ASSERT_IDENTICAL(y * cos(x * y), sin(x * y).diff(x));
  ASSERT_IDENTICAL(-2 * sin(2 * x + y), cos(2 * x + y).diff(x));
  ASSERT_IDENTICAL(1 / pow(cos(x), 2), tan(x).diff(x));
  ASSERT_IDENTICAL(-y / (x * x) / (cos(y / x) * cos(y / x)), tan(y / x).diff(x));
}

TEST(DerivativesTest, TestInverseTrig) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};

  ASSERT_IDENTICAL(constants::zero, acos(5).diff(x));
  ASSERT_IDENTICAL(-1 / sqrt(1 - x * x), acos(x).diff(x));
  ASSERT_IDENTICAL(-y / sqrt(1 - x * x * y * y), acos(x * y).diff(x));

  ASSERT_IDENTICAL(constants::zero, asin(y).diff(x));
  ASSERT_IDENTICAL(1 / sqrt(1 - x * x), asin(x).diff(x));
  ASSERT_IDENTICAL(sqrt(x) / sqrt(1 - y * y * x), asin(y * sqrt(x)).diff(y));

  ASSERT_IDENTICAL(constants::zero, atan(constants::euler).diff(y));
  ASSERT_IDENTICAL(1_s / (x * x + 1), atan(x).diff(x));
  ASSERT_IDENTICAL(3_s * (x * x) / (pow(x, 6) + 1), atan(pow(x, 3)).diff(x));
}

TEST(DerivativesTest, TestHyperbolicTrig) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(0, cosh(x).diff(y));
  ASSERT_IDENTICAL(sinh(x), cosh(x).diff(x));
  ASSERT_IDENTICAL(sinh(x * y) * y, cosh(x * y).diff(x));

  ASSERT_IDENTICAL(0, sinh(x).diff(y));
  ASSERT_IDENTICAL(cosh(x), sinh(x).diff(x));
  ASSERT_IDENTICAL(cosh(x * y) * y, sinh(x * y).diff(x));

  ASSERT_IDENTICAL(0, tanh(x).diff(y));
  ASSERT_IDENTICAL(1 - tanh(x) * tanh(x), tanh(x).diff(x));
  ASSERT_IDENTICAL((1 - tanh(x * y) * tanh(x * y)) * y, tanh(x * y).diff(x));
}

TEST(DerivativesTest, TestInverseHyperbolicTrig) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(0, acosh(x).diff(y));
  ASSERT_IDENTICAL(1 / sqrt(x - 1) * 1 / sqrt(x + 1), acosh(x).diff(x));
  ASSERT_IDENTICAL(y / sqrt(y * x - 1) * 1 / sqrt(y * x + 1), acosh(x * y).diff(x));

  ASSERT_IDENTICAL(0, asinh(x).diff(y));
  ASSERT_IDENTICAL(1 / sqrt(x * x + 1), asinh(x).diff(x));
  ASSERT_IDENTICAL(y / sqrt(x * x * y * y + 1), asinh(x * y).diff(x));

  ASSERT_IDENTICAL(0, atanh(x).diff(y));
  ASSERT_IDENTICAL(1 / (1 - x * x), atanh(x).diff(x));
  ASSERT_IDENTICAL(y / (1 - x * x * y * y), atanh(x * y).diff(x));
}

TEST(DerivativesTest, TestAtan2) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  const scalar_expr z{"z"};
  ASSERT_IDENTICAL(0, atan2(y, x).diff(z));
  ASSERT_IDENTICAL(x / (x * x + y * y), atan2(y, x).diff(y));
  ASSERT_IDENTICAL(-y / (x * x + y * y), atan2(y, x).diff(x));
  ASSERT_IDENTICAL(-y / (pow(log(x), 2) + y * y) * (1 / x), atan2(y, log(x)).diff(x));
  ASSERT_IDENTICAL(5 * x * -sin(y) / (pow(5 * x, 2) + pow(cos(y), 2)),
                   atan2(cos(y), 5 * x).diff(y));
  ASSERT_IDENTICAL(0, atan2(x, x).diff(x));
}

TEST(DerivativesTest, TestAbs) {
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  ASSERT_IDENTICAL(0, abs(x).diff(y));
  ASSERT_IDENTICAL(y / abs(y), abs(y).diff(y));
  ASSERT_IDENTICAL(cos(x) * sin(x) / abs(sin(x)), abs(sin(x)).diff(x));
}

TEST(DerivativesTest, TestSignum) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(0, signum(x).diff(y));

  ASSERT_IDENTICAL(0, signum(x).diff(x, 1));
  ASSERT_IDENTICAL(0, signum(x * x - y).diff(x, 1));

  ASSERT_IDENTICAL(derivative::create(signum(2 * x), x, 1),
                   signum(x * 2).diff(x, 1, non_differentiable_behavior::abstract));
  ASSERT_IDENTICAL(derivative::create(signum(x), x, 2),
                   signum(x).diff(x, 2, non_differentiable_behavior::abstract));
}

TEST(DerivativesTest, TestFloor) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(0, floor(x).diff(y));
  ASSERT_IDENTICAL(0, floor(x).diff(x));
  ASSERT_IDENTICAL(derivative::create(floor(x + y), x, 2),
                   floor(x + y).diff(x, 2, non_differentiable_behavior::abstract));
}

TEST(DerivativesTest, TestMaxMin) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  ASSERT_IDENTICAL(0, max(x, y).diff(z));
  ASSERT_IDENTICAL(0, min(2 * x, y - 3).diff(z));

  ASSERT_IDENTICAL(where(x < y, 0, 1), max(x, y).diff(x));
  ASSERT_IDENTICAL(where(y < x, 0, 1), min(x, y).diff(x));

  ASSERT_IDENTICAL(where(cos(x) < sin(x), cos(x), -sin(x)), max(cos(x), sin(x)).diff(x));
}

TEST(DerivativesTest, TestMatrix) {
  // Matrix derivative should do element-wise differentiation.
  const scalar_expr x{"x"};
  const scalar_expr y{"y"};
  const scalar_expr z{"z"};
  ASSERT_IDENTICAL(make_vector(0, 0, 0), make_vector(x, y, 1).diff(z));
  ASSERT_IDENTICAL(make_vector(0, 1, 0), make_vector(x, y, z).diff(y));
  ASSERT_IDENTICAL(make_vector(cos(y), 2 * x, -z / pow(x, 2)),
                   make_vector(x * cos(y), y + x * x, z / x).diff(x));
}

TEST(DerivativesTest, TestRelational) {
  // Cannot diff a relational:
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(0, iverson(x < y).diff(x));
  ASSERT_IDENTICAL(0, iverson(x == y).diff(x));
  ASSERT_IDENTICAL(derivative::create(iverson(x < y), x, 1),
                   iverson(x < y).diff(x, 1, non_differentiable_behavior::abstract));
  ASSERT_IDENTICAL(derivative::create(iverson(x == y), x, 1),
                   iverson(x == y).diff(x, 1, non_differentiable_behavior::abstract));
}

TEST(DerivativesTest, TestIversonBracket) {
  const auto [x, y] = make_symbols("x", "y");

  ASSERT_IDENTICAL(0, iverson(x < y).diff(x));
  ASSERT_IDENTICAL(0, iverson(x < y).diff(x).diff(y));

  // Check that this produces a `derivative` expression:
  ASSERT_IDENTICAL(derivative::create(iverson(x < y), x, 1),
                   iverson(x < y).diff(x, 1, non_differentiable_behavior::abstract));
  ASSERT_IDENTICAL(derivative::create(derivative::create(iverson(x < y), x, 1), y, 1),
                   iverson(x < y)
                       .diff(x, 1, non_differentiable_behavior::abstract)
                       .diff(y, 1, non_differentiable_behavior::abstract));
}

TEST(DerivativesTest, TestDerivativeExpression) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  // Create an abstract derivative expression and differentiate it.
  // Normally you wouldn't do this with a function like abs(), but it serves for a test here.
  auto f = derivative::create(abs(x + y), x, 1);
  ASSERT_IDENTICAL(derivative::create(abs(x + y), x, 2), f.diff(x));
  ASSERT_IDENTICAL(derivative::create(f, y, 1), f.diff(y));
  ASSERT_IDENTICAL(0, f.diff(z));
}

TEST(DerivativesTest, TestUnevaluated) {
  const auto [x, y, z] = make_symbols("x", "y", "z");

  // Terms between parentheses should remain in parentheses:
  ASSERT_IDENTICAL(make_unevaluated(2 * x), make_unevaluated(x * x).diff(x));
  ASSERT_IDENTICAL(make_unevaluated(x * cos(x * y)) * make_unevaluated(y * y) +
                       make_unevaluated(sin(x * y)) * make_unevaluated(2 * y),
                   (make_unevaluated(sin(x * y)) * make_unevaluated(y * y)).diff(y));
}

static auto get_unique_variables(const scalar_expr& expr) {
  auto variables = get_variables(expr);
  variables.erase(std::remove_if(variables.begin(), variables.end(),
                                 [](const variable& v) {
                                   return !std::holds_alternative<unique_variable>(v.identifier());
                                 }),
                  variables.end());
  WF_ASSERT(!variables.empty());
  return variables;
}

TEST(DerivativesTest, TestSymbolicFunction) {
  const scalar_expr x{"x", number_set::real};
  const auto [y, z] = make_symbols("y", "z");
  const auto f = symbolic_function("f");

  ASSERT_IDENTICAL(0, f(x).diff(y));
  ASSERT_IDENTICAL(0, f(x, z).diff(y));
  ASSERT_IDENTICAL(derivative::create(f(x), x, 1), f(x).diff(x));
  ASSERT_IDENTICAL(derivative::create(f(x, z), z, 1), f(x, z).diff(z));
  ASSERT_IDENTICAL(derivative::create(f(x, z), z, 2), f(x, z).diff(z, 2));

  {
    const auto g = f(2 * x).diff(x);
    const scalar_expr u1{get_unique_variables(g).front()};
    ASSERT_EQ(number_set::real, determine_numeric_set(u1));
    ASSERT_IDENTICAL(2 * substitution::create(f(u1).diff(u1), u1, 2 * x), g);
  }

  {
    const auto g = f(x, x).diff(x);
    const scalar_expr u1{get_unique_variables(g).front()};
    const scalar_expr u2{get_unique_variables(g).back()};
    ASSERT_NOT_IDENTICAL(u1, u2);
    ASSERT_IDENTICAL(substitution::create(f(u1, x).diff(u1), u1, x) +
                         substitution::create(f(x, u2).diff(u2), u2, x),
                     g);
  }

  {
    const auto g = f(x * x, sin(x * 3), y).diff(x);
    const scalar_expr u1{get_unique_variables(g).front()};
    const scalar_expr u2{get_unique_variables(g).back()};
    ASSERT_NOT_IDENTICAL(u1, u2);
    ASSERT_IDENTICAL(
        2 * x * substitution::create(f(u1, sin(x * 3), y).diff(u1), u1, x * x) +
            3 * cos(x * 3) * substitution::create(f(x * x, u2, y).diff(u2), u2, sin(x * 3)),
        g);
  }
}

TEST(DerivativesTest, TestWrtSymbolicFunction) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  const auto f = symbolic_function("f");
  const auto g = symbolic_function("g");

  // A symbolic function wrt itself:
  ASSERT_IDENTICAL(1, f(x).diff(f(x)));
  ASSERT_IDENTICAL(1, f(x, y + z).diff(f(x, y + z)));

  // Symbolic function wrt other symbolic function is zero (this matches SymPy).
  ASSERT_IDENTICAL(0, f(x).diff(g(x)));
  ASSERT_IDENTICAL(0, f(x).diff(g(f(x))));

  // Other expressions wrt symbolic function:
  ASSERT_IDENTICAL(-sin(f(x) * 2) * 2, cos(f(x) * 2).diff(f(x)));
  ASSERT_IDENTICAL(2 * f(x), (f(x) * f(x) + 2).diff(f(x)));

  // Check that: d( df(x, y)/dx ) / df(x, y) --> 0 (this also matches SymPy).
  ASSERT_IDENTICAL(0, derivative::create(f(x, y), x, 1).diff(f(x, y)));

  // Create a derivative wrt a symbolic function invocation:
  ASSERT_IDENTICAL(derivative::create(signum(f(x)), f(x), 1),
                   signum(f(x)).diff(f(x), 1, non_differentiable_behavior::abstract));
}

TEST(DerivativeTest, TestWrtDerivativeOfSymbolicFunction) {
  const auto [x, y, z] = make_symbols("x", "y", "z");
  const auto f = symbolic_function("f");
  const auto g = symbolic_function("g");

  ASSERT_IDENTICAL(0, f(x).diff(g(x).diff(x)));
  ASSERT_IDENTICAL(0, f(x).diff(g(x).diff(x, 2)));
  ASSERT_IDENTICAL(0, f(y).diff(x).diff(f(y).diff(y)));

  ASSERT_IDENTICAL(1, f(x).diff(x).diff(f(x).diff(x)));
  ASSERT_IDENTICAL(2 * f(x, y).diff(x), pow(f(x, y).diff(x), 2).diff(f(x, y).diff(x)));

  // Derivative of anything else is invalid.
  ASSERT_THROW(f(x).diff(derivative::create(x * y * 3, y, 1)), wf::type_error);
  ASSERT_THROW(f(x, cos(y)).diff(derivative::create(cos(y), y, 1)), wf::type_error);
}

TEST(DerivativesTest, TestSubstitution) {
  const scalar_expr x{"x", number_set::real};
  const auto [y, z] = make_symbols("y", "z");
  const auto f = symbolic_function("f");

  ASSERT_IDENTICAL(0, substitution::create(f(y), y, 1 + z).diff(x));
  ASSERT_IDENTICAL(0, substitution::create(f(y), f(z, y), 1 + z).diff(x));

  // Target is a function of `x`:
  ASSERT_IDENTICAL(derivative::create(substitution::create(f(y), x, z), x, 1),
                   substitution::create(f(y), x, z).diff(x));
  ASSERT_IDENTICAL(
      derivative::create(substitution::create(f(y, z), cos(x) / (3 * x), abs(x)), x, 1),
      substitution::create(f(y, z), cos(x) / (3 * x), abs(x)).diff(x));

  // Input expression to substitution is a function of `x`:
  ASSERT_IDENTICAL(substitution::create(f(x, y).diff(x), y, z * z),
                   substitution::create(f(x, y), y, z * z).diff(x));
  ASSERT_IDENTICAL(substitution::create(f(x, 10_s, y).diff(x, 2), cos(y), sin(z)),
                   substitution::create(f(x, 10_s, y), cos(y), sin(z)).diff(x, 2));

  // Replacement is a function of x:
  {
    const auto g = substitution::create(f(y), y, x).diff(x);
    ASSERT_IDENTICAL(substitution::create(f(y).diff(y), y, x), g);
  }
  {
    const auto g = substitution::create(f(y), y, sin(2 * z)).diff(z);
    ASSERT_IDENTICAL(substitution::create(f(y).diff(y), y, sin(2 * z)) * cos(2 * z) * 2, g);
  }
  {
    const auto g = substitution::create(f(y), y + 2, cos(z * z)).diff(z);
    const scalar_expr u1{get_unique_variables(g).front()};
    ASSERT_EQ(number_set::unknown, determine_numeric_set(u1));
    ASSERT_IDENTICAL(
        substitution::create(substitution::create(f(y), y + 2, u1).diff(u1), u1, cos(z * z)) *
            -sin(z * z) * 2 * z,
        g);
  }

  // Replacement and input expression are a function of `x`.
  {
    const auto g = substitution::create(f(y, x), x, y * y * y).diff(y);
    ASSERT_IDENTICAL(3 * pow(y, 2) * substitution::create(f(y, x).diff(x), x, y * y * y) +
                         substitution::create(f(y, x).diff(y), x, y * y * y),
                     g);
  }
  {
    const auto g = substitution::create(f(y, sin(x)), x, sin(2 * y)).diff(y);
    const scalar_expr u1{get_unique_variables(g).front()};
    // `real`, because we insert a variable for `sin(x)` (which is real)
    ASSERT_EQ(number_set::real, determine_numeric_set(u1));
    ASSERT_IDENTICAL(
        2 * cos(2 * y) *
                substitution::create(cos(x) * substitution::create(f(y, u1).diff(u1), u1, sin(x)),
                                     x, sin(2 * y)) +
            substitution::create(f(y, sin(x)).diff(y), x, sin(y * 2)),
        g);
  }
  {
    const auto g = substitution::create(f(pow(x, 2), y), y, sinh(x)).diff(x);
    const scalar_expr u1{get_unique_variables(g).front()};
    ASSERT_EQ(number_set::real, determine_numeric_set(u1));
    // If `substitution::create` were smarter, we could do the replacement y --> sinh(x) in the
    // second term to get f(u1, sinh(x)).diff(u1).
    ASSERT_IDENTICAL(
        cosh(x) * substitution::create(f(x * x, y).diff(y), y, sinh(x)) +
            substitution::create(2 * x * substitution::create(f(u1, y).diff(u1), u1, x * x), y,
                                 sinh(x)),
        g);
  }
}

TEST(DerivativesTest, TestStopDerivative) {
  const auto [x, y] = make_symbols("x", "y");
  ASSERT_IDENTICAL(0, stop_diff(x * y).diff(x));
  ASSERT_IDENTICAL(0, stop_diff(x * y).diff(y, 2));
  ASSERT_IDENTICAL(3 * cos(3 * y) * stop_diff(x * y), (sin(3 * y) * stop_diff(x * y)).diff(y));
  ASSERT_IDENTICAL(-sin(y) * stop_diff(2 * y), (sin(y) * stop_diff(2 * y)).diff(y, 2));
  ASSERT_NOT_IDENTICAL(stop_diff(x), stop_diff(y));
}

}  // namespace wf
