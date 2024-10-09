// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/declare_external_function.h"
#include "wf/compound_expression.h"
#include "wf/constants.h"
#include "wf/cse.h"
#include "wf/expressions/derivative_expression.h"
#include "wf/functions.h"
#include "wf/substitute.h"
#include "wf/type_annotations.h"

#include "wf_test_support/test_macros.h"

namespace wf {
namespace ta = type_annotations;

template <typename T>
annotated_custom_type<T> create_type_description() {
  custom_type_registry registry{};
  return detail::record_type<T>{}(registry);
}

struct test_type {
  scalar_expr x{0};
  scalar_expr y{0};
  ta::static_matrix<2, 1> v{0, 0};
};

template <>
struct custom_type_registrant<test_type> {
  auto operator()(custom_type_registry& registry) {
    return custom_type_builder<test_type>(registry, "test_type")
        .add_field("x", &test_type::x)
        .add_field("y", &test_type::y)
        .add_field("v", &test_type::v);
  }
};

struct test_func_1
    : declare_external_function<test_func_1, test_type, type_list<scalar_expr, scalar_expr>> {
  static constexpr std::string_view name() noexcept { return "test_func_1"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0", "arg1"); }
};

struct test_func_2 : declare_external_function<test_func_2, scalar_expr, type_list<test_type>> {
  static constexpr std::string_view name() noexcept { return "test_func_2"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0"); }
};

struct test_func_3
    : declare_external_function<test_func_3, scalar_expr, type_list<ta::static_matrix<2, 1>>> {
  static constexpr std::string_view name() noexcept { return "test_func_3"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0"); }
};

TEST(CompoundExpressionTest, TestElementSimplifyIndirection) {
  const annotated_custom_type<test_type> type = create_type_description<test_type>();
  const auto [x, y, a, b] = make_symbols("x", "y", "a", "b");

  // Make a `custom_type_construction` expression:
  const compound_expr construct = create_custom_type_construction(type.inner(), {x, y, a, b});

  // Crete elements from the compound expression:
  const std::vector<scalar_expr> elements =
      create_expression_elements(construct, type.inner().total_size());
  ASSERT_EQ(4, elements.size());

  // Should just forward the underlying scalar expressions:
  ASSERT_IDENTICAL(x, elements[0]);
  ASSERT_IDENTICAL(y, elements[1]);
  ASSERT_IDENTICAL(a, elements[2]);
  ASSERT_IDENTICAL(b, elements[3]);
}

TEST(CompoundExpressionTest, TestConstructSimplifyIndirection) {
  const annotated_custom_type<test_type> type = create_type_description<test_type>();
  const auto [x, y, a, b] = make_symbols("x", "y", "a", "b");

  // Create a custom type argument:
  const compound_expr arg = create_custom_type_argument(type.inner(), 3);
  const std::vector<scalar_expr> arg_elements =
      create_expression_elements(arg, type.inner().total_size());
  ASSERT_EQ(4, arg_elements.size());

  // Constructing a new compound expression should just point back to the original arg expression:
  const compound_expr construct = create_custom_type_construction(type.inner(), arg_elements);
  ASSERT_TRUE(construct.has_same_address(arg));
  ASSERT_IDENTICAL(arg, construct);
}

TEST(CompoundExpressionTest, TestInvokeSimplifyIndirection) {
  const annotated_custom_type<test_type> type = create_type_description<test_type>();
  const auto [external_func, _] = test_func_1::get_description_and_arg_types();
  const auto [a, b] = make_symbols("a", "b");

  // Create a function invocation via `call()`
  const test_type invocation_result = test_func_1::call(a, b);
  const compound_expr expected_invocation =
      std::get<compound_expr>(external_func.create_invocation({a, b}));

  // Check if we got the expected elements in our `test_type` object:
  const std::vector<scalar_expr> expected_invocation_elements =
      create_expression_elements(expected_invocation, type.inner().total_size());
  ASSERT_IDENTICAL(expected_invocation_elements[0], invocation_result.x);
  ASSERT_IDENTICAL(expected_invocation_elements[1], invocation_result.y);
  ASSERT_IDENTICAL(expected_invocation_elements[2], invocation_result.v[0]);
  ASSERT_IDENTICAL(expected_invocation_elements[3], invocation_result.v[1]);

  // Check that it simplifies if we construct from these elements:
  const compound_expr construct =
      create_custom_type_construction(type.inner(), expected_invocation_elements);
  ASSERT_IDENTICAL(expected_invocation, construct);
}

TEST(CompoundExpressionTest, TestSubstitute) {
  const auto [a, b, c, d] = make_symbols("a", "b", "c", "d");

  const test_type f1 = test_func_1::call(a * b / 3, cos(c) - d * 2);
  const scalar_expr f2 = test_func_2::call(f1);

  // Try substituting through f2:
  ASSERT_IDENTICAL(test_func_2::call(test_func_1::call(sin(d) / 3, cos(c) - d * 2)),
                   f2.subs(a * b, sin(d)));
  ASSERT_IDENTICAL(test_func_2::call(test_func_1::call(a * b / 3, 0)), f2.subs(cos(c), d * 2));

  // Add another layer of indirection (through custom_type_construction):
  const ta::static_matrix<2, 1> v{0, d / 22};
  const test_type f3{f2, a * pow(b, 2) - sin(d), v};
  const scalar_expr f4 = test_func_2::call(f3);

  ASSERT_IDENTICAL(test_func_2::call(test_type{f2.subs(a * b, 3), b * 3 - sin(d), v}),
                   f4.subs(a * b, 3));

  // Substitute variables:
  ASSERT_IDENTICAL(test_func_2::call(test_func_1::call(pow(b, 2) / 3, -cos(c))),
                   substitute(f2, {std::make_tuple(a, b), std::make_tuple(d, cos(c))}));

  // Check that we can replace the output of an external function call:
  ASSERT_IDENTICAL(cos(a), substitute(cos(f4), {std::make_tuple(f4, a)}));
}

TEST(CompoundExpressionTest, TestDerivative) {
  const auto [a, b, c, d] = make_symbols("a", "b", "c", "d");

  const test_type f1 = test_func_1::call(pow(a, 2) * pow(b, 3), sin(c * d));
  const scalar_expr f2 = test_func_2::call(f1);

  ASSERT_IDENTICAL(0, f2.diff(a));  //  Can't differentiate through external calls.
  ASSERT_IDENTICAL(derivative::create(f2, a, 1),
                   f2.diff(a, 1, non_differentiable_behavior::abstract));
  ASSERT_IDENTICAL(derivative::create(f2, b, 2),
                   f2.diff(b, 2, non_differentiable_behavior::abstract));

  // But we can take derivatives wrt outputs:
  ASSERT_IDENTICAL(-5 * sin(f2 * 5), cos(f2 * 5).diff(f2));
  ASSERT_IDENTICAL(2 * f2 * cos(f2 * f2), sin(f2 * f2).diff(f2));
}

TEST(CompoundExpressionTest, TestEvaluate) {
  const auto [a, b] = make_symbols("a", "b");

  const test_type f1 = test_func_1::call(a / 3, b * cos(b));
  const scalar_expr f2 = test_func_2::call(f1);

  // Test that we can substitute and evaluate the args of external calls:
  ASSERT_IDENTICAL(test_func_2::call(test_func_1::call(M_E / 3.0, (M_PI / 4.0) / std::sqrt(2.0))),
                   f2.subs(a, constants::euler).subs(b, constants::pi / 4).eval());

  const scalar_expr f3 = test_func_3::call(make_matrix(2, 1, a, b));
  ASSERT_IDENTICAL(test_func_3::call(make_matrix(2, 1, M_E, M_PI)),
                   f3.subs(a, constants::euler).subs(b, constants::pi).eval());
}

TEST(CompoundExpressionTest, TestCollectAndDistribute) {
  const auto [a, b] = make_symbols("a", "b");

  const test_type f1 =
      test_func_1::call(a * (b + 1) + a * (pow(b, 2) - b), cos(a * b - a * log(b)));
  const scalar_expr f2 = test_func_2::call(f1);

  ASSERT_IDENTICAL(test_func_2::call(test_func_1::call(a * (b * b + 1), cos(a * (b - log(b))))),
                   f2.collect(a));

  const scalar_expr f3 = test_func_3::call(
      make_matrix(2, 1, a * b + a * log(b) - a, pow(a, 2) * cos(b) + pow(a, 2) * b));
  ASSERT_IDENTICAL(test_func_3::call(make_matrix(2, 1, a * (b + log(b) - 1), a * a * (cos(b) + b))),
                   f3.collect(a));
  ASSERT_IDENTICAL(f3, f3.collect(a).distribute());
}

TEST(CompoundExpressionTest, TestCse) {
  const auto [x, y] = make_symbols("x", "y");

  // Test that we can CSE scalar-valued expressions through a compound expression.
  const test_type f1 = test_func_1::call(abs(x * y), cos(x * y) * 5);
  const scalar_expr f2 = test_func_2::call(f1) + log(test_func_2::call(f1) / 2);

  const auto [f2_cse, replacements] = eliminate_subexpressions(f2, nullptr, 2);

  const scalar_expr v0{"v0"};
  ASSERT_EQ(2, replacements.size());
  ASSERT_IDENTICAL(x * y, std::get<1>(replacements[0]));
  ASSERT_IDENTICAL(test_func_2::call(test_func_1::call(abs(v0), cos(v0) * 5)),
                   std::get<1>(replacements[1]));
}

// TODO: Arguably belongs in plain_formatter_test but it is easier to define here because we have
//  the test_type and test_functions.
TEST(CompoundExpressionTest, TestFormatting) {
  const auto [a, b, c, d] = make_symbols("a", "b", "c", "d");

  const test_type f1 = test_func_1::call(a * b / 3, cos(c) - d * 2);
  const scalar_expr f2 = test_func_2::call(f1);
  EXPECT_EQ("test_func_1(a*b/3, -2*d + cos(c)).y", f1.y.to_string());
  EXPECT_EQ("test_func_1(a*b/3, -2*d + cos(c)).v[1, 0]", f1.v[1].to_string());
  EXPECT_EQ("test_func_2(test_func_1(a*b/3, -2*d + cos(c)))", f2.to_string());

  // Construct a custom type:
  const test_type f3{a * b, b - c * cos(d), make_vector(a, 2 * b)};
  EXPECT_EQ("test_func_2(test_type(<4 expressions>))", test_func_2::call(f3).to_string());

  // Create a custom type argument:
  const annotated_custom_type<test_type> type = create_type_description<test_type>();
  const compound_expr arg = create_custom_type_argument(type.inner(), 3);
  EXPECT_EQ("$arg(3)", arg.to_string());

  // Access a member on it:
  EXPECT_EQ("$arg(3).y", compound_expression_element::create(arg, 1).to_string());
  EXPECT_EQ("$arg(3).v[0, 0]", compound_expression_element::create(arg, 2).to_string());
  EXPECT_EQ("$arg(3).v[1, 0]", compound_expression_element::create(arg, 3).to_string());
}

}  // namespace wf
