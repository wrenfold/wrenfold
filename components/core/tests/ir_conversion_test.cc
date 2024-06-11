// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.

#include "wf/code_generation/control_flow_graph.h"
#include "wf/code_generation/declare_external_function.h"
#include "wf/code_generation/expr_from_ir.h"
#include "wf/code_generation/function_evaluator.h"
#include "wf/constants.h"
#include "wf/functions.h"
#include "wf/geometry/quaternion.h"
#include "wf/type_annotations.h"

#include "wf_test_support/test_macros.h"

#include <bitset>

namespace wf {
using namespace wf::custom_literals;
namespace ta = type_annotations;

std::ostream& operator<<(std::ostream& s, const control_flow_graph& cfg) {
  s << cfg.to_string();
  return s;
}

template <typename Func, typename... Args>
auto create_ir_with_params(Func&& func, optimization_params params, const std::string_view name,
                           Args&&... args) {
  const function_description description =
      build_function_description(std::forward<Func>(func), name, std::forward<Args>(args)...);
  control_flow_graph flat_cfg{description.output_expressions(), params};
  flat_cfg.assert_invariants();
  return std::make_tuple(description.output_expressions(), std::move(flat_cfg));
}

// Given a lambda or function pointer, create the IR for the expressions it generates.
template <typename Func, typename... Args>
auto create_ir(Func&& func, const std::string_view name, Args&&... args) {
  return create_ir_with_params(std::forward<Func>(func), optimization_params{0, true}, name,
                               std::forward<Args>(args)...);
}

// Generate permutations that represent whether optional arguments are present or not.
class optional_arg_permutations {
 public:
  static constexpr std::size_t MaxOptionalArgs = 32;

  // Construct w/ vector of expressions.
  explicit optional_arg_permutations(const std::vector<expression_group>& expressions) {
    for (auto it = expressions.begin(); it != expressions.end(); ++it) {
      if (it->key.usage == expression_usage::optional_output_argument) {
        scatter_.emplace(it->key.name, scatter_.size());
      }
    }
    WF_ASSERT_LT(scatter_.size(), MaxOptionalArgs);
  }

  std::size_t num_permutations() const {
    // We encode the permutations in bitsets. So if you have three optional args, there are
    // 8 possible permutations: 000, 001, 010, 011, 100, 101, 110, 111
    const std::size_t num_optional_args = scatter_.size();
    return static_cast<std::size_t>(1) << num_optional_args;
  }

  // Get the n'th permutation of optional arguments.
  // The returned map is a mapping from `arg name` --> whether the argument is to be computed.
  std::unordered_map<std::string, bool> get_permutation(std::size_t n) const {
    WF_ASSERT_LT(n, num_permutations());
    const std::bitset<MaxOptionalArgs> permutation{n};

    std::unordered_map<std::string, bool> output{};
    output.reserve(scatter_.size());
    for (const auto& [name, bitfield_index] : scatter_) {
      output.emplace(name, permutation[bitfield_index]);
    }
    return output;
  }

 private:
  // Map from arg name to bit index in a permutation.
  std::unordered_map<std::string, std::size_t> scatter_;
};

template <typename T>
void check_output_expressions(const std::vector<expression_group>& expected_expressions,
                              const std::unordered_map<output_key, std::vector<scalar_expr>,
                                                       hash_struct<output_key>>& output_expressions,
                              const T& ir, const bool distribute_outputs) {
  for (const expression_group& group : expected_expressions) {
    auto it = output_expressions.find(group.key);
    ASSERT_TRUE(it != output_expressions.end())
        << fmt::format("Missing key ({}, {})\n", string_from_expression_usage(group.key.usage),
                       group.key.name)
        << ir;

    ASSERT_EQ(group.expressions.size(), it->second.size()) << ir;

    for (std::size_t i = 0; i < group.expressions.size(); ++i) {
      ASSERT_IDENTICAL(
          distribute_outputs ? group.expressions[i].distribute() : group.expressions[i],
          distribute_outputs ? it->second[i].distribute() : it->second[i])
          << fmt::format("Key: ({}, {}), i: {}\n", string_from_expression_usage(group.key.usage),
                         group.key.name, i)
          << ir;
    }
  }
}

void check_expressions(const std::vector<expression_group>& expected_expressions,
                       const control_flow_graph& ir, const bool distribute_outputs = false) {
  const auto [output_expressions, _] = rebuild_expression_tree(ir.first_block(), {});
  check_output_expressions(expected_expressions, output_expressions, ir, distribute_outputs);
}

void check_expressions_with_output_permutations(
    const std::vector<expression_group>& expected_expressions, const control_flow_graph& ir) {
  const optional_arg_permutations permutations{expected_expressions};
  for (std::size_t i = 0; i < permutations.num_permutations(); ++i) {
    // test permutation `i`:
    auto output_map = permutations.get_permutation(i);
    auto [output_expressions, _] = rebuild_expression_tree(ir.first_block(), std::move(output_map));
    check_output_expressions(expected_expressions, output_expressions, ir, false);
  }
}

// ReSharper disable CppPassValueParameterByConstReference

TEST(IrTest, TestNumericConstant1) {
  auto [expected_expressions, ir] = create_ir([]() { return 6_s; }, "func");
  ASSERT_EQ(1, ir.count_operation<ir::load>()) << ir;
  check_expressions(expected_expressions, ir);
  check_expressions_with_output_permutations(expected_expressions,
                                             std::move(ir).convert_conditionals_to_control_flow());
}

TEST(IrTest, TestNumericConstant2) {
  auto [expected_expressions, ir] = create_ir(
      []() {
        return std::make_tuple(return_value(2.0_s), output_arg("a", 7_s), output_arg("b", 1_s));
      },
      "func");
  ASSERT_EQ(3, ir.count_operation<ir::load>()) << ir;
  check_expressions(expected_expressions, ir);
  check_expressions_with_output_permutations(expected_expressions,
                                             std::move(ir).convert_conditionals_to_control_flow());
}

TEST(IrTest, TestNoDuplicatedCasts) {
  // Create flat IR but don't eliminate duplicates. Double check that only two casts
  // are inserted (one for 0, and one for 1).
  const auto tuple = build_function_description(
      []() -> ta::static_matrix<4, 4> { return make_identity(4); }, "func");
  const control_flow_graph ir{tuple.output_expressions()};
  ASSERT_EQ(2, ir.count_operation<ir::cast>()) << ir;
  ASSERT_EQ(2, ir.count_operation<ir::load>()) << ir;
}

TEST(IrTest, TestScalarExpressions1) {
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y) {
        scalar_expr a = x * y;
        scalar_expr b = x + y;
        return std::make_tuple(return_value(x * y), output_arg("a", a), output_arg("b", b));
      },
      "func", arg("x"), arg("y"));
  ASSERT_EQ(1, ir.count_multiplications()) << ir;
  ASSERT_EQ(1, ir.count_additions()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  ASSERT_EQ(1, output_ir.count_multiplications()) << output_ir;
  ASSERT_EQ(1, output_ir.count_additions()) << output_ir;
  ASSERT_EQ(1, output_ir.num_blocks()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestScalarExpressions2) {
  // Create an optional output:
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y, scalar_expr z) {
        scalar_expr f = x * y * sin(z * x) + 5;
        scalar_expr g = cos(z * x) * x * y - log(y - z * 2.1) * 3;
        return std::make_tuple(output_arg("f", f), optional_output_arg("g", g));
      },
      "func", arg("x"), arg("y"), arg("z"));

  EXPECT_EQ(6, ir.count_multiplications()) << ir;
  EXPECT_EQ(3, ir.count_additions()) << ir;
  EXPECT_EQ(1, ir.count_operation<ir::neg>()) << ir;
  EXPECT_EQ(1, ir.count_function(std_math_function::sin)) << ir;
  EXPECT_EQ(1, ir.count_function(std_math_function::cos)) << ir;
  EXPECT_EQ(1, ir.count_function(std_math_function::log)) << ir;
  EXPECT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  ASSERT_EQ(16, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(1, output_ir.num_conditionals()) << output_ir;
  ASSERT_EQ(3, output_ir.num_blocks()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestScalarExpressions3) {
  // y * z should get combined in the output, since it appears more:
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y, scalar_expr z) {
        scalar_expr f = (x * y * z) + (y * z) - sin(y * z) * (y * z) + log(x * z) + cos(x * y);
        return f;
      },
      "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(5, ir.count_multiplications()) << ir;
  ASSERT_EQ(4, ir.count_additions()) << ir;
  ASSERT_EQ(1, ir.count_operation<ir::neg>()) << ir;
  ASSERT_EQ(1, ir.count_function(std_math_function::sin)) << ir;
  ASSERT_EQ(1, ir.count_function(std_math_function::cos)) << ir;
  ASSERT_EQ(1, ir.count_function(std_math_function::log)) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);
  check_expressions_with_output_permutations(expected_expressions,
                                             std::move(ir).convert_conditionals_to_control_flow());
}

TEST(IrTest, TestScalarExpressions4) {
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y, scalar_expr z) {
        // Chain together a few operations (this doesn't reduce at all).
        scalar_expr f = constants::one;
        for (int i = 0; i < 20; ++i) {
          if (i & 1) {
            f = (f + x) * y;
          } else {
            f = (f - y) * z;
          }
        }
        return f;
      },
      "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(42, ir.num_operations()) << ir;
  ASSERT_EQ(1, ir.count_operation<ir::neg>()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);
  check_expressions_with_output_permutations(expected_expressions,
                                             std::move(ir).convert_conditionals_to_control_flow());
}

// Test that powers can be converted into multiplications.
TEST(IrTest, TestPowerConversion1) {
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x) {
        return 0.13 * pow(x, 2) + 1.2 * pow(x, 3) - 5.0 * pow(x, 4) + 0.9 * pow(x, 5) -
               7 * pow(x, 6);
      },
      "func", arg("x"));

  ASSERT_EQ(11, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  ASSERT_EQ(10, ir.count_multiplications()) << ir;
  ASSERT_EQ(0, ir.count_function(std_math_function::powf)) << ir;
  ASSERT_EQ(0, ir.count_function(std_math_function::powi)) << ir;
  check_expressions(expected_expressions, ir);
  check_expressions_with_output_permutations(expected_expressions,
                                             std::move(ir).convert_conditionals_to_control_flow());
}

// Test that powers of square roots are convert into sqrt operations.
TEST(IrTest, TestPowerConversion2) {
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y) {
        const scalar_expr c = x * x + y * y;
        return pow(c, 1_s / 2) + 1 / pow(c, 5_s / 2) + pow(c, 3_s / 2);
      },
      "func", arg("x"), arg("y"));

  ASSERT_EQ(11, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  ASSERT_EQ(0, ir.count_function(std_math_function::powf));
  ASSERT_EQ(0, ir.count_function(std_math_function::powi));
  ASSERT_EQ(6, ir.count_multiplications()) << ir;
  ASSERT_EQ(1, ir.count_operation<ir::div>()) << ir;
  ASSERT_EQ(1, ir.count_function(std_math_function::sqrt)) << ir;

  check_expressions(expected_expressions, ir);
  check_expressions_with_output_permutations(expected_expressions,
                                             std::move(ir).convert_conditionals_to_control_flow());
}

TEST(IrTest, TestPowerConversion3) {
  auto [expected_expressions, ir] =
      create_ir([](scalar_expr x, scalar_expr y) { return pow(x, 8) + pow(y, 9); }, "func",
                arg("x"), arg("y"));

  EXPECT_EQ(8, ir.num_operations());
  EXPECT_EQ(7, ir.count_multiplications());
  check_expressions(expected_expressions, ir);
}

TEST(IrTest, TestFactorization1) {
  const auto func = [](scalar_expr x, scalar_expr y, scalar_expr z, scalar_expr w) {
    return x * y * z * w - 3 * x * y + cos(w) * x * y - 14 + z * w;
  };
  auto [expected_expressions, ir] = create_ir_with_params(
      +func, optimization_params{0, true}, "func", arg("x"), arg("y"), arg("z"), arg("w"));

  ASSERT_EQ(10, ir.num_operations()) << ir;
  check_expressions(expected_expressions, ir, false);

  // Recompute with factorization enabled:
  auto [_, ir_factorized] = create_ir_with_params(+func, optimization_params{1, true}, "func",
                                                  arg("x"), arg("y"), arg("z"), arg("w"));

  ASSERT_EQ(8, ir_factorized.num_operations()) << ir_factorized;
  check_expressions(expected_expressions, ir_factorized, true);
}

TEST(IrTest, TestFactorization2) {
  // Test that factorization works with powers of the same variable.
  // We construct an expression that contains `x` and `z` elevated to powers greater than one.
  const auto func = [](scalar_expr x, scalar_expr y, scalar_expr z) {
    return (x * (x + y * x - z * (x + z * z))).distribute();
  };

  auto [expected_expressions, ir] = create_ir_with_params(+func, optimization_params{0, true},
                                                          "func", arg("x"), arg("y"), arg("z"));

  EXPECT_EQ(7, ir.num_operations()) << ir;
  check_expressions(expected_expressions, ir, false);

  auto [_, ir_factorized] = create_ir_with_params(+func, optimization_params{2, true}, "func",
                                                  arg("x"), arg("y"), arg("z"));

  EXPECT_EQ(8, ir_factorized.num_operations()) << ir_factorized;
  check_expressions(expected_expressions, ir_factorized, true);
}

TEST(IrTest, TestFactorization3) {
  const auto func = [](const ta::static_matrix<15, 1> v) {
    return v[5] * v[6] * v[7] * v[8] * v[0] * v[13] * v[3] +
           v[5] * v[7] * v[8] * v[9] * v[0] * v[14] * v[3] +
           v[5] * v[7] * v[8] * v[0] * v[14] * v[2] * v[3] +
           v[4] * v[5] * v[6] * v[1] * v[14] * v[3] + v[4] * v[5] * v[9] * v[1] * v[14] * v[3] +
           v[4] * v[5] * v[1] * v[14] * v[2] * v[3] + v[5] * v[8] * v[11] * v[12] * v[3] +
           v[5] * v[8] * v[11] * v[13] * v[3] + v[5] * v[8] * v[10] * v[11] * v[3];
  };
  auto [expected_expressions, ir] = create_ir(+func, "func", arg("v"));

  EXPECT_EQ(20, ir.num_operations()) << ir;
  check_expressions(expected_expressions, ir, false);

  auto [_, ir_factorized_one_pass] =
      create_ir_with_params(+func, optimization_params{1, true}, "func", arg("v"));

  EXPECT_EQ(12, ir_factorized_one_pass.num_operations()) << ir_factorized_one_pass;
  check_expressions(expected_expressions, ir_factorized_one_pass, true);

  // With two passes:
  auto [__, ir_factorized_two_pass] =
      create_ir_with_params(+func, optimization_params{2, true}, "func", arg("v"));

  EXPECT_EQ(11, ir_factorized_two_pass.num_operations()) << ir_factorized_two_pass;
  check_expressions(expected_expressions, ir_factorized_two_pass, true);
}

TEST(IrTest, TestFactorization4) {
  const auto func = [](scalar_expr a, scalar_expr b, scalar_expr c, scalar_expr d) {
    return (a * (b + c * (d + 1)) + a * (-d - 1)).distribute();
  };
  auto [expected_expressions, ir] =
      create_ir(+func, "func", arg("a"), arg("b"), arg("c"), arg("d"));

  EXPECT_EQ(7, ir.num_operations()) << ir;
  check_expressions(expected_expressions, ir, false);

  auto [_, ir_factorized_one_pass] = create_ir_with_params(
      +func, optimization_params{1, true}, "func", arg("a"), arg("b"), arg("c"), arg("d"));

  EXPECT_EQ(5, ir_factorized_one_pass.num_operations()) << ir_factorized_one_pass;
  check_expressions(expected_expressions, ir_factorized_one_pass, true);

  auto [__, ir_factorized_two_pass] = create_ir_with_params(
      +func, optimization_params{2, true}, "func", arg("a"), arg("b"), arg("c"), arg("d"));

  EXPECT_EQ(5, ir_factorized_two_pass.num_operations()) << ir_factorized_two_pass;
  check_expressions(expected_expressions, ir_factorized_two_pass, true);
}

TEST(IrTest, TestFactorization5) {
  const auto func = [](scalar_expr x, scalar_expr y) {
    return pow(x, 4) + pow(x, 8) + pow(x, 2) + x + -2 * pow(y, 3) + constants::pi * pow(y, 2) +
           y * 0.1;
  };
  auto [expected_expressions, ir] = create_ir(+func, "func", arg("x"), arg("y"));

  EXPECT_EQ(8, ir.count_multiplications()) << ir;
  check_expressions(expected_expressions, ir, false);

  for (const auto& [passes, num_ops] : {std::make_tuple(1, 7), std::make_tuple(2, 8)}) {
    auto [_, ir_factorized] =
        create_ir_with_params(+func, optimization_params{static_cast<std::size_t>(passes), true},
                              "func", arg("x"), arg("y"));
    EXPECT_EQ(num_ops, ir_factorized.count_multiplications()) << ir_factorized;
    check_expressions(expected_expressions, ir, true);
  }
}

TEST(IrTest, TestFactorization6) {
  const auto func = [](ta::static_matrix<16, 1> v) {
    const scalar_expr output =
        -1 + v[6] + v[10] + v[14] + v[15] +
        v[3] * pow(v[9], 2) *
            (-4 + v[0] + v[2] + v[5] + v[9] +
             v[3] * v[7] * v[9] *
                 (3 + v[2] + v[4] + v[7] + v[12] +
                  v[1] * v[6] * v[8] *
                      (3 + v[2] + v[8] + v[9] + v[15] +
                       v[1] * v[10] * v[11] *
                           (2 + v[7] + v[8] + 2 * v[12] +
                            v[7] * v[12] * v[15] *
                                (4 + v[2] + v[12] + v[13] + v[15] +
                                 v[0] * v[5] * v[13] *
                                     (2 + 2 * v[1] + v[5] + v[13] +
                                      v[6] * v[9] * v[14] *
                                          (v[7] + v[8] + 2 * v[12] +
                                           v[4] * v[6] * v[15] *
                                               (-5 + v[4] + v[6] + v[7] + v[14] +
                                                v[6] * v[9] * v[10] *
                                                    (4 + v[8] + v[9] + 2 * v[13] +
                                                     v[8] * v[9] * v[11])))))))));
    return output.distribute();
  };
  auto [expected_expressions, ir] = create_ir(+func, "func", arg("v"));

  EXPECT_EQ(161, ir.count_multiplications()) << ir;
  check_expressions(expected_expressions, ir, false);

  for (const auto& [passes, num_ops] :
       {std::make_tuple(1, 100), std::make_tuple(2, 69), std::make_tuple(3, 57),
        std::make_tuple(4, 46), std::make_tuple(5, 38)}) {
    auto [_, ir_factorized] = create_ir_with_params(
        +func, optimization_params{static_cast<std::size_t>(passes), true}, "func", arg("v"));

    EXPECT_EQ(num_ops, ir_factorized.count_multiplications()) << "passes: " << passes;
    check_expressions(expected_expressions, ir_factorized, true);
  }
}

TEST(IrTest, TestConditionals1) {
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x) {
        // heaviside step function:
        return where(x > 0, 1, 0);
      },
      "func", arg("x"));

  ASSERT_EQ(4, ir.num_operations()) << ir;
  ASSERT_EQ(1, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  ASSERT_EQ(4, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(1, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals2) {
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y) {
        // use the condition in one of the branches:
        const boolean_expr condition = x > y;
        return where(condition, iverson(condition) * 2, cos(y - 2));
      },
      "func", arg("x"), arg("y"));

  ASSERT_EQ(8, ir.num_operations()) << ir;
  ASSERT_EQ(1, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  ASSERT_EQ(8, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(1, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);

  const operation_counts counts = output_ir.compute_operation_counts();
  ASSERT_EQ(1, counts[operation_count_label::add]);
  ASSERT_EQ(1, counts[operation_count_label::branch]);
  ASSERT_EQ(1, counts[operation_count_label::call]);
  ASSERT_EQ(1, counts[operation_count_label::compare]);
}

TEST(IrTest, TestConditionals3) {
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y) {
        // exclusive or
        return where(x > 0, where(y > 0, 0, 1), where(y > 0, 1, 0));
      },
      "func", arg("x"), arg("y"));

  ASSERT_EQ(7, ir.num_operations()) << ir;
  ASSERT_EQ(3, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  ASSERT_EQ(7, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(3, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals4) {
  // Nested conditionals:
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y, scalar_expr z) {
        scalar_expr p = where(x > 0, cos(x), sin(z));
        scalar_expr q = where(y < -5, -log(p), tan(x + y));
        scalar_expr l = where(pow(z, 2) < y, q, q - p);
        return l * 2;
      },
      "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(19, ir.num_operations()) << ir;
  ASSERT_EQ(3, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  ASSERT_EQ(19, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(3, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals5) {
  // Optional outputs and conditionals:
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y, scalar_expr z) {
        scalar_expr p = where(x > y, cos(x) * sin(z) - sin(x) * cos(z) * 2, log(z - x) * 23);
        scalar_expr q = where(pow(z, y) < x, p * p, -cos(p) + x);
        scalar_expr f = q;
        scalar_expr g = q.diff(x);
        scalar_expr h = q.diff(x, 2);
        return std::make_tuple(return_value(f), optional_output_arg("g", g),
                               optional_output_arg("h", h));
      },
      "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(55, ir.num_operations()) << ir;
  ASSERT_EQ(6, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  ASSERT_EQ(57, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(7, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals6) {
  // Create nested conditionals several layers deep:
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y, scalar_expr z, scalar_expr w) {
        scalar_expr f = where(x > 0, x, 1 - x);
        f = where(y > 0, f * y, f * (1 - y));
        f = where(z > 0, f * z, f * (1 - z));
        return where(w > 0, f * w, f * (1 - w));
      },
      "func", arg("x"), arg("y"), arg("z"), arg("w"));

  ASSERT_EQ(24, ir.num_operations()) << ir;
  ASSERT_EQ(4, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  ASSERT_EQ(24, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(4, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals7) {
  // Nested conditionals with identical conditions:
  auto [expected_expressions, ir] =
      create_ir([](scalar_expr x, scalar_expr y,
                   scalar_expr z) { return where(x > 0, where(x > 0, y, z), 10 * z - y); },
                "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(8, ir.num_operations()) << ir;
  ASSERT_EQ(2, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  ASSERT_EQ(8, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(2, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals8) {
  // Nested conditionals (values of inner conditionals used for outer ones).
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y) {
        scalar_expr c0 = where(y > 0, cos(x * y), cos(x) + 2);
        scalar_expr c1 = where(x > 0, log(abs(y)), atan2(y, x) * 3);
        scalar_expr c2 = where(x - y > 0, c0 * 3 - sqrt(abs(c0)), pow(abs(c1), 1.0 / 3.0) / 5);
        return c2;
      },
      "func", arg("x"), arg("y"));

  ASSERT_EQ(27, ir.num_operations()) << ir;
  ASSERT_EQ(3, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  ASSERT_EQ(27, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(3, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals9) {
  auto [expected_expressions, ir] = create_ir(
      [](ta::static_matrix<2, 1> x, ta::static_matrix<2, 1> y, scalar_expr z) {
        scalar_expr m =
            where(z > x[1], (x * y.transposed() + make_identity(2) * cos(z / x[0])).squared_norm(),
                  z * 5);
        ta::static_matrix<1, 2> diff_x = make_vector(m).jacobian(x);
        ta::static_matrix<1, 2> diff_y = make_vector(m).jacobian(y);
        scalar_expr diff_z = m.diff(z);
        return std::make_tuple(return_value(m), output_arg("diff_x", diff_x),
                               output_arg("diff_y", diff_y), optional_output_arg("diff_z", diff_z));
      },
      "func", arg("x"), arg("y"), arg("z"));

  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestMatrixExpressions1) {
  // Create a matrix output:
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, const ta::static_matrix<2, 1>& v) {
        ta::static_matrix<2, 2> m = v * v.transposed() * x;
        return m;
      },
      "func", arg("x"), arg("y"));

  ASSERT_EQ(5, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  ASSERT_EQ(5, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(0, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestMatrixExpressions2) {
  // Construct a matrix w/ a repeated conditional:
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y, scalar_expr z) {
        std::vector<scalar_expr> expressions{};
        for (int i = 0; i < 16; ++i) {
          expressions.push_back(where(x > 0, pow(y, i), pow(z, 16 - z)));
        }
        return ta::static_matrix<4, 4>{matrix_expr::create(4, 4, std::move(expressions))};
      },
      "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(37, ir.num_operations()) << ir;
  ASSERT_EQ(16, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  // Conditionals should get reduced:
  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  ASSERT_EQ(37, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(1, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestMatrixExpressions3) {
  // Create matrices with conditionals and optional output arguments:
  auto [expected_expressions, ir] = create_ir(
      [](const ta::static_matrix<2, 1>& v, const ta::static_matrix<3, 3>& u,
         const ta::static_matrix<3, 1>& t) {
        const auto I3 = make_identity(3);
        const auto zeros = make_zeros(2, 3);
        ta::static_matrix<2, 3> f = where(v[0] - t[1] > 0, v * t.transposed() * (u - I3), zeros);

        const auto path_1 = u * t * t.transposed();
        const auto path_2 = (u - I3) * (u - I3).transposed();
        ta::static_matrix<3, 3> g{where(u(1, 1) < -v[1], path_1, path_2)};
        return std::make_tuple(return_value(f), optional_output_arg("g", g));
      },
      "func", arg("v"), arg("u"), arg("t"));

  EXPECT_EQ(100, ir.num_operations()) << ir;
  EXPECT_EQ(15, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  EXPECT_EQ(101, output_ir.num_operations()) << output_ir;
  EXPECT_EQ(3, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestCreateRotationMatrix) {
  const auto func = [](ta::static_matrix<3, 1> w) {
    matrix_expr R = quaternion::from_rotation_vector(w.inner(), 1.0e-16).to_rotation_matrix();
    matrix_expr R_diff = vectorize_matrix(R).jacobian(w);
    return std::make_tuple(output_arg("R", ta::static_matrix<3, 3>{R}),
                           optional_output_arg("R_D_w", ta::static_matrix<9, 3>{R_diff}));
  };
  auto [expected_expressions, ir] = create_ir(+func, "func", arg("w"));

  EXPECT_EQ(221, ir.num_operations()) << ir;
  EXPECT_EQ(134, ir.count_multiplications()) << ir;
  EXPECT_EQ(82, ir.count_additions()) << ir;
  EXPECT_EQ(13, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  EXPECT_EQ(222, output_ir.num_operations()) << output_ir;
  EXPECT_EQ(3, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);

  // Test it using factorization:
  auto [_, ir_factorized] =
      create_ir_with_params(+func, optimization_params{3, true}, "func", arg("w"));

  EXPECT_EQ(130, ir_factorized.count_multiplications()) << ir_factorized;
  check_expressions(expected_expressions, ir_factorized, true);
}

TEST(IrTest, TestLocalCoordinates) {
  const auto func = [](ta::static_matrix<4, 1> a, ta::static_matrix<4, 1> b) {
    const quaternion q_a = quaternion::from_vector_xyzw(a);
    const quaternion q_b = quaternion::from_vector_xyzw(b);
    const matrix_expr w = (q_a.conjugate() * q_b).to_rotation_vector(std::nullopt, false);
    const matrix_expr w_D_a = w.jacobian(q_a.to_vector_wxyz()) * q_a.right_retract_derivative();
    const matrix_expr w_D_b = w.jacobian(q_b.to_vector_wxyz()) * q_b.right_retract_derivative();
    return std::make_tuple(output_arg("w", ta::static_matrix<3, 1>{w}),
                           optional_output_arg("d0", ta::static_matrix<3, 3>{w_D_a}),
                           optional_output_arg("d1", ta::static_matrix<3, 3>{w_D_b}));
  };
  auto [expected_expressions, ir] = create_ir(+func, "func", arg("a"), arg("b"));

  EXPECT_EQ(266, ir.num_operations()) << ir;
  EXPECT_EQ(191, ir.count_multiplications()) << ir;
  EXPECT_EQ(115, ir.count_additions()) << ir;
  EXPECT_EQ(10, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  // Test it using factorization:
  auto [_, ir_factorized] =
      create_ir_with_params(+func, optimization_params{2, true}, "func", arg("a"), arg("b"));
  EXPECT_EQ(305, ir_factorized.num_operations()) << ir_factorized;
  EXPECT_EQ(193, ir_factorized.count_multiplications()) << ir_factorized;
  EXPECT_EQ(92, ir_factorized.count_additions()) << ir_factorized;

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  EXPECT_EQ(268, output_ir.num_operations()) << output_ir;
  EXPECT_EQ(6, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

TEST(IrTest, TestBuiltInFunctions) {
  // create expressions that use all the built-in functions
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y, scalar_expr z) {
        scalar_expr g = acos(x - log(y) * pow(x, 1.231) + pow(z, 22));
        scalar_expr h = asin(2.0 * tan(y) - abs(z) + atan(x));
        scalar_expr f = cosh(x) * sinh(y) / tanh(z) + acosh(y * x) - asinh(2 * y) + atanh(-x);
        return atan2(abs(g) + cos(y) + signum(h) * floor(h), -sin(y) + sqrt(z) - signum(x) + f);
      },
      "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(43, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  ASSERT_EQ(6, ir.count_operation<ir::neg>());
  EXPECT_EQ(1, ir.count_function(std_math_function::cos));
  EXPECT_EQ(1, ir.count_function(std_math_function::sin));
  EXPECT_EQ(1, ir.count_function(std_math_function::tan));
  EXPECT_EQ(1, ir.count_function(std_math_function::acos));
  EXPECT_EQ(1, ir.count_function(std_math_function::asin));
  EXPECT_EQ(1, ir.count_function(std_math_function::atan));
  EXPECT_EQ(1, ir.count_function(std_math_function::cosh));
  EXPECT_EQ(1, ir.count_function(std_math_function::sinh));
  EXPECT_EQ(1, ir.count_function(std_math_function::tanh));
  EXPECT_EQ(1, ir.count_function(std_math_function::acosh));
  EXPECT_EQ(1, ir.count_function(std_math_function::asinh));
  EXPECT_EQ(1, ir.count_function(std_math_function::atanh));
  EXPECT_EQ(1, ir.count_function(std_math_function::log));
  EXPECT_EQ(1, ir.count_function(std_math_function::sqrt));
  EXPECT_EQ(2, ir.count_function(std_math_function::abs));
  EXPECT_EQ(2, ir.count_function(std_math_function::signum));
  EXPECT_EQ(1, ir.count_function(std_math_function::floor));
  EXPECT_EQ(1, ir.count_function(std_math_function::atan2));
  EXPECT_EQ(1, ir.count_function(std_math_function::powi));
  EXPECT_EQ(1, ir.count_function(std_math_function::powf));
  check_expressions(expected_expressions, ir);

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  ASSERT_EQ(43, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(0, output_ir.num_conditionals()) << output_ir;
  check_expressions_with_output_permutations(expected_expressions, output_ir);
}

// Make a external function that accepts two arguments (one scalar, one matrix).
class custom_func_1 : public declare_external_function<
                          custom_func_1, scalar_expr,
                          type_list<scalar_expr, type_annotations::static_matrix<2, 2>>> {
 public:
  static constexpr std::string_view name() noexcept { return "custom_func_1"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0", "arg1"); }
};

TEST(IrTest, TestExternalFunction1) {
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y) {
        auto m = make_matrix(2, 2, x - 2 * y, log(x), -x * y, y / 4 + cos(x));
        return custom_func_1::call(cos(x) * y, m) + 5;
      },
      "func", arg("x"), arg("y"));

  check_expressions(expected_expressions, ir);
  ASSERT_EQ(1, ir.count_operation<ir::call_external_function>()) << ir;

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  check_expressions_with_output_permutations(expected_expressions, output_ir);
  ASSERT_EQ(1, output_ir.count_operation<ir::call_external_function>());
}

// Make a external function that returns a matrix.
class custom_func_2
    : public declare_external_function<custom_func_2, type_annotations::static_matrix<2, 4>,
                                       type_list<scalar_expr, scalar_expr>> {
 public:
  static constexpr std::string_view name() noexcept { return "custom_func_2"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0", "arg1"); }
};

TEST(IrTest, TestExternalFunction2) {
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y) {
        const matrix_expr m = custom_func_2::call(x + 2, y / 3);
        return ta::static_matrix<2, 4>(
            m + make_matrix(2, 4, x * y, cos(y), 0, -2, -5 * x - sin(y), 1, 0, x * y));
      },
      "func", arg("x"), arg("y"));

  check_expressions(expected_expressions, ir);
  ASSERT_EQ(1, ir.count_operation<ir::call_external_function>()) << ir;

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  check_expressions_with_output_permutations(expected_expressions, output_ir);
  ASSERT_EQ(1, output_ir.count_operation<ir::call_external_function>());
}

// A custom type used to support tests below.
struct point_2d {
  scalar_expr x{0};
  scalar_expr y{0};
};

template <>
struct custom_type_registrant<point_2d> {
  auto operator()(custom_type_registry& registry) const {
    return custom_type_builder<point_2d>(registry, "Point2d")
        .add_field("x", &point_2d::x)
        .add_field("y", &point_2d::y);
  }
};

// Make a external function that returns a custom type.
class custom_func_3 : public declare_external_function<custom_func_3, point_2d,
                                                       type_list<scalar_expr, scalar_expr>> {
 public:
  static constexpr std::string_view name() noexcept { return "custom_func_3"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0", "arg1"); }
};

TEST(IrTest, TestExternalFunction3) {
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y) {
        const point_2d p = custom_func_3::call(x + y * 5, cos(y));
        point_2d p2{p.x - 2 * x, p.y / y};
        return std::make_tuple(return_value(p), optional_output_arg("p2", p2));
      },
      "func", arg("x"), arg("y"));

  check_expressions(expected_expressions, ir);
  ASSERT_EQ(1, ir.count_operation<ir::call_external_function>()) << ir;

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  check_expressions_with_output_permutations(expected_expressions, output_ir);
  ASSERT_EQ(1, output_ir.count_operation<ir::call_external_function>());
}

// Make a external function that accepts a custom type as a parameter.
class custom_func_4
    : public declare_external_function<custom_func_4, point_2d, type_list<point_2d, scalar_expr>> {
 public:
  static constexpr std::string_view name() noexcept { return "custom_func_4"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0", "arg1"); }
};

TEST(IrTest, TestExternalFunction4) {
  auto [expected_expressions, ir] = create_ir(
      [](scalar_expr x, scalar_expr y, scalar_expr z) {
        const point_2d p = custom_func_4::call(point_2d{x * 3, y / 2}, z + constants::pi / 2);
        return sqrt(pow(p.x, 2) + pow(p.y, 2));
      },
      "func", arg("x"), arg("y"), arg("z"));

  check_expressions(expected_expressions, ir);
  ASSERT_EQ(1, ir.count_operation<ir::call_external_function>()) << ir;

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  check_expressions_with_output_permutations(expected_expressions, output_ir);
  ASSERT_EQ(1, output_ir.count_operation<ir::call_external_function>());
}

// Accept a custom type as an argument, and pass it to a external function.
TEST(IrTest, TestExternalFunction5) {
  auto [expected_expressions, ir] =
      create_ir([](point_2d p1, scalar_expr w) { return custom_func_4::call(p1, w * p1.x - 22); },
                "func", arg("p1"), arg("w"));

  check_expressions(expected_expressions, ir);
  ASSERT_EQ(1, ir.count_operation<ir::call_external_function>()) << ir;
  ASSERT_EQ(0, ir.count_operation<ir::construct>()) << ir;  //  p1 passed directly.

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  check_expressions_with_output_permutations(expected_expressions, output_ir);
  ASSERT_EQ(1, output_ir.count_operation<ir::call_external_function>());
  ASSERT_EQ(0, output_ir.count_operation<ir::construct>()) << ir;
}

class custom_func_5
    : public declare_external_function<custom_func_5, point_2d, type_list<point_2d>> {
 public:
  static constexpr std::string_view name() noexcept { return "custom_func_5"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0"); }
};

// Call one external function and pass the result to another.
TEST(IrTest, TestExternalFunction6) {
  auto [expected_expressions, ir] = create_ir(
      [](point_2d p, scalar_expr w) {
        const point_2d p2 = custom_func_5::call(p);
        const point_2d p3 = custom_func_4::call(p2, p.x * w + 5);
        const scalar_expr f = custom_func_1::call(w, make_matrix(2, 2, p2.x, p2.y, p3.x, p3.y));
        const point_2d p4{p3.y / w + f, p3.y + w * cos(p3.x)};
        return p4;
      },
      "func", arg("p"), arg("w"));

  check_expressions(expected_expressions, ir);
  ASSERT_EQ(3, ir.count_operation<ir::call_external_function>()) << ir;
  ASSERT_EQ(1, ir.count_operation<ir::construct>()) << ir;  //  Only matrix is constructed.

  const control_flow_graph output_ir = std::move(ir).convert_conditionals_to_control_flow();
  check_expressions_with_output_permutations(expected_expressions, output_ir);
  ASSERT_EQ(3, output_ir.count_operation<ir::call_external_function>());
  ASSERT_EQ(1, output_ir.count_operation<ir::construct>()) << ir;  //  Only matrix is constructed.
}

}  // namespace wf
