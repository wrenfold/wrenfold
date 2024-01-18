// Copyright 2023 Gareth Cross
#include "wf/code_generation/ir_builder.h"
#include "wf/code_generation/declare_custom_function.h"
#include "wf/code_generation/function_evaluator.h"
#include "wf/constants.h"
#include "wf/functions.h"
#include "wf/type_annotations.h"

#include "wf_test_support/test_macros.h"

#include <bitset>

namespace wf {
using namespace wf::custom_literals;
namespace ta = type_annotations;

std::ostream& operator<<(std::ostream& s, const flat_ir& b) {
  s << "Flat IR:\n" << b.to_string();
  return s;
}
std::ostream& operator<<(std::ostream& s, const output_ir& b) {
  s << "Output IR:\n" << b.to_string();
  return s;
}

// Given a lambda or function pointer, create the IR for the expressions it generates.
template <typename Func, typename... Args>
auto create_ir(Func&& func, const std::string_view name, Args&&... args) {
  const function_description description =
      build_function_description(std::forward<Func>(func), name, std::forward<Args>(args)...);
  flat_ir flat_ir{description.output_expressions()};
  flat_ir.eliminate_duplicates();
  return std::make_tuple(description.output_expressions(), std::move(flat_ir));
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
    WF_ASSERT_LESS(scatter_.size(), MaxOptionalArgs);
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
    WF_ASSERT_LESS(n, num_permutations());
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
                              const std::unordered_map<output_key, std::vector<Expr>,
                                                       hash_struct<output_key>>& output_expressions,
                              const T& ir) {
  for (const expression_group& group : expected_expressions) {
    auto it = output_expressions.find(group.key);
    ASSERT_TRUE(it != output_expressions.end())
        << fmt::format("Missing key ({}, {})\n", string_from_expression_usage(group.key.usage),
                       group.key.name)
        << ir;

    ASSERT_EQ(group.expressions.size(), it->second.size()) << ir;

    for (std::size_t i = 0; i < group.expressions.size(); ++i) {
      ASSERT_IDENTICAL(group.expressions[i], it->second[i])
          << fmt::format("Key: ({}, {}), i: {}\n", string_from_expression_usage(group.key.usage),
                         group.key.name, i)
          << ir;
    }
  }
}

void check_expressions(const std::vector<expression_group>& expected_expressions,
                       const flat_ir& ir) {
  const auto output_expressions = create_output_expression_map(ir.get_block(), {});
  check_output_expressions(expected_expressions, output_expressions, ir);
}

void check_expressions(const std::vector<expression_group>& expected_expressions,
                       const output_ir& ir) {
  const optional_arg_permutations permutations{expected_expressions};
  for (std::size_t i = 0; i < permutations.num_permutations(); ++i) {
    // test permutation `i`:
    auto output_map = permutations.get_permutation(i);
    auto output_expressions = create_output_expression_map(ir.first_block(), std::move(output_map));
    check_output_expressions(expected_expressions, output_expressions, ir);
  }
}

TEST(IrTest, TestNumericConstant1) {
  auto [expected_expressions, ir] = create_ir([]() { return 6_s; }, "func");
  ASSERT_EQ(1, ir.count_operation<ir::load>()) << ir;
  check_expressions(expected_expressions, ir);
  check_expressions(expected_expressions, output_ir{std::move(ir)});
}

// ReSharper disable CppPassValueParameterByConstReference

TEST(IrTest, TestNumericConstant2) {
  auto [expected_expressions, ir] = create_ir(
      []() {
        return std::make_tuple(return_value(2.0_s), output_arg("a", 7_s), output_arg("b", 1_s));
      },
      "func");
  ASSERT_EQ(3, ir.count_operation<ir::load>()) << ir;
  check_expressions(expected_expressions, ir);
  check_expressions(expected_expressions, output_ir{std::move(ir)});
}

TEST(IrTest, TestNoDuplicatedCasts) {
  // Create flat IR but don't eliminate duplicates. Double check that only two casts
  // are inserted (one for 0, and one for 1).
  const auto tuple = build_function_description(
      []() -> ta::static_matrix<4, 4> { return make_identity(4); }, "func");
  const flat_ir ir{tuple.output_expressions()};
  ASSERT_EQ(2, ir.count_operation<ir::cast>()) << ir;
  ASSERT_EQ(2, ir.count_operation<ir::load>()) << ir;
}

TEST(IrTest, TestScalarExpressions1) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y) {
        Expr a = x * y;
        Expr b = x + y;
        return std::make_tuple(return_value(x * y), output_arg("a", a), output_arg("b", b));
      },
      "func", arg("x"), arg("y"));
  ASSERT_EQ(1, ir.count_operation<ir::mul>()) << ir;
  ASSERT_EQ(1, ir.count_operation<ir::add>()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  output_ir output_ir{std::move(ir)};
  ASSERT_EQ(1, output_ir.count_operation<ir::mul>()) << output_ir;
  ASSERT_EQ(1, output_ir.count_operation<ir::add>()) << output_ir;
  ASSERT_EQ(1, output_ir.num_blocks()) << output_ir;
  check_expressions(expected_expressions, output_ir);
}

TEST(IrTest, TestScalarExpressions2) {
  // Create an optional output:
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y, Expr z) {
        Expr f = x * y * sin(z * x) + 5;
        Expr g = cos(z * x) * x * y - log(y - z * 2.1) * 3;
        return std::make_tuple(output_arg("f", f), optional_output_arg("g", g));
      },
      "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(6, ir.count_operation<ir::mul>()) << ir;
  ASSERT_EQ(3, ir.count_operation<ir::add>()) << ir;
  ASSERT_EQ(0, ir.count_operation<ir::neg>()) << ir;
  ASSERT_EQ(1, ir.count_function(std_math_function::sin)) << ir;
  ASSERT_EQ(1, ir.count_function(std_math_function::cos)) << ir;
  ASSERT_EQ(1, ir.count_function(std_math_function::log)) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  output_ir output_ir{std::move(ir)};
  ASSERT_EQ(15, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(1, output_ir.num_conditionals()) << output_ir;
  ASSERT_EQ(3, output_ir.num_blocks()) << output_ir;
  check_expressions(expected_expressions, output_ir);
}

TEST(IrTest, TestScalarExpressions3) {
  // y * z should get combined in the output, since it appears more:
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y, Expr z) {
        Expr f = (x * y * z) + (y * z) - sin(y * z) * (y * z) + log(x * z) + cos(x * y);
        return f;
      },
      "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(5, ir.count_operation<ir::mul>()) << ir;
  ASSERT_EQ(4, ir.count_operation<ir::add>()) << ir;
  ASSERT_EQ(1, ir.count_operation<ir::neg>()) << ir;
  ASSERT_EQ(1, ir.count_function(std_math_function::sin)) << ir;
  ASSERT_EQ(1, ir.count_function(std_math_function::cos)) << ir;
  ASSERT_EQ(1, ir.count_function(std_math_function::log)) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);
  check_expressions(expected_expressions, output_ir{std::move(ir)});
}

TEST(IrTest, TestScalarExpressions4) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y, Expr z) {
        // Chain together a few operations (this doesn't reduce at all).
        Expr f = constants::one;
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
  check_expressions(expected_expressions, output_ir{std::move(ir)});
}

// Test that powers can be converted into multiplications.
TEST(IrTest, TestPowerConversion1) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x) {
        return 0.13 * pow(x, 2) + 1.2 * pow(x, 3) - 5.0 * pow(x, 4) + 0.9 * pow(x, 5) -
               7 * pow(x, 6);
      },
      "func", arg("x"));

  ASSERT_EQ(15, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  ASSERT_EQ(10, ir.count_operation<ir::mul>()) << ir;
  ASSERT_EQ(0, ir.count_function(std_math_function::powf)) << ir;
  ASSERT_EQ(0, ir.count_function(std_math_function::powi)) << ir;
  check_expressions(expected_expressions, ir);
  check_expressions(expected_expressions, output_ir{std::move(ir)});
}

// Test that powers of square roots are convert into sqrt operations.
TEST(IrTest, TestPowerConversion2) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y) {
        Expr c = x * x + y * y;
        return pow(c, 1_s / 2) + 1 / pow(c, 5_s / 2) + pow(c, 3_s / 2);
      },
      "func", arg("x"), arg("y"));

  ASSERT_EQ(12, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  ASSERT_EQ(0, ir.count_function(std_math_function::powf));
  ASSERT_EQ(0, ir.count_function(std_math_function::powi));
  ASSERT_EQ(6, ir.count_operation<ir::mul>()) << ir;
  ASSERT_EQ(1, ir.count_operation<ir::div>()) << ir;
  ASSERT_EQ(1, ir.count_function(std_math_function::sqrt)) << ir;

  check_expressions(expected_expressions, ir);
  check_expressions(expected_expressions, output_ir{std::move(ir)});
}

TEST(IrTest, TestConditionals1) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x) {
        // heaviside step function:
        return where(x > 0, 1, 0);
      },
      "func", arg("x"));

  ASSERT_EQ(4, ir.num_operations()) << ir;
  ASSERT_EQ(1, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  output_ir output_ir{std::move(ir)};
  ASSERT_EQ(4, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(1, output_ir.num_conditionals()) << output_ir;
  check_expressions(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals2) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y) {
        // use the condition in one of the branches:
        Expr condition = x > y;
        return where(condition, condition * 2, cos(y - 2));
      },
      "func", arg("x"), arg("y"));

  ASSERT_EQ(8, ir.num_operations()) << ir;
  ASSERT_EQ(1, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  output_ir output_ir{std::move(ir)};
  ASSERT_EQ(8, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(1, output_ir.num_conditionals()) << output_ir;
  check_expressions(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals3) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y) {
        // exclusive or
        return where(x > 0, where(y > 0, 0, 1), where(y > 0, 1, 0));
      },
      "func", arg("x"), arg("y"));

  ASSERT_EQ(7, ir.num_operations()) << ir;
  ASSERT_EQ(3, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  output_ir output_ir{std::move(ir)};
  ASSERT_EQ(7, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(3, output_ir.num_conditionals()) << output_ir;
  check_expressions(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals4) {
  // Nested conditionals:
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y, Expr z) {
        Expr p = where(x > 0, cos(x), sin(z));
        Expr q = where(y < -5, -log(p), tan(x + y));
        Expr l = where(pow(z, 2) < y, q, q - p);
        return l * 2;
      },
      "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(19, ir.num_operations()) << ir;
  ASSERT_EQ(3, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  output_ir output_ir{std::move(ir)};
  ASSERT_EQ(19, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(3, output_ir.num_conditionals()) << output_ir;
  check_expressions(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals5) {
  // Optional outputs and conditionals:
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y, Expr z) {
        Expr p = where(x > y, cos(x) * sin(z) - sin(x) * cos(z) * 2, log(z - x) * 23);
        Expr q = where(pow(z, y) < x, p * p, -cos(p) + x);
        Expr f = q;
        Expr g = q.diff(x);
        Expr h = q.diff(x, 2);
        return std::make_tuple(return_value(f), optional_output_arg("g", g),
                               optional_output_arg("h", h));
      },
      "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(54, ir.num_operations()) << ir;
  ASSERT_EQ(6, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  output_ir output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(56, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(7, output_ir.num_conditionals()) << output_ir;
}

TEST(IrTest, TestConditionals6) {
  // Create nested conditionals several layers deep:
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y, Expr z, Expr w) {
        Expr f = where(x > 0, x, 1 - x);
        f = where(y > 0, f * y, f * (1 - y));
        f = where(z > 0, f * z, f * (1 - z));
        return where(w > 0, f * w, f * (1 - w));
      },
      "func", arg("x"), arg("y"), arg("z"), arg("w"));

  ASSERT_EQ(24, ir.num_operations()) << ir;
  ASSERT_EQ(4, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  output_ir output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(24, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(4, output_ir.num_conditionals()) << output_ir;
}

TEST(IrTest, TestConditionals7) {
  // Nested conditionals with identical conditions:
  auto [expected_expressions, ir] =
      create_ir([](Expr x, Expr y, Expr z) { return where(x > 0, where(x > 0, y, z), 10 * z - y); },
                "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(8, ir.num_operations()) << ir;
  ASSERT_EQ(2, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  output_ir output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(8, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(2, output_ir.num_conditionals()) << output_ir;
}

TEST(IrTest, TestConditionals8) {
  // Nested conditionals (values of inner conditionals used for outer ones).
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y) {
        Expr c0 = where(y > 0, cos(x * y), cos(x) + 2);
        Expr c1 = where(x > 0, log(abs(y)), atan2(y, x) * 3);
        Expr c2 = where(x - y > 0, c0 * 3 - sqrt(abs(c0)), pow(abs(c1), 1.0 / 3.0) / 5);
        return c2;
      },
      "func", arg("x"), arg("y"));

  ASSERT_EQ(27, ir.num_operations()) << ir;
  ASSERT_EQ(3, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  output_ir output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(27, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(3, output_ir.num_conditionals()) << output_ir;
}

TEST(IrTest, TestMatrixExpressions1) {
  // Create a matrix output:
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, const ta::static_matrix<2, 1>& v) {
        using namespace matrix_operator_overloads;
        ta::static_matrix<2, 2> m = v * v.transposed() * x;
        return m;
      },
      "func", arg("x"), arg("y"));

  ASSERT_EQ(6, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  output_ir output_ir{std::move(ir)};
  ASSERT_EQ(6, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(0, output_ir.num_conditionals()) << output_ir;
  check_expressions(expected_expressions, output_ir);
}

TEST(IrTest, TestMatrixExpressions2) {
  // Construct a matrix w/ a repeated conditional:
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y, Expr z) {
        std::vector<Expr> expressions{};
        for (int i = 0; i < 16; ++i) {
          expressions.push_back(where(x > 0, pow(y, i), pow(z, 16 - z)));
        }
        return ta::static_matrix<4, 4>{MatrixExpr::create(4, 4, std::move(expressions))};
      },
      "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(37, ir.num_operations()) << ir;
  ASSERT_EQ(16, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  // Conditionals should get reduced:
  output_ir output_ir{std::move(ir)};
  ASSERT_EQ(37, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(1, output_ir.num_conditionals()) << output_ir;
  check_expressions(expected_expressions, output_ir);
}

TEST(IrTest, TestMatrixExpressions3) {
  // Create matrices with conditionals and optional output arguments:
  auto [expected_expressions, ir] = create_ir(
      [](const ta::static_matrix<2, 1>& v, const ta::static_matrix<3, 3>& u,
         const ta::static_matrix<3, 1>& t) {
        using namespace matrix_operator_overloads;
        auto I3 = make_identity(3);
        auto zeros = make_zeros(2, 3);
        ta::static_matrix<2, 3> f = where(v[0] - t[1] > 0, v * t.transposed() * (u - I3), zeros);

        auto path_1 = u * t * t.transposed();
        auto path_2 = (u - I3) * (u - I3).transposed();
        ta::static_matrix<3, 3> g{where(u(1, 1) < -v[1], path_1, path_2)};

        return std::make_tuple(return_value(f), optional_output_arg("g", g));
      },
      "func", arg("v"), arg("u"), arg("t"));

  ASSERT_EQ(115, ir.num_operations()) << ir;
  ASSERT_EQ(15, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  output_ir output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(116, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(3, output_ir.num_conditionals()) << output_ir;
}

TEST(IrTest, TestBuiltInFunctions) {
  // create expressions that use all the built-in functions
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y, Expr z) {
        Expr g = acos(x - log(y) * pow(x, 1.231) + pow(z, 22));
        Expr h = asin(2.0 * tan(y) - abs(z));
        return atan2(abs(g) + cos(y) + signum(h), -sin(y) + sqrt(z) - signum(x));
      },
      "func", arg("x"), arg("y"), arg("z"));

  ASSERT_EQ(27, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  ASSERT_EQ(4, ir.count_operation<ir::neg>());
  EXPECT_EQ(1, ir.count_function(std_math_function::cos));
  EXPECT_EQ(1, ir.count_function(std_math_function::sin));
  EXPECT_EQ(1, ir.count_function(std_math_function::tan));
  EXPECT_EQ(1, ir.count_function(std_math_function::acos));
  EXPECT_EQ(1, ir.count_function(std_math_function::asin));
  EXPECT_EQ(1, ir.count_function(std_math_function::log));
  EXPECT_EQ(1, ir.count_function(std_math_function::sqrt));
  EXPECT_EQ(2, ir.count_function(std_math_function::abs));
  EXPECT_EQ(2, ir.count_function(std_math_function::signum));
  EXPECT_EQ(1, ir.count_function(std_math_function::atan2));
  EXPECT_EQ(1, ir.count_function(std_math_function::powi));
  EXPECT_EQ(1, ir.count_function(std_math_function::powf));
  check_expressions(expected_expressions, ir);

  output_ir output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(27, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(0, output_ir.num_conditionals()) << output_ir;
}

// Make a custom function that accepts two arguments (one scalar, one matrix).
class custom_func_1
    : public declare_custom_function<custom_func_1, Expr,
                                     type_list<Expr, type_annotations::static_matrix<2, 2>>> {
 public:
  static constexpr std::string_view name() noexcept { return "custom_func_1"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0", "arg1"); }
};

TEST(IrTest, TestCustomFunction1) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y) {
        auto m = make_matrix(2, 2, x - 2 * y, log(x), -x * y, y / 4 + cos(x));
        return custom_func_1::call(cos(x) * y, m) + 5;
      },
      "func", arg("x"), arg("y"));

  check_expressions(expected_expressions, ir);
  ASSERT_EQ(1, ir.count_operation<ir::call_custom_function>()) << ir;

  const output_ir output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(1, output_ir.count_operation<ir::call_custom_function>());
}

// Make a custom function that returns a matrix.
class custom_func_2
    : public declare_custom_function<custom_func_2, type_annotations::static_matrix<2, 4>,
                                     type_list<Expr, Expr>> {
 public:
  static constexpr std::string_view name() noexcept { return "custom_func_2"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0", "arg1"); }
};

TEST(IrTest, TestCustomFunction2) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y) {
        using namespace matrix_operator_overloads;
        const MatrixExpr m = custom_func_2::call(x + 2, y / 3);
        return ta::static_matrix<2, 4>(
            m + make_matrix(2, 4, x * y, cos(y), 0, -2, -5 * x - sin(y), 1, 0, x * y));
      },
      "func", arg("x"), arg("y"));

  check_expressions(expected_expressions, ir);
  ASSERT_EQ(1, ir.count_operation<ir::call_custom_function>()) << ir;

  const output_ir output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(1, output_ir.count_operation<ir::call_custom_function>());
}

// A custom type used to support tests below.
struct point_2d : custom_type_base<point_2d> {
  Expr x{0};
  Expr y{0};

  point_2d() = default;
  point_2d(Expr x, Expr y) : x(std::move(x)), y(std::move(y)) {}

  static auto register_type(custom_type_registry& registry) {
    return custom_type_builder<point_2d>(registry, "point_2d")
        .add_field("x", &point_2d::x)
        .add_field("y", &point_2d::y);
  }
};

// Make a custom function that returns a custom type.
class custom_func_3
    : public declare_custom_function<custom_func_3, point_2d, type_list<Expr, Expr>> {
 public:
  static constexpr std::string_view name() noexcept { return "custom_func_3"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0", "arg1"); }
};

TEST(IrTest, TestCustomFunction3) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y) {
        const point_2d p = custom_func_3::call(x + y * 5, cos(y));
        point_2d p2{p.x - 2 * x, p.y / y};
        return std::make_tuple(return_value(p), optional_output_arg("p2", p2));
      },
      "func", arg("x"), arg("y"));

  check_expressions(expected_expressions, ir);
  ASSERT_EQ(1, ir.count_operation<ir::call_custom_function>()) << ir;

  const output_ir output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(1, output_ir.count_operation<ir::call_custom_function>());
}

// Make a custom function that accepts a custom type as a parameter.
class custom_func_4
    : public declare_custom_function<custom_func_4, point_2d, type_list<point_2d, Expr>> {
 public:
  static constexpr std::string_view name() noexcept { return "custom_func_4"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0", "arg1"); }
};

TEST(IrTest, TestCustomFunction4) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y, Expr z) {
        const point_2d p = custom_func_4::call(point_2d(x * 3, y / 2), z + constants::pi / 2);
        return sqrt(pow(p.x, 2) + pow(p.y, 2));
      },
      "func", arg("x"), arg("y"), arg("z"));

  check_expressions(expected_expressions, ir);
  ASSERT_EQ(1, ir.count_operation<ir::call_custom_function>()) << ir;

  const output_ir output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(1, output_ir.count_operation<ir::call_custom_function>());
}

// Accept a custom type as an argument, and pass it to a custom function.
TEST(IrTest, TestCustomFunction5) {
  auto [expected_expressions, ir] =
      create_ir([](point_2d p1, Expr w) { return custom_func_4::call(p1, w * p1.x - 22); }, "func",
                arg("p1"), arg("w"));

  check_expressions(expected_expressions, ir);
  ASSERT_EQ(1, ir.count_operation<ir::call_custom_function>()) << ir;

  const output_ir output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(1, output_ir.count_operation<ir::call_custom_function>());
}

class custom_func_5 : public declare_custom_function<custom_func_5, point_2d, type_list<point_2d>> {
 public:
  static constexpr std::string_view name() noexcept { return "custom_func_5"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0"); }
};

// Call one custom function and pass the result to another.
TEST(IrTest, TestCustomFunction6) {
  auto [expected_expressions, ir] = create_ir(
      [](point_2d p, Expr w) {
        const point_2d p2 = custom_func_5::call(p);
        const point_2d p3 = custom_func_4::call(p2, p.x * w + 5);
        const Expr f = custom_func_1::call(w, make_matrix(2, 2, p2.x, p2.y, p3.x, p3.y));
        const point_2d p4{p3.y / w + f, p3.y + w * cos(p3.x)};
        return p4;
      },
      "func", arg("p"), arg("w"));

  check_expressions(expected_expressions, ir);
  ASSERT_EQ(3, ir.count_operation<ir::call_custom_function>()) << ir;

  const output_ir output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(3, output_ir.count_operation<ir::call_custom_function>());
}

}  // namespace wf
