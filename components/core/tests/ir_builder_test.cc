// Copyright 2023 Gareth Cross
#include "wf/code_generation/ir_builder.h"
#include "wf/functions.h"
#include "wf/type_annotations.h"

#include "wf_test_support/test_macros.h"

#include <bitset>

namespace math {
using namespace math::custom_literals;
namespace ta = type_annotations;

std::ostream& operator<<(std::ostream& s, const FlatIr& b) {
  s << "Flat IR:\n" << b.to_string();
  return s;
}
std::ostream& operator<<(std::ostream& s, const OutputIr& b) {
  s << "Output IR:\n" << b.to_string();
  return s;
}

// Given a lambda or function pointer, create the IR for the expressions it generates.
template <typename Func, typename... Args>
auto create_ir(Func&& func, const std::string_view name, Args&&... args) {
  auto tuple =
      build_function_description(std::forward<Func>(func), name, std::forward<Args>(args)...);
  std::vector<ExpressionGroup>& expressions = std::get<1>(tuple);
  FlatIr flat_ir{expressions};
  flat_ir.eliminate_duplicates();
  return std::make_tuple(std::move(expressions), std::move(flat_ir));
}

struct OptionalArgPermutations {
 public:
  static constexpr std::size_t MaxOptionalArgs = 32;

  // Construct w/ vector of expressions.
  explicit OptionalArgPermutations(const std::vector<ExpressionGroup>& expressions) {
    for (auto it = expressions.begin(); it != expressions.end(); ++it) {
      if (it->key.usage == ExpressionUsage::OptionalOutputArgument) {
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
void check_output_expressions(const std::vector<ExpressionGroup>& expected_expressions,
                              const std::unordered_map<OutputKey, std::vector<Expr>,
                                                       hash_struct<OutputKey>>& output_expressions,
                              const T& ir) {
  for (const ExpressionGroup& group : expected_expressions) {
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

void check_expressions(const std::vector<ExpressionGroup>& expected_expressions, const FlatIr& ir) {
  auto output_expressions = create_output_expression_map(ir.get_block(), {});
  check_output_expressions(expected_expressions, output_expressions, ir);
}

void check_expressions(const std::vector<ExpressionGroup>& expected_expressions,
                       const OutputIr& ir) {
  const OptionalArgPermutations permutations{expected_expressions};
  for (std::size_t i = 0; i < permutations.num_permutations(); ++i) {
    // test permutation `i`:
    auto output_map = permutations.get_permutation(i);
    auto output_expressions = create_output_expression_map(ir.first_block(), std::move(output_map));
    check_output_expressions(expected_expressions, output_expressions, ir);
  }
}

TEST(IrTest, TestNumericConstant1) {
  auto [expected_expressions, ir] = create_ir([]() { return 6_s; }, "func");
  check_expressions(expected_expressions, ir);
  check_expressions(expected_expressions, OutputIr{std::move(ir)});
}

TEST(IrTest, TestNumericConstant2) {
  auto [expected_expressions, ir] = create_ir(
      []() {
        return std::make_tuple(ReturnValue(2.0_s), OutputArg("a", 7_s), OutputArg("b", 1_s));
      },
      "func");
  check_expressions(expected_expressions, ir);
  check_expressions(expected_expressions, OutputIr{std::move(ir)});
}

TEST(IrTest, TestScalarExpressions1) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y) {
        Expr a = x * y;
        Expr b = x + y;
        return std::make_tuple(ReturnValue(x * y), OutputArg("a", a), OutputArg("b", b));
      },
      "func", Arg("x"), Arg("y"));
  ASSERT_EQ(2, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  ASSERT_EQ(2, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(0, output_ir.num_conditionals()) << output_ir;
  ASSERT_EQ(1, output_ir.num_blocks()) << output_ir;
  check_expressions(expected_expressions, output_ir);
}

TEST(IrTest, TestScalarExpressions2) {
  // Create an optional output:
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y, Expr z) {
        Expr f = x * y * sin(z * x) + 5;
        Expr g = cos(z * x) * x * y - log(y - z * 2.1) * 3;
        return std::make_tuple(OutputArg("f", f), OptionalOutputArg("g", g));
      },
      "func", Arg("x"), Arg("y"), Arg("z"));

  ASSERT_EQ(14, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  ASSERT_EQ(15, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(1, output_ir.num_conditionals()) << output_ir;
  ASSERT_EQ(3, output_ir.num_blocks()) << output_ir;
  check_expressions(expected_expressions, output_ir);
}

TEST(IrTest, TestRepeatedScalarExpressions1) {
  // y * z should get combined in the output, since it appears more:
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y, Expr z) {
        Expr f = (x * y * z) + (y * z) - sin(y * z) * (y * z) + log(x * z) + cos(x * y);
        return f;
      },
      "func", Arg("x"), Arg("y"), Arg("z"));

  ASSERT_EQ(14, ir.num_operations()) << ir;  //  TODO: Add more explicit check here.
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);
  check_expressions(expected_expressions, OutputIr{std::move(ir)});
}

TEST(IrTest, TestScalarExpressions3) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y, Expr z) {
        // Chain together a few operations (this doesn't reduce at all).
        Expr f = Constants::One;
        for (int i = 0; i < 20; ++i) {
          if (i & 1) {
            f = (f + x) * y;
          } else {
            f = (f - y) * z;
          }
        }
        return f;
      },
      "func", Arg("x"), Arg("y"), Arg("z"));

  ASSERT_EQ(43, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);
  check_expressions(expected_expressions, OutputIr{std::move(ir)});
}

// Test that powers can be converted into multiplications.
TEST(IrTest, TestPowerConversion1) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x) {
        return 0.13 * pow(x, 2) + 1.2 * pow(x, 3) - 5.0 * pow(x, 4) + 0.9 * pow(x, 5) -
               7 * pow(x, 6);
      },
      "func", Arg("x"));

  ASSERT_EQ(15, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  ASSERT_EQ(10, ir.count_operation([](ir::Mul) { return true; })) << ir;
  ASSERT_EQ(0, ir.count_operation([](ir::CallStdFunction) { return true; })) << ir;

  check_expressions(expected_expressions, ir);
  check_expressions(expected_expressions, OutputIr{std::move(ir)});
}

// Test that powers of square roots are convert into sqrt operations.
TEST(IrTest, TestPowerConversion2) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, Expr y) {
        Expr c = x * x + y * y;
        return pow(c, 1_s / 2) + 1 / pow(c, 5_s / 2) + pow(c, 3_s / 2);
      },
      "func", Arg("x"), Arg("y"));

  ASSERT_EQ(12, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  ASSERT_EQ(6, ir.count_operation([](ir::Mul) { return true; })) << ir;
  ASSERT_EQ(1, ir.count_operation([](ir::Div) { return true; })) << ir;
  ASSERT_EQ(1, ir.count_functions(StdMathFunction::Sqrt)) << ir;

  check_expressions(expected_expressions, ir);
  check_expressions(expected_expressions, OutputIr{std::move(ir)});
}

TEST(IrTest, TestConditionals1) {
  auto [expected_expressions, ir] = create_ir(
      [](Expr x) {
        // heaviside step function:
        return where(x > 0, 1, 0);
      },
      "func", Arg("x"));

  ASSERT_EQ(4, ir.num_operations()) << ir;
  ASSERT_EQ(1, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
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
      "func", Arg("x"), Arg("y"));

  ASSERT_EQ(8, ir.num_operations()) << ir;
  ASSERT_EQ(1, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
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
      "func", Arg("x"), Arg("y"));

  ASSERT_EQ(7, ir.num_operations()) << ir;
  ASSERT_EQ(3, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
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
      "func", Arg("x"), Arg("y"), Arg("z"));

  ASSERT_EQ(20, ir.num_operations()) << ir;
  ASSERT_EQ(3, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  ASSERT_EQ(20, output_ir.num_operations()) << output_ir;
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
        return std::make_tuple(ReturnValue(f), OptionalOutputArg("g", g),
                               OptionalOutputArg("h", h));
      },
      "func", Arg("x"), Arg("y"), Arg("z"));

  ASSERT_EQ(54, ir.num_operations()) << ir;
  ASSERT_EQ(6, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
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
      "func", Arg("x"), Arg("y"), Arg("z"), Arg("w"));

  ASSERT_EQ(25, ir.num_operations()) << ir;
  ASSERT_EQ(4, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(25, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(4, output_ir.num_conditionals()) << output_ir;
}

TEST(IrTest, TestConditionals7) {
  // Nested conditionals with identical conditions:
  auto [expected_expressions, ir] =
      create_ir([](Expr x, Expr y, Expr z) { return where(x > 0, where(x > 0, y, z), 10 * z - y); },
                "func", Arg("x"), Arg("y"), Arg("z"));

  ASSERT_EQ(9, ir.num_operations()) << ir;
  ASSERT_EQ(2, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(9, output_ir.num_operations()) << output_ir;
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
      "func", Arg("x"), Arg("y"));

  ASSERT_EQ(28, ir.num_operations()) << ir;
  ASSERT_EQ(3, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(28, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(3, output_ir.num_conditionals()) << output_ir;
}

TEST(IrTest, TestMatrixExpressions1) {
  // Create a matrix output:
  auto [expected_expressions, ir] = create_ir(
      [](Expr x, const ta::StaticMatrix<2, 1>& v) {
        using namespace matrix_operator_overloads;
        ta::StaticMatrix<2, 2> m = v * v.transposed() * x;
        return m;
      },
      "func", Arg("x"), Arg("y"));

  ASSERT_EQ(6, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
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
        return ta::StaticMatrix<4, 4>{MatrixExpr::create(4, 4, std::move(expressions))};
      },
      "func", Arg("x"), Arg("y"), Arg("z"));

  ASSERT_EQ(38, ir.num_operations()) << ir;
  ASSERT_EQ(16, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  // Conditionals should get reduced:
  OutputIr output_ir{std::move(ir)};
  ASSERT_EQ(38, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(1, output_ir.num_conditionals()) << output_ir;
  check_expressions(expected_expressions, output_ir);
}

TEST(IrTest, TestMatrixExpressions3) {
  // Create matrices with conditionals and optional output arguments:
  auto [expected_expressions, ir] = create_ir(
      [](const ta::StaticMatrix<2, 1>& v, const ta::StaticMatrix<3, 3>& u,
         const ta::StaticMatrix<3, 1>& t) {
        using namespace matrix_operator_overloads;
        auto I3 = make_identity(3);
        auto zeros = make_zeros(2, 3);
        ta::StaticMatrix<2, 3> f = where(v[0] - t[1] > 0, v * t.transposed() * (u - I3), zeros);

        auto path_1 = u * t * t.transposed();
        auto path_2 = (u - I3) * (u - I3).transposed();
        ta::StaticMatrix<3, 3> g{where(u(1, 1) < -v[1], path_1, path_2)};

        return std::make_tuple(ReturnValue(f), OptionalOutputArg("g", g));
      },
      "func", Arg("v"), Arg("u"), Arg("t"));

  ASSERT_EQ(115, ir.num_operations()) << ir;
  ASSERT_EQ(15, ir.num_conditionals()) << ir;
  check_expressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
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
      "func", Arg("x"), Arg("y"), Arg("z"));

  ASSERT_EQ(28, ir.num_operations()) << ir;
  ASSERT_EQ(0, ir.num_conditionals()) << ir;
  EXPECT_EQ(1, ir.count_functions(StdMathFunction::Cos));
  EXPECT_EQ(1, ir.count_functions(StdMathFunction::Sin));
  EXPECT_EQ(1, ir.count_functions(StdMathFunction::Tan));
  EXPECT_EQ(1, ir.count_functions(StdMathFunction::ArcCos));
  EXPECT_EQ(1, ir.count_functions(StdMathFunction::ArcSin));
  EXPECT_EQ(1, ir.count_functions(StdMathFunction::Log));
  EXPECT_EQ(1, ir.count_functions(StdMathFunction::Sqrt));
  EXPECT_EQ(2, ir.count_functions(StdMathFunction::Abs));
  EXPECT_EQ(2, ir.count_functions(StdMathFunction::Signum));
  EXPECT_EQ(1, ir.count_functions(StdMathFunction::Arctan2));
  EXPECT_EQ(1, ir.count_functions(StdMathFunction::Powi));
  EXPECT_EQ(1, ir.count_functions(StdMathFunction::Powf));
  check_expressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  check_expressions(expected_expressions, output_ir);
  ASSERT_EQ(28, output_ir.num_operations()) << output_ir;
  ASSERT_EQ(0, output_ir.num_conditionals()) << output_ir;
}

}  // namespace math
