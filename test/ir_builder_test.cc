// Copyright 2023 Gareth Cross
#include "code_generation/ir_builder.h"
#include "test_helpers.h"

#include "functions.h"
#include "type_annotations.h"

#include <bitset>

namespace math {
using namespace math::custom_literals;
namespace ta = type_annotations;

std::ostream& operator<<(std::ostream& s, const FlatIr& b) {
  s << "Flat IR:\n" << b.ToString();
  return s;
}
std::ostream& operator<<(std::ostream& s, const OutputIr& b) {
  s << "Output IR:\n" << b.ToString();
  return s;
}

// Given a lambda or function pointer, create the IR for the expressions it generates.
template <typename Func, typename... Args>
auto CreateIR(Func&& func, const std::string_view name, Args&&... args) {
  auto tuple =
      BuildFunctionDescription(std::forward<Func>(func), name, std::forward<Args>(args)...);
  std::vector<ExpressionGroup>& expressions = std::get<1>(tuple);
  FlatIr flat_ir{expressions};
  flat_ir.EliminateDuplicates();
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
    ASSERT_LESS(scatter_.size(), MaxOptionalArgs);
  }

  std::size_t NumPermutations() const {
    // We encode the permutations in bitsets. So if you have three optional args, there are
    // 8 possible permutations: 000, 001, 010, 011, 100, 101, 110, 111
    const std::size_t num_optional_args = scatter_.size();
    return static_cast<std::size_t>(1) << num_optional_args;
  }

  // Get the n'th permutation of optional arguments.
  // The returned map is a mapping from `arg index` --> whether the argument is to be computed.
  std::unordered_map<std::string, bool> GetPermutation(std::size_t n) const {
    ASSERT_LESS(n, NumPermutations());
    const std::bitset<MaxOptionalArgs> permutation{n};

    std::unordered_map<std::string, bool> output{};
    output.reserve(scatter_.size());
    for (const auto [name, bitfield_index] : scatter_) {
      output.emplace(name, permutation[bitfield_index]);
    }
    return output;
  }

 private:
  // Map from arg name to bit index in a permutation.
  std::unordered_map<std::string, std::size_t> scatter_;
};

template <typename T>
void CheckOutputExpressions(
    const std::vector<ExpressionGroup>& expected_expressions,
    const std::unordered_map<OutputKey, std::vector<Expr>, OutputKeyHasher>& output_expressions,
    const T& ir) {
  for (const ExpressionGroup& group : expected_expressions) {
    auto it = output_expressions.find(group.key);
    ASSERT_TRUE(it != output_expressions.end())
        << fmt::format("Missing key ({}, {})\n", StringFromExpressionUsage(group.key.usage),
                       group.key.name)
        << ir;

    ASSERT_EQ(group.expressions.size(), it->second.size()) << ir;

    for (std::size_t i = 0; i < group.expressions.size(); ++i) {
      ASSERT_IDENTICAL(group.expressions[i], it->second[i])
          << fmt::format("Key: ({}, {}), i: {}\n", StringFromExpressionUsage(group.key.usage),
                         group.key.name, i)
          << ir;
    }
  }
}

void CheckExpressions(const std::vector<ExpressionGroup>& expected_expressions, const FlatIr& ir) {
  auto output_expressions = CreateOutputExpressionMap(ir.GetBlock(), nullptr);
  CheckOutputExpressions(expected_expressions, output_expressions, ir);
}

void CheckExpressions(const std::vector<ExpressionGroup>& expected_expressions,
                      const OutputIr& ir) {
  const OptionalArgPermutations permutations{expected_expressions};
  for (std::size_t i = 0; i < permutations.NumPermutations(); ++i) {
    // test permutation `i`:
    const auto output_map = permutations.GetPermutation(i);
    auto output_expressions = CreateOutputExpressionMap(ir.FirstBlock(), &output_map);
    CheckOutputExpressions(expected_expressions, output_expressions, ir);
  }
}

TEST(IrTest, TestNumericConstant1) {
  auto [expected_expressions, ir] = CreateIR([]() { return 6_s; }, "func");
  CheckExpressions(expected_expressions, ir);
  CheckExpressions(expected_expressions, OutputIr{std::move(ir)});
}

TEST(IrTest, TestNumericConstant2) {
  auto [expected_expressions, ir] = CreateIR(
      []() {
        return std::make_tuple(ReturnValue(2.0_s), OutputArg("a", 7_s), OutputArg("b", 1_s));
      },
      "func");
  CheckExpressions(expected_expressions, ir);
  CheckExpressions(expected_expressions, OutputIr{std::move(ir)});
}

TEST(IrTest, TestScalarExpressions1) {
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x, Expr y) {
        Expr a = x * y;
        Expr b = x + y;
        return std::make_tuple(ReturnValue(x * y), OutputArg("a", a), OutputArg("b", b));
      },
      "func", Arg("x"), Arg("y"));
  ASSERT_EQ(2, ir.NumOperations()) << ir;
  ASSERT_EQ(0, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  ASSERT_EQ(2, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(0, output_ir.NumConditionals()) << output_ir;
  ASSERT_EQ(1, output_ir.NumBlocks()) << output_ir;
  CheckExpressions(expected_expressions, output_ir);
}

TEST(IrTest, TestScalarExpressions2) {
  // Create an optional output:
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x, Expr y, Expr z) {
        Expr f = x * y * sin(z * x) + 5;
        Expr g = cos(z * x) * x * y - log(y - z * 2.1) * 3;
        return std::make_tuple(OutputArg("f", f), OptionalOutputArg("g", g));
      },
      "func", Arg("x"), Arg("y"), Arg("z"));

  ASSERT_EQ(14, ir.NumOperations()) << ir;
  ASSERT_EQ(0, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  ASSERT_EQ(15, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(1, output_ir.NumConditionals()) << output_ir;
  ASSERT_EQ(3, output_ir.NumBlocks()) << output_ir;
  CheckExpressions(expected_expressions, output_ir);
}

TEST(IrTest, TestScalarExpressions3) {
  auto [expected_expressions, ir] = CreateIR(
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

  ASSERT_EQ(43, ir.NumOperations()) << ir;
  ASSERT_EQ(0, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);
  CheckExpressions(expected_expressions, OutputIr{std::move(ir)});
}

TEST(IrTest, TestConditionals1) {
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x) {
        // heaviside step function:
        return where(x > 0, 1, 0);
      },
      "func", Arg("x"));

  ASSERT_EQ(4, ir.NumOperations()) << ir;
  ASSERT_EQ(1, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  ASSERT_EQ(4, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(1, output_ir.NumConditionals()) << output_ir;
  CheckExpressions(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals2) {
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x, Expr y) {
        // use the condition in one of the branches:
        Expr condition = x > y;
        return where(condition, condition * 2, cos(y - 2));
      },
      "func", Arg("x"), Arg("y"));

  ASSERT_EQ(8, ir.NumOperations()) << ir;
  ASSERT_EQ(1, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  ASSERT_EQ(8, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(1, output_ir.NumConditionals()) << output_ir;
  CheckExpressions(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals3) {
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x, Expr y) {
        // exclusive or
        return where(x > 0, where(y > 0, 0, 1), where(y > 0, 1, 0));
      },
      "func", Arg("x"), Arg("y"));

  ASSERT_EQ(7, ir.NumOperations()) << ir;
  ASSERT_EQ(3, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  ASSERT_EQ(7, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(3, output_ir.NumConditionals()) << output_ir;
  CheckExpressions(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals4) {
  // Nested conditionals:
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x, Expr y, Expr z) {
        Expr p = where(x > 0, cos(x), sin(z));
        Expr q = where(y < -5, -log(p), tan(x + y));
        Expr l = where(pow(z, 2) < y, q, q - p);
        return l * 2;
      },
      "func", Arg("x"), Arg("y"), Arg("z"));

  ASSERT_EQ(20, ir.NumOperations()) << ir;
  ASSERT_EQ(3, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  ASSERT_EQ(20, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(3, output_ir.NumConditionals()) << output_ir;
  CheckExpressions(expected_expressions, output_ir);
}

TEST(IrTest, TestConditionals5) {
  // Optional outputs and conditionals:
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x, Expr y, Expr z) {
        Expr p = where(x > y, cos(x) * sin(z) - sin(x) * cos(z) * 2, log(z - x) * 23);
        Expr q = where(pow(z, y) < x, p * p, -cos(p) + x);
        Expr f = q;
        Expr g = q.Diff(x);
        Expr h = q.Diff(x, 2);
        return std::make_tuple(ReturnValue(f), OptionalOutputArg("g", g),
                               OptionalOutputArg("h", h));
      },
      "func", Arg("x"), Arg("y"), Arg("z"));

  ASSERT_EQ(56, ir.NumOperations()) << ir;
  ASSERT_EQ(6, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  CheckExpressions(expected_expressions, output_ir);
  ASSERT_EQ(58, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(7, output_ir.NumConditionals()) << output_ir;
}

TEST(IrTest, TestConditionals6) {
  // Create nested conditionals several layers deep:
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x, Expr y, Expr z, Expr w) {
        Expr f = where(x > 0, x, 1 - x);
        f = where(y > 0, f * y, f * (1 - y));
        f = where(z > 0, f * z, f * (1 - z));
        return where(w > 0, f * w, f * (1 - w));
      },
      "func", Arg("x"), Arg("y"), Arg("z"), Arg("w"));

  ASSERT_EQ(25, ir.NumOperations()) << ir;
  ASSERT_EQ(4, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  CheckExpressions(expected_expressions, output_ir);
  ASSERT_EQ(25, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(4, output_ir.NumConditionals()) << output_ir;
}

TEST(IrTest, TestMatrixExpressions1) {
  // Create a matrix output:
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x, const ta::StaticMatrix<2, 1>& v) {
        using namespace matrix_operator_overloads;
        auto v_outer = v * v.Transpose();
        ta::StaticMatrix<2, 2> m{static_cast<MatrixExpr>(v_outer * x)};
        return m;
      },
      "func", Arg("x"), Arg("y"));

  ASSERT_EQ(7, ir.NumOperations()) << ir;
  ASSERT_EQ(0, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  ASSERT_EQ(7, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(0, output_ir.NumConditionals()) << output_ir;
  CheckExpressions(expected_expressions, output_ir);
}

TEST(IrTest, TestMatrixExpressions2) {
  // Construct a matrix w/ a repeated conditional:
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x, Expr y, Expr z) {
        std::vector<Expr> expressions{};
        for (int i = 0; i < 16; ++i) {
          expressions.push_back(where(x > 0, pow(y, i), pow(z, 16 - z)));
        }
        return ta::StaticMatrix<4, 4>{MatrixExpr::Create(4, 4, std::move(expressions))};
      },
      "func", Arg("x"), Arg("y"), Arg("z"));

  ASSERT_EQ(52, ir.NumOperations()) << ir;
  ASSERT_EQ(16, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  // Conditionals should get reduced:
  OutputIr output_ir{std::move(ir)};
  ASSERT_EQ(52, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(1, output_ir.NumConditionals()) << output_ir;
  CheckExpressions(expected_expressions, output_ir);
}

TEST(IrTest, TestMatrixExpressions3) {
  // Create matrices with conditionals and optional output arguments:
  auto [expected_expressions, ir] = CreateIR(
      [](const ta::StaticMatrix<2, 1>& v, const ta::StaticMatrix<3, 3>& u,
         const ta::StaticMatrix<3, 1>& t) {
        using namespace matrix_operator_overloads;
        auto I3 = Identity(3);
        auto zeros = static_cast<Expr>(Zeros(2, 3));
        Expr f = where(v[0] - t[1] > 0, v * t.Transpose() * (u - I3), zeros);

        auto path_1 = u * t * t.Transpose();
        auto path_2 = (u - I3) * (u - I3).Transpose();
        MatrixExpr g{where(u(1, 1) < -v[1], path_1, path_2)};

        return std::make_tuple(ReturnValue(ta::StaticMatrix<2, 3>{static_cast<MatrixExpr>(f)}),
                               OptionalOutputArg("g", ta::StaticMatrix<3, 3>{g}));
      },
      "func", Arg("v"), Arg("u"), Arg("t"));

  ASSERT_EQ(116, ir.NumOperations()) << ir;
  ASSERT_EQ(15, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  CheckExpressions(expected_expressions, output_ir);
  ASSERT_EQ(117, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(3, output_ir.NumConditionals()) << output_ir;
}

TEST(IrTest, TestBuiltInFunctions) {
  // create expressions that use all the built-in functions
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x, Expr y, Expr z) {
        Expr g = acos(x - log(y));
        Expr h = asin(2.0 * tan(y) - abs(z));
        return atan2(abs(x) + cos(y) + signum(y), -sin(y) + sqrt(z) - signum(x));
      },
      "func", Arg("x"), Arg("y"), Arg("z"));

  ASSERT_EQ(14, ir.NumOperations()) << ir;
  ASSERT_EQ(0, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  CheckExpressions(expected_expressions, output_ir);
  ASSERT_EQ(14, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(0, output_ir.NumConditionals()) << output_ir;
}

}  // namespace math
