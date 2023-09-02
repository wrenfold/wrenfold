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
        scatter_.emplace(it->key.arg_position, scatter_.size());
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
  std::unordered_map<std::size_t, bool> GetPermutation(std::size_t n) const {
    ASSERT_LESS(n, NumPermutations());
    const std::bitset<MaxOptionalArgs> permutation{n};

    std::unordered_map<std::size_t, bool> output{};
    output.reserve(scatter_.size());
    for (const auto [arg_index, bitfield_index] : scatter_) {
      output.emplace(arg_index, permutation[bitfield_index]);
    }
    return output;
  }

 private:
  // Map from arg index to bit index in a permutation.
  std::unordered_map<std::size_t, std::size_t> scatter_;
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
                       group.key.arg_position)
        << ir;

    ASSERT_EQ(group.expressions.size(), it->second.size()) << ir;

    for (std::size_t i = 0; i < group.expressions.size(); ++i) {
      ASSERT_IDENTICAL(group.expressions[i], it->second[i])
          << fmt::format("Key: ({}, {}), i: {}\n", StringFromExpressionUsage(group.key.usage),
                         group.key.arg_position, i)
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
      [](Expr& a, Expr& b) {
        a = 7_s;
        b = 1_s;
        return 2.0_s;
      },
      "func", Arg("a"), Arg("b", true));
  CheckExpressions(expected_expressions, ir);
  CheckExpressions(expected_expressions, OutputIr{std::move(ir)});
}

TEST(IrTest, TestScalarExpressions1) {
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x, Expr y, Expr& a, Expr& b) {
        a = x * y;
        b = x + y;
        return x * y;
      },
      "func", Arg("x"), Arg("y"), Arg("a"), Arg("b"));
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
      [](Expr x, Expr y, Expr z, Expr& f, Expr& g) {
        f = x * y * sin(z * x) + 5;
        g = cos(z * x) * x * y - log(y - z * 2.1) * 3;
      },
      "func", Arg("x"), Arg("y"), Arg("z"), Arg("f"), Arg("g", true));

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
      [](Expr x, Expr y, Expr z, Expr& f) {
        // Chain together a few operations (this doesn't reduce at all).
        f = Constants::One;
        for (int i = 0; i < 20; ++i) {
          if (i & 1) {
            f = (f + x) * y;
          } else {
            f = (f - y) * z;
          }
        }
      },
      "func", Arg("x"), Arg("y"), Arg("z"), Arg("f"));

  ASSERT_EQ(43, ir.NumOperations()) << ir;
  ASSERT_EQ(0, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);
  CheckExpressions(expected_expressions, OutputIr{std::move(ir)});
}

TEST(IrTest, TestConditionals1) {
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x, Expr& f) {
        // heaviside step function:
        f = where(x > 0, 1, 0);
      },
      "func", Arg("x"), Arg("f"));

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
      [](Expr x, Expr y, Expr& f) {
        // use the condition in one of the branches:
        Expr condition = x > y;
        f = where(condition, condition * 2, cos(y - 2));
      },
      "func", Arg("x"), Arg("y"), Arg("f"));

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
      [](Expr x, Expr y, Expr& f) {
        // exclusive or
        f = where(x > 0, where(y > 0, 0, 1), where(y > 0, 1, 0));
      },
      "func", Arg("x"), Arg("y"), Arg("f"));

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
      [](Expr x, Expr y, Expr z, Expr& f) {
        Expr p = where(x > 0, cos(x), sin(z));
        Expr q = where(y < -5, -log(p), tan(x + y));
        Expr l = where(pow(z, 2) < y, q, q - p);
        f = l * 2;
      },
      "func", Arg("x"), Arg("y"), Arg("z"), Arg("f"));

  ASSERT_EQ(20, ir.NumOperations()) << ir;
  ASSERT_EQ(3, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  ASSERT_EQ(20, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(3, output_ir.NumConditionals()) << output_ir;
  CheckExpressions(expected_expressions, output_ir);

  fmt::print("{}\n", output_ir.ToString());
}

TEST(IrTest, TestConditionals5) {
  // Optional outputs and conditionals:
  auto [expected_expressions, ir] = CreateIR(
      [](Expr x, Expr y, Expr z, Expr& f, Expr& g, Expr& h) {
        Expr p = where(x > y, cos(x) * sin(z) - sin(x) * cos(z) * 2, log(z - x) * 23);
        Expr q = where(pow(z, y) < x, p * p, -cos(p) + x);
        f = q;
        g = q.Diff(x);
        h = q.Diff(x, 2);
      },
      "func", Arg("x"), Arg("y"), Arg("z"), Arg("f"), Arg("g", true), Arg("h", true));

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
      [](Expr x, Expr y, Expr z, Expr w, Expr& f) {
        f = where(x > 0, x, 1 - x);
        f = where(y > 0, f * y, f * (1 - y));
        f = where(z > 0, f * z, f * (1 - z));
        f = where(w > 0, f * w, f * (1 - w));
      },
      "func", Arg("x"), Arg("y"), Arg("z"), Arg("w"), Arg("f"));

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
      [](Expr x, const ta::StaticMatrix<2, 1>& v, ta::StaticMatrix<2, 2>& m) {
        using namespace matrix_operator_overloads;
        auto v_outer = v * v.Transpose();
        m = v_outer * x;
      },
      "func", Arg("x"), Arg("y"), Arg("m"));

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
      [](Expr x, Expr y, Expr z, ta::StaticMatrix<4, 4>& m) {
        std::vector<Expr> expressions{};
        for (int i = 0; i < 16; ++i) {
          expressions.push_back(where(x > 0, pow(y, i), pow(z, 16 - z)));
        }
        m = MatrixExpr::Create(4, 4, std::move(expressions));
      },
      "func", Arg("x"), Arg("y"), Arg("z"), Arg("m"));

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
         const ta::StaticMatrix<3, 1>& t, ta::StaticMatrix<2, 3>& f, ta::StaticMatrix<3, 3>& g) {
        using namespace matrix_operator_overloads;
        auto I3 = Identity(3);
        auto zeros = static_cast<Expr>(Zeros(2, 3));
        f = where(v[0] - t[1] > 0, v * t.Transpose() * (u - I3), zeros);

        auto path_1 = u * t * t.Transpose();
        auto path_2 = (u - I3) * (u - I3).Transpose();
        g = where(u(1, 1) < -v[1], path_1, path_2);
      },
      "func", Arg("v"), Arg("u"), Arg("t"), Arg("f"), Arg("g", true));

  ASSERT_EQ(116, ir.NumOperations()) << ir;
  ASSERT_EQ(15, ir.NumConditionals()) << ir;
  CheckExpressions(expected_expressions, ir);

  OutputIr output_ir{std::move(ir)};
  CheckExpressions(expected_expressions, output_ir);
  ASSERT_EQ(117, output_ir.NumOperations()) << output_ir;
  ASSERT_EQ(3, output_ir.NumConditionals()) << output_ir;
}

}  // namespace math
