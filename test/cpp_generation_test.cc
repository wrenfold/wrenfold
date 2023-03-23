// Copyright 2023 Gareth Cross
#include "code_generation.h"
#include "numeric_testing.h"
#include "test_helpers.h"
#include "type_annotations.h"

#include "test_expressions.h"  //  Symbolic test functions.

#include "generated.h"

template <typename T>
struct fmt::formatter<T, std::enable_if_t<std::is_base_of_v<Eigen::DenseBase<T>, T>, char>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& m, FormatContext& ctx) const -> decltype(ctx.out()) {
    const Eigen::IOFormat heavy(Eigen::FullPrecision, 0, ", ", ";\n", "[", "]", "[", "]");
    std::stringstream ss;
    ss << m.format(heavy);
    return fmt::format_to(ctx.out(), "{}", ss.str());
  }
};

namespace math {

TEST(CppGenerationTest, TestSimpleMultiplyAdd) {
  auto evaluator = CreateEvaluator(&SimpleMultiplyAdd);
  ASSERT_NEAR(evaluator(1.5, -2.2, 0.11), simple_multiply_add(1.5, -2.2, 0.11), 1.0e-15);
  ASSERT_NEAR(evaluator(5.112, -0.01, -4.2), simple_multiply_add(5.112, -0.01, -4.2), 1.0e-15);
}

TEST(CodeGenerationTest, TestCodeGen) {
  //  auto evaluator = CreateEvaluator(&do_stuff);
  //
  //  double out_1;
  //  Eigen::Matrix<double, 2, 2> out_2;
  //  auto result = evaluator(5.0, 2.0, -3.0, 1.2, out_1, out_2);
  //  fmt::print("out_1 = {}\n", out_1);
  //  fmt::print("out_2:\n{}\n", out_2);
  //
  //  fmt::print("return_1 = {}\n", std::get<0>(result));
  //  fmt::print("return_2 = {}\n", std::get<1>(result));
  //
  //  result = do_stuff_gen(5.0, 2.0, -3.0, 1.2, out_1, out_2);
  //  fmt::print("out_1 = {}\n", out_1);
  //  fmt::print("out_2:\n{}\n", out_2);
  //
  //  fmt::print("return_1 = {}\n", std::get<0>(result));
  //  fmt::print("return_2 = {}\n", std::get<1>(result));
}

}  // namespace math
