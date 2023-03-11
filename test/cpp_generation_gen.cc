// Copyright 2023 Gareth Cross
#include <filesystem>

#include <fmt/format.h>
#include <fmt/os.h>

#include "code_generation.h"
#include "cpp_code_generator.h"
#include "function_evaluator.h"
#include "matrix_functions.h"
#include "type_annotations.h"

#include "config_variables.h"  //  generated from cmake
#include "test_expressions.h"

namespace math {

namespace ta = type_annotations;

std::tuple<Expr, ta::StaticMatrix<2, 2>> do_stuff(Expr w, Expr x, Expr y, Expr z, Expr& f,
                                                  ta::StaticMatrix<2, 2>& m_out) {
  const Expr ret_val = ((w + x) + y * (z - 2) / (x * w)) * ((w + x) + y * (z - 2) + 1) * 5;
  f = y * w * 2;
  m_out = ta::StaticMatrix<2, 2>::Create(w * x, z - 2, w + x, 10.0);
  return std::make_tuple(ret_val, ta::StaticMatrix<2, 2>::Create(w * x + x, z - 2, f, -2.0));
}

template <typename Func, typename... Args>
void GenerateFunc(Func func, const std::string_view name, Args&&... args) {
  const auto [desc, expressions] =
      BuildFunctionDescription(func, name, std::forward<Args>(args)...);

  IrBuilder ir{expressions};
  ir.EliminateDuplicates();
  auto ast = ir.CreateAST(desc);

  CodeGeneratorImpl<CppCodeGenerator> generator(CppCodeGenerator{});
  const std::string code = generator.Generate(desc, ast);

  const std::string code_with_preamble =
      "// Machine generated code.\n#include <cmath>\n#include <optional>\n#include "
      "<tuple>\n\n#include "
      "<Eigen/Core>\n\n" +
      code;

  const std::string path =
      (std::filesystem::path(TEST_OUTPUT_DIR) / fmt::format("{}.h", name)).string();
  fmt::output_file(path).print("{}\n", code_with_preamble);
  fmt::print("Writing to: {}\n", path);
}

Expr simple_multiply(Expr x, Expr y) { return x * y; }

}  // namespace math

int main() {
  using namespace math;
  GenerateFunc(&SimpleMultiplyAdd, "simple_multiply_add", "x", "y", "z");
  //  GenerateFunc(&do_stuff, "do_stuff_gen", "w", "x", "y", "z", Arg("f"), Arg("m_out", true));
  return 0;
}
