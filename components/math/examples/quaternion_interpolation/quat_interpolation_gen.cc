// Copyright 2023 Gareth Cross
#include <filesystem>
#include <fstream>

#include "code_gen_helpers.h"
#include "code_generation/cpp_code_generator.h"
#include "quat_interpolation_expressions.h"

int main() {
  using namespace math;
  std::string code = "// Machine generated code.\n#include <cmath>\n\n#include <span.h>\n\n";

  code += "namespace gen {\n\n";

  CppCodeGenerator gen{};
  generate_func(gen, code, &quaternion_interpolation, "quaternion_interpolation", "q0", "q1",
                "alpha");

  code += "} // namespace gen";

  std::ofstream output{"generated.h"};
  output << code;
  output.flush();
  return 0;
}
