// Copyright 2023 Gareth Cross
#include <filesystem>
#include <fstream>

#include "wf/code_generation/cpp_code_generator.h"

#include "quat_interpolation_expressions.h"
#include "wf_test_support/code_gen_helpers.h"

int main() {
  using namespace math;
  std::string code =
      "// Machine generated code.\n#include <cmath>\n\n#include <wf_runtime/span.h>\n\n";

  code += "namespace gen {\n\n";

  cpp_code_generator gen{};
  generate_func(gen, code, &quaternion_interpolation, "quaternion_interpolation", "q0", "q1",
                "alpha");

  code += "} // namespace gen";

  std::ofstream output{"generated.h"};
  output << code;
  output.flush();
  return 0;
}
