// Copyright 2023 Gareth Cross
#include <filesystem>
#include <fstream>

#include "wf/code_generation/cpp_code_generator.h"
#include "wf_test_support/code_gen_helpers.h"

#include "test_expressions.h"

namespace wf {

class custom_cpp_code_generator final : public cpp_code_generator {
 public:
  using cpp_code_generator::cpp_code_generator;
  using cpp_code_generator::operator();

  std::string operator()(const ast::call_custom_function& func) const override {
    return fmt::format("test::{}({})", func.function.name(), join(*this, ", ", func.args));
  }

  std::string operator()(const custom_type& custom) const override {
    if (custom.is_native_type<symbolic::Point2d>()) {
      return "wf::numeric::Point2d";
    }
    if (custom.is_native_type<symbolic::Circle>()) {
      return "wf::numeric::Circle";
    }
    return cpp_code_generator::operator()(custom);
  }

  // We need to specify how matrices are declared.
  std::string operator()(const ast::declaration_type_annotation& decl) const override {
    if (const matrix_type* mat = std::get_if<matrix_type>(&decl.type); mat != nullptr) {
      return fmt::format("Eigen::Matrix<Scalar, {}, {}>", mat->rows(), mat->cols());
    } else if (const custom_type* custom = std::get_if<custom_type>(&decl.type);
               custom != nullptr) {
      return operator()(*custom);
    }
    return cpp_code_generator::operator()(decl);
  }

  // ... And how they are constructed:
  std::string operator()(const ast::construct_matrix& construct) const override {
    const std::string args = join(*this, ", ", construct.args);
    return fmt::format("(Eigen::Matrix<Scalar, {}, {}>() << {}).finished()", construct.type.rows(),
                       construct.type.cols(), args);
  }

  // ... And returned.
  std::string operator()(const ast::return_type_annotation& ret) const override {
    if (ret.type) {
      if (const matrix_type* mat = std::get_if<matrix_type>(&ret.type.value()); mat != nullptr) {
        return fmt::format("Eigen::Matrix<Scalar, {}, {}>", mat->rows(), mat->cols());
      }
    }
    return cpp_code_generator::operator()(ret);
  }
};

}  // namespace wf

int main() {
  using namespace wf;
  std::string code =
      "// Machine generated code.\n#pragma once\n#include <cmath>\n\n#include "
      "<wf_runtime/span.h>\n\n";

  code += "namespace gen {\n\n";

  custom_cpp_code_generator gen{};
  generate_func(gen, code, &simple_multiply_add, "simple_multiply_add", arg("x"), arg("y"),
                arg("z"));
  generate_func(gen, code, &vector_rotation_2d, "vector_rotation_2d", arg("theta"), arg("v"));
  generate_func(gen, code, &vector_norm_3d, "vector_norm_3d", arg("v"));
  generate_func(gen, code, &heaviside, "heaviside", arg("x"));
  generate_func(gen, code, &exclusive_or, "exclusive_or", arg("x"), arg("y"));
  generate_func(gen, code, &signum_and_abs, "signum_and_abs", arg("x"));
  generate_func(gen, code, &atan2_with_derivatives, "atan2_with_derivatives", arg("y"), arg("x"));
  generate_func(gen, code, &nested_conditionals_1, "nested_conditionals_1", arg("x"), arg("y"));
  generate_func(gen, code, &nested_conditionals_2, "nested_conditionals_2", arg("x"), arg("y"));
  generate_func(gen, code, &create_rotation_matrix, "create_rotation_matrix", arg("w"));
  generate_func(gen, code, &rotation_vector_from_matrix, "rotation_vector_from_matrix", arg("R"));
  generate_func(gen, code, &no_required_outputs, "no_required_outputs", arg("x"));
  generate_func(gen, code, &custom_type_1, "custom_type_1", arg("p"));
  generate_func(gen, code, &custom_type_2, "custom_type_2", arg("theta"), arg("radius"));
  generate_func(gen, code, &nested_custom_type_1, "nested_custom_type_1", arg("c"), arg("p"));
  generate_func(gen, code, &custom_function_call_1, "custom_function_call_1", arg("x"), arg("y"));
  generate_func(gen, code, &custom_function_call_2, "custom_function_call_2", arg("u"), arg("v"));
  generate_func(gen, code, &custom_function_call_3, "custom_function_call_3", arg("x"), arg("v"));
  generate_func(gen, code, &custom_function_call_4, "custom_function_call_4", arg("a"), arg("b"));
  generate_func(gen, code, &custom_function_call_5, "custom_function_call_5", arg("c"), arg("x"),
                arg("y"));

  code += "} // namespace gen";

  std::ofstream output{GENERATOR_OUTPUT_FILE};
  output << code;
  output.flush();
  return 0;
}
