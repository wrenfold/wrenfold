// Copyright 2023 Gareth Cross
#include <filesystem>
#include <fstream>

#include "wf/code_generation/cpp_code_generator.h"
#include "wf_test_support/code_gen_helpers.h"

#include "test_expressions_codegen.h"

namespace wf {

class custom_cpp_code_generator final : public cpp_code_generator {
 public:
  using cpp_code_generator::cpp_code_generator;
  using cpp_code_generator::operator();

  std::string operator()(const ast::call_external_function& func) const override {
    return fmt::format("test::{}({})", func.function.name(), join(", ", func.args, *this));
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
    const std::string args = join(", ", construct.args, *this);
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
      "<wrenfold/span.h>\n\n";

  code += "namespace gen {\n\n";
  code += generate_test_expressions(custom_cpp_code_generator{});
  code += "} // namespace gen";

  std::ofstream output{GENERATOR_OUTPUT_FILE, std::ios::binary | std::ios::out};
  output.write(code.data(), code.size());
  output.flush();
  WF_ASSERT(output.good());
  return 0;
}
