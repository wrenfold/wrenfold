// Copyright 2023 Gareth Cross
#include <fstream>

#include "wf/code_generation/rust_code_generator.h"

#include "test_expressions_codegen.h"
#include "wf_test_support/code_gen_helpers.h"

namespace wf {

class custom_rust_code_generator final : public rust_code_generator {
 public:
  using rust_code_generator::rust_code_generator;
  using rust_code_generator::operator();

  std::string operator()(const wf::custom_type& custom) const override {
    if (custom.is_native_type<wf::symbolic::Point2d>()) {
      return "crate::types::Point2d";
    }
    if (custom.is_native_type<wf::symbolic::Circle>()) {
      return "crate::types::Circle";
    }
    return rust_code_generator::operator()(custom);
  }

  std::string operator()(const ast::call_external_function& func) const override {
    return "crate::external_functions::" + rust_code_generator::operator()(func);
  }

  std::string operator()(const ast::get_field& get) const override {
    if (get.type.is_native_type<symbolic::Point2d>()) {
      // Customize access to the x/y fields so they use operators:
      return fmt::format("{}.{}()", make_view(get.arg), get.field);
    }
    return rust_code_generator::operator()(get);
  }

  std::string operator()(const ast::construct_custom_type& construct) const override {
    if (construct.type.is_native_type<symbolic::Point2d>()) {
      WF_ASSERT_EQUAL(2, construct.field_values.size());
      const auto& [_, x] = construct.field_values[0];
      const auto& [__, y] = construct.field_values[1];
      return fmt::format("{}::new({}, {})", make_view(construct.type), make_view(x), make_view(y));
    }
    return rust_code_generator::operator()(construct);
  }

  std::string operator()(const ast::declaration_type_annotation& decl) const override {
    if (const matrix_type* mat = std::get_if<matrix_type>(&decl.type); mat != nullptr) {
      return fmt::format("nalgebra::SMatrix<f64, {}, {}>", mat->rows(), mat->cols());
    } else if (const custom_type* custom = std::get_if<custom_type>(&decl.type);
               custom != nullptr) {
      return operator()(*custom);
    }
    return rust_code_generator::operator()(decl);
  }

  std::string operator()(const ast::construct_matrix& construct) const override {
    const std::string args = join(*this, ", ", construct.args);
    return fmt::format("nalgebra::SMatrix::<f64, {}, {}>::new({})", construct.type.rows(),
                       construct.type.cols(), args);
  }

  std::string operator()(const ast::return_type_annotation& ret) const override {
    if (ret.type) {
      if (const matrix_type* mat = std::get_if<matrix_type>(&ret.type.value()); mat != nullptr) {
        return fmt::format("nalgebra::SMatrix<f64, {}, {}>", mat->rows(), mat->cols());
      }
    }
    return rust_code_generator::operator()(ret);
  }
};

}  // namespace wf

int main() {
  using namespace wf;
  const std::string code = "//! Machine generated code.\n#![cfg_attr(rustfmt, rustfmt_skip)]\n\n" +
                           generate_test_expressions(custom_rust_code_generator{});

  // Write in binary to stop windows from turning LF into CRLF.
  std::ofstream output{GENERATOR_OUTPUT_FILE, std::ios::binary | std::ios::out};
  output.write(code.data(), code.size());
  output.flush();
  WF_ASSERT(output.good());
  return 0;
}
