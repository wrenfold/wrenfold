// Copyright 2023 Gareth Cross
#include <fstream>

#include "wf/code_generation/rust_code_generator.h"

#include "test_expressions.h"
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

  std::string operator()(const ast::call_custom_function& func) const override {
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
  std::string code = "//! Machine generated code.\n#![cfg_attr(rustfmt, rustfmt_skip)]\n\n";

  custom_rust_code_generator gen{};
  generate_func(gen, code, &simple_multiply_add, "simple_multiply_add", "x", "y", "z");
  generate_func(gen, code, &vector_rotation_2d, "vector_rotation_2d", arg("theta"), arg("v"));
  generate_func(gen, code, &vector_norm_3d, "vector_norm_3d", "v");
  generate_func(gen, code, &heaviside, "heaviside", arg("x"));
  generate_func(gen, code, &exclusive_or, "exclusive_or", arg("x"), arg("y"));
  generate_func(gen, code, &signum_and_abs, "signum_and_abs", arg("x"));
  generate_func(gen, code, &atan2_with_derivatives, "atan2_with_derivatives", arg("y"), arg("x"));
  generate_func(gen, code, &nested_conditionals_1, "nested_conditionals_1", arg("x"), arg("y"));
  generate_func(gen, code, &nested_conditionals_2, "nested_conditionals_2", arg("x"), arg("y"));
  generate_func(gen, code, &create_rotation_matrix, "create_rotation_matrix", arg("w"));
  generate_func(gen, code, &no_required_outputs, "no_required_outputs", arg("x"));
  generate_func(gen, code, &custom_type_1, "custom_type_1", arg("p"));
  generate_func(gen, code, &custom_type_2, "custom_type_2", arg("theta"), arg("radius"));
  generate_func(gen, code, &nested_custom_type_1, "nested_custom_type_1", arg("c"), arg("p"));
  generate_func(gen, code, &custom_function_call_1, "custom_function_call_1", arg("x"), arg("y"));
  generate_func(gen, code, &custom_function_call_2, "custom_function_call_2", arg("x"), arg("v"));
  generate_func(gen, code, &custom_function_call_3, "custom_function_call_3", arg("x"), arg("v"));
  generate_func(gen, code, &custom_function_call_4, "custom_function_call_4", arg("a"), arg("b"));
  generate_func(gen, code, &custom_function_call_5, "custom_function_call_5", arg("c"), arg("x"),
                arg("y"));
  generate_func(gen, code, &custom_function_call_6, "custom_function_call_6", arg("x"), arg("y"));

  // Write in binary to stop windows from turning LF into CRLF.
  std::ofstream output{GENERATOR_OUTPUT_FILE, std::ios::binary | std::ios::out};
  output.write(code.data(), code.size());
  output.flush();
  WF_ASSERT(output.good());
  return 0;
}
