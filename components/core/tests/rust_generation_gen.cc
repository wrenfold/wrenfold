// Copyright 2023 Gareth Cross
#include <fstream>
#include <tuple>

#include "wf/code_generation/rust_code_generator.h"

#include "test_expressions.h"
#include "wf_test_support/code_gen_helpers.h"

class custom_rust_code_generator final : public wf::rust_code_generator {
 public:
  using rust_code_generator::rust_code_generator;
  using rust_code_generator::operator();

  std::string operator()(const wf::custom_type& custom) const override {
    if (custom.is_native_type<wf::symbolic::Point2d>()) {
      return "types::Point2d";
    }
    if (custom.is_native_type<wf::symbolic::Circle>()) {
      return "types::Circle";
    }
    return rust_code_generator::operator()(custom);
  }

  std::string operator()(const wf::ast::get_field& get) const override {
    if (get.type.is_native_type<wf::symbolic::Point2d>()) {
      // Customize access to the x/y fields so they use operators:
      return fmt::format("{}.{}()", make_view(get.arg), get.field);
    }
    return rust_code_generator::operator()(get);
  }

  std::string operator()(const wf::ast::construct_custom_type& construct) const override {
    if (construct.type.is_native_type<wf::symbolic::Point2d>()) {
      WF_ASSERT_EQUAL(2, construct.field_values.size());
      const auto& [_, x] = construct.field_values[0];
      const auto& [__, y] = construct.field_values[1];
      return fmt::format("{}::new({}, {})", make_view(construct.type), make_view(x), make_view(y));
    }
    return rust_code_generator::operator()(construct);
  }
};

int main() {
  using namespace wf;
  std::string code = "// Machine generated code.\n\n";

  custom_rust_code_generator gen{};
  generate_func(gen, code, &simple_multiply_add, "simple_multiply_add", "x", "y", "z");
  generate_func(
      gen, code,
      [](Expr theta, ta::static_matrix<2, 1> v) {
        auto [v_rot, v_rot_D_theta] = vector_rotation_2d(theta, v);
        return std::make_tuple(v_rot.to_output_arg("v_rot"), std::move(v_rot_D_theta));
      },
      "vector_rotation_2d", "theta", "v");
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

  std::ofstream output{GENERATOR_OUTPUT_FILE};
  output << code;
  output.flush();
  return 0;
}
