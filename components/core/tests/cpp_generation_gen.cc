// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <fstream>

#include "wf/code_generation/cpp_code_generator.h"

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
    if (custom.is_native_type<symbolic::MixedNumerics>()) {
      return "wf::numeric::MixedNumerics";
    }
    if (custom.is_native_type<symbolic::FancyAggregateType>()) {
      return "wf::numeric::FancyAggregateType";
    }
    return cpp_code_generator::operator()(custom);
  }

  std::string operator()(const matrix_type& mat) const override {
    return fmt::format("Eigen::Matrix<Scalar, {}, {}>", mat.rows(), mat.cols());
  }

  // ... And how they are constructed:
  std::string operator()(const ast::construct_matrix& construct) const override {
    const std::string args = join(", ", construct.args, *this);
    return fmt::format("(Eigen::Matrix<Scalar, {}, {}>() << {}).finished()", construct.type.rows(),
                       construct.type.cols(), args);
  }
};

}  // namespace wf

int main() {
  using namespace wf;

  const custom_cpp_code_generator generator{cpp_matrix_type_behavior::generic_span};
  const std::string code = generator.apply_preamble(generate_test_expressions(generator), "gen");

  std::ofstream output{GENERATOR_OUTPUT_FILE, std::ios::binary | std::ios::out};
  output.write(code.data(), code.size());
  output.flush();
  WF_ASSERT(output.good());
  return 0;
}
