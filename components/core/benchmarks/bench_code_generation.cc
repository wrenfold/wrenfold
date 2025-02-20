// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
// Benchmark of creating IR for some modest expressions.
#include <benchmark/benchmark.h>

#include "wf/code_generation/ast_conversion.h"
#include "wf/code_generation/control_flow_graph.h"
#include "wf/code_generation/cpp_code_generator.h"
#include "wf/code_generation/function_evaluator.h"
#include "wf/expression.h"
#include "wf/geometry/quaternion.h"
#include "wf/output_annotations.h"
#include "wf/type_annotations.h"

namespace wf {
namespace ta = type_annotations;

// TODO: De-dup with quat_interpolation_expressions example.
auto quaternion_interpolation(ta::static_matrix<4, 1> q0_vec, ta::static_matrix<4, 1> q1_vec,
                              scalar_expr alpha) {
  const quaternion q0 = quaternion::from_vector_xyzw(q0_vec);
  const quaternion q1 = quaternion::from_vector_xyzw(q1_vec);
  const matrix_expr q_delta_tangent = (q0.conjugate() * q1).to_rotation_vector(1.0e-16);
  const quaternion q_interp =
      q0 * quaternion::from_rotation_vector(q_delta_tangent * alpha, 1.0e-16);

  ta::static_matrix<3, 3> D_q0 = q_interp.right_local_coordinates_derivative() *
                                 q_interp.jacobian(q0) * q0.right_retract_derivative();
  ta::static_matrix<3, 3> D_q1 = q_interp.right_local_coordinates_derivative() *
                                 q_interp.jacobian(q1) * q1.right_retract_derivative();

  return std::make_tuple(output_arg("q_out", ta::static_matrix<4, 1>(q_interp.to_vector_xyzw())),
                         optional_output_arg("D_q0", std::move(D_q0)),
                         optional_output_arg("D_q1", std::move(D_q1)));
}

// Benchmark interpolation between two quaternions and then computing the jacobian.
static void BM_CreateFlatIr(benchmark::State& state) {
  const function_description description = build_function_description(
      &quaternion_interpolation, "quaternion_interpolation", arg("q0"), arg("q1"), arg("alpha"));

  for (auto _ : state) {
    control_flow_graph flat_ir{description, optimization_params{}};
    benchmark::DoNotOptimize(flat_ir);
  }
}

BENCHMARK(BM_CreateFlatIr)->Iterations(1000)->Unit(benchmark::kMillisecond);

static void BM_SimplifyIr(benchmark::State& state) {
  const function_description description = build_function_description(
      &quaternion_interpolation, "quaternion_interpolation", arg("q0"), arg("q1"), arg("alpha"));

  for (auto _ : state) {
    state.PauseTiming();
    control_flow_graph flat_ir{description, std::nullopt};
    state.ResumeTiming();
    flat_ir.apply_simplifications(optimization_params{false, true});
    benchmark::DoNotOptimize(flat_ir);
  }
}

BENCHMARK(BM_SimplifyIr)->Iterations(200)->Unit(benchmark::kMillisecond);

void BM_ConvertIr(benchmark::State& state) {
  const function_description description = build_function_description(
      &quaternion_interpolation, "quaternion_interpolation", arg("q0"), arg("q1"), arg("alpha"));

  for (auto _ : state) {
    state.PauseTiming();
    control_flow_graph flat_ir{description, optimization_params{}};
    state.ResumeTiming();
    // Convert to the non-flat IR.
    control_flow_graph output_ir = std::move(flat_ir).convert_conditionals_to_control_flow(true);
    benchmark::DoNotOptimize(output_ir);
  }
}

BENCHMARK(BM_ConvertIr)->Iterations(200)->Unit(benchmark::kMillisecond);

void BM_GenerateCpp(benchmark::State& state) {
  const function_description description = build_function_description(
      &quaternion_interpolation, "quaternion_interpolation", arg("q0"), arg("q1"), arg("alpha"));

  const control_flow_graph output_cfg =
      control_flow_graph{description, optimization_params()}.convert_conditionals_to_control_flow(
          true);

  for (auto _ : state) {
    ast::function_definition definition = ast::create_ast(output_cfg, description);
    std::string code =
        std::invoke(cpp_code_generator{cpp_matrix_type_behavior::generic_span}, definition);
    benchmark::DoNotOptimize(code);
  }
}

BENCHMARK(BM_GenerateCpp)->Iterations(200)->Unit(benchmark::kMillisecond);

}  // namespace wf

BENCHMARK_MAIN();
