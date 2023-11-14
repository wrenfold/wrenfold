// Benchmark of creating IR for some modest expressions.
#include <benchmark/benchmark.h>

#include "code_generation/ir_builder.h"
#include "expression.h"
#include "function_evaluator.h"
#include "geometry/quaternion.h"
#include "output_annotations.h"
#include "type_annotations.h"

namespace math {
using namespace matrix_operator_overloads;
namespace ta = type_annotations;

// TODO: De-dup with quat_interpolation_expressions example.
auto quaternion_interpolation(ta::StaticMatrix<4, 1> q0_vec, ta::StaticMatrix<4, 1> q1_vec,
                              Expr alpha) {
  using namespace matrix_operator_overloads;

  const Quaternion q0 = Quaternion::from_vector_xyzw(q0_vec);
  const Quaternion q1 = Quaternion::from_vector_xyzw(q1_vec);
  const MatrixExpr q_delta_tangent = (q0.conjugate() * q1).to_rotation_vector(1.0e-16);
  const Quaternion q_interp =
      q0 * Quaternion::from_rotation_vector(q_delta_tangent * alpha, 1.0e-16);

  ta::StaticMatrix<3, 3> D_q0 = q_interp.right_local_coordinates_derivative() *
                                q_interp.to_vector_wxyz().jacobian(q0.to_vector_wxyz()) *
                                q0.right_retract_derivative();
  ta::StaticMatrix<3, 3> D_q1 = q_interp.right_local_coordinates_derivative() *
                                q_interp.to_vector_wxyz().jacobian(q1.to_vector_wxyz()) *
                                q1.right_retract_derivative();

  return std::make_tuple(OutputArg("q_out", ta::StaticMatrix<4, 1>(q_interp.to_vector_xyzw())),
                         OptionalOutputArg("D_q0", std::move(D_q0)),
                         OptionalOutputArg("D_q1", std::move(D_q1)));
}

// Benchmark interpolation between two quaternions and then computing the jacobian.
static void BM_CreateFlatIrMediumComplexity(benchmark::State& state) {
  auto tuple = build_function_description(&quaternion_interpolation, "quaternion_interpolation",
                                          Arg("q0"), Arg("q1"), Arg("alpha"));
  const std::vector<ExpressionGroup>& expressions = std::get<1>(tuple);

  for (auto _ : state) {
    FlatIr flat_ir{expressions};
    flat_ir.eliminate_duplicates();
    benchmark::DoNotOptimize(flat_ir);
  }
}

BENCHMARK(BM_CreateFlatIrMediumComplexity)->Iterations(20)->Unit(benchmark::kMillisecond);

}  // namespace math

BENCHMARK_MAIN();
