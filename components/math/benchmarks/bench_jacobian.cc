// Benchmark computing jacobian of a complex expression.
#include <benchmark/benchmark.h>

#include "expression.h"
#include "geometry/quaternion.h"

namespace math {
using namespace matrix_operator_overloads;

// Benchmark interpolation between two quaternions and then computing the jacobian.
static void BM_QuaternionInterpolateJacobian(benchmark::State& state) {
  const Quaternion q0 = Quaternion::from_name_prefix("q0");
  const Quaternion q1 = Quaternion::from_name_prefix("q1");
  const Expr alpha{"alpha"};
  const MatrixExpr q_delta_tangent = (q0.conjugate() * q1).to_rotation_vector(1.0e-16);
  const Quaternion q_interp =
      q0 * Quaternion::from_rotation_vector(q_delta_tangent * alpha, 1.0e-16);

  for (auto _ : state) {
    MatrixExpr output = q_interp.to_vector_wxyz().jacobian(q0.to_vector_wxyz());
    benchmark::DoNotOptimize(output);
  }
}

BENCHMARK(BM_QuaternionInterpolateJacobian)->Iterations(10)->Unit(benchmark::kMillisecond);

}  // namespace math

BENCHMARK_MAIN();
