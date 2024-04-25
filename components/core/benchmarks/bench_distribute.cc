// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
// Benchmark distribution.
#include <benchmark/benchmark.h>

#include "wf/expression.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {
using namespace custom_literals;

// Benchmark the distribute operation.
static void BM_Distribute(benchmark::State& state) {
  constexpr int num_multiplied_series = 6;

  // Create a bunch of quadratic terms multiplied together:
  // (c0 * x00^2 + c1 * x00 + c2) * (k0 * x01^2 + k1*x01 + k2) * (...)
  scalar_expr input_mul = 1;
  for (int i = 0; i < num_multiplied_series; ++i) {
    const scalar_expr var{fmt::format("x{:02}", i)};
    const scalar_expr sum = 1_s / (i + 1) * var * var + (i + 1) * var + i * 3;
    input_mul = input_mul * sum;
  }

  for (auto _ : state) {
    scalar_expr output = input_mul.distribute();
    benchmark::DoNotOptimize(output);
  }
}
BENCHMARK(BM_Distribute)->Iterations(2000)->Unit(benchmark::kMillisecond);

}  // namespace wf

BENCHMARK_MAIN();
