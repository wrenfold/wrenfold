// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
// Benchmark factorizer.
#include <benchmark/benchmark.h>

#include "wf/code_generation/factorizer.h"

namespace wf {

static void BM_Factorizer(benchmark::State& state) {
  const std::vector<std::vector<std::uint32_t>> terms = {
      {5, 6, 7, 8, 0, 14, 3}, {5, 7, 8, 9, 0, 14, 3}, {5, 7, 8, 0, 14, 2, 3},
      {4, 5, 6, 1, 14, 3},    {4, 5, 9, 1, 14, 3},    {4, 5, 1, 14, 2, 3},
      {5, 8, 11, 12, 3},      {5, 8, 11, 13, 3},      {5, 8, 10, 11, 3},
  };

  for (auto _ : state) {
    auto factorizations = compute_factorizations(terms, 15);
    benchmark::DoNotOptimize(factorizations);
  }
}

BENCHMARK(BM_Factorizer)->Iterations(10000)->Unit(benchmark::kMicrosecond);

}  // namespace wf

BENCHMARK_MAIN();
