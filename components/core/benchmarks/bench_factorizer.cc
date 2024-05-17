// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
// Benchmark factorizer.
#include <benchmark/benchmark.h>

#include "wf/code_generation/factorizer.h"

namespace wf {

static void BM_Factorizer1(benchmark::State& state) {
  const auto [terms, num_vars] = create_term_bitsets({
      {5, 6, 7, 8, 0, 14, 3},
      {5, 7, 8, 9, 0, 14, 3},
      {5, 7, 8, 0, 14, 2, 3},
      {4, 5, 6, 1, 14, 3},
      {4, 5, 9, 1, 14, 3},
      {4, 5, 1, 14, 2, 3},
      {5, 8, 11, 12, 3},
      {5, 8, 11, 13, 3},
      {5, 8, 10, 11, 3},
  });

  for (auto _ : state) {
    auto factorizations = compute_ranked_factorizations(terms, num_vars, 4);
    benchmark::DoNotOptimize(factorizations);
  }
}

BENCHMARK(BM_Factorizer1)->Iterations(10000)->Unit(benchmark::kMicrosecond);

static void BM_Factorizer2(benchmark::State& state) {
  const auto [terms, num_vars] = create_term_bitsets({
      {0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
      {0, 1, 3, 4, 5, 6, 7, 9, 10, 11},
      {1, 3, 4, 5, 6, 7, 9, 12, 13, 14},
      {1, 2, 3, 4, 6, 8, 9, 15, 16, 17},
      {1, 2, 3, 8, 15, 16, 18, 19, 20, 21},
      {1, 2, 3, 4, 8, 16, 19, 20, 21, 22, 23},
      {1, 3, 4, 6, 9, 10, 11, 15, 16, 17},
      {1, 3, 10, 11, 15, 16, 18, 19, 20, 21},
      {1, 3, 4, 10, 11, 16, 19, 20, 21, 22, 23},
      {1, 3, 4, 6, 9, 12, 14, 16, 17},
      {1, 3, 12, 14, 16, 18, 19, 20, 21},
      {1, 3, 4, 12, 14, 15, 16, 19, 20, 21, 22, 23},
      {9, 16, 19, 24, 25},
      {1, 3, 4, 6, 10, 15, 16, 19, 20, 24},
  });

  for (auto _ : state) {
    auto factorization = compute_ranked_factorizations(terms, num_vars, 4);
    benchmark::DoNotOptimize(factorization);
  }
}

BENCHMARK(BM_Factorizer2)->Iterations(10000)->Unit(benchmark::kMicrosecond);

}  // namespace wf

BENCHMARK_MAIN();
