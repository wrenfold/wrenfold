// Benchmark addition + multiplication operations.
#include <benchmark/benchmark.h>

#include "wf/expression.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

// Benchmark adding repeated symbols.
static void BM_ScalarAddition(benchmark::State& state) {
  constexpr int num_terms = 100;
  constexpr int num_symbols = 10;

  std::vector<scalar_expr> symbols;
  for (int i = 0; i < num_symbols; ++i) {
    symbols.emplace_back(fmt::format("x{:02}", i));
  }

  for (auto _ : state) {
    scalar_expr output = 0;
    for (int i = 0; i < num_terms; ++i) {
      output = output + symbols[i % num_symbols];
    }
    benchmark::DoNotOptimize(output);
  }
}
BENCHMARK(BM_ScalarAddition)->Iterations(10000)->Unit(benchmark::kMillisecond);

// Benchmark multiplying repeated symbols.
static void BM_ScalarMultiplication(benchmark::State& state) {
  constexpr int num_terms = 100;
  constexpr int num_symbols = 10;

  std::vector<scalar_expr> symbols;
  for (int i = 0; i < num_symbols; ++i) {
    symbols.emplace_back(fmt::format("x{:02}", i));
  }

  for (auto _ : state) {
    scalar_expr output = 1;
    for (int i = 0; i < num_terms; ++i) {
      output = output * symbols[i % num_symbols];
    }
    benchmark::DoNotOptimize(output);
  }
}
BENCHMARK(BM_ScalarMultiplication)->Iterations(10000)->Unit(benchmark::kMillisecond);

}  // namespace wf

BENCHMARK_MAIN();
