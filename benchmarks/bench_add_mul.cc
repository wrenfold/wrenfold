// Benchmark addition + multiplication operations.
#include <benchmark/benchmark.h>
#include <fmt/format.h>

#include "expression.h"

namespace math {

// Benchmark adding repeated symbols.
static void BM_ScalarAddition(benchmark::State& state) {
  constexpr int num_terms = 100;
  constexpr int num_symbols = 10;

  std::vector<Expr> symbols;
  for (int i = 0; i < num_symbols; ++i) {
    symbols.emplace_back(fmt::format("x{:02}", i));
  }

  for (auto _ : state) {
    Expr output = 0;
    for (int i = 0; i < num_terms; ++i) {
      output = output + symbols[i % num_symbols];
    }
    benchmark::DoNotOptimize(output);
  }
}
BENCHMARK(BM_ScalarAddition)->Iterations(2000)->Unit(benchmark::kMillisecond);

// Benchmark multiplying repeated symbols.
static void BM_ScalarMultiplication(benchmark::State& state) {
  constexpr int num_terms = 100;
  constexpr int num_symbols = 10;

  std::vector<Expr> symbols;
  for (int i = 0; i < num_symbols; ++i) {
    symbols.emplace_back(fmt::format("x{:02}", i));
  }

  for (auto _ : state) {
    Expr output = 1;
    for (int i = 0; i < num_terms; ++i) {
      output = output * symbols[i % num_symbols];
    }
    benchmark::DoNotOptimize(output);
  }
}
BENCHMARK(BM_ScalarMultiplication)->Iterations(2000)->Unit(benchmark::kMillisecond);

}  // namespace math

BENCHMARK_MAIN();
