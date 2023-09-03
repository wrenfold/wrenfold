// Benchmark addition + multiplication operations.
#include <benchmark/benchmark.h>
#include <fmt/format.h>

#include "expression.h"

namespace math {

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
  }
}
BENCHMARK(BM_ScalarAddition)->Iterations(1000)->Unit(benchmark::kMillisecond);

}  // namespace math

BENCHMARK_MAIN();
