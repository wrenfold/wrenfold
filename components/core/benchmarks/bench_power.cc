// Copyright 2024 Gareth Cross
// Benchmark pow() simplifications.
#include <benchmark/benchmark.h>

#include <random>

#include "wf/expression.h"
#include "wf/functions.h"

namespace wf {
using namespace custom_literals;

// Benchmark creating a power of a variable.
static void BM_PowCreateVariable(benchmark::State& state) {
  const scalar_expr x{"x"};
  for (auto _ : state) {
    scalar_expr output = pow(x, 2);
    benchmark::DoNotOptimize(output);
  }
}
BENCHMARK(BM_PowCreateVariable)->Iterations(500000)->Unit(benchmark::kNanosecond);

// Benchmark combining nested powers with integer exponents.
static void BM_PowCreateCombineIntegers(benchmark::State& state) {
  const scalar_expr x{"x"};
  for (auto _ : state) {
    scalar_expr output = pow(pow(pow(x, 2), 2), 3);
    benchmark::DoNotOptimize(output);
  }
}
BENCHMARK(BM_PowCreateCombineIntegers)->Iterations(500000)->Unit(benchmark::kNanosecond);

// Benchmark computing pow(int, rational)
static void BM_PowIntegerRational(benchmark::State& state) {
  std::default_random_engine engine{42};  // NOLINT(*-msc51-cpp)
  for (auto _ : state) {
    state.PauseTiming();
    scalar_expr x = 1;
    for (const auto base : {2, 3, 5, 7, 11, 13}) {
      x = x * pow(base, std::uniform_int_distribution{1, 4}(engine));
    }
    state.ResumeTiming();
    scalar_expr output = pow(x, 3_s / 5);
    benchmark::DoNotOptimize(output);
  }
}
BENCHMARK(BM_PowIntegerRational)->Iterations(100000)->Unit(benchmark::kMicrosecond);

// Benchmark distributing the pow() operation over multiplied terms.
static void BM_PowDistribute(benchmark::State& state) {
  const scalar_expr x{"x", number_set::real_non_negative};
  const scalar_expr y{"y", number_set::real};
  const scalar_expr z{"z", number_set::unknown};
  const scalar_expr f = x * abs(y) * z * pow(y, 2) * 3_s / 2;

  for (auto _ : state) {
    scalar_expr output = pow(f, 1_s / 2);
    benchmark::DoNotOptimize(output);
  }
}
BENCHMARK(BM_PowDistribute)->Iterations(100000)->Unit(benchmark::kMicrosecond);

}  // namespace wf

BENCHMARK_MAIN();
