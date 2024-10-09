// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
// Benchmark factorizer.
#include <benchmark/benchmark.h>
#include <random>

#include "wf/constants.h"
#include "wf/expression.h"
#include "wf/functions.h"

namespace wf {

static void BM_PlainFormatterSmallExpression(benchmark::State& state) {
  const auto [x, y] = make_symbols("x", "y");
  const scalar_expr f =
      cos(constants::pi * x + scalar_expr{3} / 5) * sin(y) - pow(x, 2) * sin(y) + 5;

  for (auto _ : state) {
    std::string text = f.to_string();
    benchmark::DoNotOptimize(text);
  }
}

BENCHMARK(BM_PlainFormatterSmallExpression)->Iterations(10000)->Unit(benchmark::kMicrosecond);

static void BM_PlainFormatterLargeExpression(benchmark::State& state) {
  // Generate a complex expression to format:
  const auto [x, y] = make_symbols("x", "y");

  constexpr int depth = 100;
  std::default_random_engine engine{0};
  scalar_expr expr = x;
  for (int i = 0; i < depth; ++i) {
    switch (std::uniform_int_distribution<int>{0, 9}(engine)) {
      case 0: {
        expr = expr + x;
      } break;
      case 1: {
        expr = expr * y;
      } break;
      case 2: {
        expr = expr * expr;
      } break;
      case 3: {
        expr = cos(expr);
      } break;
      case 4: {
        expr = expr / (1 + y);
      } break;
      case 5: {
        expr = pow(expr, 3);
      } break;
      case 6: {
        expr = expr - 5;
      } break;
      case 7: {
        expr = expr - sin(y);
      } break;
      case 8: {
        expr = expr * x + expr * y;
      } break;
      case 9: {
        expr = expr - 1 / y;
      } break;
    }
  }

  for (auto _ : state) {
    std::string text = expr.to_string();
    benchmark::DoNotOptimize(text);
  }
}

// before: 2726 --> 164
BENCHMARK(BM_PlainFormatterLargeExpression)->Iterations(1000)->Unit(benchmark::kMicrosecond);

}  // namespace wf

BENCHMARK_MAIN();
