#pragma once
#include <cstdint>
#include <vector>

namespace math {

// Perform the wheel factorization algorithm. Finds a prime number that factorizes `n_in`.
// I haven't optimized this much, but in benchmarks it beats trial division.
inline constexpr int64_t WheelFactorization(const int64_t n_in) {
  if (n_in % 2 == 0) {
    return 2;
  } else if (n_in % 3 == 0) {
    return 3;
  } else if (n_in % 5 == 0) {
    return 5;
  }
  int64_t k = 7;
  int64_t i = 0;
  // These are the increments between the first primes: [7, 11, 13, 17, 19, 23, 29, 31]
  constexpr int inc_length = 8;
  constexpr int inc[inc_length] = {4, 2, 4, 2, 4, 6, 2, 6};
  while (k * k <= n_in) {
    if (n_in % k == 0) {
      return k;
    }
    k += inc[i];
    i = (i + 1) % inc_length;
  }
  return n_in;
}

// Result of `ComputePrimeFactors`.
struct PrimeFactor {
  int64_t base;
  int64_t exponent;
};

// Compute the prime factors of `n_in`. TODO: Use small vector here.
inline std::vector<PrimeFactor> ComputePrimeFactors(const int64_t n_in) {
  std::vector<PrimeFactor> result;
  result.reserve(10);
  int64_t x = n_in;
  while (x != 1) {
    const int64_t factor = WheelFactorization(x);
    if (!result.empty() && result.back().base == factor) {
      // This is a repeated factor.
      result.back().exponent += 1;
    } else {
      result.push_back(PrimeFactor{factor, 1});
    }
    x /= factor;
  }
  return result;
}

// Integer power function.
// TODO: Check for overflow.
constexpr int64_t Pow(int64_t base, int64_t exp) {
  int64_t result = 1;
  for (;;) {
    if (exp & 1) {
      result *= base;
    }
    exp /= 2;
    if (exp == 0) {
      break;
    }
    base *= base;
  }
  return result;
}

}  // namespace math