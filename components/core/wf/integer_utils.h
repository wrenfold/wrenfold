// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <cstdint>
#include <optional>
#include <vector>

#include "wf/utility/checked_int.h"
#include "wf/utility/ordering.h"

namespace wf {

// Perform the wheel factorization algorithm. Finds a prime number that factorizes `n_in`.
// I haven't optimized this much, but in benchmarks it beats trial division.
constexpr int64_t wheel_factorization(const int64_t n_in) noexcept {
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

// Result of `compute_prime_factors`.
struct prime_factor {
  checked_int base;
  checked_int exponent;
};

// Compute the prime factors of `n_in`. TODO: Use small vector here.
inline std::vector<prime_factor> compute_prime_factors(const checked_int n_in) {
  std::vector<prime_factor> result;
  result.reserve(10);
  checked_int x = n_in;
  if (x == 0) {
    return result;
  } else if (x < 0) {
    result.push_back(prime_factor{-1, 1});
    x = -x;
  }
  while (x != 1) {
    const int64_t factor = wheel_factorization(static_cast<std::int64_t>(x));
    if (!result.empty() && result.back().base == factor) {
      // This is a repeated factor.
      result.back().exponent += 1;
    } else {
      result.push_back(prime_factor{factor, 1});
    }
    x /= factor;
  }
  return result;
}

// Integer power function.
constexpr checked_int integer_power(checked_int base, std::uint64_t exp) {
  checked_int result = 1;
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

// Integer power with `checked_int` exponent. Sign of `exp` is checked and domain_error will be
// thrown.
constexpr checked_int integer_power(const checked_int base, const checked_int exp) {
  return integer_power(base, static_cast<std::uint64_t>(exp));
}

// Compare integral and floating point value.
// Based on: https://stackoverflow.com/questions/58734034
// Simplified to only support signed integer, and cases where range of `F` exceeds that of `I`.
// You can compare int64_t and f64, for example. You cannot compare int64_t and f32.
template <typename I, typename F>
constexpr std::optional<relative_order> compare_int_float(const I i, const F f) {
  static_assert(std::is_integral_v<I> && std::is_signed_v<I>, "First argument must be ");
  static_assert(std::is_floating_point_v<F>);

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4056)  //  Incorrect detection of floating point overflow.
#endif
  // Min is a power of two (2 ^ bits), max will be (2^bits - 1)
  // This means min can be represented exactly as F (or it overflows to -inf).
  // We round max up:
  constexpr F I_min_as_float = static_cast<F>(std::numeric_limits<I>::min());
  constexpr F I_max_as_float_plus_1 = static_cast<F>(std::numeric_limits<I>::max() / 2 + 1) * 2;

  // If it overflows to infinity, the range of `I` exceeds `F` and we can always truncate `F` to
  // `I`. that either we overflow, or the conversion did not truncate.
  constexpr bool min_overflowed = I_min_as_float == -std::numeric_limits<F>::infinity();
  constexpr bool max_overflowed = I_max_as_float_plus_1 == std::numeric_limits<F>::infinity();
  // We don't need or support the case where `I` exceeds `F`:
  static_assert(!min_overflowed && !max_overflowed, "Range of I cannot exceed F");
  static_assert(static_cast<I>(I_min_as_float) == std::numeric_limits<I>::min());
  static_assert(static_cast<I>(I_max_as_float_plus_1 / 2) - 1 == std::numeric_limits<I>::max() / 2);
#ifdef _MSC_VER
#pragma warning(pop)
#endif

  if (std::isnan(f)) {
    // Can't compare nan:
    return std::nullopt;
  } else if (!std::isfinite(f)) {
    // Since range of `I` is smaller than `F`, inf is always > or < than any integral value.
    return f < 0 ? relative_order::greater_than : relative_order::less_than;
  }

  // Check if `f` can be truncated to type `I`.
  if (f >= I_min_as_float) {
    // Either upper bound of `I` exceeds range of `F`, or `f` <= I_max_as_float_plus_1 - 1
    if (f - I_max_as_float_plus_1 <= static_cast<F>(-1)) {
      // Compare integral part
      const I f_truncated = static_cast<I>(f);
      if (f_truncated < i) {
        return relative_order::greater_than;
      }
      if (f_truncated > i) {
        return relative_order::less_than;
      }
      // Compare fractional part
      const F f_fractional = f - static_cast<F>(f_truncated);
      if (f_fractional < 0) {
        return relative_order::greater_than;
      }
      if (f_fractional > 0) {
        return relative_order::less_than;
      }
      return relative_order::equal;
    }
    // `f` is outside the range of `I`, so `i` must be smaller.
    return relative_order::less_than;
  }
  // `f` is < than the min value of I, so order must be greater.
  return relative_order::greater_than;
}

}  // namespace wf
