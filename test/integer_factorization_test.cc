#include <numeric>

#include <fmt/format.h>

#include "integer_factorization.h"
#include "test_helpers.h"

namespace math {

// Simple brute force check for prime-ness.
bool TrialDivisionIsPrime(const int64_t n_in) {
  int64_t x = n_in;
  int64_t factor = 2;
  while (x > 1 && factor != n_in) {
    if (x % factor == 0) {
      return false;
    } else {
      // try next factor
      ++factor;
    }
  }
  return true;
}

template <typename T>
std::string Join(const std::vector<T>& data) {
  auto it = data.begin();
  if (it == data.end()) {
    return {};
  }
  std::string out;
  fmt::format_to(std::back_inserter(out), "{}", *it);
  for (++it; it != data.end(); ++it) {
    fmt::format_to(std::back_inserter(out), ", {}", *it);
  }
  return out;
}

TEST(TestIntegerFactorization, TextComputePrimeFactors) {
  // Factor numbers 1 -> 100000. Takes about ~1.5 seconds in debug mode on M2.
  std::vector<int64_t> result{};
  for (int64_t i = 1; i < 100000; ++i) {
    result.clear();
    ComputePrimeFactors(i, result);
    // Check that the factors multiply out to the input number.
    const int64_t product = std::accumulate(result.begin(), result.end(), 1l, std::multiplies<>());
    ASSERT_EQ(product, i);
    // Check that all the factors are prime:
    for (const int64_t factor : result) {
      ASSERT_TRUE(TrialDivisionIsPrime(factor))
          << fmt::format("i = {}, factors = [{}]\n", i, Join(result));
    }
  }
}

TEST(TestIntegerFactorization, TextComputePrimeFactors2) {
  // clang-format off
  const std::vector<std::vector<int64_t>> primes = {
    {373, 937, 1583, 2029},
    {197, 1753, 2609, 3001},
    {983, 1291, 2939, 3083},
    {7, 191, 3491, 3529},
    {617, 1451, 2203, 2293},
    {929, 1013, 1291, 2003},
    {283, 541, 1801, 3023},
    {521, 2099, 2179, 2267},
    {41, 757, 1721, 2063},
    {409, 863, 2137, 3413},
    {1031, 2269, 2707, 3323},
    {479, 1447, 1559, 3467},
    {3, 887, 1187, 2411},
    {1009, 1289, 3121, 3517},
    {127, 691, 1201, 2777},
    {97, 229, 991, 1409},
    {1997, 2383, 2803, 3391},
    {1447, 1979, 3023, 3449},
    {43, 769, 2087, 2473},
    {157, 311, 1013, 2957},
  };
  // clang-format on

  // Factor numbers 1 -> 100000
  std::vector<int64_t> result{};
  for (const auto& expected_factors : primes) {
    const int64_t product =
        std::accumulate(expected_factors.begin(), expected_factors.end(), 1l, std::multiplies<>());
    result.clear();
    ComputePrimeFactors(product, result);
    std::sort(result.begin(), result.end());
    // Check we got the same factors out:
    ASSERT_EQ(result.size(), expected_factors.size());
    for (std::size_t i = 0; i < expected_factors.size(); ++i) {
      ASSERT_EQ(expected_factors[i], result[i]);
    }
  }
}

}  // namespace math
