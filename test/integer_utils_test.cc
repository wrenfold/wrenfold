#include <numeric>

#include <fmt/format.h>

#include "integer_utils.h"
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

template <typename T, typename Converter>
std::string Join(const std::vector<T>& data, Converter converter) {
  auto it = data.begin();
  if (it == data.end()) {
    return {};
  }
  std::string out;
  fmt::format_to(std::back_inserter(out), "{}", converter(*it));
  for (++it; it != data.end(); ++it) {
    fmt::format_to(std::back_inserter(out), ", {}", converter(*it));
  }
  return out;
}

struct PrimeFactorsOrder {
  bool operator()(const PrimeFactor& a, const PrimeFactor& b) const { return a.base < b.base; }
};

struct FormatFactor {
  std::string operator()(const PrimeFactor& x) const {
    return fmt::format("{}^{}", x.base, x.exponent);
  }
};

TEST(IntegerUtils, TestComputePrimeFactors) {
  // Factor numbers 1 -> 100000. Takes about ~1.5 seconds in debug mode on M2.
  for (int64_t i = 1; i < 100000; ++i) {
    const std::vector<PrimeFactor> result = ComputePrimeFactors(i);
    ASSERT_TRUE(std::is_sorted(result.begin(), result.end(), PrimeFactorsOrder{}));
    // Check that the factors multiply out to the input number.
    const int64_t product = std::accumulate(
        result.begin(), result.end(), 1l,
        [](int64_t product, const PrimeFactor& f) { return product * Pow(f.base, f.exponent); });
    ASSERT_EQ(product, i);
    // Check that all the factors are prime:
    for (const PrimeFactor factor : result) {
      ASSERT_TRUE(TrialDivisionIsPrime(factor.base))
          << fmt::format("i = {}, factors = [{}]\n", i, Join(result, FormatFactor{}));
    }
  }
}

TEST(IntegerUtils, TestComputePrimeFactors2) {
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
  for (const auto& expected_factors : primes) {
    const int64_t product =
        std::accumulate(expected_factors.begin(), expected_factors.end(), 1l, std::multiplies<>());
    const std::vector<PrimeFactor> result = ComputePrimeFactors(product);
    ASSERT_TRUE(std::is_sorted(result.begin(), result.end(), PrimeFactorsOrder{}));
    // Check we got the same factors out:
    ASSERT_EQ(result.size(), expected_factors.size());
    for (std::size_t i = 0; i < expected_factors.size(); ++i) {
      ASSERT_EQ(expected_factors[i], result[i].base);
      ASSERT_EQ(1, result[i].exponent);
    }
  }
}

TEST(IntegerUtils, TestPow) {
  ASSERT_EQ(Pow(1, 1), 1);
  ASSERT_EQ(Pow(2, 0), 1);
  ASSERT_EQ(Pow(-1, 1), -1);
  ASSERT_EQ(Pow(-1, 2), 1);
  ASSERT_EQ(Pow(-3, 0), 1);
  ASSERT_EQ(Pow(5, 3), 125);
  ASSERT_EQ(Pow(3, 4), 81);
  ASSERT_EQ(Pow(7, 2), 49);
  ASSERT_EQ(Pow(-7, 2), 49);
  ASSERT_EQ(Pow(-6, 3), -216);
  ASSERT_EQ(Pow(3, 8), 6561);
}

}  // namespace math
