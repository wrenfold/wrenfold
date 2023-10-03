#include <numeric>

#include <fmt/format.h>

#include "index_range.h"
#include "integer_utils.h"
#include "test_helpers.h"

#include "expressions/numeric_expressions.h"

// Format PrimeFactor
template <>
struct fmt::formatter<math::PrimeFactor, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::PrimeFactor& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}^{}", x.base, x.exponent);
  }
};

std::ostream& operator<<(std::ostream& out, const math::RelativeOrder order) {
  out << string_from_relative_order(order);
  return out;
}

std::ostream& operator<<(std::ostream& out, const std::optional<math::RelativeOrder> order) {
  out << (order.has_value() ? string_from_relative_order(order.value()) : "nullopt");
  return out;
}

namespace math {
using namespace math::custom_literals;

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

struct PrimeFactorsOrder {
  bool operator()(const PrimeFactor& a, const PrimeFactor& b) const { return a.base < b.base; }
};

TEST(IntegerUtils, TestComputePrimeFactors) {
  // Factor numbers 1 -> 10000.
  for (int64_t i = 1; i < 10000; ++i) {
    const std::vector<PrimeFactor> result = compute_prime_factors(i);
    ASSERT_TRUE(std::is_sorted(result.begin(), result.end(), PrimeFactorsOrder{}));
    // Check that the factors multiply out to the input number.
    const int64_t product = std::accumulate(result.begin(), result.end(), 1l,
                                            [](int64_t product, const PrimeFactor& f) {
                                              return product * integer_power(f.base, f.exponent);
                                            });
    ASSERT_EQ(product, i);
    // Check that all the factors are prime:
    for (const PrimeFactor factor : result) {
      ASSERT_TRUE((factor.base == -1 && factor.exponent == 1) || TrialDivisionIsPrime(factor.base))
          << fmt::format("i = {}, factors = [{}]\n", i, fmt::join(result, ", "));
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
    {-1, 617, 1451, 2203, 2293},
    {929, 1013, 1291, 2003},
    {283, 541, 1801, 3023},
    {521, 2099, 2179, 2267},
    {41, 757, 1721, 2063},
    {-1, 409, 863, 2137, 3413},
    {1031, 2269, 2707, 3323},
    {479, 1447, 1559, 3467},
    {3, 887, 1187, 2411},
    {1009, 1289, 3121, 3517},
    {-1, 127, 691, 1201, 2777},
    {97, 229, 991, 1409},
    {1997, 2383, 2803, 3391},
    {1447, 1979, 3023, 3449},
    {-1, 43, 769, 2087, 2473},
    {157, 311, 1013, 2957},
  };
  // clang-format on

  // Factor numbers 1 -> 100000
  for (const auto& expected_factors : primes) {
    const int64_t product = std::accumulate(expected_factors.begin(), expected_factors.end(),
                                            static_cast<int64_t>(1), std::multiplies<>());
    const std::vector<PrimeFactor> result = compute_prime_factors(product);
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
  ASSERT_EQ(integer_power(1, 1), 1);
  ASSERT_EQ(integer_power(2, 0), 1);
  ASSERT_EQ(integer_power(-1, 1), -1);
  ASSERT_EQ(integer_power(-1, 2), 1);
  ASSERT_EQ(integer_power(-3, 0), 1);
  ASSERT_EQ(integer_power(5, 3), 125);
  ASSERT_EQ(integer_power(3, 4), 81);
  ASSERT_EQ(integer_power(7, 2), 49);
  ASSERT_EQ(integer_power(-7, 2), 49);
  ASSERT_EQ(integer_power(-6, 3), -216);
  ASSERT_EQ(integer_power(3, 8), 6561);
}

template <typename T>
constexpr T n_inf() {
  return -std::numeric_limits<T>::infinity();
}

template <typename T>
constexpr T p_inf() {
  return std::numeric_limits<T>::infinity();
}

template <typename T, typename Input>
constexpr T next_down(const Input v) {
  return std::nextafter(static_cast<T>(v), n_inf<T>());
}
template <typename T, typename Input>
constexpr T next_up(const Input v) {
  return std::nextafter(static_cast<T>(v), p_inf<T>());
}

inline constexpr int64_t operator""_i64(unsigned long long x) { return static_cast<int64_t>(x); }

TEST(IntegerUtils, Testcompare_int_float) {
  // Invalid to compare w/ NaN:
  EXPECT_EQ(std::nullopt, compare_int_float(7, std::numeric_limits<float>::quiet_NaN()));
  EXPECT_EQ(std::nullopt, compare_int_float(13, std::numeric_limits<double>::quiet_NaN()));

  // min() for doubles is the smallest positive value
  EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(0, std::numeric_limits<float>::min()));
  EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(0, std::numeric_limits<double>::min()));
  EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(0_i64, std::numeric_limits<double>::min()));

  EXPECT_EQ(RelativeOrder::GreaterThan, compare_int_float(0, -std::numeric_limits<float>::min()));
  EXPECT_EQ(RelativeOrder::GreaterThan, compare_int_float(0, -std::numeric_limits<double>::min()));
  EXPECT_EQ(RelativeOrder::GreaterThan,
            compare_int_float(0_i64, -std::numeric_limits<double>::min()));

  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(0, 0.0f));
  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(0_i64, 0.0));
  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(0, -0.0f));
  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(0_i64, -0.0));

  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(1, 1.0f));
  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(1_i64, 1.0));
  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(-1, -1.0f));
  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(-1_i64, -1.0));

  // Infinity:
  constexpr auto i32_min = std::numeric_limits<int32_t>::min();
  constexpr auto i32_max = std::numeric_limits<int32_t>::max();
  constexpr auto i64_min = std::numeric_limits<int64_t>::min();
  constexpr auto i64_max = std::numeric_limits<int64_t>::max();

  EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(i32_max, p_inf<float>()));
  EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(i64_max, p_inf<double>()));

  EXPECT_EQ(RelativeOrder::GreaterThan, compare_int_float(i32_min, n_inf<float>()));
  EXPECT_EQ(RelativeOrder::GreaterThan, compare_int_float(i64_min, n_inf<double>()));

  // Values near int min/max:
  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(i32_min, static_cast<float>(i32_min)));
  EXPECT_EQ(RelativeOrder::GreaterThan, compare_int_float(i32_min, next_down<float>(i32_min)));
  EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(i32_min, next_up<float>(i32_min)));

  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(i64_min, static_cast<double>(i64_min)));
  EXPECT_EQ(RelativeOrder::GreaterThan, compare_int_float(i64_min, next_down<double>(i64_min)));
  EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(i64_min, next_up<double>(i64_min)));

  // int::max gets rounded up in float, so this should be LessThan (not equal):
  EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(i32_max, static_cast<float>(i32_max)));
  EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(i64_max, static_cast<double>(i64_max)));
  EXPECT_EQ(RelativeOrder::GreaterThan, compare_int_float(i32_max, next_down<float>(i32_max)));
  EXPECT_EQ(RelativeOrder::GreaterThan, compare_int_float(i64_max, next_down<double>(i64_max)));

  // largest integral value that can be represented:
  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(2 << 24, static_cast<float>(2 << 24)));
  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(2ll << 53, static_cast<double>(2ll << 53)));
  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(-(2 << 24), -static_cast<float>(2 << 24)));
  EXPECT_EQ(RelativeOrder::Equal, compare_int_float(-(2ll << 53), -static_cast<double>(2ll << 53)));

  EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(0, 0.5));
  EXPECT_EQ(RelativeOrder::GreaterThan, compare_int_float(0, -0.5));

  for (const int sign : {-1, 1}) {
    for (const int exp : make_range(0, 16)) {
      const int value = std::pow(10, exp) * sign;
      if (exp <= 6) {
        EXPECT_EQ(RelativeOrder::GreaterThan, compare_int_float(value, next_down<float>(value)));
        EXPECT_EQ(RelativeOrder::Equal, compare_int_float(value, static_cast<float>(value)));
        EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(value, next_up<float>(value)));
      }
      EXPECT_EQ(RelativeOrder::GreaterThan, compare_int_float(value, next_down<double>(value)));
      EXPECT_EQ(RelativeOrder::Equal, compare_int_float(value, static_cast<double>(value)));
      EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(value, next_up<double>(value)));
    }
  }

  EXPECT_EQ(RelativeOrder::GreaterThan, compare_int_float(10, -1.0e40));
  EXPECT_EQ(RelativeOrder::GreaterThan, compare_int_float(-13420052, -2.235e61));
  EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(6448282, 2.9e34));
  EXPECT_EQ(RelativeOrder::LessThan, compare_int_float(2008ll, 1.562e67));
}

}  // namespace math
