// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/factorizer.h"

#include <random>

#include "wf/algorithm_utils.h"
#include "wf/index_range.h"
#include "wf_test_support/test_macros.h"

namespace wf {

inline factor_bits fill_bits(const std::initializer_list<int> indices) {
  factor_bits b{};
  for (auto index : indices) {
    WF_ASSERT_LESS(index, b.size());
    b.set(index);
  }
  return b;
}

inline auto make_step(const std::initializer_list<int> vars,
                      const std::initializer_list<int> terms) {
  return std::make_tuple(fill_bits(vars), fill_bits(terms));
}

template <typename... Ts>
auto make_factorization(Ts&&... args) {
  return factorization{std::forward<Ts>(args)...};
}

// Allow formatting of factorizations.
std::ostream& operator<<(std::ostream& s, const factorization& f) {
  s << f.to_string();
  return s;
}

void check_factorizations(const std::vector<factorization>& facs,
                          const absl::Span<const std::vector<std::uint32_t>> input_terms) {
  for (const factorization& fac : facs) {
    // Check that all the factors aren't trampling on each other:
    for (std::size_t i = 0; i < fac.size(); ++i) {
      const auto [vi, ti] = fac[i];
      for (std::size_t j = i + 1; j < fac.size(); ++j) {
        const auto [vj, tj] = fac[j];
        ASSERT_TRUE((vi & vj).count() == 0 || (ti & tj).count() == 0)
            << fmt::format("vi = {}, ti = {}, ti = {}, tj = {}", bit_range(vi), bit_range(ti),
                           bit_range(vj), bit_range(tj));
      }
    }

    // Check that the assignments are valid compare to the original input terms:
    for (const auto& [vars, terms] : fac.steps()) {
      for (const std::size_t term : bit_range(terms)) {
        for (const std::size_t var : bit_range(vars)) {
          ASSERT_TRUE(contains(input_terms[term], var));
        }
      }
    }
  }
}

TEST(FactorizerTest, NoFactors) {
  // No factors possible:
  const std::vector<std::vector<std::uint32_t>> terms = {{0, 1}, {2, 3}, {4, 5}};
  const auto factorizations = compute_factorizations(terms, 6);
  ASSERT_EQ(0, factorizations.size());
}

TEST(FactorizerTest, Test1) {
  // Simple, one variable can be removed from two terms:
  const std::vector<std::vector<std::uint32_t>> terms = {{0, 1}, {0, 2}};
  const auto factorizations = compute_factorizations(terms, 3);

  check_factorizations(factorizations, terms);
  ASSERT_EQ(1, factorizations.size());
  ASSERT_EQ(make_factorization(make_step({0}, {0, 1})), factorizations[0]);
}

TEST(FactorizerTest, Test2) {
  // One variable can be removed from 3 of 4 terms:
  const std::vector<std::vector<std::uint32_t>> terms = {{0, 1}, {0, 2, 3}, {0, 4}, {5}};
  const auto factorizations = compute_factorizations(terms, 6);

  check_factorizations(factorizations, terms);
  ASSERT_EQ(1, factorizations.size());
  ASSERT_EQ(make_factorization(make_step({0}, {0, 1, 2})), factorizations[0]);
}

TEST(FactorizerTest, Test3) {
  // Two independent groups:
  const std::vector<std::vector<std::uint32_t>> terms = {{0, 1}, {0, 2, 3}, {4, 5}, {4, 6}};
  const auto factorizations = compute_factorizations(terms, 7);

  check_factorizations(factorizations, terms);
  ASSERT_EQ(1, factorizations.size());
  ASSERT_EQ(make_factorization(make_step({0}, {0, 1}), make_step({4}, {2, 3})), factorizations[0]);
}

TEST(FactorizerTest, Test4) {
  // Three independent groups:
  const std::vector<std::vector<std::uint32_t>> terms = {{0, 1}, {0, 2}, {3, 4},
                                                         {3, 5}, {6, 7}, {6, 8}};
  const auto factorizations = compute_factorizations(terms, 9);

  check_factorizations(factorizations, terms);
  ASSERT_EQ(1, factorizations.size());
  ASSERT_EQ(
      make_factorization(make_step({0}, {0, 1}), make_step({3}, {2, 3}), make_step({6}, {4, 5})),
      factorizations[0]);
}

TEST(FactorizerTest, Test5) {
  // Two possible factorizations:
  const std::vector<std::vector<std::uint32_t>> terms = {{0, 1}, {0, 2}, {2, 3}, {3, 4}};
  const auto factorizations = compute_factorizations(terms, 5);

  check_factorizations(factorizations, terms);
  ASSERT_EQ(2, factorizations.size());
  ASSERT_EQ(make_factorization(make_step({0}, {0, 1}), make_step({3}, {2, 3})), factorizations[0]);
  ASSERT_EQ(make_factorization(make_step({2}, {1, 2})), factorizations[1]);
}

TEST(FactorizerTest, Test6) {
  // Two possible factorizations:
  // a*x + a*y + x*b + y*b
  //  --> x*(a + b) + y*(a + b)
  //  --> a*(x + y) + b*(x + y)
  const std::vector<std::vector<std::uint32_t>> terms = {{0, 1}, {0, 2}, {1, 3}, {2, 3}};
  const auto factorizations = compute_factorizations(terms, 5);

  check_factorizations(factorizations, terms);
  ASSERT_EQ(2, factorizations.size());
  ASSERT_EQ(make_factorization(make_step({0}, {0, 1}), make_step({3}, {2, 3})), factorizations[0]);
  ASSERT_EQ(make_factorization(make_step({1}, {0, 2}), make_step({2}, {1, 3})), factorizations[1]);
}

TEST(FactorizerTest, Test7) {
  // Some remaining terms:
  const std::vector<std::vector<std::uint32_t>> terms = {{0, 1}, {0, 2}, {3, 4}, {5}};
  const auto factorizations = compute_factorizations(terms, 6);

  check_factorizations(factorizations, terms);
  ASSERT_EQ(1, factorizations.size());

  const auto remaining = factorizations[0].unfactored_terms(terms.size());
  ASSERT_FALSE(remaining[0]);
  ASSERT_FALSE(remaining[1]);
  ASSERT_TRUE(remaining[2]);
  ASSERT_TRUE(remaining[3]);
}

TEST(FactorizerTest, Test8) {
  const std::vector<std::vector<std::uint32_t>> terms = {
      {5, 6, 7, 8, 0, 14, 3}, {5, 7, 8, 9, 0, 14, 3}, {5, 7, 8, 0, 14, 2, 3},
      {4, 5, 6, 1, 14, 3},    {4, 5, 9, 1, 14, 3},    {4, 5, 1, 14, 2, 3},
      {5, 8, 11, 12, 3},      {5, 8, 11, 13, 3},      {5, 8, 10, 11, 3},
  };

  const auto factorizations = compute_factorizations(terms, 15);

  check_factorizations(factorizations, terms);
  ASSERT_EQ(27, factorizations.size());
  ASSERT_EQ(make_factorization(make_step({0, 3, 5, 7, 8, 14}, {0, 1, 2}),
                               make_step({1, 3, 4, 5, 14}, {3, 4, 5}),
                               make_step({3, 5, 8, 11}, {6, 7, 8})),
            factorizations[0]);
}

TEST(FactorizerTest, TestRandom) {
  // Generate random sequences and factorize them.
  constexpr std::size_t num_vars = 8;
  constexpr std::size_t num_terms = 12;
  constexpr int num_tests = 100;

  std::default_random_engine engine{13};
  std::uniform_int_distribution<std::uint32_t> var_dist{0, num_vars - 1};

  std::array<std::vector<std::uint32_t>, num_terms> terms{};
  for (auto& vec : terms) {
    constexpr std::size_t vars_per_term = 6;
    vec.resize(vars_per_term, 0);
  }

  for (std::size_t i = 0; i < num_tests; ++i) {
    for (auto& term : terms) {
      for (auto& var : term) {
        var = var_dist(engine);
      }
    }

    const auto factorizations = compute_factorizations(terms, num_vars);
    check_factorizations(factorizations, terms);
  }
}

}  // namespace wf
