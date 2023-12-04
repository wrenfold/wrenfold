// Copyright 2023 Gareth Cross
#pragma once
#include <unordered_map>
#include <variant>
#include <vector>

#include "wf/absl_imports.h"
#include "wf/constants.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"
#include "wf/hashing.h"

namespace wf {

// A multiplication of `N` terms.
class multiplication {
 public:
  static constexpr std::string_view name_str = "Multiplication";
  static constexpr bool is_leaf_node = false;
  using container_type = absl::InlinedVector<Expr, 16>;

  // Move-construct.
  explicit multiplication(container_type&& terms) : terms_(std::move(terms)) {
    WF_ASSERT_GREATER_OR_EQ(terms_.size(), 2);
    sort_terms();
  }

  // Access specific argument.
  const Expr& operator[](const std::size_t i) const { return terms_[i]; }

  // Number of arguments.
  std::size_t size() const noexcept { return terms_.size(); }

  // Iterators.
  container_type::const_iterator begin() const noexcept { return terms_.begin(); }
  container_type::const_iterator end() const noexcept { return terms_.end(); }

  // All terms must be identical.
  bool is_identical_to(const multiplication& other) const {
    if (size() != other.size()) {
      return false;
    }
    return std::equal(begin(), end(), other.begin(), is_identical_struct<Expr>{});
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void for_each(Operation&& operation) const {
    std::for_each(begin(), end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    container_type transformed{};
    transformed.reserve(size());
    std::transform(begin(), end(), std::back_inserter(transformed),
                   std::forward<Operation>(operation));
    return multiplication::from_operands(transformed);
  }

  // Construct from a span of operands. Result is automatically simplified.
  static Expr from_operands(absl::Span<const Expr> span);

 private:
  void sort_terms() {
    // We leave the integer/rational/float part in front.
    // TODO: Add a BinaryMul where the first term is always int/rational/float.
    const auto begin = std::find_if(terms_.begin(), terms_.end(), [](const Expr& term) {
      return !term.is_type<integer_constant, rational_constant, float_constant>();
    });
    std::sort(begin, terms_.end(), [](const Expr& a, const Expr& b) {
      if (a.get_hash() < b.get_hash()) {
        return true;
      } else if (a.get_hash() > b.get_hash()) {
        return false;
      } else {
        return expression_order_struct{}(a, b);
      }
    });
  }

  container_type terms_;
};

template <>
struct hash_struct<multiplication> {
  std::size_t operator()(const multiplication& mul) const {
    return hash_all(0, mul.begin(), mul.end());
  }
};

// Split a multiplication up into numerical values and non-numerical expressions.
// Returns [coefficient, multiplicand] where the coefficient is the numerical part.
// If there are no numerical terms, the coefficient will be one.
std::pair<Expr, Expr> split_multiplication(const multiplication& mul, const Expr& mul_abstract);

// Convert an expression into a coefficient and a multiplicand. This operation checks if
// expr is a multiplication. If it is, we extract all numeric constants and return them
// as the first value. The remaining terms form a new multiplication, which is returned as
// the second value.
std::pair<Expr, Expr> as_coeff_and_mul(const Expr& expr);

// Helper object used to execute multiplications.
struct multiplication_parts {
  multiplication_parts() = default;
  explicit multiplication_parts(std::size_t capacity) { terms.reserve(capacity); }

  // Construct from existing multiplication.
  explicit multiplication_parts(const multiplication& mul, bool factorize_integers);

  // Rational coefficient.
  rational_constant rational_coeff{1, 1};
  // Floating point coefficient:
  std::optional<float_constant> float_coeff{};
  // Map from base to exponent.
  std::unordered_map<Expr, Expr, hash_struct<Expr>, is_identical_struct<Expr>> terms{};
  // Number of infinities.
  std::size_t num_infinities{0};

  // Update the internal product by multiplying on `arg`.
  void multiply_term(const Expr& arg, bool factorize_integers = false);

  // Nuke any terms w/ a zero exponent and normalize powers of integers.
  void normalize_coefficients();

  // Create the resulting multiplication.
  Expr create_multiplication() const;
};

// A decomposition of `Multiplication` that is more convenient for printing.
// This is defined here and not in one particular formatter, since it is likely useful more than
// once.
struct multiplication_format_parts {
  struct base_exp {
    Expr base;
    Expr exponent;
  };

  bool is_negative{false};
  std::vector<std::variant<integer_constant, float_constant, base_exp>> numerator;
  std::vector<std::variant<integer_constant, float_constant, base_exp>> denominator;
};

// Create `multiplication_format_parts` from a multiplication.
multiplication_format_parts get_formatting_info(const multiplication& mul);

}  // namespace wf
