// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <variant>
#include <vector>

#include "wf/expression.h"
#include "wf/expressions/addition.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/power.h"
#include "wf/expressions/special_constants.h"
#include "wf/utility/algorithms.h"
#include "wf/utility/hashing.h"
#include "wf/utility/stack_allocator.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/inlined_vector.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

// A multiplication of `N` terms.
class multiplication {
 public:
  static constexpr std::string_view name_str = "Multiplication";
  static constexpr bool is_leaf_node = false;
  using container_type = absl::InlinedVector<scalar_expr, 16>;
  struct no_sort {};

  // Move-construct.
  explicit multiplication(container_type&& terms) : terms_(std::move(terms)) {
    WF_ASSERT_GE(terms_.size(), 2);
    sort_terms();
  }

  // Move construct and do not sort.
  explicit multiplication(no_sort, container_type&& terms) : terms_(std::move(terms)) {
    WF_ASSERT_GE(terms_.size(), 2);
  }

  // Construct from a pair of multiplied terms.
  explicit multiplication(scalar_expr a, scalar_expr b) : terms_{std::move(a), std::move(b)} {
    sort_terms();
  }

  // Access specific argument.
  const scalar_expr& operator[](const std::size_t i) const { return terms_[i]; }

  // Number of arguments.
  std::size_t size() const noexcept { return terms_.size(); }

  // Iterators.
  container_type::const_iterator begin() const noexcept { return terms_.begin(); }
  container_type::const_iterator end() const noexcept { return terms_.end(); }

  // Get terms in the multiplication, sorted into canonical order.
  std::vector<scalar_expr> sorted_terms() const;

  // Map all the terms in this multiplication to get a new expression.
  template <typename Operation>
  scalar_expr map_children(Operation&& operation) const {
    return multiplication::from_operands(terms_, std::forward<Operation>(operation));
  }

  // Child expressions of the multiplication.
  constexpr const container_type& children() const noexcept { return terms_; }

  // Construct from a container of operands while applying `operation`.
  template <typename Container, typename Operation>
  static scalar_expr from_operands(const Container& container, Operation&& operation);

  // Construct from a container of operands. Result is automatically simplified.
  template <typename Container>
  static scalar_expr from_operands(const Container& container);

  // Construct from two operands: a * b.
  static scalar_expr from_two_operands(const scalar_expr& a, const scalar_expr& b);

  // Multiply a numerical constant into he provided addition.
  static scalar_expr multiply_into_addition(const addition& add,
                                            const scalar_expr& numerical_constant) {
    const addition::container_type add_args = transform_map<addition::container_type>(
        add, [&](const scalar_expr& f) { return f * numerical_constant; });
    return addition::from_operands(add_args);  // TODO: make this a move.
  }

 private:
  void sort_terms();
  container_type terms_;
};

template <>
struct hash_struct<multiplication> {
  std::size_t operator()(const multiplication& mul) const noexcept {
    return hash_all(0, mul.begin(), mul.end());
  }
};

template <>
struct is_identical_struct<multiplication> {
  bool operator()(const multiplication& a, const multiplication& b) const {
    return a.size() == b.size() &&
           std::equal(a.begin(), a.end(), b.begin(), is_identical_struct<scalar_expr>{});
  }
};

template <>
struct order_struct<multiplication> {
  relative_order operator()(const multiplication& a, const multiplication& b) const {
    return lexicographical_order(a, b, order_struct<scalar_expr>{});
  }
};

// Returns true if the expression has a numerical coefficient (an integer, rational, or float).
// If this function returns true, then `as_coeff_and_mul` will return a non-unit coefficient.
bool has_numeric_coefficient(const scalar_expr& expr);

// Convert an expression into a coefficient and a multiplicand. This operation checks if
// expr is a multiplication. If it is, we extract all numeric constants and return them
// as the first value. The remaining terms form a new multiplication, which is returned as
// the second value.
std::pair<scalar_expr, scalar_expr> as_coeff_and_mul(const scalar_expr& expr);

// Helper object used to manipulate multiplications.
// Stores a map from {base -> exponent}. As terms are multiplied, the exponent is incremented
// or decremented appropriately. Finally, `create_multiplication` is called to flatten
// the contents back into a `multiplication` object.
struct multiplication_parts {
  using constant_coeff = std::variant<integer_constant, rational_constant, float_constant,
                                      undefined, complex_infinity>;

  // Construct with capacity.
  explicit multiplication_parts(const std::size_t capacity, const bool factorize_integers = false)
      : factorize_integers_(factorize_integers) {
    terms_.reserve(capacity);
  }

  // Construct from existing multiplication.
  explicit multiplication_parts(const multiplication& mul, bool factorize_integers);

  // Update the internal product by multiplying on `arg`.
  void multiply_term(const scalar_expr& arg);

  // Nuke any terms w/ a zero exponent and normalize powers of integers.
  void normalize_coefficients();

  // Create the resulting multiplication.
  // This is ref-qualified because we move expressions out of the map.
  scalar_expr create_multiplication() &&;

  // Visitor operations.
  void operator()(const multiplication& mul);
  void operator()(const power& pow);
  template <typename T, typename = enable_if_does_not_contain_type_t<T, multiplication, power>>
  void operator()(const T&, const scalar_expr& input_expression);

  constexpr const auto& coeff() const noexcept { return coeff_; }
  constexpr const auto& terms() const noexcept { return terms_; }

  constexpr auto& coeff() noexcept { return coeff_; }
  constexpr auto& terms() noexcept { return terms_; }

 private:
  // Insert base**exponent into `terms`, applying simplifications in the process.
  void insert_power(const scalar_expr& base, const scalar_expr& exponent);

  template <typename T>
  void insert_integer_factors(const T& factors, bool positive);

  // Stack storage for the map:
  using map_value_type = std::pair<const scalar_expr, scalar_expr>;
  using stack_allocator_type = stack_allocator<1024, alignof(map_value_type)>;
  stack_allocator_type allocator_;

  // Constant coefficient.
  constant_coeff coeff_{integer_constant{1}};

  // Map from base to exponent.
  using stl_stack_allocator_type =
      stl_stack_allocator_with_fallback<map_value_type, stack_allocator_type::capacity>;
  std::unordered_map<scalar_expr, scalar_expr, hash_struct<scalar_expr>,
                     is_identical_struct<scalar_expr>, stl_stack_allocator_type>
      terms_{stl_stack_allocator_type(allocator_)};

  // If true, factorize integers into primes and insert them into `terms`, instead of
  // updating `numeric_coeff`. Rationals are broken into positive and negative powers
  // of primes.
  bool factorize_integers_{false};
};

// A decomposition of `multiplication` that is more convenient for printing.
struct multiplication_format_parts {
  bool is_negative{false};
  std::vector<std::variant<integer_constant, float_constant, power>> numerator;
  std::vector<std::variant<integer_constant, float_constant, power>> denominator;
};

// Create `multiplication_format_parts` from a multiplication.
multiplication_format_parts get_formatting_info(const multiplication& mul);

// Implementation of `multiplication` methods:
template <typename Container, typename Operation>
scalar_expr multiplication::from_operands(const Container& container, Operation&& operation) {
  const std::size_t sz = std::size(container);
  if (sz == 0) {
    throw invalid_argument_error("Need at least one operand to construct multiplication.");
  }
  if (sz == 1) {
    return operation(*container.begin());
  }

  // Check for `addition * constant`.
  // Combinations for > 2 args are handled by `create_multiplication`.
  if (sz == 2) {
    auto it = container.begin();
    const scalar_expr& first = operation(*it);
    const scalar_expr& second = operation(*std::next(it));
    return from_two_operands(first, second);
  }

  multiplication_parts parts{sz};
  for (const scalar_expr& term : container) {
    parts.multiply_term(operation(term));
  }
  parts.normalize_coefficients();
  return std::move(parts).create_multiplication();
}

template <typename Container>
scalar_expr multiplication::from_operands(const Container& container) {
  return from_operands(container, [](const auto& x) -> const auto& { return x; });
}

inline scalar_expr multiplication::from_two_operands(const scalar_expr& arg0,
                                                     const scalar_expr& arg1) {
  if (const addition* add = get_if<const addition>(arg0);
      add && arg1.is_type<integer_constant, rational_constant, float_constant>()) {
    return multiply_into_addition(*add, arg1);
  } else if (add = get_if<const addition>(arg1);
             add && arg0.is_type<integer_constant, float_constant, rational_constant>()) {
    return multiply_into_addition(*add, arg0);
  }
  multiplication_parts parts{2};
  parts.multiply_term(arg0);
  parts.multiply_term(arg1);
  parts.normalize_coefficients();
  return std::move(parts).create_multiplication();
}

}  // namespace wf
