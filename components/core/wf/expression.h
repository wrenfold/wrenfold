// Copyright 2023 Gareth Cross
#pragma once
#include <memory>
#include <ostream>
#include <string>

#include "wf/expression_variant.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/fmt_imports.h"
#include "wf/hashing.h"
#include "wf/operations.h"

namespace wf {

// Empty type that we use to specify a list of types for `expression_variant`.
struct scalar_meta_type {};

template <>
struct type_list_trait<scalar_meta_type> {
  // All the scalar-valued expressions.
  // clang-format off
  using types = type_list<
    class addition,
    class cast_bool,
    class conditional,
    class symbolic_constant,
    class derivative,
    class float_constant,
    class function,
    class complex_infinity,
    class integer_constant,
    class multiplication,
    class power,
    class rational_constant,
    class relational,
    class undefined,
    class variable
    >;
  // clang-format on
};

// An abstract scalar-valued expression.
class Expr {
 public:
  using storage_type = expression_variant<scalar_meta_type>;

  // Construct variable with name and specific numeric set:
  explicit Expr(std::string_view name, number_set set = number_set::unknown);

  // Enable if the type `T` is a numeric type that we allow to be implicitly converted to `Expr`.
  template <typename T>
  using enable_if_supports_implicit_conversion =
      std::enable_if_t<(std::is_integral_v<T> && !std::is_same_v<T, bool>) ||
                       std::is_floating_point_v<T>>;

  // Implicit construction from integers and floats.
  // enable_if argument is a trick we use until c++20 and constraints.
  template <typename T, typename = enable_if_supports_implicit_conversion<T>>
  Expr(T v) : Expr(construct_implicit(v)) {}

  // Construct from anything that can be fed into `storage_type`.
  template <typename T, typename = storage_type::enable_if_is_constructible_t<T>>
  explicit Expr(T&& arg) noexcept(std::is_nothrow_constructible_v<storage_type, decltype(arg)>)
      : impl_(std::forward<T>(arg)) {}

  // Construct from rational.
  explicit Expr(rational_constant r);

  // Construct from expression variant.
  explicit Expr(storage_type contents) noexcept : impl_(std::move(contents)) {}

  // Test if the two expressions have the same underlying address.
  bool has_same_address(const Expr& other) const noexcept {
    return impl_.get_address() == other.impl_.get_address();
  }

  // Test if the two expressions are identical.
  bool is_identical_to(const Expr& other) const {
    if (has_same_address(other)) {
      return true;
    }
    return impl_.index() == other.impl_.index() && is_identical_to_internal(other);
  }

  // Check if the underlying expression is one of the specified types.
  template <typename... Ts>
  bool is_type() const noexcept {
    return impl_.is_type<Ts...>();
  }

  // Get the underlying type name as a string.
  std::string_view type_name() const;

  // Return the unique index of the underlying type.
  constexpr std::size_t type_index() const noexcept { return impl_.index(); }

  // Get the hash of the expression.
  constexpr std::size_t get_hash() const noexcept { return impl_.hash(); }

  // Access underlying implementation.
  constexpr const storage_type& impl() const noexcept { return impl_; }

  // Convert to string.
  std::string to_string() const;

  // Convert to string of the expression tree.
  // Defined in tree_formatter.cc
  std::string to_expression_tree_string() const;

  // Negation operator.
  Expr operator-() const;

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  Expr diff(const Expr& var, int reps = 1,
            non_differentiable_behavior behavior = non_differentiable_behavior::constant) const {
    return wf::diff(*this, var, reps, behavior);
  }

  // Distribute terms in this expression.
  Expr distribute() const { return wf::distribute(*this); }

  // Create a new expression by recursively substituting `replacement` for `target`.
  Expr subs(const Expr& target, const Expr& replacement) const {
    return wf::substitute(*this, target, replacement);
  }

  // Create a new expression by recursively replacing [variable, replacement] pairs.
  Expr substitute_variables(absl::Span<const std::tuple<Expr, Expr>> pairs) const {
    return wf::substitute_variables(*this, pairs);
  }

  // Collect terms in this expression.
  template <typename... Args>
  Expr collect(Args&&... args) const {
    if constexpr (sizeof...(Args) == 1) {
      return wf::collect(*this, std::forward<Args>(args)...);
    } else {
      return wf::collect_many(*this, {std::forward<Args>(args)...});
    }
  }

  // Evaluate into float.
  Expr eval() const { return wf::evaluate(*this); }

 protected:
  friend class MatrixExpr;

  // Construct constant from float.
  static Expr from_float(double x);

  // Construct from integer.
  static Expr from_int(std::int64_t x);

  // True if this is identical to the provided expression.
  bool is_identical_to_internal(const Expr& other) const;

  // TODO: Use checked casts here + safe numeric type.
  template <typename T>
  static Expr construct_implicit(T v) {
    static_assert(std::is_integral_v<T> || std::is_floating_point_v<T>);
    if constexpr (std::is_integral_v<T>) {
      return from_int(static_cast<std::int64_t>(v));
    } else {
      return from_float(static_cast<double>(v));
    }
  }

  storage_type impl_;
};

static_assert(std::is_nothrow_move_constructible_v<Expr> && std::is_nothrow_move_assignable_v<Expr>,
              "Should be movable");

// ostream support
inline std::ostream& operator<<(std::ostream& stream, const Expr& x) {
  stream << x.to_string();
  return stream;
}

// Math operators.
Expr operator+(const Expr& a, const Expr& b);
Expr operator-(const Expr& a, const Expr& b);
Expr operator*(const Expr& a, const Expr& b);
Expr operator/(const Expr& a, const Expr& b);

// Comparison operators. These create relational expressions, rather than directly returning bool.
Expr operator<(const Expr& a, const Expr& b);
Expr operator>(const Expr& a, const Expr& b);
Expr operator<=(const Expr& a, const Expr& b);
Expr operator>=(const Expr& a, const Expr& b);
Expr operator==(const Expr& a, const Expr& b);

// Determine relative order of two expressions (for sorting).
// Can be used to sort expressions into a canonical order, for instance to sort them.
// Implemented in ordering.cc
relative_order expression_order(const Expr& a, const Expr& b);

// Predicate for sorting expressions.
struct expression_order_struct {
  bool operator()(const Expr& a, const Expr& b) const {
    return expression_order(a, b) == relative_order::less_than;
  }
};

// Get operation precedence (order of operations).
// Implemented in expression.cc
precedence get_precedence(const Expr& expr);

// Check for strict equality. For use in template parameter lists for maps and sets.
template <typename T>
struct is_identical_struct {
  bool operator()(const T& a, const T& b) const { return a.is_identical_to(b); }
};

// Custom literal suffix support.
namespace custom_literals {
inline Expr operator"" _s(unsigned long long int arg) { return Expr{arg}; }
inline Expr operator"" _s(long double arg) { return Expr{arg}; }
}  // namespace custom_literals

// Create a tuple of `Expr` from string arguments.
template <typename... Args>
auto make_symbols(Args&&... args) {
  static_assert(std::disjunction_v<std::is_constructible<std::string_view, std::decay_t<Args>>...>,
                "Argument types must be coercible to string_view");
  return std::make_tuple(Expr{std::forward<Args>(args)}...);
}

// Support hashing of Expr
template <>
struct hash_struct<Expr> {
  std::size_t operator()(const Expr& x) const noexcept { return x.get_hash(); }
};

// Make a unique variable symbol.
Expr make_unique_variable_symbol(number_set set);

// Create an `Expr` with underlying type `T` and constructor args `Args`.
template <typename T, typename... Args>
Expr make_expr(Args&&... args) {
  return Expr{T{std::forward<Args>(args)...}};
}

// Cast expression to const pointer of the specified type.
// Returned pointer is valid in scope only as long as the argument `x` survives.
template <typename T>
const T* cast_ptr(const Expr& x) {
  if (x.is_type<T>()) {
    const T& concrete = x.impl().cast_unchecked<T>();
    return &concrete;
  } else {
    return nullptr;
  }
}

// Cast expression to const reference of the specified type. TypeError is thrown if the cast is
// invalid.
template <typename T>
const T& cast_checked(const Expr& x) {
  if (x.is_type<T>()) {
    const T& concrete = x.impl().cast_unchecked<T>();
    return concrete;
  } else {
    throw type_error("Cannot cast expression of type `{}` to `{}`", x.type_name(), T::name_str);
  }
}

// Cast expression with no checking. UB will occur if the wrong type is accessed.
template <typename T>
const T& cast_unchecked(const Expr& x) {
  const T& concrete = x.impl().cast_unchecked<T>();
  return concrete;
}

}  // namespace wf

// libfmt support:
template <>
struct fmt::formatter<wf::Expr> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::Expr& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    // TODO: This could be implemented so as to avoid the string copy.
    //  It would require all types being visible at the formatter location though.
    return fmt::format_to(ctx.out(), "{}", x.to_string());
  }
};
