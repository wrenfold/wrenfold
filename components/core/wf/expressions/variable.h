// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/plain_formatter.h"

namespace wf {

// A type that is substituted into user-provided expressions during code-generation.
// Typically, the user does not create these directly.
class function_argument_variable {
 public:
  constexpr function_argument_variable(const std::size_t arg_index,
                                       const std::size_t element_index) noexcept
      : arg_index_(arg_index), element_index_(element_index) {}

  constexpr bool operator<(const function_argument_variable& other) const noexcept {
    return std::make_pair(arg_index_, element_index_) <
           std::make_pair(other.arg_index_, other.element_index_);
  }
  constexpr bool operator==(const function_argument_variable& other) const noexcept {
    return arg_index_ == other.arg_index_ && element_index_ == other.element_index_;
  }

  // Which function argument this refers to.
  constexpr std::size_t arg_index() const noexcept { return arg_index_; }

  // Index into the function argument.
  // For a matrix arg, this is a flat index into the matrix.
  constexpr std::size_t element_index() const noexcept { return element_index_; }

 private:
  std::size_t arg_index_;
  std::size_t element_index_;
};

// A variable designated by a unique integer index that is not reused.
class unique_variable {
 public:
  // Create a new variable w/ the next index.
  unique_variable() : index_{next_unique_variable_index()} {}

  bool operator<(const unique_variable& other) const noexcept { return index_ < other.index_; }
  bool operator==(const unique_variable& other) const noexcept { return index_ == other.index_; }

  // Retrieve the underlying index.
  constexpr std::size_t index() const noexcept { return index_; }

 private:
  static std::size_t next_unique_variable_index();

  std::size_t index_;
};

// A variable w/ a user-provided name.
class named_variable {
 public:
  explicit named_variable(std::string name) noexcept : name_{std::move(name)} {}

  bool operator<(const named_variable& other) const noexcept { return name_ < other.name_; }
  bool operator==(const named_variable& other) const noexcept { return name_ == other.name_; }

  // String name of the variable.
  constexpr const std::string& name() const noexcept { return name_; }

 private:
  std::string name_;
};

// A named variable used in an expression.
// This can be one of three underlying types: named_variable, function_argument_variable,
// unique_variable.
class variable {
 public:
  static constexpr std::string_view name_str = "Variable";
  static constexpr bool is_leaf_node = true;

  // There are different kinds of variables, but the different kinds don't matter to most visitors.
  // We don't expose that information at the expression level, and instead store it inside Variable.
  using identifier_type = std::variant<named_variable, function_argument_variable, unique_variable>;

  // Construct variable from user-provided name.
  variable(std::string name, const number_set set)
      : identifier_{std::in_place_type_t<named_variable>(), std::move(name)}, set_(set) {}

  variable(identifier_type identifier, const number_set set) noexcept
      : identifier_(std::move(identifier)), set_(set) {}

  // Access the variant of different variable representations.
  constexpr const auto& identifier() const noexcept { return identifier_; }

  // The numeric set this variable belongs to.
  constexpr number_set set() const noexcept { return set_; }

  // Convert the identifier to a string.
  std::string to_string() const;

  // Create a function argument expression.
  static scalar_expr create_function_argument(const std::size_t arg_index,
                                              const std::size_t element_index) {
    // TODO: Support creating function arguments that are not real.
    return make_expr<variable>(function_argument_variable(arg_index, element_index),
                               number_set::real);
  }

 private:
  identifier_type identifier_;
  number_set set_;
};

template <>
struct hash_struct<named_variable> {
  std::size_t operator()(const named_variable& v) const noexcept {
    return hash_string_fnv(v.name());
  }
};

template <>
struct is_identical_struct<named_variable> {
  bool operator()(const named_variable& a, const named_variable& b) const noexcept {
    return a.name() == b.name();
  }
};

template <>
struct hash_struct<unique_variable> {
  constexpr std::size_t operator()(const unique_variable& v) const noexcept { return v.index(); }
};

template <>
struct is_identical_struct<unique_variable> {
  constexpr bool operator()(const unique_variable& a, const unique_variable& b) const noexcept {
    return a.index() == b.index();
  }
};

template <>
struct hash_struct<function_argument_variable> {
  constexpr std::size_t operator()(const function_argument_variable& v) const noexcept {
    return hash_combine(v.arg_index(), v.element_index());
  }
};

template <>
struct is_identical_struct<function_argument_variable> {
  constexpr bool operator()(const function_argument_variable& a,
                            const function_argument_variable& b) const noexcept {
    return a.arg_index() == b.arg_index() && a.element_index() == b.element_index();
  }
};

template <>
struct hash_struct<number_set> {
  constexpr std::size_t operator()(number_set set) const noexcept {
    return static_cast<std::size_t>(set);
  }
};

template <>
struct hash_struct<variable> {
  std::size_t operator()(const variable& v) const noexcept {
    return hash_args(hash_variant<variable::identifier_type>{}(v.identifier()), v.set());
  }
};

template <>
struct is_identical_struct<variable> {
  bool operator()(const variable& a, const variable& b) const {
    // Comparing identifiers will use the std::variant::operator==.
    return a.identifier() == b.identifier() && a.set() == b.set();
  }
};

template <>
struct order_struct<variable> {
  relative_order operator()(const variable& a, const variable& b) const {
    // Invoking std::variant::operator<
    if (a.identifier() < b.identifier()) {
      return relative_order::less_than;
    } else if (are_identical(a, b)) {
      return relative_order::equal;
    }
    return relative_order::greater_than;
  }
};

}  // namespace wf
