// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/code_generation/types.h"
#include "wf/expression.h"
#include "wf/utility/hashing.h"
#include "wf/utility/ordering.h"

namespace wf {

// A type that is substituted into user-provided expressions during code-generation.
// Typically, the user does not create these directly.
class function_argument_variable {
 public:
  static constexpr std::string_view name_str = "FunctionArgumentVariable";
  static constexpr bool is_leaf_node = true;

  function_argument_variable(std::string name, type_variant argument_type,
                             const std::size_t element_index) noexcept
      : name_(std::move(name)),
        argument_type_(std::move(argument_type)),
        element_index_(element_index) {}

  // Which function argument this refers to.
  constexpr const std::string& argument_name() const noexcept { return name_; }

  // Index into the function argument.
  // For a matrix arg, this is a flat index into the matrix.
  constexpr std::size_t element_index() const noexcept { return element_index_; }

  // The type of the argument.
  constexpr const type_variant& argument_type() const noexcept { return argument_type_; }

  // The numeric type of this variable.
  numeric_primitive_type primitive_type() const noexcept;

 private:
  std::string name_;
  type_variant argument_type_;
  std::size_t element_index_;
};

// A variable designated by a unique integer index that is not reused.
class unique_variable {
 public:
  static constexpr std::string_view name_str = "UniqueVariable";
  static constexpr bool is_leaf_node = true;

  // Create a new variable w/ the next index.
  explicit unique_variable(const number_set set)
      : index_{next_unique_variable_index()}, set_(set) {}

  // Retrieve the underlying index.
  constexpr std::size_t index() const noexcept { return index_; }

  // Numeric set the unique variable belongs to.
  constexpr number_set set() const noexcept { return set_; }

 private:
  static std::size_t next_unique_variable_index();

  std::size_t index_;
  number_set set_;
};

// A variable w/ a user-provided name.
class variable {
 public:
  static constexpr std::string_view name_str = "Variable";
  static constexpr bool is_leaf_node = true;

  explicit variable(std::string name, const number_set set) noexcept
      : name_{std::move(name)}, set_(set) {}

  // String name of the variable.
  constexpr const std::string& name() const noexcept { return name_; }

  // Numeric set the variable belongs to.
  constexpr number_set set() const noexcept { return set_; }

 private:
  std::string name_;
  number_set set_;
};

template <>
struct hash_struct<variable> {
  std::size_t operator()(const variable& v) const noexcept {
    return hash_combine(hash_string_fnv(v.name()), static_cast<std::size_t>(v.set()));
  }
};

template <>
struct is_identical_struct<variable> {
  bool operator()(const variable& a, const variable& b) const noexcept {
    return a.name() == b.name() && a.set() == b.set();
  }
};

template <>
struct order_struct<variable> {
  constexpr relative_order operator()(const variable& a, const variable& b) const noexcept {
    return order_by(a.name(), b.name()).and_then_by(a.set(), b.set());
  }
};

template <>
struct hash_struct<unique_variable> {
  constexpr std::size_t operator()(const unique_variable& v) const noexcept {
    return hash_combine(v.index(), static_cast<std::size_t>(v.set()));
  }
};

template <>
struct is_identical_struct<unique_variable> {
  constexpr bool operator()(const unique_variable& a, const unique_variable& b) const noexcept {
    return a.index() == b.index() && a.set() == b.set();
  }
};

template <>
struct order_struct<unique_variable> {
  constexpr relative_order operator()(const unique_variable& a,
                                      const unique_variable& b) const noexcept {
    return order_by(a.index(), b.index()).and_then_by(a.set(), b.set());
  }
};

template <>
struct hash_struct<function_argument_variable> {
  std::size_t operator()(const function_argument_variable& v) const noexcept {
    return hash_combine(hash_combine(hash_string_fnv(v.argument_name()), v.element_index()),
                        static_cast<std::size_t>(v.primitive_type()));
  }
};

template <>
struct is_identical_struct<function_argument_variable> {
  bool operator()(const function_argument_variable& a,
                  const function_argument_variable& b) const noexcept {
    return a.argument_name() == b.argument_name() && a.element_index() == b.element_index() &&
           a.primitive_type() == b.primitive_type();
  }
};

template <>
struct order_struct<function_argument_variable> {
  relative_order operator()(const function_argument_variable& a,
                            const function_argument_variable& b) const noexcept {
    return order_by(a.argument_name(), b.argument_name())
        .and_then_by(a.element_index(), b.element_index())
        .and_then_by(a.primitive_type(), b.primitive_type());
  }
};

}  // namespace wf
