// Copyright 2023 Gareth Cross
#pragma once
#include "wf/expression_concept.h"
#include "wf/expression_impl.h"
#include "wf/plain_formatter.h"

namespace math {

// A type that is substituted into user-provided expressions during code-generation.
// Typically, the user does not create these directly.
class FuncArgVariable {
 public:
  constexpr FuncArgVariable(std::size_t arg_index, std::size_t element_index) noexcept
      : arg_index_(arg_index), element_index_(element_index) {}

  constexpr bool operator<(const FuncArgVariable& other) const noexcept {
    return std::make_pair(arg_index_, element_index_) <
           std::make_pair(other.arg_index_, other.element_index_);
  }
  constexpr bool operator==(const FuncArgVariable& other) const noexcept {
    return (arg_index_ == other.arg_index_) && (element_index_ == other.element_index_);
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
class UniqueVariable {
 public:
  // Create a new variable w/ the next index.
  UniqueVariable() noexcept : index_{next_unique_variable_index()} {}

  bool operator<(const UniqueVariable& other) const noexcept { return index_ < other.index_; }
  bool operator==(const UniqueVariable& other) const noexcept { return index_ == other.index_; }

  // Retrieve the underlying index.
  constexpr std::size_t index() const noexcept { return index_; }

 private:
  static std::size_t next_unique_variable_index() noexcept;

  std::size_t index_;
};

// A variable w/ a user-provided name.
class NamedVariable {
 public:
  explicit NamedVariable(std::string_view name) noexcept : name_{name} {}

  bool operator<(const NamedVariable& other) const noexcept { return name_ < other.name_; }
  bool operator==(const NamedVariable& other) const noexcept { return name_ == other.name_; }

  // String name of the variable.
  constexpr const std::string& name() const noexcept { return name_; }

 private:
  std::string name_;
};

// A named variable used in an expression.
// This can be one of three underlying types: NamedVariable, FuncArg, UniqueVariable
class Variable {
 public:
  static constexpr std::string_view name_str = "Variable";
  static constexpr bool is_leaf_node = true;

  // There are different kinds of variables, but the different kinds don't matter to most visitors.
  // We don't expose that information at the expression level, and instead store it inside Variable.
  using IdentifierType = std::variant<NamedVariable, FuncArgVariable, UniqueVariable>;

  // Construct variable from user-provided name.
  explicit Variable(std::string name) noexcept(
      std::is_nothrow_constructible_v<IdentifierType, NamedVariable&&>)
      : identifier_{NamedVariable(std::move(name))}, set_(number_set::real) {}

  Variable(IdentifierType identifier,
           number_set set) noexcept(std::is_nothrow_move_constructible_v<IdentifierType>)
      : identifier_(std::move(identifier)), set_(set) {}

  // Check if two variables are the same.
  bool is_identical_to(const Variable& other) const {
    return identifier_ == other.identifier_ && set_ == other.set_;
  }

  // Access the variant of different variable representations.
  constexpr const auto& identifier() const noexcept { return identifier_; }

  // The numeric set this variable belongs to.
  constexpr number_set set() const noexcept { return set_; }

  // Convert the identifier to a string.
  std::string to_string() const {
    struct string_converter {
      std::string operator()(const NamedVariable& n) const { return n.name(); }
      std::string operator()(const FuncArgVariable& f) const {
        return fmt::format("$arg({}, {})", f.arg_index(), f.element_index());
      }
      std::string operator()(const UniqueVariable& u) const {
        return fmt::format("$u_{}", u.index());
      }
    };
    return std::visit(string_converter{}, identifier_);
  }

  // Create a function argument expression.
  static Expr create_function_argument(std::size_t arg_index, std::size_t element_index) {
    // TODO: Support creating function arguments that are not real.
    return make_expr<Variable>(FuncArgVariable(arg_index, element_index), number_set::real);
  }

 private:
  IdentifierType identifier_;
  number_set set_;
};

template <>
struct hash_struct<NamedVariable> {
  std::size_t operator()(const NamedVariable& v) const noexcept {
    return hash_string_fnv(v.name());
  }
};

template <>
struct hash_struct<UniqueVariable> {
  constexpr std::size_t operator()(const UniqueVariable& v) const noexcept { return v.index(); }
};

template <>
struct hash_struct<FuncArgVariable> {
  constexpr std::size_t operator()(const FuncArgVariable& v) const noexcept {
    return hash_combine(v.arg_index(), v.element_index());
  }
};

template <>
struct hash_struct<number_set> {
  constexpr std::size_t operator()(number_set set) const noexcept {
    return static_cast<std::size_t>(set);
  }
};

template <>
struct hash_struct<Variable> {
  std::size_t operator()(const Variable& v) const {
    std::size_t seed = v.identifier().index();
    seed = hash_combine(seed, std::visit([](const auto& x) { return hash(x); }, v.identifier()));
    return hash_args(seed, v.set());
  }
};

}  // namespace math
