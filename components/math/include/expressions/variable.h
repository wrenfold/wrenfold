// Copyright 2023 Gareth Cross
#pragma once
#include "expression_concept.h"
#include "expression_impl.h"
#include "plain_formatter.h"

namespace math {

class FuncArg {
 public:
  constexpr FuncArg(std::size_t arg_index, std::size_t element_index) noexcept
      : arg_index_(arg_index), element_index_(element_index) {}

  constexpr bool operator<(const FuncArg& other) const noexcept {
    return std::make_pair(arg_index_, element_index_) <
           std::make_pair(other.arg_index_, other.element_index_);
  }
  constexpr bool operator==(const FuncArg& other) const noexcept {
    return (arg_index_ == other.arg_index_) && (element_index_ == other.element_index_);
  }

  constexpr std::size_t arg_index() const noexcept { return arg_index_; }
  constexpr std::size_t element_index() const noexcept { return element_index_; }

 private:
  std::size_t arg_index_;
  std::size_t element_index_;
};

class UniqueVariable {
 public:
  // Create a new variable w/ the next index.
  UniqueVariable() noexcept : index_{next_unique_variable_index()} {}

  bool operator<(const UniqueVariable& other) const noexcept { return index_ < other.index_; }
  bool operator==(const UniqueVariable& other) const noexcept { return index_ == other.index_; }

  constexpr std::size_t index() const noexcept { return index_; }

 private:
  static std::size_t next_unique_variable_index() noexcept;

  std::size_t index_;
};

class NamedVariable {
 public:
  explicit NamedVariable(std::string name) noexcept : name_(std::move(name)) {}

  bool operator<(const NamedVariable& other) const noexcept { return name_ < other.name_; }
  bool operator==(const NamedVariable& other) const noexcept { return name_ == other.name_; }

  constexpr const std::string& name() const noexcept { return name_; }

 private:
  std::string name_;
};

// A named variable used in an expression.
class Variable {
 public:
  static constexpr std::string_view NameStr = "Variable";
  static constexpr bool IsLeafNode = true;

  // Construct variable from user-provided name.
  explicit Variable(std::string name) noexcept : content_{NamedVariable(std::move(name))} {}

  template <typename T>
  explicit Variable(T&& content) noexcept(std::is_nothrow_constructible_v<decltype(content_), T>)
      : content_(std::forward<T>(content)) {}

  // Check if two variables are the same.
  bool is_identical_to(const Variable& other) const { return content_ == other.content_; }

  // Access the variant of different variable representations.
  constexpr const auto& content() const noexcept { return content_; }

  // Convert to string.
  std::string to_string() const {
    struct string_converter {
      std::string operator()(const NamedVariable& n) const { return n.name(); }
      std::string operator()(const FuncArg& f) const {
        return fmt::format("$arg({}, {})", f.arg_index(), f.element_index());
      }
      std::string operator()(const UniqueVariable& u) const {
        return fmt::format("$u_{}", u.index());
      }
    };
    return std::visit(string_converter{}, content_);
  }

  // Create a function argument expression.
  static Expr create_function_argument(std::size_t arg_index, std::size_t element_index) {
    return make_expr<Variable>(FuncArg(arg_index, element_index));
  }

 private:
  std::variant<NamedVariable, FuncArg, UniqueVariable> content_;
};

template <>
struct hash_struct<NamedVariable> {
  constexpr std::size_t operator()(const NamedVariable& v) const noexcept {
    return hash_string_fnv(v.name());
  }
};

template <>
struct hash_struct<UniqueVariable> {
  constexpr std::size_t operator()(const UniqueVariable& v) const noexcept { return v.index(); }
};

template <>
struct hash_struct<FuncArg> {
  constexpr std::size_t operator()(const FuncArg& v) const noexcept {
    return hash_combine(v.arg_index(), v.element_index());
  }
};

template <>
struct hash_struct<Variable> {
  std::size_t operator()(const Variable& v) const {
    const std::size_t seed = v.content().index();
    const std::size_t inner_hash = std::visit(
        [](const auto& x) {
          using T = std::decay_t<decltype(x)>;
          return hash_struct<T>{}(x);
        },
        v.content());
    return hash_combine(seed, inner_hash);
  }
};

}  // namespace math
