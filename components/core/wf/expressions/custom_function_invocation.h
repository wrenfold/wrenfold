// Copyright 2024 Gareth Cross
#pragma once
#include "wf/code_generation/custom_function.h"
#include "wf/compound_expression.h"

namespace wf {

//
class custom_type_argument {
 public:
  static constexpr std::string_view name_str = "CustomTypeArgument";
  static constexpr bool is_leaf_node = true;

  custom_type_argument(custom_type type, const std::size_t arg_index, std::string name) noexcept
      : type_(std::move(type)), arg_index_(arg_index), name_(std::move(name)) {}

  // The custom type.
  const custom_type& type() const noexcept { return type_; }

  // Which argument was this type passed as?
  constexpr std::size_t arg_index() const noexcept { return arg_index_; }

  // Name of the argument (used for printing).
  constexpr const std::string& name() const noexcept { return name_; }

  constexpr bool is_identical_to(const custom_type_argument& other) const noexcept {
    return arg_index_ == other.arg_index_ && name_ == other.name_ &&
           are_identical(type_, other.type_);
  }

 private:
  custom_type type_;
  std::size_t arg_index_;
  std::string name_;
};

template <>
struct hash_struct<custom_type_argument> {
  std::size_t operator()(const custom_type_argument& c) const noexcept {
    return hash_args(c.arg_index(), c.name(), c.type());
  }
};

template <>
struct order_struct<custom_type_argument> {
  relative_order operator()(const custom_type_argument& a,
                            const custom_type_argument& b) const noexcept {
    return order_by(a.type().name(), b.type().name())
        .and_then_by(a.name(), b.name())
        .and_then_by_comparison(a.arg_index(), b.arg_index());
  }
};

// A captured argument may be: a scalar, a matrix, or a compound expression.
using captured_argument = std::variant<Expr, MatrixExpr, compound_expr>;

template <>
struct hash_struct<captured_argument> : hash_variant<captured_argument> {};
template <>
struct is_identical_struct<captured_argument> : is_identical_variant<captured_argument> {};
template <>
struct order_struct<captured_argument> : order_variant<captured_argument> {};

// An invocation of a custom user-defined function.
class custom_function_invocation {
 public:
  static constexpr std::string_view name_str = "CustomFunctionInvocation";
  static constexpr bool is_leaf_node = false;

  using container_type = std::vector<captured_argument>;

  custom_function_invocation(custom_function func, container_type args);

  // The custom function being called.
  const custom_function& function() const noexcept { return function_; }

  // The arguments to the function.
  const container_type& args() const noexcept { return args_; }

  // Iterators over argument.
  auto begin() const noexcept { return args_.begin(); }
  auto end() const noexcept { return args_.end(); }

  // Function type and argument must match.
  bool is_identical_to(const custom_function_invocation& other) const;

  template <typename F>
  compound_expr map_children(F&& f) const {
    container_type args_out{};
    args_out.reserve(args_.size());
    std::transform(begin(), end(), std::back_inserter(args_out),
                   [&f](const captured_argument& arg) -> captured_argument { return f(arg); });
    return compound_expr(std::in_place_type_t<custom_function_invocation>{}, function_,
                         std::move(args_out));
  }

 private:
  custom_function function_;
  container_type args_;
};

template <>
struct hash_struct<custom_function_invocation> {
  std::size_t operator()(const custom_function_invocation& func) const {
    return hash_all(func.function().hash(), func.args());
  }
};

// To order function invocations, we need to be able to order types. This is tricky to do completely
// unambiguously because we can't meaningfully order python types. Two types could easily have the
// same name and members. We can do "best effort" by sorting by function name, and then by the
// argument content.
template <>
struct order_struct<custom_function_invocation> {
  relative_order operator()(const custom_function_invocation& a,
                            const custom_function_invocation& b) const {
    return order_by(a.function().name(), b.function().name()).and_then_by(a.args(), b.args());
  }
};

// An instance of a custom type constructed from user-defined expressions.
class custom_type_construction {
 public:
  static constexpr std::string_view name_str = "CustomTypeConstruction";
  static constexpr bool is_leaf_node = false;
  using container_type = std::vector<Expr>;

  custom_type_construction(custom_type type, std::vector<Expr> args) noexcept
      : type_(std::move(type)), args_(std::move(args)) {}

  // The type being constructed.
  constexpr const custom_type& type() const noexcept { return type_; }

  // Flattened list of sub-expressions that make up the custom type.
  constexpr const container_type& args() const noexcept { return args_; }

  // Number of arguments to the constructor.
  std::size_t size() const noexcept { return args_.size(); }

  const Expr& at(const std::size_t index) const {
    WF_ASSERT_LESS(index, size());
    return args_[index];
  }

  bool is_identical_to(const custom_type_construction& other) const {
    return are_identical(type_, other.type_) && all_identical(args_, other.args_);
  }

  // Iterators over expressions that make up the custom type.
  auto begin() const noexcept { return args_.begin(); }
  auto end() const noexcept { return args_.end(); }

  template <typename F>
  compound_expr map_children(F&& f) const {
    container_type args_out{};
    args_out.reserve(args_.size());
    std::transform(begin(), end(), std::back_inserter(args_out), std::forward<F>(f));
    return compound_expr(std::in_place_type_t<custom_type_construction>{}, type_,
                         std::move(args_out));
  }

 private:
  custom_type type_;
  container_type args_;
};

template <>
struct hash_struct<custom_type_construction> {
  std::size_t operator()(const custom_type_construction& construction) const {
    return hash_all(construction.type().hash(), construction.args());
  }
};

template <>
struct order_struct<custom_type_construction> {
  relative_order operator()(const custom_type_construction& a,
                            const custom_type_construction& b) const {
    return order_by(a.type().name(), b.type().name()).and_then_by(a.args(), b.args());
  }
};

// A single member of a compound expression. This expression acts similar to a variable - it is a
// single field from a custom type that was produced by a function invocation.
class compound_expression_element {
 public:
  static constexpr std::string_view name_str = "CompoundExpressionElement";
  static constexpr bool is_leaf_node = false;

  compound_expression_element(compound_expr provenance, const std::size_t index) noexcept
      : provenance_(std::move(provenance)), index_(index) {}

  // The invocation that produced this expression.
  constexpr const compound_expr& provenance() const noexcept { return provenance_; }

  // Index into the flattened struct denoting which member we are accessing.
  constexpr std::size_t index() const noexcept { return index_; }

  bool is_identical_to(const compound_expression_element& other) const {
    return index_ == other.index_ && provenance_.is_identical_to(other.provenance_);
  }

  // Iterators for iterating over our single expression.
  constexpr auto begin() const noexcept { return &provenance_; }
  constexpr auto end() const noexcept { return begin() + 1; }

  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    return Expr(std::in_place_type_t<compound_expression_element>{}, operation(provenance_),
                index_);
  }

 private:
  compound_expr provenance_;
  std::size_t index_;
};

template <>
struct hash_struct<compound_expression_element> {
  std::size_t operator()(const compound_expression_element& func) const {
    return hash_args(func.index(), func.provenance());
  }
};

template <>
struct order_struct<compound_expression_element> {
  relative_order operator()(const compound_expression_element& a,
                            const compound_expression_element& b) const {
    return order_by(a.provenance(), b.provenance()).and_then_by_comparison(a.index(), b.index());
  }
};

}  // namespace wf
