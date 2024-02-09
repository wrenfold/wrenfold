// Copyright 2024 Gareth Cross
#pragma once
#include "wf/algorithm_utils.h"
#include "wf/code_generation/types.h"
#include "wf/compound_expression.h"
#include "wf/hashing.h"
#include "wf/ordering.h"

namespace wf {

// Denote an instance of a custom type passed as an input argument to a genreated function.
class custom_type_argument {
 public:
  static constexpr std::string_view name_str = "CustomTypeArgument";
  static constexpr bool is_leaf_node = true;

  custom_type_argument(custom_type type, const std::size_t arg_index) noexcept
      : type_(std::move(type)), arg_index_(arg_index) {}

  // The custom type.
  const custom_type& type() const noexcept { return type_; }

  // Which argument was this type passed as?
  constexpr std::size_t arg_index() const noexcept { return arg_index_; }

 private:
  custom_type type_;
  std::size_t arg_index_;
};

template <>
struct hash_struct<custom_type_argument> {
  std::size_t operator()(const custom_type_argument& c) const noexcept {
    return hash_args(c.arg_index(), c.type());
  }
};

template <>
struct is_identical_struct<custom_type_argument> {
  bool operator()(const custom_type_argument& a, const custom_type_argument& b) const noexcept {
    return a.arg_index() == b.arg_index() && are_identical(a.type(), b.type());
  }
};

template <>
struct order_struct<custom_type_argument> {
  relative_order operator()(const custom_type_argument& a,
                            const custom_type_argument& b) const noexcept {
    return order_by(a.type().name(), b.type().name())
        .and_then_by_comparison(a.arg_index(), b.arg_index());
  }
};

// An instance of a custom type constructed from user-defined expressions.
class custom_type_construction {
 public:
  static constexpr std::string_view name_str = "CustomTypeConstruction";
  static constexpr bool is_leaf_node = false;
  using container_type = std::vector<scalar_expr>;

  custom_type_construction(custom_type type, std::vector<scalar_expr> args);

  // The type being constructed.
  constexpr const custom_type& type() const noexcept { return type_; }

  // Flattened list of sub-expressions that make up the custom type.
  constexpr const container_type& args() const noexcept { return args_; }

  // Number of arguments to the constructor.
  std::size_t size() const noexcept { return args_.size(); }

  // Get the specified argument.
  const scalar_expr& at(const std::size_t index) const {
    WF_ASSERT_LESS(index, size());
    return args_[index];
  }

  // Iterators over expressions that make up the custom type.
  auto begin() const noexcept { return args_.begin(); }
  auto end() const noexcept { return args_.end(); }

  template <typename F>
  compound_expr map_children(F&& f) const {
    return create(type_, transform_map<container_type>(args_, std::forward<F>(f)));
  }

  // Try to create a `custom_type_construction` expression. The expression may simplify to
  // `external_function_invocation` or `custom_type_argument` in some cases.
  static compound_expr create(custom_type type, container_type args);

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
struct is_identical_struct<custom_type_construction> {
  bool operator()(const custom_type_construction& a, const custom_type_construction& b) const {
    return are_identical(a.type(), b.type()) && are_identical(a.args(), b.args());
  }
};

template <>
struct order_struct<custom_type_construction> {
  relative_order operator()(const custom_type_construction& a,
                            const custom_type_construction& b) const {
    return order_by(a.type().name(), b.type().name()).and_then_by(a.args(), b.args());
  }
};

}  // namespace wf
