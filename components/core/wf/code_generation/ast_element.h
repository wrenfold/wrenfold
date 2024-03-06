// Copyright 2024 Gareth Cross
#pragma once
#include <memory>

#include "wf/type_list.h"

namespace wf::ast {
class ast_element;

namespace detail {
// Forward declare.
template <typename T>
const auto& cast_to_type(const ast_element& element) noexcept;
}  // namespace detail

// A list of (most of) the types that appear in the output AST. We store these inside `ast_element`.
// clang-format off
using ast_element_types = type_list<
    struct add,
    struct assign_temporary,
    struct assign_output_matrix,
    struct assign_output_scalar,
    struct assign_output_struct,
    struct boolean_literal,
    struct branch,
    struct call_external_function,
    struct call_std_function,
    struct cast,
    struct comment,
    struct compare,
    struct construct_custom_type,
    struct construct_matrix,
    struct declaration,
    struct divide,
    struct float_literal,
    struct get_argument,
    struct get_field,
    struct get_matrix_element,
    struct integer_literal,
    struct multiply,
    struct negate,
    struct optional_output_branch,
    struct special_constant,
    struct return_object,
    struct variable_ref
    >;
// clang-format on

// Store an immutable type-erased instance of one of the types in `ast_element_types`. Alongside the
// object contents, we store an index (similar to a variant) and provide a `visit` method for
// accessing the underlying ast type. This object is reference counted, since the same ast element
// may appear more than once in the output.
// TODO: This type bears a lot of similarities to expression_variant. There may be a path to
//  depuplicating these a bit.
class ast_element {
 public:
  using types = ast_element_types;

  // Check if the decayed type `T` is in our list of supported types.
  template <typename T>
  using enable_if_is_constructible_t = enable_if_contains_type_t<std::decay_t<T>, types>;

  // Copy or move construct from an instance of type `T`.
  template <typename T, typename U = std::decay_t<T>, typename = enable_if_is_constructible_t<T>>
  explicit ast_element(T&& value) noexcept(std::is_nothrow_constructible_v<U, decltype(value)>)
      : ptr_{std::make_shared<const model<U>>(std::forward<T>(value))} {}

  // In-place construct type `T` from `Args`.
  template <typename T, typename... Args, typename = enable_if_is_constructible_t<T>>
  explicit ast_element(std::in_place_type_t<T>, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<T, decltype(args)...>)
      : ptr_{std::make_shared<const model<T>>(std::in_place_t{}, std::forward<Args>(args)...)} {}

  // Return index indicating which type is stored.
  std::size_t index() const noexcept { return ptr_->index(); }

  // Access the underlying shared pointer.
  constexpr const auto& impl() const noexcept { return ptr_; }

 private:
  // Abstract base type for our contents.
  class concept_base {
   public:
    virtual ~concept_base() = default;

    template <typename T>
    explicit concept_base(std::in_place_type_t<T>) noexcept
        : index_(type_list_index_v<T, ast_element_types>) {}

    // Index of the ast element in `ast_element_types`.
    constexpr std::size_t index() const noexcept { return index_; }

   private:
    std::size_t index_;
  };

  // Concrete storage of the underlying value of type T.
  template <typename T>
  class model final : public concept_base {
   public:
    using value_type = T;

    // Copy or move construct.
    explicit model(value_type contents) noexcept(std::is_nothrow_move_constructible_v<value_type>)
        : concept_base(std::in_place_type_t<T>{}), contents_(std::move(contents)) {}

    // In-place construction of contents.
    template <typename... Args>
    explicit model(std::in_place_t, Args&&... args) noexcept(
        std::is_nothrow_constructible_v<value_type, decltype(args)...>)
        : concept_base(std::in_place_type_t<T>{}), contents_{std::forward<Args>(args)...} {}

    constexpr const value_type& contents() const noexcept { return contents_; }
    constexpr value_type& contents() noexcept { return contents_; }

   private:
    T contents_;
  };

  // Cast with no type-checking.
  template <typename T>
  friend const auto& ::wf::ast::detail::cast_to_type(const ast_element& element) noexcept;

  // Our storage, which cannot be null. We use shared-ptr since the same AST element can be
  // re-used in some places. TODO: Support small object optimization?
  std::shared_ptr<const concept_base> ptr_;
};

}  // namespace wf::ast
