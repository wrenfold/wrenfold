// Copyright 2024 Gareth Cross
#pragma once
#include <memory>
#include <vector>

#include "wf/code_generation/function_description.h"
#include "wf/code_generation/types.h"

namespace wf {

// Variant of possible expression types.
// TODO: This should be declared somewhere more general.
using any_expression = std::variant<Expr, matrix_expr, compound_expr>;

template <>
struct hash_struct<any_expression> : hash_variant<any_expression> {};
template <>
struct is_identical_struct<any_expression> : is_identical_variant<any_expression> {};
template <>
struct order_struct<any_expression> : order_variant<any_expression> {};

// A user-defined function. External functions are opaque to the library - we know their signature,
// but nothing about the internal operation. The user is responsible for mapping this object to some
// function in their code base. For now all user-defined functions are assumed to be pure - in other
// words they do not produce side-effects.
class external_function {
 public:
  external_function(std::string name, std::vector<argument> arguments, type_variant return_type);

  // Name of the function. This does not necessarily match the name in the generated code (which the
  // user is free to override as they see fit in the code-generator).
  const std::string& name() const noexcept { return impl_->name; }

  // Arguments to the function.
  const std::vector<argument>& arguments() const noexcept { return impl_->arguments; }

  // Expected number of arguments.
  std::size_t num_arguments() const noexcept { return impl_->arguments.size(); }

  // Find the index for a given argument.
  std::optional<std::size_t> arg_position(std::string_view name) const;

  // Get the argument at the specified position.
  const argument& argument_at(std::size_t position) const {
    WF_ASSERT_LESS(position, num_arguments());
    return impl_->arguments[position];
  }

  // The return type.
  const type_variant& return_type() const noexcept { return impl_->return_type; }

  // Get the return type as the specific type `T`, or assert.
  template <typename T>
  const T& return_type_as() const {
    const T* x = std::get_if<T>(&return_type());
    WF_ASSERT(x != nullptr, "Return type is not `{}`", typeid(T).name());
    return *x;
  }

  // Is the return value a scalar?
  bool is_scalar_valued() const noexcept {
    return std::holds_alternative<scalar_type>(impl_->return_type);
  }

  // The hash.
  std::size_t hash() const noexcept { return impl_->hash; }

  // Check if addresses of two functions are the same.
  bool has_same_address(const external_function& other) const noexcept {
    return impl_ == other.impl_;
  }

  // Create an invocation of this function.
  // The arguments are type-checked aginst the provided expressions.
  any_expression create_invocation(std::vector<any_expression> args) const;

 private:
  struct impl {
    // String name of the function.
    std::string name;
    // Arguments to the function.
    // `argument` is a bit over-parameterized, because we only support input args.
    // That said, it seems better than introducing another type just yet.
    std::vector<argument> arguments;
    // Function return type, which may not be void (at this time).
    type_variant return_type;
    // Cached hash for the external function.
    std::size_t hash;

    static std::shared_ptr<const impl> create(std::string name, std::vector<argument> arguments,
                                              type_variant return_type);
  };
  non_null<std::shared_ptr<const impl>> impl_;
};

template <>
struct hash_struct<external_function> {
  std::size_t operator()(const external_function& f) const noexcept { return f.hash(); }
};

template <>
struct is_identical_struct<external_function> {
  bool operator()(const external_function& a, const external_function& b) const noexcept;
};

}  // namespace wf
