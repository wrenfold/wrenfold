// Copyright 2023 Gareth Cross
#pragma once
#include <memory>
#include <string>
#include <typeindex>
#include <variant>
#include <vector>

#include "wf/assertions.h"
#include "wf/checked_pointers.h"
#include "wf/enumerations.h"
#include "wf/hashing.h"

namespace wf {

// Represent a scalar argument type (float, int, etc).
class scalar_type {
 public:
  static constexpr std::string_view snake_case_name_str = "scalar_type";

  explicit constexpr scalar_type(const code_numeric_type numeric_type) noexcept
      : numeric_type_(numeric_type) {}

  // The underlying numeric type.
  constexpr code_numeric_type numeric_type() const noexcept { return numeric_type_; }

 private:
  code_numeric_type numeric_type_;
};

template <>
struct hash_struct<scalar_type> {
  constexpr std::size_t operator()(const scalar_type s) const noexcept {
    return static_cast<std::size_t>(s.numeric_type());
  }
};

template <>
struct is_identical_struct<scalar_type> {
  constexpr bool operator()(const scalar_type a, const scalar_type b) const noexcept {
    return a.numeric_type() == b.numeric_type();
  }
};

// Represent a matrix argument type. The dimensions are known at generation time.
class matrix_type {
 public:
  static constexpr std::string_view snake_case_name_str = "matrix_type";

  constexpr matrix_type(index_t rows, index_t cols) noexcept : rows_(rows), cols_(cols) {}

  constexpr index_t rows() const noexcept { return rows_; }
  constexpr index_t cols() const noexcept { return cols_; }

  constexpr std::tuple<index_t, index_t> dimensions() const noexcept {
    return std::make_tuple(rows_, cols_);
  }

  constexpr std::size_t size() const noexcept { return static_cast<std::size_t>(rows_ * cols_); }

  // Convert flat index to [row, col] indices (assuming row major order).
  std::tuple<index_t, index_t> compute_indices(std::size_t element) const {
    WF_ASSERT_LESS(element, size());
    return std::make_tuple(static_cast<index_t>(element) / cols_,
                           static_cast<index_t>(element) % cols_);
  }

 private:
  index_t rows_;
  index_t cols_;
};

template <>
struct hash_struct<matrix_type> {
  constexpr std::size_t operator()(const matrix_type& m) const noexcept {
    return hash_combine(static_cast<std::size_t>(m.rows()), static_cast<std::size_t>(m.cols()));
  }
};

template <>
struct is_identical_struct<matrix_type> {
  constexpr bool operator()(const matrix_type& a, const matrix_type& b) const noexcept {
    return a.dimensions() == b.dimensions();
  }
};

// Wrapper around py::type (we implement this in the wrapper module).
// Holds something that implements `concept` and stores a py::type.
class erased_pytype {
 public:
  class concept {
   public:
    virtual ~concept() = default;
    virtual bool is_identical_to(const concept& other) const = 0;
    virtual std::size_t hash() const = 0;
  };

  template <typename T, typename... Args,
            typename = std::enable_if_t<std::is_base_of_v<concept, T>>>
  explicit erased_pytype(std::in_place_type_t<T>, Args&&... args)
      : impl_(std::make_unique<T>(std::forward<Args>(args)...)) {}

  bool is_identical_to(const erased_pytype& other) const {
    return impl_->is_identical_to(*other.impl_.get());
  }

  std::size_t hash() const { return impl_->hash(); }

  // Unchecked cast.
  template <typename T>
  const T& as() const noexcept {
    return static_cast<const T&>(*impl_.get());
  }

 private:
  non_null<std::unique_ptr<const concept>> impl_;
};

// A user-defined type that we support as an input/output to functions.
class custom_type {
 public:
  static constexpr std::string_view snake_case_name_str = "custom_type";

  using underlying_type_variant = std::variant<std::type_index, erased_pytype>;

  // Construct with fields. Asserts that all fields have unique names.
  custom_type(std::string name, std::vector<class struct_field> fields,
              underlying_type_variant underlying_type);

  // Access a field by name. May return nullptr if the field does not exist.
  const struct_field* field_by_name(std::string_view name) const noexcept;

  // Check if underlying address matches.
  bool has_same_address(const custom_type& other) const noexcept { return impl_ == other.impl_; }

  // Name of the type.
  const std::string& name() const noexcept { return impl_->name; }

  // Access all fields.
  const auto& fields() const noexcept { return impl_->fields; }

  // Access the underlying type variant.
  const auto& underlying_type() const noexcept { return impl_->underying_type; }

  // Hash of this object.
  std::size_t hash() const noexcept { return impl_->hash; }

  // Access the erased py::type.
  maybe_null<const erased_pytype*> underying_pytype() const noexcept {
    return std::get_if<erased_pytype>(&impl_->underying_type);
  }

  // Number of fields.
  std::size_t size() const noexcept { return impl_->fields.size(); }

  // The total size (including all sub-structs).
  std::size_t total_size() const noexcept;

  // Check if the underlying native type is C++ type `T`.
  template <typename T>
  bool is_native_type() const noexcept {
    if (const std::type_index* type_index = std::get_if<std::type_index>(&impl_->underying_type);
        type_index != nullptr) {
      return *type_index == typeid(T);
    }
    return false;
  }

 private:
  struct impl {
    // Name of the type.
    std::string name;
    // All the fields in the type. If this type was defined in python, this vector should be ordered
    // the same as the fields on the python type.
    std::vector<struct_field> fields;
    // Either the type_index of the C++ type, or a unique pointer to an object that stores
    // a strong reference to the `py::type` that represents this type in python.
    underlying_type_variant underying_type;
    // Cached hash of this object.
    std::size_t hash;
  };

  static std::shared_ptr<const impl> create_impl(std::string name, std::vector<struct_field> fields,
                                                 underlying_type_variant underlying_type);

  // This object is saved in multiple places, and returned into python.
  non_null<std::shared_ptr<const impl>> impl_;
};

template <>
struct hash_struct<custom_type> {
  std::size_t operator()(const custom_type& c) const noexcept { return c.hash(); }
};

template <>
struct is_identical_struct<custom_type> {
  bool operator()(const custom_type& a, const custom_type& b) const noexcept;
};

// Variant over possible types that can appear in generated code.
using type_variant = std::variant<scalar_type, matrix_type, custom_type>;

template <>
struct hash_struct<type_variant> : hash_variant<type_variant> {};
template <>
struct is_identical_struct<type_variant> : is_identical_variant<type_variant> {};

// Stores a type-erased object that can set/get the C++ member variable corresponding to a given
// field. This applies only to custom types that are defined in C++. The native field accessor is
// used to unpack or pack a flat vector of symbolic variables into and out of a C++ struct.
class native_field_accessor {
 public:
  // Our 'concept' type can't expose any useful members, because it would need to use types
  // that we are trying to erase here. We just need a virtual base to downcast elsewhere.
  class concept {
   public:
    virtual ~concept() = default;
  };

  template <typename T>
  using enable_if_implements_concept_v = std::enable_if_t<std::is_base_of_v<concept, T>>;

  // Construct with an object that inherits from `concept`.
  template <typename T, typename = enable_if_implements_concept_v<T>>
  explicit native_field_accessor(T&& arg)
      : impl_(std::make_shared<const T>(std::forward<T>(arg))) {}

  // Construct empty.
  native_field_accessor() noexcept = default;

  // Unchecked downcast to type `U`.
  template <typename U>
  const U& as() const {
    return static_cast<const U&>(*impl_.get());
  }

  operator bool() const noexcept { return static_cast<bool>(impl_); }  // NOLINT

 private:
  // shared_ptr because field is copied by the python wrapper.
  maybe_null<std::shared_ptr<const concept>> impl_{nullptr};
};

// A field on a custom type.
// We need to define this here so that the definition of custom_type is visible for type_variant.
// We can still use field above in `custom_type` because std::vector does not require the type
// be visible (as of c++17): https://stackoverflow.com/questions/56975491/
class struct_field {
 public:
  // Construct with name and type. Asserts that type is non-empty.
  struct_field(std::string name, type_variant type);

  // Construct with name, type, and setter/getter.
  struct_field(std::string name, type_variant type, native_field_accessor accessor);

  constexpr const std::string& name() const noexcept { return name_; }
  constexpr const auto& type() const noexcept { return type_; }
  constexpr const auto& native_accessor() const noexcept { return native_accessor_; }

 private:
  // Name of the field (these must be unique).
  std::string name_;
  // The type of the field.
  type_variant type_;
  // Object that can get/set the field on the corresponding C++ type.
  // This value is empty for python types.
  native_field_accessor native_accessor_;
};

template <>
struct hash_struct<struct_field> {
  std::size_t operator()(const struct_field& field) const noexcept {
    return hash_combine(hash_string_fnv(field.name()), hash(field.type()));
  }
};

template <>
struct is_identical_struct<struct_field> {
  bool operator()(const struct_field& a, const struct_field& b) const noexcept {
    return a.name() == b.name() && is_identical_struct<type_variant>{}(a.type(), b.type());
  }
};

namespace detail {
// Supporting struct for `iterate_custom_type_fields`.
template <typename F>
struct iterate_custom_type_fields_struct {
  explicit iterate_custom_type_fields_struct(const F& f) noexcept : f_(f) {}

  void operator()(const custom_type& c) {
    for (const struct_field& field : c.fields()) {
      if constexpr (std::is_invocable_v<F, const custom_type&, std::size_t>) {
        f_(c, index());
      }
      std::visit(*this, field.type());
    }
  }

  void operator()(const scalar_type s) {
    f_(s, index());
    ++index_;
  }

  void operator()(const matrix_type& m) {
    f_(m, index());
    index_ += m.size();
  }

 private:
  constexpr std::size_t index() const noexcept { return index_; }

  const F& f_;
  std::size_t index_{0};
};
}  // namespace detail

// Iterate over the fields of a custom type with the provided lambda.
template <typename F>
void iterate_custom_type_fields(const custom_type& type, F&& f) {
  detail::iterate_custom_type_fields_struct<std::decay_t<F>>{f}(type);
}

// Represent the operation of reading a field on a custom type.
class field_access {
 public:
  // Construct with strong ptr of the custom type we are accessing. Asserts that `type` is non-null.
  field_access(custom_type type, std::string name);

  // The underlying type we are accessing a member on.
  constexpr const custom_type& type() const noexcept { return type_; }

  // Underlying field name.
  constexpr const std::string& field_name() const noexcept { return field_name_; }

 private:
  custom_type type_;
  std::string field_name_;
};

// Represent the operation of reading from a matrix nested within a custom user-specified type.
class matrix_access {
 public:
  constexpr matrix_access(const matrix_type type, const std::size_t element_index) noexcept
      : type_(type), element_index_(element_index) {}

  // Type of matrix we are reading from.
  constexpr const matrix_type& type() const noexcept { return type_; }

  // Return row and column indices used to access the matrix.
  std::tuple<index_t, index_t> indices() const { return type_.compute_indices(element_index_); }

  // Return row index.
  index_t row() const { return std::get<0>(indices()); }

  // Return col index.
  index_t col() const { return std::get<1>(indices()); }

 private:
  matrix_type type_;
  std::size_t element_index_;
};

using access_variant = std::variant<field_access, matrix_access>;

// Given a flat index, determine the sequence of accessors required to find the appropriate element
// in an aggregate custom type. Performs a linear traversal of the object hierarchy until we reach
// the index'th element, recording every read required.
std::vector<access_variant> determine_access_sequence(const custom_type& top_level_type,
                                                      std::size_t index);

}  // namespace wf
