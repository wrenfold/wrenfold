// Copyright 2023 Gareth Cross
#pragma once
#include <any>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "wf/assertions.h"
#include "wf/enumerations.h"

namespace wf {

// Variant over possible types that can appear in generated code.
using type_variant =
    std::variant<class scalar_type, class matrix_type, std::shared_ptr<const class custom_type>>;

// Represent a scalar argument type (float, int, etc).
class scalar_type {
 public:
  explicit constexpr scalar_type(code_numeric_type numeric_type) noexcept
      : numeric_type_(numeric_type) {}

  // The underlying numeric type.
  constexpr code_numeric_type numeric_type() const noexcept { return numeric_type_; }

 private:
  code_numeric_type numeric_type_;
};

// Represent a matrix argument type. The dimensions are known at generation time.
class matrix_type {
 public:
  constexpr matrix_type(index_t rows, index_t cols) noexcept : rows_(rows), cols_(cols) {}

  constexpr index_t rows() const noexcept { return rows_; }
  constexpr index_t cols() const noexcept { return cols_; }

  constexpr std::size_t size() const noexcept { return static_cast<std::size_t>(rows_ * cols_); }

  // Convert flat index to [row, col] indices (assuming row major order).
  std::tuple<index_t, index_t> compute_indices(std::size_t element) const {
    WF_ASSERT_LESS(element, size());
    return std::make_pair(static_cast<index_t>(element) / cols_,
                          static_cast<index_t>(element) % cols_);
  }

 private:
  index_t rows_;
  index_t cols_;
};

// A user-defined type that we support as an input/output to functions.
class custom_type {
 public:
  // Shared pointer to const. Once a type is assembled, it is immutable.
  using const_shared_ptr = std::shared_ptr<const custom_type>;

  // Unfortunately, plain non-const shared_ptr is still required for interacting with python.
  using shared_ptr = std::shared_ptr<custom_type>;

  // A field on a custom type.
  class field {
   public:
    // Construct with name and type. Asserts that type is non-empty.
    field(std::string name_in, type_variant type_in);

    constexpr const std::string& name() const noexcept { return name_; }
    constexpr const type_variant& type() const noexcept { return type_; }

   private:
    // Name of the field (these must be unique).
    std::string name_;
    // The type of the field.
    type_variant type_;
  };

  // Construct with fields. Asserts that all fields have unique names.
  custom_type(std::string name, std::vector<field> fields, std::any python_type);

  // Access a field by name. May return nullptr if the field does not exist.
  const field* field_by_name(std::string_view name) const noexcept;

  // Name of the type.
  constexpr const std::string& name() const noexcept { return name_; }

  // Access all fields.
  constexpr const auto& fields() const noexcept { return fields_; }

  // py::type (or empty) for types that are defined in python.
  constexpr const auto& python_type() const noexcept { return python_type_; }

  // Number of fields.
  std::size_t size() const noexcept { return fields_.size(); }

 private:
  // Name of the type.
  std::string name_;

  // All the fields in the type. If this type was defined in python, this vector should be ordered
  // the same as the fields on the python type.
  std::vector<field> fields_;

  // An opaque strong reference to the `py::type` that represents this type in python.
  // We hold this as std::any so that pybind11 is not an explicit dependency here.
  std::any python_type_{};
};

// Represent the operation of reading a field on a custom type.
class field_access {
 public:
  // Construct with strong ptr of the custom type we are accessing. Asserts that `type` is non-null.
  field_access(custom_type::const_shared_ptr type, std::string name);

  // The underlying type we are accessing a member on.
  constexpr const custom_type::const_shared_ptr& type() const noexcept { return type_; }

  // Underlying field name.
  constexpr const std::string& field_name() const noexcept { return field_name_; }

 private:
  custom_type::const_shared_ptr type_;
  std::string field_name_;
};

// Represent the operation of reading from a matrix nested within a custom user-specified type.
class matrix_access {
 public:
  constexpr matrix_access(matrix_type type, std::size_t element_index) noexcept
      : type_(type), element_index_(element_index) {}

  // Type of matrix we are reading from.
  constexpr const matrix_type& type() const noexcept { return type_; }

  // Return row and column indices used to access the matrix.
  std::tuple<index_t, index_t> indices() const { return type_.compute_indices(element_index_); }

  // Return row index.
  index_t row() const { return std::get<0>(type_.compute_indices(element_index_)); }

  // Return col index.
  index_t col() const { return std::get<1>(type_.compute_indices(element_index_)); }

 private:
  matrix_type type_;
  std::size_t element_index_;
};

using access_variant = std::variant<field_access, matrix_access>;

// Given a flat index, determine the sequence of accessors required to find the appropriate element
// in an aggregate custom type. Performs a linear traversal of the object hierarchy until we reach
// the index'th element, recording every read required.
std::vector<access_variant> determine_access_sequence(
    const custom_type::const_shared_ptr& top_level_type, std::size_t index);

}  // namespace wf
