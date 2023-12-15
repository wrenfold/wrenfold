// Copyright 2023 Gareth Cross
#pragma once
#include <memory>
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
  std::pair<index_t, index_t> compute_indices(std::size_t element) const {
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
  custom_type(std::string name, std::vector<field> fields);

  // Access a field by name. May return nullptr if the field does not exist.
  const field* field_by_name(std::string_view name) noexcept;

  // Name of the type.
  constexpr const std::string& name() const noexcept { return name_; }

  // Access all fields.
  constexpr const auto& fields() const noexcept { return fields_; }

  // Number of fields.
  std::size_t size() const noexcept { return fields_.size(); }

 private:
  // Name of the type.
  std::string name_;

  // All the fields in the type. If this type was defined in python, this vector should be ordered
  // the same as the fields on the python type.
  std::vector<field> fields_;
};

}  // namespace wf
