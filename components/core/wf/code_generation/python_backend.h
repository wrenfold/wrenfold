// wrenfold symbolic code generator.
// Copyright (c) 2026 wrenfold contributors
// For license information refer to accompanying LICENSE file.
#pragma once
#include <span>
#include <string>
#include <string_view>

#include "wf/code_generation/python_code_generator.h"

namespace wf::detail {

// Internal runtime semantics for generated Python. Operands have already been formatted through
// python_code_generator's virtual methods, so backend operations do not bypass user overrides.
// Adapters are stateless and emit self-contained expressions without extra runtime imports.
class python_backend {
 public:
  virtual ~python_backend() = default;

  constexpr std::string_view module_alias() const noexcept { return module_alias_; }
  constexpr std::string_view module_import() const noexcept { return module_import_; }
  constexpr std::string_view matrix_type_name() const noexcept { return matrix_type_name_; }

  virtual std::string convert_scalar(std::string_view value, numeric_primitive_type type,
                                     python_generator_float_width width) const = 0;
  virtual std::string convert_matrix(std::string_view value, const matrix_type& type,
                                     python_generator_float_width width) const = 0;
  virtual std::string construct_matrix(std::span<const std::string> values, const matrix_type& type,
                                       python_generator_float_width width) const = 0;
  virtual std::string assign_matrix(std::string_view destination, std::string_view value) const = 0;
  virtual std::string matrix_size(std::string_view value) const = 0;

  virtual std::string floating_literal(std::string_view value,
                                       python_generator_float_width width) const;
  std::string symbolic_constant(symbolic_constant_enum value,
                                python_generator_float_width width) const;
  virtual std::string call_math(std_math_function function, std::span<const std::string> args,
                                python_generator_float_width width) const;
  std::string select(std::string_view condition, std::string_view left,
                     std::string_view right) const;

 protected:
  constexpr python_backend(std::string_view module_alias, std::string_view module_import,
                           std::string_view matrix_type_name) noexcept
      : module_alias_(module_alias),
        module_import_(module_import),
        matrix_type_name_(matrix_type_name) {}

  std::string dtype(numeric_primitive_type type, python_generator_float_width width) const;

 private:
  std::string_view module_alias_;
  std::string_view module_import_;
  std::string_view matrix_type_name_;
};

// Return an immutable adapter shared by generators targeting the same runtime. Configuration is
// passed per operation to preserve the generator's existing copy and subclass behavior.
const python_backend& get_python_backend(python_generator_target target);

}  // namespace wf::detail
