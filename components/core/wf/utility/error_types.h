// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <string_view>

#include "wf/utility/third_party_imports.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

// Store all the types of exceptions we can throw.
namespace wf {

// Base type for errors.
struct exception_base : std::exception {
  // Construct with moved message.
  explicit exception_base(std::string&& message) noexcept : message_(std::move(message)) {}

  // Construct with format specifier and arguments.
  template <typename... Ts>
  explicit exception_base(std::string_view fmt, Ts&&... args)
      : exception_base(fmt::vformat(fmt, fmt::make_format_args(args...))) {}

  // Retrieve error message as a string view.
  [[nodiscard]] std::string_view message() const noexcept { return message_; }

  // Implement std::exception
  const char* what() const noexcept override { return message_.c_str(); }

 private:
  std::string message_;
};

// Thrown when assertions fire.
struct assertion_error final : exception_base {
  using exception_base::exception_base;
};

// Throw when an invalid type conversion occurs.
struct type_error final : exception_base {
  using exception_base::exception_base;
};

// Throw on invalid numeric arithmetic.
struct arithmetic_error final : exception_base {
  using exception_base::exception_base;
};

// Thrown when accessing invalid matrix dimensions.
struct dimension_error final : exception_base {
  using exception_base::exception_base;
};

// Thrown when function is provided with input outside its domain.
struct domain_error final : exception_base {
  using exception_base::exception_base;
};

// Thrown when an invalid argument is specified.
struct invalid_argument_error final : exception_base {
  using exception_base::exception_base;
};

}  // namespace wf
