// Copyright 2023 Gareth Cross
#pragma once
#include <string_view>

#include "wf/fmt_imports.h"

// Store all the types of exceptions we can throw.
namespace math {

// Base type for errors.
struct exception_base : public std::exception {
  // Construct with moved message.
  explicit exception_base(std::string&& message) noexcept : message_(std::move(message)) {}

  // Construct with format specifier and arguments.
  template <typename... Ts>
  explicit exception_base(std::string_view fmt, Ts&&... args)
      : exception_base(fmt::format(fmt, std::forward<Ts>(args)...)) {}

  // Retrieve error message as a string view.
  [[nodiscard]] std::string_view message() const noexcept { return message_; }

  // Implement std::exception
  const char* what() const noexcept override { return message_.c_str(); }

  virtual ~exception_base() = default;

 private:
  std::string message_;
};

// Thrown when assertions fire.
struct assertion_error : public exception_base {
  using exception_base::exception_base;
};

// Throw when an invalid type conversion occurs.
struct type_error : public exception_base {
  using exception_base::exception_base;
};

// Thrown when accessing invalid matrix dimensions.
struct dimension_error : public exception_base {
  using exception_base::exception_base;
};

// Thrown when function is provided with input outside its domain.
struct domain_error : public exception_base {
  using exception_base::exception_base;
};

}  // namespace math
