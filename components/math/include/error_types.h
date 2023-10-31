// Copyright 2023 Gareth Cross
#pragma once
#include <string_view>

#include "fmt_imports.h"

// Store all the types of exceptions we can throw.
namespace math {

// Base type for errors.
struct Exception : public std::exception {
  // Construct with moved message.
  explicit Exception(std::string&& message) noexcept : message_(std::move(message)) {}

  // Construct with format specifier and arguments.
  template <typename... Ts>
  explicit Exception(std::string_view fmt, Ts&&... args)
      : Exception(fmt::format(fmt, std::forward<Ts>(args)...)) {}

  // Retrieve error message as a string view.
  [[nodiscard]] std::string_view message() const noexcept { return message_; }

  // Implement std::exception
  const char* what() const noexcept override { return message_.c_str(); }

  virtual ~Exception() = default;

 private:
  std::string message_;
};

// Thrown when assertions fire.
struct AssertionError : public Exception {
  using Exception::Exception;
};

// Throw when an invalid type conversion occurs.
struct TypeError : public Exception {
  using Exception::Exception;
};

// Thrown when accessing invalid matrix dimensions.
struct DimensionError : public Exception {
  using Exception::Exception;
};

// Thrown when function is provided with input outside its domain.
struct DomainError : public Exception {
  using Exception::Exception;
};

}  // namespace math
