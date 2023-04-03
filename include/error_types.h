// Copyright 2023 Gareth Cross
#pragma once
#include <string_view>

#ifdef _MSC_VER
// Silence some warnings that libfmt can trigger w/ Microsoft compiler.
#pragma warning(push)
#pragma warning(disable : 4583)
#pragma warning(disable : 4582)
#endif  // _MSC_VER
#include <fmt/format.h>
#ifdef _MSC_VER
#pragma warning(pop)
#endif  // _MSC_VER

// Store all the types of exceptions we can throw.
namespace math {

// Base type for errors.
struct Exception : public std::exception {
  // ConstructMatrix with moved message.
  explicit Exception(std::string&& message) noexcept : message_(std::move(message)) {}

  // ConstructMatrix with format specifier and arguments.
  template <typename... Ts>
  explicit Exception(std::string_view fmt, Ts&&... args)
      : Exception(fmt::format(fmt, std::forward<Ts>(args)...)) {}

  // Retrieve error message as a string view.
  [[nodiscard]] std::string_view Message() const noexcept { return message_; }

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

}  // namespace math
