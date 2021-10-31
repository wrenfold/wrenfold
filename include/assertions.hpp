// Copyright 2021 Gareth Cross
#pragma once
#define ASSERTS_ENABLED

#ifdef _MSC_VER
// Silence some warnings that libfmt can trigger w/ Microsoft compiler.
#pragma warning(push)
#pragma warning(disable : 4583)
#pragma warning(disable : 4582)
#endif  // _MSC_VER
#include <fmt/ostream.h>
#ifdef _MSC_VER
#pragma warning(pop)
#endif  // _MSC_VER

// Generates an exception w/ a formatted string.
template <typename... Ts>
void RaiseAssert(const char* const condition, const char* const file, const int line,
                 const char* const reason_fmt = nullptr, Ts&&... args) {
  const std::string details =
      reason_fmt ? fmt::format(reason_fmt, std::forward<Ts>(args)...) : "None";
  const std::string err = fmt::format("Assertion failed: {}\nFile: {}\nLine: {}\nDetails: {}\n",
                                      condition, file, line, details);
  throw std::runtime_error(err);
}

// Version that prints args A & B as well. For binary comparisons.
template <typename A, typename B, typename... Ts>
void RaiseAssertBinaryOp(const char* const condition, const char* const file, const int line,
                         const char* const a_name, A&& a, const char* const b_name, B&& b,
                         const char* const reason_fmt = nullptr, Ts&&... args) {
  const std::string details =
      reason_fmt ? fmt::format(reason_fmt, std::forward<Ts>(args)...) : "None";
  const std::string err = fmt::format(
      "Assertion failed: {}\n"
      "Operands are: {} = {}, {} = {}\n"
      "File: {}\nLine: {}\nDetails: {}\n",
      condition, a_name, std::forward<A>(a), b_name, std::forward<B>(b), file, line, details);
  throw std::runtime_error(err);
}

// Assertion macros.
// Based on: http://cnicholson.net/2009/02/stupid-c-tricks-adventures-in-assert
#ifdef ASSERTS_ENABLED
#define ASSERT_IMPL(cond, file, line, handler, ...) \
  do {                                              \
    if (!static_cast<bool>(cond)) {                 \
      handler(#cond, file, line, ##__VA_ARGS__);    \
    }                                               \
  } while (false)
#else
#define ASSERT_IMPL(cond, file, line, handler, ...) \
  do {                                              \
    (void)sizeof((condition));                      \
  } while (0)
#endif

// Macro to use when defining an assertion.
#define ASSERT(cond, ...) ASSERT_IMPL(cond, __FILE__, __LINE__, RaiseAssert, ##__VA_ARGS__)
#define ASSERT_EQUAL(a, b, ...) \
  ASSERT_IMPL((a) == (b), __FILE__, __LINE__, RaiseAssertBinaryOp, #a, a, #b, b, ##__VA_ARGS__)
#define ASSERT_NOT_EQUAL(a, b, ...) \
  ASSERT_IMPL((a) != (b), __FILE__, __LINE__, RaiseAssertBinaryOp, #a, a, #b, b, ##__VA_ARGS__)
#define ASSERT_LESS(a, b, ...) \
  ASSERT_IMPL((a) < (b), __FILE__, __LINE__, RaiseAssertBinaryOp, #a, a, #b, b, ##__VA_ARGS__)
#define ASSERT_GREATER(a, b, ...) \
  ASSERT_IMPL((a) > (b), __FILE__, __LINE__, RaiseAssertBinaryOp, #a, a, #b, b, ##__VA_ARGS__)
#define ASSERT_LESS_OR_EQ(a, b, ...) \
  ASSERT_IMPL((a) <= (b), __FILE__, __LINE__, RaiseAssertBinaryOp, #a, a, #b, b, ##__VA_ARGS__)
#define ASSERT_GREATER_OR_EQ(a, b, ...) \
  ASSERT_IMPL((a) >= (b), __FILE__, __LINE__, RaiseAssertBinaryOp, #a, a, #b, b, ##__VA_ARGS__)
