// Copyright 2021 Gareth Cross
#pragma once
#include "wf/error_types.h"
#include "wf/fmt_imports.h"

namespace wf {

// Generates an exception w/ a formatted string.
template <typename... Ts>
void raise_assert(const char* const condition, const char* const file, const int line,
                  const char* const reason_fmt = nullptr, Ts&&... args) {
  const std::string details =
      reason_fmt ? fmt::format(reason_fmt, std::forward<Ts>(args)...) : "None";
  std::string err = fmt::format("Assertion failed: {}\nFile: {}\nLine: {}\nDetails: {}\n",
                                condition, file, line, details);
  throw assertion_error(std::move(err));
}

// Version that prints args A & B as well. For binary comparisons.
template <typename A, typename B, typename... Ts>
void raise_assert_binary_op(const char* const condition, const char* const file, const int line,
                            const char* const a_name, A&& a, const char* const b_name, B&& b,
                            const char* const reason_fmt = nullptr, Ts&&... args) {
  const std::string details =
      reason_fmt ? fmt::format(reason_fmt, std::forward<Ts>(args)...) : "None";
  std::string err = fmt::format(
      "Assertion failed: {}\n"
      "Operands are: {} = {}, {} = {}\n"
      "File: {}\nLine: {}\nDetails: {}\n",
      condition, a_name, std::forward<A>(a), b_name, std::forward<B>(b), file, line, details);
  throw assertion_error(std::move(err));
}

}  // namespace wf

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wgnu-zero-variadic-macro-arguments"
#endif  // __clang__

// Assertion macros.
// Based on: http://cnicholson.net/2009/02/stupid-c-tricks-adventures-in-assert
#define WF_ASSERT_IMPL(cond, file, line, handler, ...) \
  do {                                                 \
    if (!static_cast<bool>(cond)) {                    \
      handler(#cond, file, line, ##__VA_ARGS__);       \
    }                                                  \
  } while (false)

// Macro to use when defining an assertion.
#define WF_ASSERT(cond, ...) \
  WF_ASSERT_IMPL(cond, __FILE__, __LINE__, wf::raise_assert, ##__VA_ARGS__)

#define WF_ASSERT_EQUAL(a, b, ...)                                                         \
  WF_ASSERT_IMPL((a) == (b), __FILE__, __LINE__, wf::raise_assert_binary_op, #a, a, #b, b, \
                 ##__VA_ARGS__)

#define WF_ASSERT_NOT_EQUAL(a, b, ...)                                                     \
  WF_ASSERT_IMPL((a) != (b), __FILE__, __LINE__, wf::raise_assert_binary_op, #a, a, #b, b, \
                 ##__VA_ARGS__)

#define WF_ASSERT_LESS(a, b, ...)                                                         \
  WF_ASSERT_IMPL((a) < (b), __FILE__, __LINE__, wf::raise_assert_binary_op, #a, a, #b, b, \
                 ##__VA_ARGS__)

#define WF_ASSERT_GREATER(a, b, ...)                                                      \
  WF_ASSERT_IMPL((a) > (b), __FILE__, __LINE__, wf::raise_assert_binary_op, #a, a, #b, b, \
                 ##__VA_ARGS__)

#define WF_ASSERT_LESS_OR_EQ(a, b, ...)                                                    \
  WF_ASSERT_IMPL((a) <= (b), __FILE__, __LINE__, wf::raise_assert_binary_op, #a, a, #b, b, \
                 ##__VA_ARGS__)

#define WF_ASSERT_GREATER_OR_EQ(a, b, ...)                                                 \
  WF_ASSERT_IMPL((a) >= (b), __FILE__, __LINE__, wf::raise_assert_binary_op, #a, a, #b, b, \
                 ##__VA_ARGS__)

#ifdef __clang__
#pragma clang diagnostic pop
#endif  // __clang__
