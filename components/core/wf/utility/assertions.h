// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/utility/error_types.h"  // wf::assertion_error

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
#include <fmt/ranges.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {
namespace detail {

// Generates an exception w/ a formatted string.
template <typename... Ts>
std::string format_assert(const std::string_view condition, const std::string_view file,
                          const int line, const std::string_view reason_fmt = {}, Ts&&... args) {
  std::string err = fmt::format("Assertion failed: {}\nFile: {}\nLine: {}", condition, file, line);
  if (!reason_fmt.empty()) {
    err.append("\nDetails: ");
    fmt::vformat_to(std::back_inserter(err), reason_fmt, fmt::make_format_args(args...));
  }
  return err;
}

// Version that prints args A & B as well. For binary comparisons.
template <typename A, typename B, typename... Ts>
std::string format_assert_binary(const std::string_view condition, const std::string_view file,
                                 const int line, const std::string_view a_name, A&& a,
                                 const std::string_view b_name, B&& b,
                                 const std::string_view reason_fmt = {}, Ts&&... args) {
  std::string err = fmt::format(
      "Assertion failed: {}\n"
      "Operands are: `{}` = {}, `{}` = {}\n"
      "File: {}\nLine: {}",
      condition, a_name, std::forward<A>(a), b_name, std::forward<B>(b), file, line);
  if (!reason_fmt.empty()) {
    err.append("\nDetails: ");
    fmt::vformat_to(std::back_inserter(err), reason_fmt, fmt::make_format_args(args...));
  }
  return err;
}

}  // namespace detail
}  // namespace wf

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wgnu-zero-variadic-macro-arguments"
#endif  // __clang__

// Assertion macros.
// Based on: http://cnicholson.net/2009/02/stupid-c-tricks-adventures-in-assert
#define WF_ASSERT_IMPL(cond, file, line, handler, ...)                      \
  do {                                                                      \
    if (!static_cast<bool>(cond)) {                                         \
      throw wf::assertion_error(handler(#cond, file, line, ##__VA_ARGS__)); \
    }                                                                       \
  } while (false)

#define WF_ASSERT_ALWAYS_IMPL(file, line, handler, ...)                             \
  do {                                                                              \
    throw wf::assertion_error(handler("Assert always", file, line, ##__VA_ARGS__)); \
  } while (false)

// Macro to use when defining an assertion.
#define WF_ASSERT(cond, ...) \
  WF_ASSERT_IMPL(cond, __FILE__, __LINE__, wf::detail::format_assert, ##__VA_ARGS__)

#define WF_ASSERT_ALWAYS(...) \
  WF_ASSERT_ALWAYS_IMPL(__FILE__, __LINE__, wf::detail::format_assert, ##__VA_ARGS__)

#define WF_ASSERT_EQ(a, b, ...)                                                                  \
  WF_ASSERT_IMPL((a) == (b), __FILE__, __LINE__, wf::detail::format_assert_binary, #a, a, #b, b, \
                 ##__VA_ARGS__)

#define WF_ASSERT_NE(a, b, ...)                                                                  \
  WF_ASSERT_IMPL((a) != (b), __FILE__, __LINE__, wf::detail::format_assert_binary, #a, a, #b, b, \
                 ##__VA_ARGS__)

#define WF_ASSERT_LT(a, b, ...)                                                                 \
  WF_ASSERT_IMPL((a) < (b), __FILE__, __LINE__, wf::detail::format_assert_binary, #a, a, #b, b, \
                 ##__VA_ARGS__)

#define WF_ASSERT_GT(a, b, ...)                                                                 \
  WF_ASSERT_IMPL((a) > (b), __FILE__, __LINE__, wf::detail::format_assert_binary, #a, a, #b, b, \
                 ##__VA_ARGS__)

#define WF_ASSERT_LE(a, b, ...)                                                                  \
  WF_ASSERT_IMPL((a) <= (b), __FILE__, __LINE__, wf::detail::format_assert_binary, #a, a, #b, b, \
                 ##__VA_ARGS__)

#define WF_ASSERT_GE(a, b, ...)                                                                  \
  WF_ASSERT_IMPL((a) >= (b), __FILE__, __LINE__, wf::detail::format_assert_binary, #a, a, #b, b, \
                 ##__VA_ARGS__)

#ifdef __clang__
#pragma clang diagnostic pop
#endif  // __clang__
