// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <type_traits>  //  std::integral_constant

namespace wf {

// Designate an unreachable block of code. Based on cppreference.com implementation.
[[noreturn]] inline void unreachable() {
#if defined(_MSC_VER) && !defined(__clang__)
  __assume(false);
#else  // GCC, Clang
  __builtin_unreachable();
#endif
}

#define _switch_cases_4(_i, num_cases, stamp) \
  stamp(_i + 0, num_cases);                   \
  stamp(_i + 1, num_cases);                   \
  stamp(_i + 2, num_cases);                   \
  stamp(_i + 3, num_cases)

#define _switch_cases_16(_i, num_cases, stamp) \
  _switch_cases_4(_i + 0, num_cases, stamp);   \
  _switch_cases_4(_i + 4, num_cases, stamp);   \
  _switch_cases_4(_i + 8, num_cases, stamp);   \
  _switch_cases_4(_i + 12, num_cases, stamp)

#define _switch_cases_64(_i, num_cases, stamp) \
  _switch_cases_16(_i + 0, num_cases, stamp);  \
  _switch_cases_16(_i + 16, num_cases, stamp); \
  _switch_cases_16(_i + 32, num_cases, stamp); \
  _switch_cases_16(_i + 48, num_cases, stamp)

#define _switch_cases_256(_i, num_cases, stamp) \
  _switch_cases_64(_i + 0, num_cases, stamp);   \
  _switch_cases_64(_i + 64, num_cases, stamp);  \
  _switch_cases_64(_i + 128, num_cases, stamp); \
  _switch_cases_64(_i + 192, num_cases, stamp)

// Create a single switch case. We pass this macro to the `_switch_cases_N` macro,
// which in turn "invokes it" with an index `_i`.
#define _single_switch_case(_i, num_cases)                    \
  case _i: {                                                  \
    if constexpr (_i < num_cases) {                           \
      return func(std::integral_constant<std::size_t, _i>()); \
    }                                                         \
  } break;

#define _make_switch_case(index_var, num_cases, max_cases)        \
  switch (index_var) {                                            \
    _switch_cases_##max_cases(0, num_cases, _single_switch_case); \
    default:                                                      \
      break;                                                      \
  }

namespace detail {

// Generate a switch statement over variable `index` with `Num` cases.
// `func` will be invoked in each switch case with a std::integral_constant for that index.
// This function expands to a jump table. Last tested under gcc 12.3.0, clang 16.0.6, and
// MSVC 19.37.32822. The return type of this function is the same as that of `func` itself.
// This implementation is based on the standard library implementation for std::variant.
template <std::size_t Num, typename F>
constexpr auto visit_switch(const std::size_t index, F&& func) {
  static_assert(Num <= 256, "Max number of cases is 256.");
  // Depending on how many cases there, make the switch statement longer.
  if constexpr (Num <= 4) {
    _make_switch_case(index, Num, 4);
  } else if constexpr (Num <= 16) {
    _make_switch_case(index, Num, 16);
  } else if constexpr (Num <= 64) {
    _make_switch_case(index, Num, 64);
  } else {
    _make_switch_case(index, Num, 256);
  }
  wf::unreachable();
}

}  // namespace detail

#undef _make_switch_case
#undef _switch_cases_256
#undef _switch_cases_64
#undef _switch_cases_16
#undef _switch_cases_4

}  // namespace wf
