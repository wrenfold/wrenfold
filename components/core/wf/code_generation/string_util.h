#pragma once
#include <array>
#include <string_view>

namespace wf {

// Constexpr snake-case --> camel-case conversion.
// Result is truncated to length `N` (including null terminator).
template <std::size_t N>
constexpr auto camel_case_from_snake_case(const std::string_view str) noexcept {
  // Poor person's constexpr to_upper for ASCII roman letters.
  const auto to_upper = [](char c) constexpr {
    if (c >= 'a' && c <= 'z') {
      return static_cast<char>(static_cast<int>(c) - 32);
    }
    return c;
  };

  std::array<char, N> result{static_cast<char>(0)};
  auto it = str.cbegin();
  if (it == str.cend()) {
    return result;
  }

  // First letter is always capitalized.
  auto out = result.begin();
  *out = to_upper(*it);
  for (++it, ++out; it != str.cend() && std::next(out) != result.end(); ++it, ++out) {
    if (*it == '_') {
      ++it;
      if (it != str.cend()) {
        *out = to_upper(*it);
      }
    } else {
      *out = *it;
    }
  }
  *out = 0;  //  Null terminate.
  return result;
}

static_assert(std::string_view{camel_case_from_snake_case<8>("hello_world").data()} == "HelloWo");
static_assert(std::string_view{camel_case_from_snake_case<16>("hello_world").data()} ==
              "HelloWorld");

}  // namespace wf
