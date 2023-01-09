// Copyright 2023 Gareth Cross
#include <fmt/format.h>

namespace math {

// Join elements of a container w/ a specified separator.
template <typename Container, typename Converter>
std::string Join(const std::string_view& sep, const Container& data, Converter converter) {
  auto it = data.begin();
  if (it == data.end()) {
    return {};
  }
  std::string out;
  fmt::format_to(std::back_inserter(out), "{}", converter(*it));
  for (++it; it != data.end(); ++it) {
    fmt::format_to(std::back_inserter(out), "{}{}", sep, converter(*it));
  }
  return out;
}

}  // namespace math
