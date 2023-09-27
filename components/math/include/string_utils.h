// Copyright 2023 Gareth Cross
#include <fmt/format.h>

namespace math {

// Join elements of a container w/ a specified separator. This version accepts a pair of iterators.
template <typename Iterator, typename Converter>
void Join(std::string& out, const std::string_view sep, Iterator begin, Iterator end,
          Converter converter) {
  if (begin == end) {
    return;
  }
  fmt::format_to(std::back_inserter(out), "{}", converter(*begin));
  for (++begin; begin != end; ++begin) {
    fmt::format_to(std::back_inserter(out), "{}{}", sep, converter(*begin));
  }
}

// Join elements of a container w/ a specified separator.
template <typename Container, typename Converter>
void Join(std::string& out, const std::string_view sep, const Container& data,
          Converter converter) {
  Join(out, sep, data.begin(), data.end(), std::move(converter));
}

// Join elements of a container w/ a specified separator. This variant returns the string directly.
template <typename Container, typename Converter>
std::string Join(const std::string_view sep, const Container& data, Converter converter) {
  std::string out{};
  Join(out, sep, data, std::move(converter));
  return out;
}

}  // namespace math
