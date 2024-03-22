// Copyright 2023 Gareth Cross
#pragma once
#include <string>

#include "wf/third_party_imports.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

// Some utilities for formatting strings.
namespace wf {

template <typename Formatter, typename T>
struct fmt_view;

namespace detail {
// Convert type `T` into something we capture in `fmt_view`.
template <typename T>
struct convert_to_captured_type {
 private:
  using U = std::remove_reference_t<T>;

 public:
  // If `T` is an r-value, we will move it into type `U`.
  // It is an array, decay it to a pointer.
  // Otherwise, just take type `T` (which may be a reference or a value).
  using type = std::conditional_t<std::is_rvalue_reference_v<T>, U,
                                  std::conditional_t<std::is_array_v<U>, std::decay_t<U>, T>>;
};
template <typename T>
using convert_to_captured_type_t = typename convert_to_captured_type<T>::type;

// Captures a callable formatter and a bunch of arguments.
// `Formatter` may be a reference (const or otherwise).
template <typename Formatter, typename T>
struct fmt_view;
template <typename Formatter, typename... Args>
struct fmt_view<Formatter, std::tuple<Args...>> {
  Formatter formatter;
  std::tuple<Args...> tuple;
};

}  // namespace detail

// Join using the provided formatter and separator.
template <typename Formatter, typename Container>
std::string join(const std::string_view separator, const Container& container,
                 Formatter&& formatter) {
  auto it = container.begin();
  if (it == container.end()) {
    return "";
  }
  // TODO: It would be preferable if the formatter wrote into `result` instead of returning copies.
  std::string result{};
  result += formatter(*it);
  for (++it; it != container.end(); ++it) {
    result.append(separator);
    result += formatter(*it);
  }
  return result;
}

// Join, but with an iteration index included in the arguments to `formatter`.
template <typename Container, typename Formatter>
std::string join_enumerate(const std::string_view separator, const Container& container,
                           Formatter&& formatter) {
  std::size_t index = 0;
  return join(separator, container, [&](const auto& arg) { return formatter(index++, arg); });
}

// Indent a string and prefix/suffix it with open and closing brackets.
template <typename Formatter, typename Container>
void join_and_indent(std::string& output, const std::size_t indendation,
                     const std::string_view open, const std::string_view close,
                     const std::string_view separator, const Container& container,
                     Formatter&& formatter) {
  output.append(open);
  const std::string joined = join(separator, container, std::forward<Formatter>(formatter));

  output.reserve(output.size() + joined.size());

  // Indent the first line:
  output.insert(output.end(), indendation, ' ');

  for (auto char_it = joined.begin(); char_it != joined.end(); ++char_it) {
    output.push_back(*char_it);
    // Every instance of a newline should have indentation (as long as the string is not ending).
    if (*char_it == '\n' && std::next(char_it) != joined.end()) {
      output.insert(output.end(), indendation, ' ');
    }
  }
  output.append(close);
}

// Wrap an argument to fmt::formatter, such that the underlying argument will be
// formatted by calling back into an object of type `Formatter`.
template <typename Formatter, typename... Args>
auto make_fmt_view(Formatter&& formatter, Args&&... args) {
  std::tuple<detail::convert_to_captured_type_t<decltype(args)>...> tup{
      std::forward<Args>(args)...};
  return detail::fmt_view<detail::convert_to_captured_type_t<decltype(formatter)>, decltype(tup)>{
      std::forward<Formatter>(formatter), std::move(tup)};
}

}  // namespace wf

// Formatter that handles the fmt_view type.
// This formatter takes the user-specified `Formatter` object from the fmt_view,
// and delegates back to the appropriate operator() on that object.
template <typename Formatter, typename... Args>
struct fmt::formatter<wf::detail::fmt_view<Formatter, std::tuple<Args...>>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::detail::fmt_view<Formatter, std::tuple<Args...>>& view,
              FormatContext& ctx) const -> decltype(ctx.out()) {
    // Invoke the user provided method w/ the nested formatter:
    const auto result = std::apply(
        [&view](auto&&... args) { return view.formatter(std::forward<decltype(args)>(args)...); },
        view.tuple);

    // Append the result
    return std::copy(result.begin(), result.end(), ctx.out());
  }
};
