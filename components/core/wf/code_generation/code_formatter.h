// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/ir_builder.h"
#include "wf/index_range.h"

namespace math {

template <typename Formatter, typename T>
struct fmt_view;

template <typename Formatter, typename Container>
struct fmt_join_view;

class CodeFormatter {
 public:
  CodeFormatter() = default;

  // Format into internal string buffer.
  template <typename... Args>
  void format(const std::string_view fmt, Args&&... args) {
    if constexpr (sizeof...(args) > 0) {
      fmt::format_to(std::back_inserter(output_), fmt, std::forward<Args>(args)...);
    } else {
      output_.append(fmt);
    }
  }

  // Join using the provided formatter and separator.
  template <typename Formatter, typename Container>
  void join(Formatter&& formatter, const std::string_view separator, const Container& container) {
    auto it = container.begin();
    if (it == container.end()) {
      return;
    }
    formatter(*this, *it);
    for (++it; it != container.end(); ++it) {
      format(separator);
      formatter(*this, *it);
    }
  }

  // Invoke the user-provided callable. Any content added in the scope of the
  // callable will be intended by `indent` spaces and wrapped in `open` and `close`.
  template <typename Callable>
  void with_indentation(const int indent, const std::string_view open, const std::string_view close,
                        Callable&& callable) {
    WF_ASSERT_GREATER_OR_EQ(indent, 0);
    // Move output_ -> appended
    std::string appended{};
    std::swap(output_, appended);
    format(open);
    callable();
    // Restore appended -> output_
    std::swap(appended, output_);
    // Copy appended into output_, adding indentation as required
    append_with_indentation(appended, indent);
    format(close);
  }

  const std::string& get_output() const { return output_; }

 private:
  void append_with_indentation(const std::string& appended, const int indentation) {
    for (auto it = appended.begin(); it != appended.end(); ++it) {
      output_.push_back(*it);
      if (*it == '\n' && std::next(it) != appended.end()) {
        for (int i = 0; i < indentation; ++i) {
          output_.push_back(' ');
        }
      }
    }
  }

  // TODO: gcc/clang/msvc all allow ~24 bytes for small string. Could potentially
  // replace this w/ a custom type if we want more than that.
  std::string output_{};
};

namespace detail {

// If `T` is an r-value reference, get the decayed type.
// Otherwise, get a const T&.
template <typename T>
struct convert_rvalue_ref_to_value {
  using type =
      std::conditional_t<std::is_rvalue_reference_v<T>, std::decay_t<T>, const std::decay_t<T>&>;
};
template <typename T>
using convert_rvalue_ref_to_value_t = typename convert_rvalue_ref_to_value<T>::type;

// Captures a callable formatter and a bunch of arguments.
// `Formatter` may be a reference in this context.
template <typename Formatter, typename T>
struct fmt_view;

template <typename Formatter, typename... Args>
struct fmt_view<Formatter, std::tuple<Args...>> {
  Formatter formatter;
  std::tuple<Args...> tuple;
};

template <typename Formatter, typename Container>
struct fmt_join_view {
  Formatter formatter;
  Container container;  //  Container, which may be a const lvalue reference.
  std::string_view separator;
};

}  // namespace detail

// Wrap an argument to fmt::formatter, such that the underlying argument will be
// formatted by calling back into an object of type `Formatter`.
template <typename Formatter, typename... Args>
auto make_fmt_view(Formatter&& formatter, Args&&... args) {
  // Convert r-value references to values. These are moved into the tuple. l-value references
  // are store as `const Args&` in the tuple to avoid copies.
  std::tuple<detail::convert_rvalue_ref_to_value_t<decltype(args)>...> tup{
      std::forward<Args>(args)...};
  return detail::fmt_view<detail::convert_rvalue_ref_to_value_t<decltype(formatter)>,
                          decltype(tup)>{std::forward<Formatter>(formatter), std::move(tup)};
}

template <typename Formatter, typename Container>
auto make_join_view(Formatter&& formatter, const std::string_view separator,
                    Container&& container) {
  // Determine if args are r-values or l-values, and move them if appropriate.
  // The resulting JoinView will store either values or const references.
  return detail::fmt_join_view<detail::convert_rvalue_ref_to_value_t<decltype(formatter)>,
                               detail::convert_rvalue_ref_to_value_t<decltype(container)>>{
      std::forward<Formatter>(formatter), std::forward<Container>(container), separator};
}

}  // namespace math

// Formatter that handles the fmt_view type.
// This formatter takes the user-specified `Formatter` object from the fmt_view,
// and delegates back to the appropriate operator() on that object.
template <typename Formatter, typename... Args>
struct fmt::formatter<math::detail::fmt_view<Formatter, std::tuple<Args...>>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::detail::fmt_view<Formatter, std::tuple<Args...>>& view,
              FormatContext& ctx) const -> decltype(ctx.out()) {
    math::CodeFormatter nested_formatter{};

    // Invoke the user provided method w/ the nested formatter:
    std::apply(
        [&view, &nested_formatter](auto&&... args) {
          return view.formatter(nested_formatter, std::forward<decltype(args)>(args)...);
        },
        view.tuple);

    // Append the result
    const auto& result = nested_formatter.get_output();
    return std::copy(result.begin(), result.end(), ctx.out());
  }
};

template <typename Formatter, typename Container>
struct fmt::formatter<math::detail::fmt_join_view<Formatter, Container>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::detail::fmt_join_view<Formatter, Container>& view,
              FormatContext& ctx) const -> decltype(ctx.out()) {
    math::CodeFormatter nested_formatter{};
    nested_formatter.join(view.formatter, view.separator, view.container);
    const auto& result = nested_formatter.get_output();
    return std::copy(result.begin(), result.end(), ctx.out());
  }
};