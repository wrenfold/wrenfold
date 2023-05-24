// Copyright 2023 Gareth Cross
#include "code_generation.h"
#include "index_range.h"

namespace math {

template <typename Formatter, typename T>
struct FmtView;

template <typename Formatter, typename Container>
struct FmtJoinView;

class CodeFormatter {
 public:
  CodeFormatter() = default;
  explicit CodeFormatter(int indent) : indentation_(indent) {
    ASSERT_GREATER_OR_EQ(indentation_, 0);
  }

  // Format into internal string buffer.
  template <typename... Args>
  void Format(const std::string_view fmt, Args&&... args) {
    (SetFormatterPtr(std::forward<Args>(args)), ...);
    fmt::format_to(std::back_inserter(output_), fmt, std::forward<Args>(args)...);
  }

  // Append a plain string.
  void Append(const std::string_view str) { output_ += str; }

  // Join using the provided formatter and separator.
  template <typename Formatter, typename Container>
  void Join(Formatter&& formatter, const std::string_view separator, const Container& container) {
    auto it = container.begin();
    if (it == container.end()) {
      return;
    }
    formatter(*this, *it);
    for (++it; it != container.end(); ++it) {
      Append(separator);
      formatter(*this, *it);
    }
  }

  // Invoke the user-provided callable. Any content added in the scope of the
  // callable will be intended by `indent` spaces and wrapped in `open` and `close`.
  template <typename Callable>
  void WithIndentation(const int indent, const std::string_view open, const std::string_view close,
                       Callable callable) {
    ASSERT_GREATER_OR_EQ(indent, 0);
    // Move output_ -> appended
    std::string appended{};
    std::swap(output_, appended);
    Append(open);
    //    indentation_ += indent;
    callable();
    // Restore appended -> output_
    std::swap(appended, output_);
    // Copy appended into output_, adding indentation as required
    AppendWithIndentation(appended, indent);
    //    indentation_ -= indent;
    Append(close);
  }

  int GetIndentation() const { return indentation_; }

  const std::string& GetOutput() const { return output_; }

 private:
  // Do nothing to most types.
  template <typename T>
  constexpr void SetFormatterPtr(T&&) const {}

  // Modify FmtView to point at `this`.
  template <typename UserFormatter, typename T>
  void SetFormatterPtr(FmtView<UserFormatter, T>&& view) const {
    view.code_formatter = this;
  }

  // Modify FmtView to point at `this`.
  template <typename UserFormatter, typename T>
  void SetFormatterPtr(FmtJoinView<UserFormatter, T>&& view) const {
    view.code_formatter = this;
  }

  void AppendWithIndentation(const std::string& appended, const int indentation) {
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
  int indentation_{0};
};

template <typename Formatter, typename... Args>
struct FmtView<Formatter, std::tuple<Args...>> {
  Formatter formatter;
  std::tuple<Args...> tuple;
  const class CodeFormatter* code_formatter{nullptr};
};

// Wrap an argument to fmt::formatter, such that the underlying argument will be
// formatted by calling back into an object of type `Formatter`.
template <typename Formatter, typename... Args>
auto View(Formatter&& formatter, Args&&... args) {
  // Convert r-value references to values. These are moved into the tuple. l-value references
  // are store as `const Args&` in the tuple to avoid copies.
  std::tuple<typename DecayRValueToValue<decltype(args)>::Type...> tup{std::forward<Args>(args)...};
  using FormatterType = typename DecayRValueToValue<decltype(formatter)>::Type;
  return FmtView<FormatterType, decltype(tup)>{std::forward<Formatter>(formatter), std::move(tup)};
}

// TODO: Unused - delete this.
template <typename Formatter, typename Container>
struct FmtJoinView {
  Formatter formatter;
  Container container;  //  Container, which may be a const lvalue reference.
  std::string_view separator;
  const class CodeFormatter* code_formatter{nullptr};
};

template <typename Formatter, typename Container>
auto Join(Formatter&& formatter, const std::string_view separator, Container&& container) {
  // Determine if args are r-values or l-values, and move them if appropriate.
  // The resulting JoinView will store either values or const references.
  using FormatterType = typename DecayRValueToValue<decltype(formatter)>::Type;
  using ContainerType = typename DecayRValueToValue<decltype(container)>::Type;
  return FmtJoinView<FormatterType, ContainerType>{std::forward<Formatter>(formatter),
                                                   std::forward<Container>(container), separator};
}

}  // namespace math

// Formatter that handles the FmtView type.
// This formatter takes the user-specified `Formatter` object from the FmtView,
// and delegates back to the appropriate operator() on that object.
template <typename Formatter, typename... Args>
struct fmt::formatter<math::FmtView<Formatter, std::tuple<Args...>>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::FmtView<Formatter, std::tuple<Args...>>& view, FormatContext& ctx) const
      -> decltype(ctx.out()) {
    // Create a child formatter w/ the same indentation as the one that was used to create
    // this view.
    ASSERT(view.code_formatter);
    math::CodeFormatter nested_formatter{view.code_formatter->GetIndentation()};

    // Invoke the user provided method w/ the nested formatter:
    std::apply(
        [&view, &nested_formatter](auto&&... args) {
          return view.formatter(nested_formatter, std::forward<decltype(args)>(args)...);
        },
        view.tuple);

    // Append the result
    const auto& result = nested_formatter.GetOutput();
    return std::copy(result.begin(), result.end(), ctx.out());
  }
};

template <typename Formatter, typename Container>
struct fmt::formatter<math::FmtJoinView<Formatter, Container>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::FmtJoinView<Formatter, Container>& view, FormatContext& ctx) const
      -> decltype(ctx.out()) {
    // Create a child formatter w/ the same indentation.
    ASSERT(view.code_formatter);
    math::CodeFormatter nested_formatter{view.code_formatter->GetIndentation()};
    nested_formatter.Join(view.formatter, view.separator, view.container);
    const auto& result = nested_formatter.GetOutput();
    return std::copy(result.begin(), result.end(), ctx.out());
  }
};
