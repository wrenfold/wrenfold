// Copyright 2023 Gareth Cross
#include "code_generation.h"

namespace math {

class CodeFormatter {
 public:
  // Construct w/ a non-const reference to the generator.
  explicit CodeFormatter(CodeGeneratorBase& generator) : generator_(generator) {}

  // Instantiate new formatter by copying properties from parent.
  explicit CodeFormatter(const CodeFormatter& parent)
      : generator_(parent.generator_), indentation_(parent.indentation_) {}

  // Format into the output buffer.
  template <typename... Args>
  void Format(const std::string_view fmt, Args&&... args) {
    fmt::format_to(std::back_inserter(output_), fmt, std::forward<Args>(args)...);
  }

  template <typename Generator, typename... Args>
  void FormatWith(const Generator& code_generator, const std::string_view fmt,
                  const Args&... args) {
    fmt::format_to(std::back_inserter(output_), fmt, Intercept(code_generator, args)...);
  }

  // Append string w/o format args.
  void Append(const std::string_view str) { output_ += str; }

  template <typename Callable>
  void WithIndentation(int indent, const std::string_view open, const std::string_view close,
                       Callable callable) {
    CodeFormatter child{*this};
    child.SetIndentation(indentation_ + indent);
    child.Append(open);
    callable(child);
    output_.reserve(output_.size() + child.GetOutput().size());
    for (const char c : child.GetOutput()) {
      output_.push_back(c);
      if (c == '\n') {
        for (int i = 0; i < child.GetIndentation(); ++i) {
          output_.push_back(' ');
        }
      }
    }
    // Discard anything we inserted after the final newline.
    while (!output_.empty() && output_.back() == ' ') {
      output_.pop_back();
    }
    Append(close);
  }

  // Discard any trailing whitespace or commas.
  // TODO: Kind of lazy, but this is often easier to do in code than a proper join.
  void RightTrimTrailingWhitespaceAndComma() {
    while (!output_.empty()) {
      const bool is_space_or_comma = output_.back() == ',' || std::isspace(output_.back());
      if (is_space_or_comma) {
        output_.pop_back();
      } else {
        break;
      }
    }
  }

  const std::string& GetOutput() const { return output_; }

  int GetIndentation() const { return indentation_; }

  void SetIndentation(int indentation) { indentation_ = indentation; }

  template <typename Generator, typename ArgType>
  struct FormatRecursively {
    const CodeFormatter& parent;
    const Generator& generator;
    const ArgType& arg;
  };

 protected:
  // Intercept certain types and convert them to custom formatting types.
  template <typename Generator, typename T>
  auto Intercept(const Generator& code_generator, const T& input) {
    using TDecay = std::decay_t<T>;
    if constexpr (std::is_same_v<TDecay, ast::Type> || std::is_same_v<TDecay, ast::Variant> ||
                  std::is_same_v<TDecay, ast::VariantPtr>) {
      return FormatRecursively<Generator, T>{*this, code_generator, input};
    } else {
      return input;
    }
  }

  CodeGeneratorBase& generator_;
  std::string output_;
  int indentation_{0};
};

}  // namespace math

template <typename Generator, typename T>
struct fmt::formatter<math::CodeFormatter::FormatRecursively<Generator, T>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::CodeFormatter::FormatRecursively<Generator, T>& spec,
              FormatContext& ctx) const -> decltype(ctx.out()) {
    const math::CodeFormatter& parent = spec.parent;
    // We can't recurse on the same formatter, so instantiate a child and build a new
    // string. Would be nice this was small string, so it didn't have to allocate?
    math::CodeFormatter child{parent};
    spec.generator.Format(child, spec.arg);
    return fmt::format_to(ctx.out(), "{}", child.GetOutput());
  }
};
