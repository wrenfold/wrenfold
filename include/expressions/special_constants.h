// Copyright 2022 Gareth Cross
#pragma once
#include "constants.h"
#include "expression_impl.h"

namespace math {

// A symbolic constant, like pi or euler's number.
class Constant : public ExpressionImpl<Constant> {
 public:
  static constexpr std::string_view NameStr = "Constant";
  static constexpr bool IsLeafNode = true;

  // ConstructMatrix with name.
  explicit Constant(SymbolicConstants Name) : name_(Name) {}

  // Check if symbolic constants are the same.
  bool IsIdenticalToImplTyped(const Constant& other) const { return name_ == other.name_; }

  // Access name.
  SymbolicConstants GetName() const { return name_; }

 protected:
  SymbolicConstants name_;
};

// Complex infinity.
// TODO: Should this store an enum to support different types of infinities?
// For example, complex, +real, -real, etc.
class Infinity : public ExpressionImpl<Infinity> {
 public:
  static constexpr std::string_view NameStr = "Infinity";
  static constexpr bool IsLeafNode = true;

  Infinity() = default;
  constexpr bool IsIdenticalToImplTyped(const Infinity&) const { return true; }
};

// Convert symbolic constant enum to string constant.
// For debugging purposes.
inline constexpr std::string_view StringFromSymbolicConstant(SymbolicConstants value) {
  switch (value) {
    case SymbolicConstants::Euler:
      return "e";
    case SymbolicConstants::Pi:
      return "pi";
    case SymbolicConstants::True:
      return "true";
    case SymbolicConstants::False:
      return "false";
  }
  return "<INVALID ENUM VALUE>";
}

// Order constants by their enum values.
inline bool operator<(const Constant& a, const Constant& b) { return a.GetName() < b.GetName(); }

}  // namespace math

// Formatter for printing in assertions.
template <>
struct fmt::formatter<math::Constant, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::Constant& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", math::StringFromSymbolicConstant(x.GetName()));
  }
};
