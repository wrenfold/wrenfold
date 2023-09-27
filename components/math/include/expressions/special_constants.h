// Copyright 2022 Gareth Cross
#pragma once
#include "constants.h"
#include "expression_impl.h"

namespace math {

// A symbolic constant, like pi or euler's number.
class Constant {
 public:
  static constexpr std::string_view NameStr = "Constant";
  static constexpr bool IsLeafNode = true;

  // Construct with name.
  explicit constexpr Constant(SymbolicConstants name) : name_(name) {}

  // Check if symbolic constants are the same.
  constexpr bool IsIdenticalTo(const Constant& other) const { return name_ == other.name_; }

  // Access name.
  constexpr SymbolicConstants GetName() const { return name_; }

 protected:
  SymbolicConstants name_;
};

// Complex infinity.
// TODO: Should this store an enum to support different types of infinities?
// For example, complex, +real, -real, etc.
class Infinity {
 public:
  static constexpr std::string_view NameStr = "Infinity";
  static constexpr bool IsLeafNode = true;

  Infinity() = default;
  constexpr bool IsIdenticalTo(const Infinity&) const { return true; }
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

// Convert `SymbolicConstants` to a floating point double.
constexpr double DoubleFromSymbolicConstant(SymbolicConstants constant) {
  switch (constant) {
    case SymbolicConstants::Euler:
      return M_E;
    case SymbolicConstants::Pi:
      return M_PI;
    case SymbolicConstants::True:
      return 1.0;
    case SymbolicConstants::False:
      return 0.0;
  }
  return std::numeric_limits<double>::quiet_NaN();
}

// Order constants by their enum values.
inline constexpr bool operator<(const Constant& a, const Constant& b) {
  return a.GetName() < b.GetName();
}

template <>
struct Hash<Constant> {
  constexpr std::size_t operator()(const Constant& c) const {
    return static_cast<std::size_t>(c.GetName());
  }
};

template <>
struct Hash<Infinity> {
  constexpr std::size_t operator()(const Infinity&) const {
    constexpr auto inf_hash = HashString("inf");
    return inf_hash;
  }
};

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
