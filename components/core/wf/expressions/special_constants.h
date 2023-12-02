// Copyright 2022 Gareth Cross
#pragma once
#include "wf/constants.h"
#include "wf/expression_impl.h"

namespace math {

// A symbolic constant, like pi or euler's number.
class Constant {
 public:
  static constexpr std::string_view NameStr = "Constant";
  static constexpr bool IsLeafNode = true;

  // Construct with name.
  explicit constexpr Constant(SymbolicConstants name) noexcept : name_(name) {}

  // Check if symbolic constants are the same.
  constexpr bool is_identical_to(const Constant& other) const noexcept {
    return name_ == other.name_;
  }

  // Access name.
  constexpr SymbolicConstants name() const noexcept { return name_; }

 protected:
  SymbolicConstants name_;
};

// Complex infinity (the north-pole of the riemann sphere).
class Infinity {
 public:
  static constexpr std::string_view NameStr = "ComplexInfinity";
  static constexpr bool IsLeafNode = true;

  constexpr Infinity() noexcept = default;
  constexpr bool is_identical_to(const Infinity&) const noexcept { return true; }
};

// Result of invalid expressions.
class Undefined {
 public:
  static constexpr std::string_view NameStr = "Undefined";
  static constexpr bool IsLeafNode = true;

  Undefined() noexcept = default;
  constexpr bool is_identical_to(const Undefined&) const noexcept { return true; }
};

// Convert `SymbolicConstants` to a floating point double.
constexpr double double_from_symbolic_constant(SymbolicConstants constant) noexcept {
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
inline constexpr bool operator<(const Constant& a, const Constant& b) noexcept {
  return a.name() < b.name();
}

template <>
struct hash_struct<Constant> {
  constexpr std::size_t operator()(const Constant& c) const noexcept {
    return static_cast<std::size_t>(c.name());
  }
};

template <>
struct hash_struct<Infinity> {
  constexpr std::size_t operator()(const Infinity&) const noexcept {
    constexpr auto inf_hash = hash_string_fnv("inf");
    return inf_hash;
  }
};

template <>
struct hash_struct<Undefined> {
  constexpr std::size_t operator()(const Undefined&) const noexcept {
    constexpr auto undef_hash = hash_string_fnv("undef");
    return undef_hash;
  }
};

}  // namespace math

// Formatter for printing in assertions.
template <>
struct fmt::formatter<math::Constant, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::Constant& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", math::string_from_symbolic_constant(x.name()));
  }
};
