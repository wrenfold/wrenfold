// Copyright 2022 Gareth Cross
#pragma once
#include "wf/constants.h"
#include "wf/expression_impl.h"

namespace math {

// A symbolic constant, like pi or euler's number.
class Constant {
 public:
  static constexpr std::string_view name_str = "Constant";
  static constexpr bool is_leaf_node = true;

  // Construct with name.
  explicit constexpr Constant(symbolic_constants name) noexcept : name_(name) {}

  // Check if symbolic constants are the same.
  constexpr bool is_identical_to(const Constant& other) const noexcept {
    return name_ == other.name_;
  }

  // Access name.
  constexpr symbolic_constants name() const noexcept { return name_; }

 protected:
  symbolic_constants name_;
};

// Complex infinity (the north-pole of the riemann sphere).
class Infinity {
 public:
  static constexpr std::string_view name_str = "ComplexInfinity";
  static constexpr bool is_leaf_node = true;

  constexpr Infinity() noexcept = default;
  constexpr bool is_identical_to(const Infinity&) const noexcept { return true; }
};

// Result of invalid expressions.
class Undefined {
 public:
  static constexpr std::string_view name_str = "Undefined";
  static constexpr bool is_leaf_node = true;

  Undefined() noexcept = default;
  constexpr bool is_identical_to(const Undefined&) const noexcept { return true; }
};

// Convert `symbolic_constants` to a floating point double.
constexpr double double_from_symbolic_constant(symbolic_constants constant) noexcept {
  switch (constant) {
    case symbolic_constants::euler:
      return M_E;
    case symbolic_constants::pi:
      return M_PI;
    case symbolic_constants::boolean_true:
      return 1.0;
    case symbolic_constants::boolean_false:
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
