// Copyright 2022 Gareth Cross
#pragma once
#include "wf/enumerations.h"
#include "wf/hashing.h"

namespace wf {

// A symbolic constant, like pi or euler's number.
class symbolic_constant {
 public:
  static constexpr std::string_view name_str = "Constant";
  static constexpr bool is_leaf_node = true;

  // Construct with name.
  explicit constexpr symbolic_constant(symbolic_constant_enum name) noexcept : name_(name) {}

  // Access name.
  constexpr symbolic_constant_enum name() const noexcept { return name_; }

 protected:
  symbolic_constant_enum name_;
};

// Complex infinity (the north-pole of the riemann sphere).
class complex_infinity {
 public:
  static constexpr std::string_view name_str = "ComplexInfinity";
  static constexpr bool is_leaf_node = true;

  constexpr complex_infinity() noexcept = default;
  constexpr bool is_identical_to(const complex_infinity&) const noexcept { return true; }
};

// Result of invalid expressions.
class undefined {
 public:
  static constexpr std::string_view name_str = "Undefined";
  static constexpr bool is_leaf_node = true;

  undefined() noexcept = default;
  constexpr bool is_identical_to(const undefined&) const noexcept { return true; }
};

// Convert `symbolic_constant_enum` to a floating point double.
constexpr double double_from_symbolic_constant(symbolic_constant_enum constant) noexcept {
  switch (constant) {
    case symbolic_constant_enum::euler:
      return M_E;
    case symbolic_constant_enum::pi:
      return M_PI;
    case symbolic_constant_enum::boolean_true:
      return 1.0;
    case symbolic_constant_enum::boolean_false:
      return 0.0;
  }
  return std::numeric_limits<double>::quiet_NaN();
}

// Order constants by their enum values.
inline constexpr bool operator<(const symbolic_constant& a, const symbolic_constant& b) noexcept {
  return a.name() < b.name();
}

template <>
struct hash_struct<symbolic_constant> {
  constexpr std::size_t operator()(const symbolic_constant& c) const noexcept {
    return static_cast<std::size_t>(c.name());
  }
};

template <>
struct is_identical_struct<symbolic_constant> {
  constexpr bool operator()(const symbolic_constant& a, const symbolic_constant& b) const noexcept {
    return a.name() == b.name();
  }
};

template <>
struct hash_struct<complex_infinity> {
  constexpr std::size_t operator()(const complex_infinity&) const noexcept {
    constexpr auto inf_hash = hash_string_fnv("inf");
    return inf_hash;
  }
};

template <>
struct hash_struct<undefined> {
  constexpr std::size_t operator()(const undefined&) const noexcept {
    constexpr auto undef_hash = hash_string_fnv("undef");
    return undef_hash;
  }
};

}  // namespace wf

// Formatter for printing in assertions.
template <>
struct fmt::formatter<wf::symbolic_constant, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::symbolic_constant& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", wf::string_from_symbolic_constant(x.name()));
  }
};
