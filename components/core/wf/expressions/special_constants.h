// Copyright 2022 Gareth Cross
#pragma once
#include "wf/enumerations.h"
#include "wf/hashing.h"
#include "wf/ordering.h"
#include "wf/third_party_imports.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

// A symbolic constant, like pi or euler's number.
class symbolic_constant {
 public:
  static constexpr std::string_view name_str = "Constant";
  static constexpr bool is_leaf_node = true;

  // Construct with name.
  explicit constexpr symbolic_constant(const symbolic_constant_enum name) noexcept : name_(name) {}

  // Access name.
  constexpr symbolic_constant_enum name() const noexcept { return name_; }

 protected:
  symbolic_constant_enum name_;
};

// Convert `symbolic_constant_enum` to a floating point double.
constexpr double double_from_symbolic_constant(const symbolic_constant_enum constant) noexcept {
  switch (constant) {
    case symbolic_constant_enum::euler:
      return M_E;
    case symbolic_constant_enum::pi:
      return M_PI;
  }
  return std::numeric_limits<double>::quiet_NaN();
}

// Order constants by their enum values.
constexpr bool operator<(const symbolic_constant& a, const symbolic_constant& b) noexcept {
  return a.name() < b.name();
}

// Complex infinity (the north-pole of the riemann sphere).
class complex_infinity {
 public:
  static constexpr std::string_view name_str = "ComplexInfinity";
  static constexpr bool is_leaf_node = true;
};

constexpr bool operator==(const complex_infinity&, const complex_infinity&) noexcept {
  return true;
}
constexpr bool operator!=(const complex_infinity&, const complex_infinity&) noexcept {
  return false;
}

// Result of invalid expressions.
class undefined {
 public:
  static constexpr std::string_view name_str = "Undefined";
  static constexpr bool is_leaf_node = true;
};

constexpr bool operator==(const undefined&, const undefined&) noexcept { return true; }
constexpr bool operator!=(const undefined&, const undefined&) noexcept { return false; }

// The imaginary constant `i`.
class imaginary_unit {
 public:
  static constexpr std::string_view name_str = "ImaginaryUnit";
  static constexpr bool is_leaf_node = true;
};

constexpr bool operator==(const imaginary_unit&, const imaginary_unit&) noexcept { return true; }
constexpr bool operator!=(const imaginary_unit&, const imaginary_unit&) noexcept { return false; }

// A boolean constant (true or false).
class boolean_constant {
 public:
  static constexpr std::string_view name_str = "BooleanConstant";
  static constexpr bool is_leaf_node = true;

  // Construct with true or false.
  explicit constexpr boolean_constant(const bool value) noexcept : value_(value) {}

  // Access value.
  constexpr bool value() const noexcept { return value_; }

  // Implicit cast to bool.
  operator bool() const noexcept { return value_; }

 private:
  bool value_;
};

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
struct order_struct<symbolic_constant> {
  constexpr relative_order operator()(const symbolic_constant& a,
                                      const symbolic_constant& b) const noexcept {
    return order_by_comparison(a, b);
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
struct is_identical_struct<complex_infinity> {
  constexpr bool operator()(const complex_infinity&, const complex_infinity&) const noexcept {
    return true;
  }
};

template <>
struct order_struct<complex_infinity> {
  constexpr relative_order operator()(const complex_infinity&,
                                      const complex_infinity&) const noexcept {
    return relative_order::equal;
  }
};

template <>
struct hash_struct<undefined> {
  constexpr std::size_t operator()(const undefined&) const noexcept {
    constexpr auto undef_hash = hash_string_fnv("undef");
    return undef_hash;
  }
};

template <>
struct is_identical_struct<undefined> {
  constexpr bool operator()(const undefined&, const undefined&) const noexcept { return true; }
};

template <>
struct order_struct<undefined> {
  constexpr relative_order operator()(const undefined&, const undefined&) const noexcept {
    return relative_order::equal;
  }
};

template <>
struct hash_struct<imaginary_unit> {
  constexpr std::size_t operator()(const imaginary_unit&) const noexcept {
    constexpr auto i_hash = hash_string_fnv("i");
    return i_hash;
  }
};

template <>
struct is_identical_struct<imaginary_unit> {
  constexpr bool operator()(const imaginary_unit&, const imaginary_unit&) const noexcept {
    return true;
  }
};

template <>
struct order_struct<imaginary_unit> {
  constexpr relative_order operator()(const imaginary_unit&, const imaginary_unit&) const noexcept {
    return relative_order::equal;
  }
};

template <>
struct hash_struct<boolean_constant> {
  constexpr std::size_t operator()(const boolean_constant& c) const noexcept {
    return static_cast<std::size_t>(c.value());
  }
};

template <>
struct is_identical_struct<boolean_constant> {
  constexpr bool operator()(const boolean_constant& a, const boolean_constant& b) const noexcept {
    return a.value() == b.value();
  }
};

template <>
struct order_struct<boolean_constant> {
  constexpr relative_order operator()(const boolean_constant& a,
                                      const boolean_constant& b) const noexcept {
    return order_by_comparison(a.value(), b.value());
  }
};

}  // namespace wf

// Formatter for printing symbolic_constant.
template <>
struct fmt::formatter<wf::symbolic_constant, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::symbolic_constant& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", wf::string_from_symbolic_constant(x.name()));
  }
};
