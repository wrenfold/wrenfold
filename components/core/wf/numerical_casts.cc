// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/numerical_casts.h"

#include "wf/constants.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/numeric_expressions.h"

namespace wf {

using complex_double = std::complex<double>;

struct convert_to_complex_visitor {
  explicit convert_to_complex_visitor(bool allow_integer_conversion) noexcept
      : allow_integer_conversion_(allow_integer_conversion) {}

  template <typename T,
            typename = enable_if_does_not_contain_type_t<T, float_constant, integer_constant,
                                                         addition, multiplication, imaginary_unit>>
  std::optional<complex_double> operator()(const T&) const noexcept {
    return std::nullopt;
  }

  std::optional<complex_double> operator()(const addition& add) const noexcept {
    if (add.size() != 2) {
      // We only care about additions of the form: a + i*b
      return std::nullopt;
    }
    // This is a bit over-general in that it will also match things like a + b or a*i + b*i,
    // where a & b are floats. That should be fine in this context though.
    if (const std::optional<complex_double> maybe_left = visit(add[0], *this);
        maybe_left.has_value()) {
      if (const std::optional<complex_double> maybe_right = visit(add[1], *this);
          maybe_right.has_value()) {
        return *maybe_left + *maybe_right;
      }
    }
    return std::nullopt;
  }

  constexpr std::optional<complex_double> operator()(const float_constant f) const noexcept {
    return complex_double{f.value(), 0.0};
  }

  constexpr std::optional<complex_double> operator()(const imaginary_unit) const noexcept {
    return complex_double{0.0, 1.0};
  }

  constexpr std::optional<complex_double> operator()(const integer_constant i) const noexcept {
    if (allow_integer_conversion_) {
      return complex_double{static_cast<double>(i.value()), 0.0};
    } else {
      return std::nullopt;
    }
  }

  std::optional<complex_double> operator()(const multiplication& mul,
                                           const scalar_expr& mul_abstract) const noexcept {
    if (mul.size() != 2) {
      // We only care about multiplications of the form: v * i
      return std::nullopt;
    }
    // TODO: Should not need the abstract value to do this.
    const auto [coeff, maybe_i] = as_coeff_and_mul(mul_abstract);
    if (is_i(maybe_i)) {
      if (const float_constant* f = get_if<const float_constant>(coeff); f != nullptr) {
        return complex_double{0.0, f->value()};
      } else if (const integer_constant* i = get_if<const integer_constant>(coeff); i != nullptr) {
        return complex_double{0.0, static_cast<double>(i->value())};
      }
    }
    return std::nullopt;
  }

 private:
  bool allow_integer_conversion_;
};

std::optional<std::variant<std::int64_t, double, std::complex<double>>> numerical_cast(
    const scalar_expr& expr) {
  if (const float_constant* f = get_if<const float_constant>(expr); f != nullptr) {
    return f->value();
  } else if (const integer_constant* i = get_if<const integer_constant>(expr); i != nullptr) {
    return static_cast<std::int64_t>(i->value());
  } else if (const auto result = complex_cast(expr, true); result.has_value()) {
    return *result;
  }
  return std::nullopt;
}

std::optional<std::complex<double>> complex_cast(const scalar_expr& expr,
                                                 const bool allow_integer_conversion) {
  return visit(expr, convert_to_complex_visitor{allow_integer_conversion});
}

}  // namespace wf
