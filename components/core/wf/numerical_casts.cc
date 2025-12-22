// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/numerical_casts.h"

#include "wf/expression_visitor.h"

namespace wf {

using complex_double = std::complex<double>;

struct convert_to_complex_visitor {
  template <typename T, typename = enable_if_does_not_contain_type_t<
                            T, float_constant, addition, multiplication, imaginary_unit>>
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

  std::optional<complex_double> operator()(const float_constant f) const noexcept {
    return complex_double{f.value(), 0.0};
  }

  std::optional<complex_double> operator()(const imaginary_unit) const noexcept {
    return complex_double{0.0, 1.0};
  }

  std::optional<complex_double> operator()(const multiplication& mul,
                                           const scalar_expr& mul_abstract) const noexcept {
    if (mul.size() != 2) {
      // We only care about multiplications of the form: v * i
      return std::nullopt;
    }
    // TODO: Should not need the abstract value to do this.
    const auto [coeff, maybe_i] = as_coeff_and_mul(mul_abstract);
    if (const float_constant* f = get_if<const float_constant>(coeff);
        f != nullptr && is_i(maybe_i)) {
      return complex_double{0.0, f->value()};
    }
    return std::nullopt;
  }
};

// TODO: Should support complex expressions of integers as well.
std::optional<std::variant<std::int64_t, double, std::complex<double>>> numerical_cast(
    const scalar_expr& expr) {
  if (const float_constant* f = get_if<const float_constant>(expr); f != nullptr) {
    return f->value();
  } else if (const integer_constant* i = get_if<const integer_constant>(expr); i != nullptr) {
    return static_cast<std::int64_t>(i->value());
  } else if (const auto result = complex_cast(expr); result.has_value()) {
    return *result;
  }
  return std::nullopt;
}

std::optional<std::complex<double>> complex_cast(const scalar_expr& expr) {
  return visit(expr, convert_to_complex_visitor{});
}

}  // namespace wf
