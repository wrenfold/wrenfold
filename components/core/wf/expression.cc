#include "wf/expression.h"

#include "wf/assertions.h"
#include "wf/constants.h"
#include "wf/expressions/all_expressions.h"
#include "wf/plain_formatter.h"
#include "wf/visitor_impl.h"

namespace wf {

Expr::Expr(const std::string_view name, const number_set set)
    : Expr(make_expr<variable>(named_variable(name), set)) {}

static Expr simplify_rational(rational_constant r) {
  if (const auto as_int = r.try_convert_to_integer(); as_int.has_value()) {
    return Expr(as_int->get_value());
  }
  return Expr{Expr::storage_type(rational_constant(r))};
}

Expr::Expr(const rational_constant r) : Expr(simplify_rational(r)) {}

Expr Expr::from_float(const double x) {
  if (x == 0) {
    return constants::zero;
  }
  WF_ASSERT(std::isfinite(x), "Float values must be finite: {}", x);
  return make_expr<float_constant>(x);
}

Expr Expr::from_int(const std::int64_t x) {
  if (x == 0) {
    return constants::zero;
  } else if (x == 1) {
    return constants::one;
  } else if (x == -1) {
    return constants::negative_one;
  }
  return make_expr<integer_constant>(x);
}

bool Expr::is_identical_to_internal(const Expr& other) const {
  return impl_.visit([&](const auto& v) -> bool {
    using T = std::decay_t<decltype(v)>;
    return v.is_identical_to(other.impl_.cast_unchecked<T>());
  });
}

std::string_view Expr::type_name() const {
  return impl_.visit([](const auto& v) noexcept -> std::string_view {
    using T = std::decay_t<decltype(v)>;
    return T::name_str;
  });
}

std::string Expr::to_string() const {
  plain_formatter formatter{};
  visit(*this, formatter);
  return formatter.take_output();
}

Expr Expr::operator-() const {
  return multiplication::from_operands({constants::negative_one, *this});
}

Expr operator+(const Expr& a, const Expr& b) {
  // See note on absl::Span() constructor, the lifetimes here are valid.
  // We are constructing an initializer_list.
  return addition::from_operands({a, b});
}

Expr operator-(const Expr& a, const Expr& b) {
  return a + multiplication::from_operands({constants::negative_one, b});
}

Expr operator*(const Expr& a, const Expr& b) { return multiplication::from_operands({a, b}); }

Expr operator/(const Expr& a, const Expr& b) {
  auto one_over_b = power::create(b, constants::negative_one);
  return multiplication::from_operands({a, one_over_b});
}

Expr operator<(const Expr& a, const Expr& b) {
  return relational::create(relational_operation::less_than, a, b);
}

Expr operator>(const Expr& a, const Expr& b) {
  return relational::create(relational_operation::less_than, b, a);
}

Expr operator<=(const Expr& a, const Expr& b) {
  return relational::create(relational_operation::less_than_or_equal, a, b);
}

Expr operator>=(const Expr& a, const Expr& b) {
  return relational::create(relational_operation::less_than_or_equal, b, a);
}

Expr operator==(const Expr& a, const Expr& b) {
  return relational::create(relational_operation::equal, a, b);
}

// Visitor to determine mathematical precedence.
struct precedence_visitor {
  template <typename T>
  constexpr precedence operator()(const T&) const noexcept {
    if constexpr (std::is_same_v<multiplication, T>) {
      return precedence::multiplication;
    } else if constexpr (std::is_same_v<addition, T>) {
      return precedence::addition;
    } else if constexpr (std::is_same_v<power, T>) {
      return precedence::power;
    } else if constexpr (std::is_same_v<rational_constant, T>) {
      return precedence::multiplication;
    } else if constexpr (std::is_same_v<relational, T>) {
      return precedence::relational;
    } else {
      return precedence::none;
    }
  }
};

precedence get_precedence(const Expr& expr) { return visit(expr, precedence_visitor{}); }

Expr make_unique_variable_symbol(number_set set) {
  return make_expr<variable>(unique_variable(), set);
}

}  // namespace wf
