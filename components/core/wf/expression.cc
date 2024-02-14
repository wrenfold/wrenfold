#include "wf/expression.h"

#include "wf/assertions.h"
#include "wf/constants.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"
#include "wf/plain_formatter.h"

namespace wf {

scalar_expr::scalar_expr(const std::string_view name, const number_set set)
    : scalar_expr(std::in_place_type_t<variable>{}, named_variable(name), set) {}

static scalar_expr simplify_rational(rational_constant r) {
  if (const auto as_int = r.try_convert_to_integer(); as_int.has_value()) {
    return scalar_expr(*as_int);
  }
  return scalar_expr(std::in_place_type_t<rational_constant>{}, r);
}

scalar_expr::scalar_expr(const rational_constant r) : scalar_expr(simplify_rational(r)) {}

scalar_expr scalar_expr::from_float(const double x) {
  if (x == 0) {
    return constants::zero;
  }
  WF_ASSERT(std::isfinite(x), "Float values must be finite: {}", x);
  return make_expr<float_constant>(x);
}

scalar_expr scalar_expr::from_int(const checked_int x) {
  if (x == 0) {
    return constants::zero;
  } else if (x == 1) {
    return constants::one;
  } else if (x == -1) {
    return constants::negative_one;
  }
  return make_expr<integer_constant>(x);
}

std::string scalar_expr::to_string() const {
  plain_formatter formatter{};
  visit(*this, formatter);
  return formatter.take_output();
}

scalar_expr scalar_expr::operator-() const {
  return multiplication::from_operands({constants::negative_one, *this});
}

scalar_expr operator+(const scalar_expr& a, const scalar_expr& b) {
  // See note on absl::Span() constructor, the lifetimes here are valid.
  // We are constructing an initializer_list.
  return addition::from_operands({a, b});
}

scalar_expr operator-(const scalar_expr& a, const scalar_expr& b) {
  return a + multiplication::from_operands({constants::negative_one, b});
}

scalar_expr operator*(const scalar_expr& a, const scalar_expr& b) {
  return multiplication::from_operands({a, b});
}

scalar_expr operator/(const scalar_expr& a, const scalar_expr& b) {
  auto one_over_b = power::create(b, constants::negative_one);
  return multiplication::from_operands({a, one_over_b});
}

scalar_expr operator<(const scalar_expr& a, const scalar_expr& b) {
  return relational::create(relational_operation::less_than, a, b);
}

scalar_expr operator>(const scalar_expr& a, const scalar_expr& b) {
  return relational::create(relational_operation::less_than, b, a);
}

scalar_expr operator<=(const scalar_expr& a, const scalar_expr& b) {
  return relational::create(relational_operation::less_than_or_equal, a, b);
}

scalar_expr operator>=(const scalar_expr& a, const scalar_expr& b) {
  return relational::create(relational_operation::less_than_or_equal, b, a);
}

scalar_expr operator==(const scalar_expr& a, const scalar_expr& b) {
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

precedence get_precedence(const scalar_expr& expr) { return visit(expr, precedence_visitor{}); }

scalar_expr make_unique_variable_symbol(number_set set) {
  return make_expr<variable>(unique_variable(), set);
}

}  // namespace wf
