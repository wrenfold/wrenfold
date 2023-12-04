// Copyright 2023 Gareth Cross
#include <algorithm>

#include "wf/common_visitors.h"
#include "wf/expressions/all_expressions.h"
#include "wf/matrix_expression.h"
#include "wf/operations.h"
#include "wf/visitor_impl.h"

namespace math {
using namespace math::custom_literals;

// Steps:
//  (1) substitute values
//  (2) see if it produces a case we can handle w/ Hôpital's rule
//  (3) try to apply the rule
class limit_visitor {
 public:
  // x is the variable we are taking the limit wrt to.
  // 0+ is the value of `x` at the limit.
  explicit limit_visitor(const Expr& x)
      : x_(x),
        x_typed_(cast_checked<variable>(x_)),
        positive_inf_{positive_inf_placeholder()},
        negative_inf_{negative_inf_placeholder()} {
    if (x_typed_.set() != number_set::real_non_negative) {
      throw domain_error("Domain of limit variable `{}` is {}, but it should be {}.", x_,
                         string_from_number_set(x_typed_.set()),
                         string_from_number_set(number_set::real_non_negative));
    }
  }

  // Check the cache, and if no value exists, visit with this:
  std::optional<Expr> visit(const Expr& expr) {
    auto it = cache_.find(expr);
    if (it != cache_.end()) {
      return it->second;
    }
    std::optional<Expr> result = visit_with_expr(expr, *this);
    cache_.emplace(expr, result);
    return result;
  }

  std::optional<Expr> visit_no_inf(const Expr& expr) {
    std::optional<Expr> result = visit(expr);
    // Don't allow positive/negative affine infinity to leak out of the limit visitor.
    if (result.has_value() &&
        (result->is_identical_to(positive_inf_) || result->is_identical_to(negative_inf_))) {
      return std::nullopt;
    }
    return result;
  }

  std::optional<Expr> operator()(const addition& add) {
    addition::container_type f_funcs{}, g_funcs{}, terms{};
    for (const Expr& expr : add) {
      std::optional<Expr> child = visit(expr);
      if (!child || is_undefined(*child) || is_complex_infinity(*child)) {
        return std::nullopt;
      } else if (child->is_identical_to(positive_inf_)) {
        f_funcs.push_back(expr);
      } else if (child->is_identical_to(negative_inf_)) {
        g_funcs.push_back(expr);
      } else {
        terms.push_back(std::move(*child));
      }
    }

    const bool has_non_constant_terms = std::any_of(
        terms.begin(), terms.end(), [](const Expr& v) { return !is_numeric_or_constant(v); });

    if (f_funcs.empty() != g_funcs.empty()) {
      // only one kind of infinity:
      if (has_non_constant_terms) {
        return std::nullopt;
      } else {
        return f_funcs.empty() ? negative_inf_ : positive_inf_;
      }
    }

    if (!f_funcs.empty() && !g_funcs.empty()) {
      const Expr f = addition::from_operands(f_funcs);
      const Expr g = addition::from_operands(g_funcs);
      std::optional<Expr> indeterminate_term = visit((1 / g + 1 / f) * (g * f));
      if (!indeterminate_term) {
        return std::nullopt;
      }
      terms.push_back(std::move(*indeterminate_term));
    }

    return addition::from_operands(terms);
  }

  std::optional<Expr> operator()(const cast_bool& cast) {
    std::optional<Expr> arg = visit(cast.arg());
    if (!arg) {
      return std::nullopt;
    }
    return cast_int_from_bool(*arg);
  }

  std::optional<Expr> operator()(const conditional&) {
    // We don't support limits of conditionals yet.
    return std::nullopt;
  }

  std::optional<Expr> operator()(const symbolic_constant&, const Expr& expr) const { return expr; }

  std::optional<Expr> operator()(const derivative&, const Expr& expr) const {
    throw type_error(
        "Cannot take limits of expressions containing `{}`. Encountered expression: {}",
        derivative::name_str, expr);
  }

  // Evaluate Hôpital's rule on a limit of the form:
  //  lim[x->c] f(x) / g(x)
  std::optional<Expr> hopitals_rule_quotient(const Expr& f, const Expr& g) {
    // Take the derivative and substitute:
    const Expr df = f.diff(x_);
    const Expr dg = g.diff(x_);
    const Expr df_over_dg = df / dg;

    // Some useful prints when debugging this method:
#if 0
    const std::string prefix(num_recursions_, ' ');
    fmt::print("{}f: {}\n", prefix, f);
    fmt::print("{}g: {}\n", prefix, g);
    fmt::print("{}df: {} --> {}\n", prefix, f, df);
    fmt::print("{}dg: {} --> {}\n", prefix, g, dg);
    fmt::print("{}df_over_dg: {}\n", prefix, df_over_dg);
#endif

    // TODO: Actually detect recursion properly. For now we just bail if the complexity grows too
    //  large.
    constexpr std::size_t max_recursions = 4;
    if (num_recursions_ >= max_recursions) {
      return std::nullopt;
    }

    ++num_recursions_;
    std::optional<Expr> df_over_dg_eval = visit(df_over_dg);
    --num_recursions_;
    if (!df_over_dg_eval || is_undefined(*df_over_dg_eval)) {
      return std::nullopt;
    }
    return df_over_dg_eval;
  }

  struct integral_powers {
    std::vector<integer_constant> powers{};

    bool are_all_identical_up_to_sign() const {
      WF_ASSERT(!powers.empty());
      const auto first = powers.begin();
      return std::all_of(std::next(first), powers.end(),
                         [&](const integer_constant& i) { return first->abs() == i.abs(); });
    }

    std::tuple<int, int> determine_signs() const {
      int num_positive = 0;
      int num_negative = 0;
      for (const integer_constant i : powers) {
        if (i.get_value() >= 0) {
          ++num_positive;
        } else {
          ++num_negative;
        }
      }
      return std::make_tuple(num_positive, num_negative);
    }

    bool are_all_one() const {
      return std::all_of(powers.begin(), powers.end(),
                         [&](const integer_constant& i) { return i.get_value() == 1; });
    }
  };

  template <typename Container>
  std::optional<integral_powers> extract_integer_exponents(const Container& mul) {
    integral_powers result{};
    for (const Expr& expr : mul) {
      auto [base, exp] = as_base_and_exp(expr);
      if (const integer_constant* i = cast_ptr<integer_constant>(exp); i != nullptr) {
        result.powers.push_back(*i);
      } else {
        return std::nullopt;
      }
    }
    return {std::move(result)};
  }

  // Process multiplied terms that evaluate to a combination of zeros and infinities.
  // This is used to handle multiplications that produce an indeterminate forms:
  //  ∞ * 0, ∞ / ∞, 0 / 0
  template <typename Container>
  std::optional<Expr> process_indeterminate_form_multiplied_terms_1(const Container& mul) {
    // Try to extract common integer powers:
    // TODO: Clean this up and generalize to any common power...
    if (std::optional<integral_powers> integral_exponents = extract_integer_exponents(mul);
        integral_exponents.has_value() && integral_exponents->are_all_identical_up_to_sign() &&
        !integral_exponents->are_all_one()) {
      const integer_constant int_power = integral_exponents->powers.front().abs();
      const auto [num_positive, num_negative] = integral_exponents->determine_signs();

      std::vector<Expr> terms{};
      if (num_positive == 0 || num_negative == 0) {
        // all negative or positive
        std::transform(mul.begin(), mul.end(), std::back_inserter(terms), [](const Expr& v) {
          auto [base, _] = as_base_and_exp(v);
          return base;
        });
      } else {
        // mixed
        std::transform(mul.begin(), mul.end(), std::back_inserter(terms), [&](const Expr& v) {
          auto [base, exponent] = as_base_and_exp(v);
          return power::create(base, exponent / int_power.get_value());
        });
      }

      std::optional<Expr> inner_limit = process_indeterminate_form_multiplied_terms_2(terms);
      if (inner_limit.has_value()) {
        if (num_positive == 0) {
          return power::create(std::move(*inner_limit), -int_power.get_value());
        } else {
          return power::create(std::move(*inner_limit), int_power.get_value());
        }
      } else {
        return std::nullopt;
      }
    }
    return process_indeterminate_form_multiplied_terms_2(mul);
  }

  template <typename Container>
  std::optional<Expr> process_indeterminate_form_multiplied_terms_2(const Container& mul) {
    // `f_funcs` contains functions that evaluate to zero, while `g_funcs` contains functions that
    // evaluate to infinity.
    multiplication::container_type f_funcs{}, g_funcs{};
    for (const Expr& expr : mul) {
      std::optional<Expr> child = visit(expr);
      WF_ASSERT(child.has_value(), "Already checked this expression is valid: {}", expr);
      if (is_zero(*child)) {
        // Collect the powers of `x_` as we extract terms, in the hope that the expression will
        // simplify:
        f_funcs.push_back(expr.collect(x_));
      } else if (is_inf(*child)) {
        g_funcs.push_back(expr.collect(x_));
      } else {
        throw type_error("Child expression should be zero or infinity. Found: {}", *child);
      }
    }

    // We have both infinity and zero, so we can apply the rule for an indeterminate form:
    //  lim[x->c] f(x)g(x) where lim[x->c] f(x) = 0 and lim[x->c] g(x) = ∞
    //
    // Which we can re-write as:
    //  lim[x->c] f(x)g(x) = lim[x->c] f(x) / (1 / g(x)) = 0 / 0
    //
    // Or as:
    //  lim[x->c] f(x)g(x) = lim[x->c] g(x) / (1 / f(x)) = ∞ / ∞
    //
    // Either form qualifies for Hôpital's rule, but one might recurse indefinitely.
    const Expr f = multiplication::from_operands(f_funcs);
    const Expr g = multiplication::from_operands(g_funcs);

    // TODO: This is slower than it needs to be, we can make a better guess as to what form
    //  to use by inspecting the powers of `f` and `g`.
    std::optional<Expr> result = hopitals_rule_quotient(f, pow(g, -1));
    if (!result.has_value()) {
      // Try switching indeterminate forms:
      //  lim[x->c] f(x) / g(x) = inf/inf -----> lim[x->c] [f(x)]^-1 / [g(x)]^-1 = 0 / 0
      result = hopitals_rule_quotient(g, pow(f, -1));
    }
    return result;
  }

  template <typename Container>
  std::optional<Expr> process_multiplied_terms(const Container& mul) {
    // visit children and sort them:
    //  0 --> f_funcs
    //  infinity --> g_funcs
    //  everything else --> remainder
    multiplication::container_type indeterminate_terms{}, remainder{};

    std::size_t num_zeros = 0;
    std::size_t num_infinities = 0;
    std::size_t inf_sign_bits = 0;
    for (const Expr& expr : mul) {
      std::optional<Expr> child = visit(expr);
      if (!child || is_complex_infinity(*child) || is_undefined(*child)) {
        return std::nullopt;
      }
      if (is_zero(*child)) {
        ++num_zeros;
        indeterminate_terms.push_back(expr);
      } else if (is_inf(*child)) {
        ++num_infinities;
        inf_sign_bits += child->is_identical_to(negative_inf_);
        indeterminate_terms.push_back(expr);
      } else {
        remainder.push_back(std::move(*child));
      }
    }

    // We need at least one: ∞ * 0
    // Otherwise we delegate to Multiplication::from_operands
    if (num_infinities == 0) {
      if (num_zeros > 0) {
        remainder.push_back(constants::zero);
      }
      return multiplication::from_operands(remainder);
    }

    // We have at least one infinity. Do we have zeros?
    if (num_zeros == 0) {
      // There are no zeros, so we can't apply an indeterminate form.
      std::size_t sign_bits = inf_sign_bits;
      for (const Expr& v : remainder) {
        if (is_numeric_or_constant(v)) {
          if (is_negative_number(v)) {
            ++sign_bits;
          }
        } else {
          // We can't evaluate limits of the form: g(y) * ∞ (since we don't know the form of `g` or
          // `y`).
          return std::nullopt;
        }
      }
      if (sign_bits & 1) {
        return negative_inf_;
      } else {
        return positive_inf_;
      }
    }

    std::optional<Expr> indeterminate_result =
        process_indeterminate_form_multiplied_terms_1(indeterminate_terms);
    if (!indeterminate_result) {
      return std::nullopt;
    }
    remainder.push_back(std::move(*indeterminate_result));
    return multiplication::from_operands(remainder);
  }

  std::optional<Expr> operator()(const multiplication& mul) {
    return process_multiplied_terms(mul);
  }

  bool is_inf(const Expr& v) const {
    return v.is_identical_to(positive_inf_) || v.is_identical_to(negative_inf_);
  }

  std::optional<Expr> process_power(const Expr& b, const Expr& e) {
    // Substitute into base and exponent (first extract constant coefficients):
    auto base_sub_opt = visit(b);
    if (!base_sub_opt) {
      return std::nullopt;
    }
    auto exp_sub_opt = visit(e);
    if (!exp_sub_opt) {
      return std::nullopt;
    }

    const Expr base_sub = std::move(*base_sub_opt);
    const Expr exp_sub = std::move(*exp_sub_opt);
    if (is_undefined(base_sub) || is_complex_infinity(base_sub) || is_undefined(exp_sub) ||
        is_complex_infinity(exp_sub)) {
      return std::nullopt;
    }

    WF_ASSERT(!exp_sub.is_identical_to(constants::boolean_false));

    if (is_zero(exp_sub) && (is_inf(base_sub) || is_zero(base_sub))) {
      // 0 ^ 0 is an indeterminate form, and ∞ ^ 0
      // lim[x->c] f(x)^g(x) = e ^ (lim[x->c] g(x) * log(f(x)))
      // Where f(x) base `b` and g(x) is exponent `e`.

      // TODO: There is a requirement that f(x) approaches 0 from above, if the result is to be
      //  a real value (for 0^0). We are not currently enforcing this.
      std::optional<Expr> exponent = visit(e * log(b));
      if (!exponent) {
        return std::nullopt;
      }
      return power::create(constants::euler, std::move(*exponent));
    } else if (is_one(base_sub) && is_inf(exp_sub)) {
      // 1 ^ ∞ is an indeterminate form
      // lim[x->c] f(x)^g(x) = e ^ (lim[x->c] log(f(x)) * g(x))
      // Where f(x) -> 1 (the base) and g(x) -> ∞ (the exponent)
      std::optional<Expr> exponent = visit(log(b) * e);
      if (!exponent) {
        return std::nullopt;
      }
      return power::create(constants::euler, std::move(*exponent));
    } else if (is_zero(base_sub)) {
      // base is zero, exponent is either function or a (+/-) constant
      if (is_numeric_or_constant(exp_sub)) {
        if (is_negative_number(exp_sub)) {
          // (1/0)^(positive number) --> ∞
          return positive_inf_;
        } else {
          // 0^positive --> 0
          return constants::zero;
        }
      }
    } else if (is_inf(base_sub)) {
      if (base_sub.is_identical_to(positive_inf_)) {
        if (is_numeric_or_constant(exp_sub)) {
          return is_negative_number(exp_sub) ? constants::zero : positive_inf_;
        }
      } else {
        if (is_numeric_or_constant(exp_sub) && is_negative_number(exp_sub)) {
          return constants::zero;  // (-∞)^u where u < 0
        } else if (const integer_constant* exp_int = cast_ptr<integer_constant>(exp_sub);
                   exp_int != nullptr) {
          WF_ASSERT(!exp_int->is_zero() && !exp_int->is_negative(), "value = {}", *exp_int);
          if (exp_int->is_even()) {
            return positive_inf_;
          } else {
            return negative_inf_;
          }
        } else {
          return std::nullopt;
        }
      }
      return std::nullopt;  // ∞^g(y), or -∞^(positive value)
    } else if (is_inf(exp_sub)) {
      if (is_numeric_or_constant(base_sub)) {
        if (is_negative_number(base_sub)) {
          //  In the context of limits, we treat (negative value)^∞ as undefined.
          return std::nullopt;
        } else {
          // (positive) ^ ∞ --> ∞
          // (positive) ^ -∞ --> 0
          return exp_sub.is_identical_to(positive_inf_) ? positive_inf_ : constants::zero;
        }
      } else {
        // We don't handle g(y)^∞
        return std::nullopt;
      }
    }

    return power::create(base_sub, exp_sub);
  }

  // TODO: We need to handle the case of functions going to infinity.
  std::optional<Expr> operator()(const function& func) {
    function::container_type args{};
    for (const Expr& arg : func) {
      std::optional<Expr> arg_limit = visit(arg);
      if (!arg_limit) {
        return std::nullopt;
      } else {
        args.push_back(std::move(*arg_limit));
      }
    }

    switch (func.enum_value()) {
      case built_in_function::ln: {
        WF_ASSERT_EQUAL(1, args.size());
        if (is_zero(args[0])) {
          // In the context of a limit, allow log(0) --> -∞
          return negative_inf_;
        }
      } break;
      default:
        break;
    }
    return function::create(func.enum_value(), std::move(args));
  }

  std::optional<Expr> operator()(const complex_infinity&, const Expr& expr) const { return expr; }
  std::optional<Expr> operator()(const integer_constant&, const Expr& expr) { return expr; }
  std::optional<Expr> operator()(const float_constant&, const Expr& expr) { return expr; }
  std::optional<Expr> operator()(const rational_constant&, const Expr& expr) { return expr; }

  std::optional<Expr> operator()(const power& pow) {
    return process_power(pow.base(), pow.exponent());
  }

  std::optional<Expr> operator()(const relational& rel, const Expr&) {
    // For relationals, we do a substitution:
    std::optional<Expr> left = visit(rel.left());
    if (!left) {
      return std::nullopt;
    }
    std::optional<Expr> right = visit(rel.right());
    if (!right) {
      return std::nullopt;
    }
    return relational::create(rel.operation(), std::move(*left), std::move(*right));
  }

  std::optional<Expr> operator()(const undefined&) const { return constants::undefined; }

  std::optional<Expr> operator()(const variable& var, const Expr& expr) const {
    if (x_typed_.is_identical_to(var)) {
      return constants::zero;  //  TODO: Support any value here.
    }
    return expr;
  }

 private:
  static Expr positive_inf_placeholder() {
    static const Expr p_inf = make_unique_variable_symbol(number_set::unknown);
    return p_inf;
  }

  static Expr negative_inf_placeholder() {
    static const Expr n_inf = make_unique_variable_symbol(number_set::unknown);
    return n_inf;
  }

  const Expr& x_;
  const variable& x_typed_;

  // Placeholders we insert for positive/negative.
  Expr positive_inf_;
  Expr negative_inf_;

  // Cache of sub-expressions we've already evaluated the limit on.
  std::unordered_map<Expr, std::optional<Expr>, hash_struct<Expr>, is_identical_struct<Expr>>
      cache_{};

  // Used to detect the # of times hôpital's rule has recursed.
  std::size_t num_recursions_{0};
};

// TODO: Add support for limits going to infinity.
std::optional<Expr> limit(const Expr& f_of_x, const Expr& x) {
  if (!x.is_type<variable>()) {
    throw type_error(
        "Limit argument `x` must be a variable. Encountered expression of type `{}`: {}",
        x.type_name(), x);
  }
  return limit_visitor{x}.visit_no_inf(f_of_x);
}

std::optional<MatrixExpr> limit(const MatrixExpr& f_of_x, const Expr& x) {
  if (!x.is_type<variable>()) {
    throw type_error(
        "Limit argument `x` must be a variable. Encountered expression of type `{}`: {}",
        x.type_name(), x);
  }
  // Reuse the visitor, so that we get the benefit of caching results:
  limit_visitor visitor{x};

  std::vector<Expr> values;
  values.reserve(f_of_x.size());
  for (const Expr& f : f_of_x.as_matrix()) {
    std::optional<Expr> f_lim = visitor.visit_no_inf(f);
    if (!f_lim) {
      return std::nullopt;
    } else {
      values.push_back(std::move(*f_lim));
    }
  }
  return MatrixExpr::create(f_of_x.rows(), f_of_x.cols(), std::move(values));
}

}  // namespace math
