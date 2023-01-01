// Copyright 2022 Gareth Cross
#include "expressions/multiplication.h"

#include <algorithm>

#include "expressions/constant_expressions.h"
#include "expressions/power.h"

namespace math {

template <typename F>
auto AccumulateAndRemove(std::vector<Expr>& input, F&& func) {
  // Deduce the type the user wants:
  using FuncTraits = function_traits<std::decay_t<F>>;
  using ReturnType = typename FuncTraits::ReturnType;
  using Argument = typename FuncTraits::template DecayedArgType<1>;
  static_assert(FuncTraits::Arity == 2, "Must be a function of two arguments");
  static_assert(std::is_same_v<Argument, ReturnType>);

  // For every matching element, run the user-provided function.
  // They will be moved into the range [new_end, ...]
  std::optional<Argument> result{};
  const auto new_end = std::remove_if(input.begin(), input.end(),
                                      [&result, func = std::move(func)](const Expr& expr) {
                                        const Argument* const with_type = TryCast<Argument>(expr);
                                        if (with_type) {
                                          if (result) {
                                            result = func(result.value(), *with_type);
                                          } else {
                                            result = *with_type;
                                          }
                                          return true;
                                        }
                                        return false;
                                      });
  // Resize the original container.
  input.erase(new_end, input.end());
  return result;
}

template <typename T>
std::vector<T> RemoveAndReturnTyped(std::vector<Expr>& input) {
  const auto old_end = input.end();
  const auto new_end =
      std::remove_if(input.begin(), old_end, [](const Expr& expr) { return IsType<T>(expr); });
  std::vector<T> result;
  result.reserve(std::distance(new_end, old_end));
  for (auto it = new_end; it != old_end; ++it) {
    result.push_back(*it->template StaticCast<T>());
  }
  input.erase(new_end, old_end);
  return result;
}

static Expr ConvertIntegerToConstant(const Integer& int_value) {
  ASSERT_NOT_EQUAL(int_value.GetValue(), 0);
  if (int_value.GetValue() == 1) {
    return Constants::One;
  } else if (int_value.GetValue() == -1) {
    return Constants::NegativeOne;
  } else {
    return MakeExpr<Integer>(int_value);
  }
}

// Rule:
//  float -> promote everything to float
//  rational -> promote integers to rationals
//  integer -> leave everything as integers
static Expr MultiplyScalars(const std::optional<Float>& float_value,
                            const std::optional<Integer>& int_value) {
  if (float_value) {
    Float result = *float_value;
    if (int_value) {
      // Promote integers to float.
      result = result * static_cast<Float>(*int_value);
    }
    // TODO: Should we check if the result of multiplication is actually just an integer?
    return MakeExpr<Float>(result);
  } else if (int_value) {
    return ConvertIntegerToConstant(*int_value);
  }
  return Constants::One;
}

Multiplication::Multiplication(std::vector<Expr> args) {
  // Pull out all the integer constants:
  const std::optional<Integer> integer_constant =
      AccumulateAndRemove(args, std::multiplies<Integer>());
  // Pull out all the floats:
  const std::optional<Float> float_constant = AccumulateAndRemove(args, std::multiplies<Float>());

  // Pull out special constants and sort them into canonical order:
  std::vector<Constant> special_constants = RemoveAndReturnTyped<Constant>(args);
  std::sort(special_constants.begin(), special_constants.end(),
            [&](const Constant& a, const Constant& b) { return a.GetName() < b.GetName(); });

  // Place numeric constants back at the front:
  args.insert(args.begin(), MultiplyScalars(float_constant, integer_constant));

  // TODO: Combine products with the same base!
  args_ = std::move(args);
}

// bool Multiplication::HasNegativeExponents() const {
//   return std::any_of(args_.begin(), args_.end(), [](const Expr& expr) {
//     return VisitLambda(expr,
//                        [](const Power& pow) {
//                          const Expr coeff = CoefficientVisitor::GetCoefficient(pow.Exponent());
//                          return IsNegativeNumber(coeff);
//                        })
//         .value_or(false);
//   });
// }

static Expr MaybeMakeMultiplication(std::vector<Expr> args) {
  if (args.empty()) {
    return Constants::One;
  } else if (args.size() < 2) {
    return args.front();
  }
  return MakeExpr<Multiplication>(std::move(args));
}

std::pair<Expr, Expr> Multiplication::SplitByExponent() const {
  std::vector<Expr> numerator{};
  std::vector<Expr> denominator{};
  for (const Expr& expr : args_) {
    // Pull the base and exponent:
    auto [base, exponent] = AsBaseAndExponent(expr);
    // Sort into numerator and denominator, depending on sign of the exponent:
    const Expr exponent_coeff = CoefficientVisitor::GetCoefficient(exponent);
    if (IsNegativeNumber(exponent_coeff)) {
      if (exponent_coeff.IsIdenticalTo(Constants::NegativeOne)) {
        denominator.push_back(std::move(base));
      } else {
        denominator.push_back(Power::Create(std::move(base), -exponent));
      }
    } else {
      numerator.push_back(expr);
    }
  }
  return std::make_pair(MaybeMakeMultiplication(std::move(numerator)),
                        MaybeMakeMultiplication(std::move(denominator)));
}

Expr Multiplication::FromTwoOperands(const Expr& a, const Expr& b) {
  // Check if either argument is zero:
  if (IsZero(a) || IsZero(b)) {
    return Constants::Zero;
  }
  if (IsOne(a)) {
    return b;
  }
  if (IsOne(b)) {
    return a;
  }

  const Multiplication* a_mul = TryCast<Multiplication>(a);
  const Multiplication* b_mul = TryCast<Multiplication>(b);

  // a * b * c * d -> Multiplication(a, b, c, d)
  // a * b * (c + d) -> Multiplication(a, b, c + d)
  // (a + b) * (c + d) -> Multiplication(a + b, c + d)

  std::vector<Expr> args;
  args.reserve(2);
  if (a_mul) {
    args.insert(args.end(), a_mul->args_.begin(), a_mul->args_.end());
  } else {
    args.push_back(a);
  }
  if (b_mul) {
    args.insert(args.end(), b_mul->args_.begin(), b_mul->args_.end());
  } else {
    args.push_back(b);
  }
  return MakeExpr<Multiplication>(std::move(args));
}

}  // namespace math
