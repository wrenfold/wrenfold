// Copyright 2022 Gareth Cross
#include "expressions/multiplication.h"

#include <algorithm>

#include "expressions/numeric_expressions.h"
#include "expressions/power.h"

namespace math {

template <typename F>
auto AccumulateAndRemove(std::vector<Expr>& input, F&& func) {
  // Deduce the type the user wants:
  using FuncTraits = FunctionTraits<std::decay_t<F>>;
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

Multiplication::Multiplication(std::vector<Expr> args) : NAryOp(std::move(args)) {}

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
  auto num =
      numerator.empty() ? Constants::One : Multiplication::FromOperands(std::move(numerator));
  auto den =
      denominator.empty() ? Constants::One : Multiplication::FromOperands(std::move(denominator));
  return std::make_pair(std::move(num), std::move(den));
}

Expr Multiplication::FromOperands(const std::vector<Expr>& args) {
  ASSERT(!args.empty());
  if (args.size() < 2) {
    return args.front();
  }

  // Check for zeros:
  const bool contains_zeros = std::any_of(args.begin(), args.end(), &IsZero);
  if (contains_zeros) {
    return Constants::Zero;
  }

  std::vector<Expr> unpacked_args;
  unpacked_args.reserve(args.size());
  for (const Expr& arg : args) {
    if (const Multiplication* const mul = TryCast<Multiplication>(arg)) {
      // Multiplications must be flattened:
      const auto& mul_args = mul->Args();
      unpacked_args.insert(unpacked_args.end(), mul_args.begin(), mul_args.end());
    } else {
      unpacked_args.push_back(arg);
    }
  }

  // Now canonicalize the arguments:
  CanonicalizeArguments(unpacked_args);
  ASSERT(!unpacked_args.empty());

  if (unpacked_args.size() == 1) {
    // After canonicalization, only one term remained.
    // This could occur if the whole chain of multiplications evaluated to a constant.
    return unpacked_args.front();
  }
  return MakeExpr<Multiplication>(std::move(unpacked_args));
}

void Multiplication::CanonicalizeArguments(std::vector<Expr>& args) {
  // Pull out all the integer constants:
  const std::optional<Integer> integer_constant =
      AccumulateAndRemove(args, std::multiplies<Integer>());

  // Pull out all the floats:
  const std::optional<Float> float_constant = AccumulateAndRemove(args, std::multiplies<Float>());

  // Pull out special constants and sort them into canonical order:
  //  std::vector<Constant> special_constants = RemoveAndReturnTyped<Constant>(args);
  //  std::sort(special_constants.begin(), special_constants.end(),
  //            [&](const Constant& a, const Constant& b) { return a.GetName() < b.GetName(); });

  // Place numeric constants back at the front:
  args.insert(args.begin(), MultiplyScalars(float_constant, integer_constant));
}

}  // namespace math
