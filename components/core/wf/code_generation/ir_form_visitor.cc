// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/ir_form_visitor.h"

#include "wf/code_generation/ir_consumer_vector.h"
#include "wf/enumerations.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/variable.h"
#include "wf/plain_formatter.h"
#include "wf/utility/overloaded_visit.h"
#include "wf/utility/scoped_trace.h"
#include "wf/utility_visitors.h"

namespace wf {
ir_form_visitor::ir_form_visitor(control_flow_graph& output_graph)
    : output_graph_(output_graph), output_block_(output_graph.first_block()) {
  WF_ASSERT_EQ(1, output_graph.num_blocks(), "Output graph should only have one block.");
}

ir::value_ptr ir_form_visitor::operator()(const addition& add) {
  // Convert to values, then make sure appropriate casts are inserted:
  auto args = transform_map<absl::InlinedVector<ir::value_ptr, 8>>(add, *this);
  return create_add_or_mul_with_operands<ir::add>(std::move(args));
}

ir::value_ptr ir_form_visitor::operator()(const boolean_constant& b) {
  return push_operation(ir::load{b}, numeric_primitive_type::boolean);
}

ir::value_ptr ir_form_visitor::operator()(const complex_infinity&) const {
  throw type_error("Cannot generate code for complex infinity.");
}

ir::value_ptr ir_form_visitor::operator()(const compound_expression_element& el) {
  const ir::value_ptr compound_val = operator()(el.provenance());
  return overloaded_visit(
      compound_val->type(),
      [&](ir::void_type) -> ir::value_ptr {
        WF_ASSERT_ALWAYS("Compound expression cannot have void type. name = {}, index = {}",
                         compound_val->name(), el.index());
      },
      [&](scalar_type) { return compound_val; },
      [&](const matrix_type&) {
        return push_operation(ir::get{el.index()}, numeric_primitive_type::floating_point,
                              compound_val);
      },
      [&](const custom_type& custom) {
        // Determine the numeric type of the member we are accessing:
        const numeric_primitive_type type = determine_member_type(custom, el.index());
        return push_operation(ir::get{el.index()}, type, compound_val);
      });
}

ir::value_ptr ir_form_visitor::operator()(const conditional& cond) {
  const ir::value_ptr condition = operator()(cond.condition());
  const ir::value_ptr if_branch = operator()(cond.if_branch());
  const ir::value_ptr else_branch = operator()(cond.else_branch());
  const numeric_primitive_type promoted_type =
      std::max(if_branch->numeric_type(), else_branch->numeric_type());
  return push_operation(ir::cond{}, promoted_type, condition, maybe_cast(if_branch, promoted_type),
                        maybe_cast(else_branch, promoted_type));
}

ir::value_ptr ir_form_visitor::operator()(const custom_type_argument& arg) {
  return push_operation(ir::load{arg}, arg.type());
}

ir::value_ptr ir_form_visitor::operator()(const custom_type_construction& construct) {
  absl::InlinedVector<ir::value_ptr, 8> operands{};
  operands.reserve(construct.size());

  iterate_custom_type_fields(
      construct.type(),
      make_overloaded(
          [&](const std::size_t index, const scalar_type s) {
            operands.push_back(maybe_cast(operator()(construct.at(index)), s.numeric_type()));
          },
          [&](const std::size_t index, const matrix_type& m) {
            for (std::size_t i = 0; i < m.size(); ++i) {
              const ir::value_ptr mat_element = operator()(construct.at(index + i));
              operands.push_back(maybe_cast(mat_element, numeric_primitive_type::floating_point));
            }
          }));

  return push_operation(ir::construct{construct.type()}, construct.type(), operands);
}

ir::value_ptr ir_form_visitor::operator()(const external_function_invocation& invoke) {
  const external_function& f = invoke.function();

  // Generate values for every argument. Insert casts for scalars if required.
  auto operands = transform_enumerate_map<absl::InlinedVector<ir::value_ptr, 8>>(
      invoke, [&](const std::size_t index, const auto& arg) {
        // Convert the argument.
        const ir::value_ptr val = std::visit(*this, arg);

        // Type check the argument, and cast scalars.
        const auto& expected_type = f.argument_at(index).type();
        return std::visit(
            [&]<typename T0, typename T1>(const T0& expected, const T1& actual) -> ir::value_ptr {
              if constexpr (!std::is_same_v<T0, T1>) {
                WF_ASSERT_ALWAYS("Mismatched argument types. Expected: {}, Actual: {}", expected,
                                 actual);
              } else if constexpr (std::is_same_v<T0, scalar_type>) {
                return maybe_cast(val, expected.numeric_type());
              }
              return val;
            },
            expected_type, val->type());
      });

  // Visit the return type so we can convert it:
  return std::visit(
      [&](const auto& return_type) {
        return push_operation(ir::call_external_function{f}, return_type, operands);
      },
      f.return_type());
}

ir::value_ptr ir_form_visitor::operator()(const derivative&) const {
  throw type_error("Cannot generate code for expressions containing `{}`.", derivative::name_str);
}

ir::value_ptr ir_form_visitor::operator()(const float_constant& f) {
  return push_operation(ir::load{f}, numeric_primitive_type::floating_point);
}

ir::value_ptr ir_form_visitor::operator()(const function_argument_variable& var) {
  return push_operation(ir::load{var}, var.primitive_type());
}

static constexpr std_math_function std_math_function_from_built_in(const built_in_function name) {
  switch (name) {
    case built_in_function::cos:
      return std_math_function::cos;
    case built_in_function::sin:
      return std_math_function::sin;
    case built_in_function::tan:
      return std_math_function::tan;
    case built_in_function::arccos:
      return std_math_function::acos;
    case built_in_function::arcsin:
      return std_math_function::asin;
    case built_in_function::arctan:
      return std_math_function::atan;
    case built_in_function::cosh:
      return std_math_function::cosh;
    case built_in_function::sinh:
      return std_math_function::sinh;
    case built_in_function::tanh:
      return std_math_function::tanh;
    case built_in_function::arccosh:
      return std_math_function::acosh;
    case built_in_function::arcsinh:
      return std_math_function::asinh;
    case built_in_function::arctanh:
      return std_math_function::atanh;
    case built_in_function::log:
      return std_math_function::log;
    case built_in_function::abs:
      return std_math_function::abs;
    case built_in_function::signum:
      return std_math_function::signum;
    case built_in_function::floor:
      return std_math_function::floor;
    case built_in_function::arctan2:
      return std_math_function::atan2;
  }
  WF_ASSERT_ALWAYS("Invalid enum value: {}", string_from_built_in_function(name));
}

// Determine the return-type of a call to a built-in math function.
// For some functions, this is determined by the input type. For most functions it is just a
// floating point scalar.
template <typename Container>
static numeric_primitive_type std_function_output_type(const std_math_function func,
                                                       const Container& args) {
  switch (func) {
    case std_math_function::abs: {
      WF_ASSERT_EQ(1, args.size());
      return args[0]->numeric_type();
    }
    case std_math_function::floor:
    case std_math_function::signum: {
      // TODO: signum/sign could adopt the type of its argument, but we need to either add another
      //  enum value or pass type information explicitly to the ast. I'm not sure which to do yet
      //  so I am deferring this decision.
      return numeric_primitive_type::integral;
    }
    default:
      break;
  }
  return numeric_primitive_type::floating_point;
}

ir::value_ptr ir_form_visitor::operator()(const built_in_function_invocation& func) {
  const std_math_function enum_value = std_math_function_from_built_in(func.enum_value());

  // Convert args to values:
  auto args = transform_map<absl::InlinedVector<ir::value_ptr, 4>>(func, *this);

  const numeric_primitive_type numeric_type = std_function_output_type(enum_value, args);
  return push_operation(ir::call_std_function{enum_value}, numeric_type, args);
}

ir::value_ptr ir_form_visitor::operator()(const imaginary_unit&) const {
  throw type_error("Cannot generate code for expressions containing `{}`.",
                   imaginary_unit::name_str);
}

ir::value_ptr ir_form_visitor::operator()(const integer_constant& i) {
  return push_operation(ir::load{i}, numeric_primitive_type::integral);
}

ir::value_ptr ir_form_visitor::operator()(const iverson_bracket& bracket) {
  const ir::value_ptr arg = operator()(bracket.arg());
  return push_operation(ir::cast{numeric_primitive_type::integral},
                        numeric_primitive_type::integral, arg);
}

ir::value_ptr ir_form_visitor::operator()(const matrix& mat) {
  const matrix_type mat_type{mat.rows(), mat.cols()};
  return push_operation(ir::construct(mat_type), mat_type,
                        transform_map<std::vector>(mat, [this](const scalar_expr& arg) {
                          return maybe_cast(this->operator()(arg),
                                            numeric_primitive_type::floating_point);
                        }));
}

ir::value_ptr ir_form_visitor::operator()(const multiplication& mul) {
  // We need to copy the contents so we can replace some terms:
  absl::InlinedVector<scalar_expr, 16> mul_terms{mul.begin(), mul.end()};

  const auto is_negative_integer = [](const scalar_expr& s) {
    if (const integer_constant* as_int = get_if<const integer_constant>(s);
        as_int != nullptr && as_int->is_negative() && *as_int != integer_constant(-1)) {
      return true;
    }
    return false;
  };

  // If the multiplication contains a negative integer, then negate it and put -1 in.
  // This facilitates identifying more common terms.
  if (const auto it = std::ranges::find_if(mul_terms, is_negative_integer); it != mul_terms.end()) {
    *it = -*it;
    mul_terms.push_back(constants::negative_one);
  }

  // Optionally group operands that have similar exponents into a single power operation:
  absl::InlinedVector<ir::value_ptr, 16> mul_value_operands{};
  if constexpr (constexpr bool group_operands_during_conversion = false;
                group_operands_during_conversion) {
    remove_and_group_mul_operands_by_exponent(mul_terms, mul_value_operands);
  }

  // Convert some integer powers into multiplications:
  convert_mul_operands_with_power_expansion(mul_terms, mul_value_operands);
  WF_ASSERT(!mul_value_operands.empty());
  if (mul_value_operands.size() == 1) {
    return mul_value_operands[0];
  } else {
    return create_add_or_mul_with_operands<ir::mul>(mul_value_operands);
  }
}

template <typename T, typename Container>
ir::value_ptr ir_form_visitor::create_add_or_mul_with_operands(Container args) {
  WF_ASSERT_GT(args.size(), 1);
  auto promoted_type = numeric_primitive_type::integral;
  for (const ir::value_ptr v : args) {
    promoted_type = std::max(promoted_type, v->numeric_type());
  }
  for (auto& arg : args) {
    arg = maybe_cast(arg, promoted_type);
  }
  return push_operation(T{}, promoted_type, args);
}

template <typename Container, typename ContainerOut>
void ir_form_visitor::remove_and_group_mul_operands_by_exponent(Container& operands,
                                                                ContainerOut& grouped_operands) {
  // Built a map from absolute integer exponent to a vector of bases that have that exponent.
  std::unordered_map<integer_constant, absl::InlinedVector<scalar_expr, 4>,
                     hash_struct<integer_constant>, is_identical_struct<integer_constant>>
      exponent_to_base;
  exponent_to_base.reserve(operands.size());

  for (const scalar_expr& child : operands) {
    if (const power* p = get_if<const power>(child); p != nullptr) {
      if (const integer_constant* exp = get_if<const integer_constant>(p->exponent());
          exp != nullptr) {
        // Map from absolute value of integer exponent to the base, and a bool recording the sign
        // of the exponent.
        if (exp->is_negative()) {
          exponent_to_base[exp->abs()].push_back(constants::one / p->base());
        } else {
          exponent_to_base[*exp].push_back(p->base());
        }
      }
    }
  }

  // We can only group when there are more than two bases sharing the same exponent.
  map_erase_if(exponent_to_base, [](const auto& pair) { return pair.second.size() < 2; });

  // Sort map keys into a deterministic order.
  auto sorted_exponents = transform_map<absl::InlinedVector<integer_constant, 8>>(
      exponent_to_base, [](const auto& pair) { return pair.first; });
  std::sort(sorted_exponents.begin(), sorted_exponents.end());

  // Discard anything in operands that was retained in `exponent_to_base`.
  remove_if(operands, [&exponent_to_base](const scalar_expr& child) {
    // Go back and remove from `operands` everything we will be placing into `grouped_operands`
    // below:
    if (const power* p = get_if<const power>(child); p != nullptr) {
      if (const integer_constant* exp = get_if<const integer_constant>(p->exponent());
          exp != nullptr) {
        // Use abs() here because we keyed the map by absolute exponent above.
        return exponent_to_base.contains(exp->abs());
      }
    }
    return false;
  });

  std::ranges::transform(
      sorted_exponents, std::back_inserter(grouped_operands), [&](const integer_constant& exp) {
        WF_ASSERT(!exp.is_negative());
        const auto& bases = exponent_to_base.at(exp);

        absl::InlinedVector<ir::value_ptr, 4> bases_as_values;
        bases_as_values.reserve(bases.size());
        convert_mul_operands_with_power_expansion(bases, bases_as_values);

        const auto product = create_add_or_mul_with_operands<ir::mul>(bases_as_values);
        return exp.value() <= 16
                   ? exponentiate_by_squaring(product, static_cast<std::uint64_t>(exp.value()))
                   : create_power_call(product, exp);
      });
}

template <typename ContainerIn, typename ContainerOut>
void ir_form_visitor::convert_mul_operands_with_power_expansion(const ContainerIn& expressions,
                                                                ContainerOut& operands_out) {
  for (const scalar_expr& child : expressions) {
    if (const power* p = get_if<const power>(child); p != nullptr) {
      if (const auto maybe_extracted = pow_extract_base_and_integer_exponent(*p);
          maybe_extracted.has_value()) {
        const auto [base, exponent] = *maybe_extracted;
        operands_out.insert(operands_out.cend(), exponent, base);
        continue;  //  skip adding into `operands_out` below
      }
    }
    operands_out.push_back(operator()(child));
  }
}

// Apply exponentiation by squaring to implement a power of an integer.
ir::value_ptr ir_form_visitor::exponentiate_by_squaring(ir::value_ptr base, std::size_t exponent) {
  if (exponent == 0) {
    return operator()(constants::one);
  }
  std::optional<ir::value_ptr> result{};
  for (;;) {
    if (exponent & 1) {
      result = result.has_value() ? push_operation(ir::mul{}, base->numeric_type(), *result, base)
                                  : base;
    }
    exponent /= 2;
    if (exponent == 0) {
      break;
    }
    base = push_operation(ir::mul{}, base->numeric_type(), base, base);
  }
  return result.value();
}

std::optional<std::tuple<ir::value_ptr, std::size_t>>
ir_form_visitor::pow_extract_base_and_integer_exponent(const power& p) {
  constexpr int max_integer_mul_exponent = 16;
  if (const integer_constant* exp_int = get_if<const integer_constant>(p.exponent());
      exp_int != nullptr && exp_int->is_positive()) {
    // Maximum exponent below which we rewrite `pow` as a series of multiplications.
    // Have not experimented with this cutoff much, but on GCC94 and Clang17, using a series of
    // multiplications is still faster even past x^32.
    if (exp_int->value() <= max_integer_mul_exponent) {
      const ir::value_ptr base =
          maybe_cast(operator()(p.base()), numeric_primitive_type::floating_point);
      return std::make_tuple(base, static_cast<std::uint64_t>(exp_int->value()));
    }
  } else if (const rational_constant* exp_rational = get_if<const rational_constant>(p.exponent());
             exp_rational != nullptr && exp_rational->is_positive()) {
    // If the denominator is 1/2 and the exponent is small, it is faster to do power
    // exponentiation followed by sqrt. This is not the case for cbrt, where pow() is the same
    // approximate performance.
    if (exp_rational->denominator() == 2 && exp_rational->numerator() <= max_integer_mul_exponent) {
      const ir::value_ptr base =
          maybe_cast(operator()(p.base()), numeric_primitive_type::floating_point);
      const ir::value_ptr sqrt = push_operation(ir::call_std_function{std_math_function::sqrt},
                                                numeric_primitive_type::floating_point, base);
      return std::make_tuple(sqrt, static_cast<std::uint64_t>(exp_rational->numerator()));
    }
  }
  return std::nullopt;
}

ir::value_ptr ir_form_visitor::create_reciprocal(const ir::value_ptr reciprocal_value) {
  const ir::value_ptr one = operator()(constants::one);
  constexpr auto promoted_type = numeric_primitive_type::floating_point;
  return push_operation(ir::div{}, promoted_type, maybe_cast(one, promoted_type),
                        maybe_cast(reciprocal_value, promoted_type));
}

// `Exponent` may be a scalar_expr, or an integer constant.
template <typename Exponent>
ir::value_ptr ir_form_visitor::create_power_call(ir::value_ptr base, const Exponent& exponent) {
  // TODO: Support (int ** int) powers?
  if (const ir::value_ptr exp = operator()(exponent);
      exp->numeric_type() == numeric_primitive_type::integral) {
    return push_operation(ir::call_std_function{std_math_function::powi},
                          numeric_primitive_type::floating_point, base, exp);
  } else {
    return push_operation(ir::call_std_function{std_math_function::powf},
                          numeric_primitive_type::floating_point, base,
                          maybe_cast(exp, numeric_primitive_type::floating_point));
  }
}

ir::value_ptr ir_form_visitor::operator()(const power& power) {
  // Check if this exponent has a negative coefficient on it:
  if (const auto [exp_coefficient, exp_mul] = as_coeff_and_mul(power.exponent());
      is_negative_number(exp_coefficient)) {
    // Write the power as: 1 / pow(base, -exponent)
    const scalar_expr reciprocal = pow(power.base(), -power.exponent());
    return create_reciprocal(operator()(reciprocal));
  }

  if (const auto maybe_rewrite = pow_extract_base_and_integer_exponent(power);
      maybe_rewrite.has_value()) {
    const auto [base, exponent] = *maybe_rewrite;
    return exponentiate_by_squaring(base, exponent);
  }
  const ir::value_ptr base =
      maybe_cast(operator()(power.base()), numeric_primitive_type::floating_point);
  return create_power_call(base, power.exponent());
}

ir::value_ptr ir_form_visitor::operator()(const rational_constant& r) {
  return push_operation(ir::load{r}, numeric_primitive_type::floating_point);
}

ir::value_ptr ir_form_visitor::operator()(const relational& relational) {
  const ir::value_ptr left = operator()(relational.left());
  const ir::value_ptr right = operator()(relational.right());
  const numeric_primitive_type promoted_type =
      std::max(left->numeric_type(), right->numeric_type());
  return push_operation(ir::compare{relational.operation()}, numeric_primitive_type::boolean,
                        maybe_cast(left, promoted_type), maybe_cast(right, promoted_type));
}

ir::value_ptr ir_form_visitor::operator()(const stop_derivative& nd) {
  // Has no meaning at this step, so strip it:
  return operator()(nd.arg());
}

ir::value_ptr ir_form_visitor::operator()(const substitution&) const {
  throw type_error("Cannot generate code with expressions containing: `{}`",
                   substitution::name_str);
}

ir::value_ptr ir_form_visitor::operator()(const symbolic_constant& constant) {
  return push_operation(ir::load{constant}, numeric_primitive_type::floating_point);
}

ir::value_ptr ir_form_visitor::operator()(const symbolic_function_invocation& invocation) const {
  throw type_error("Cannot generate code with expressions containing `{}`: function = {}",
                   symbolic_function_invocation::name_str, invocation.function().name());
}

ir::value_ptr ir_form_visitor::operator()(const undefined&) const {
  throw type_error("Cannot generate code with expressions containing: {}", undefined::name_str);
}

ir::value_ptr ir_form_visitor::operator()(const unevaluated& u) {
  // Parenthetical only matters in the context of the symbolic tree.
  return operator()(u.contents());
}

ir::value_ptr ir_form_visitor::operator()(const unique_variable& var) const {
  throw type_error(
      "Cannot generate code containing unique variable: {} (Did you intend to substitute something "
      "for this variable?)",
      plain_formatter{}(var));
}

ir::value_ptr ir_form_visitor::operator()(const variable& var) {
  return push_operation(ir::load{var}, numeric_primitive_type::floating_point);
}

template <typename T, typename>
ir::value_ptr ir_form_visitor::operator()(const T& expr) {
  auto& map = cache_.get<T>();
  if (auto it = map.find(expr); it != map.end()) {
    return it->second;
  } else {
    const auto [insertion_it, _] = map.emplace(expr, visit(expr, *this));
    return insertion_it->second;
  }
}

void ir_form_visitor::add_output_value(const output_key& key, const any_expression& expr,
                                       const type_variant& type) {
  WF_FUNCTION_TRACE();
  const any_expression sorted_expr = sorter_.sort_expression(expr);
  const ir::value_ptr output_value = overloaded_visit(
      sorted_expr,
      [&](const scalar_expr& x) {
        WF_ASSERT(std::holds_alternative<scalar_type>(type));
        return maybe_cast(operator()(x), std::get<scalar_type>(type).numeric_type());
      },
      [&](const auto& x) { return operator()(x); });

  push_operation(ir::save{key}, ir::void_type{}, output_value);
}

template <typename OpType, typename Type, typename... Args>
ir::value_ptr ir_form_visitor::push_operation(OpType&& op, Type type, Args&&... args) {
  if constexpr (std::is_same_v<Type, numeric_primitive_type>) {
    return push_operation(std::forward<OpType>(op), scalar_type(type), std::forward<Args>(args)...);
  } else {
    return create_operation(output_graph_.values_, output_block_, std::forward<OpType>(op),
                            std::move(type), std::forward<Args>(args)...);
  }
}

ir::value_ptr ir_form_visitor::maybe_cast(ir::value_ptr input, numeric_primitive_type output_type) {
  if (input->numeric_type() != output_type) {
    if (const auto it = cached_casts_.find(std::make_tuple(input, output_type));
        it != cached_casts_.end()) {
      return it->second;
    } else {
      // Insert a new cast and cache it:
      ir::value_ptr cast = push_operation(ir::cast{output_type}, output_type, input);
      cached_casts_.emplace(std::make_tuple(input, output_type), cast);
      return cast;
    }
  }
  return input;
}

template <typename T, typename X>
X expression_sorter::operator()(const T& concrete, const X& expr) {
  if constexpr (std::is_same_v<T, addition> || std::is_same_v<T, multiplication>) {
    // Copy and sort all children
    auto sorted_children = transform_map<typename T::container_type>(concrete, *this);
    std::sort(sorted_children.begin(), sorted_children.end(),
              [](const scalar_expr& a, const scalar_expr& b) {
                return determine_order(a, b) == relative_order::less_than;
              });
    return make_expr<T>(typename T::no_sort{}, std::move(sorted_children));
  } else if constexpr (!T::is_leaf_node) {
    return concrete.map_children(*this);
  } else {
    return expr;
  }
}

template <typename X, typename>
X expression_sorter::operator()(const X& expr) {
  return cache_.get_or_insert(expr, [this](const X& x) { return visit(x, *this); });
}

any_expression expression_sorter::operator()(const any_expression& expr) {
  return std::visit([this](const auto& inner) -> any_expression { return this->operator()(inner); },
                    expr);
}

template <typename X>
X expression_sorter::sort_expression(const X& expr) {
  WF_FUNCTION_TRACE();
  return operator()(expr);
}
}  // namespace wf
