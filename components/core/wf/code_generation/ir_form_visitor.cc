// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/ir_form_visitor.h"

#include "wf/expression_visitor.h"
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

inline numeric_primitive_type determine_member_type(const custom_type& custom,
                                                    const std::size_t index) {
  const auto sequence = determine_access_sequence(custom, index);
  WF_ASSERT(!sequence.empty());

  const access_variant& last_element = sequence.back();
  return overloaded_visit(
      last_element,
      // Matrices are always float for now:
      [](const matrix_access&) constexpr { return numeric_primitive_type::floating_point; },
      [&](const field_access& access) -> numeric_primitive_type {
        // Retrieve the field from the type:
        const auto accessed_field = access.get_field();
        // Because this is a leaf, it must be a scalar. This assumption may eventually be relaxed.
        const scalar_type* const scalar = std::get_if<scalar_type>(&accessed_field->type());
        WF_ASSERT(
            scalar != nullptr,
            "Field `{}` on type `{}` is not a scalar (when accessing element {} of type `{}`)",
            access.field_name(), access.type().name(), index, custom.name());
        return scalar->numeric_type();
      });
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
            [&](const auto& expected, const auto& actual) -> ir::value_ptr {
              if constexpr (!std::is_same_v<decltype(expected), decltype(actual)>) {
                WF_ASSERT_ALWAYS("Mismatched argument types. Expected: {}, Actual: {}", expected,
                                 actual);
              } else if constexpr (std::is_same_v<decltype(expected), const scalar_type&>) {
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
  if (const auto it = std::find_if(mul_terms.begin(), mul_terms.end(), is_negative_integer);
      it != mul_terms.end()) {
    *it = -*it;
    mul_terms.push_back(constants::negative_one);
  }

  // Convert operands into values, converting some integer powers into multiplications in the
  // process.
  absl::InlinedVector<ir::value_ptr, 16> mul_operands{};
  for (const scalar_expr& child : mul_terms) {
    if (const power* p = get_if<const power>(child); p != nullptr) {
      if (const auto maybe_extracted = pow_extract_base_and_integer_exponent(*p);
          maybe_extracted.has_value()) {
        const auto [base, exponent] = *maybe_extracted;
        mul_operands.insert(mul_operands.cend(), exponent, base);
        continue;  //  skip adding into mul_operands below
      }
    }
    mul_operands.push_back(operator()(child));
  }
  return create_add_or_mul_with_operands<ir::mul>(mul_operands);
}

template <typename T, typename Container>
ir::value_ptr ir_form_visitor::create_add_or_mul_with_operands(Container args) {
  auto promoted_type = numeric_primitive_type::integral;
  for (const ir::value_ptr v : args) {
    promoted_type = std::max(promoted_type, v->numeric_type());
  }
  for (auto& arg : args) {
    arg = maybe_cast(arg, promoted_type);
  }
  return push_operation(T{}, promoted_type, args);
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

ir::value_ptr ir_form_visitor::operator()(const unevaluated& u) {
  // Parenthetical only matters in the context of the symbolic tree.
  return operator()(u.contents());
}

ir::value_ptr ir_form_visitor::operator()(const power& power) {
  // Check if this exponent has a negative coefficient on it:
  if (const auto [exp_coefficient, exp_mul] = as_coeff_and_mul(power.exponent());
      is_negative_number(exp_coefficient)) {
    // Construct the reciprocal version of this power.
    const scalar_expr reciprocal = pow(power.base(), -power.exponent());
    const ir::value_ptr reciprocal_value = operator()(reciprocal);

    // Write the power as: 1 / pow(base, -exponent)
    const ir::value_ptr one = operator()(constants::one);
    constexpr numeric_primitive_type promoted_type = numeric_primitive_type::floating_point;
    return push_operation(ir::div{}, promoted_type, maybe_cast(one, promoted_type),
                          maybe_cast(reciprocal_value, promoted_type));
  }

  if (const auto maybe_rewrite = pow_extract_base_and_integer_exponent(power);
      maybe_rewrite.has_value()) {
    const auto [base, exponent] = *maybe_rewrite;
    return exponentiate_by_squaring(base, exponent);
  }

  // Otherwise do it by calling pow:
  const ir::value_ptr base =
      maybe_cast(operator()(power.base()), numeric_primitive_type::floating_point);

  // TODO: Support (int ** int) powers?
  if (const ir::value_ptr exp = operator()(power.exponent());
      exp->numeric_type() == numeric_primitive_type::integral) {
    return push_operation(ir::call_std_function{std_math_function::powi},
                          numeric_primitive_type::floating_point, base, exp);
  } else {
    return push_operation(ir::call_std_function{std_math_function::powf},
                          numeric_primitive_type::floating_point, base,
                          maybe_cast(exp, numeric_primitive_type::floating_point));
  }
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

ir::value_ptr ir_form_visitor::operator()(const substitution&) const {
  throw type_error("Cannot generate code with expressions containing: {}", substitution::name_str);
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

inline numeric_primitive_type get_variable_type(const variable& var) {
  if (const function_argument_variable* arg =
          std::get_if<function_argument_variable>(&var.identifier());
      arg != nullptr) {
    return arg->primitive_type();
  } else {
    return numeric_primitive_type::floating_point;
  }
}

ir::value_ptr ir_form_visitor::operator()(const variable& var) {
  return push_operation(ir::load{var}, get_variable_type(var));
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
