// Copyright 2024 Gareth Cross
#include "wf/code_generation/ir_form_visitor.h"

#include "wf/common_visitors.h"
#include "wf/expression_visitor.h"

namespace wf {

ir_form_visitor::ir_form_visitor(control_flow_graph& output_graph, operation_term_counts counts)
    : output_graph_(output_graph),
      output_block_(output_graph.first_block()),
      counts_(std::move(counts)) {
  WF_ASSERT_EQUAL(1, output_graph.num_blocks(), "Output graph should only have one block.");
}

ir::value_ptr ir_form_visitor::operator()(const addition& add, const scalar_expr& add_abstract) {
  // For additions, first check if the negated version has already been cached:
  const scalar_expr negative_add = -add_abstract;
  if (const auto it = computed_values_.find(negative_add); it != computed_values_.end()) {
    const auto promoted_type = std::max(it->second->numeric_type(), code_numeric_type::integral);
    const ir::value_ptr negative_one =
        maybe_cast(operator()(constants::negative_one), promoted_type);
    return push_operation(ir::mul{}, promoted_type, it->second, negative_one);
  }
  return convert_addition_or_multiplication(add);
}

ir::value_ptr ir_form_visitor::operator()(const cast_bool& cast) {
  const ir::value_ptr arg = operator()(cast.arg());
  return push_operation(ir::cast{code_numeric_type::integral}, code_numeric_type::integral, arg);
}

ir::value_ptr ir_form_visitor::operator()(const complex_infinity&) const {
  throw type_error("Cannot generate code for complex infinity.");
}

ir::value_ptr ir_form_visitor::operator()(const compound_expression_element& el) {
  const ir::value_ptr compound_val = operator()(el.provenance());
  return overloaded_visit(
      compound_val->type(),
      [&](ir::void_type) -> ir::value_ptr {
        WF_ASSERT_ALWAYS("Compount expression cannot have void type. name = {}, index = {}",
                         compound_val->name(), el.index());
      },
      [&](scalar_type) { return compound_val; },
      [&](const auto&) {
        return push_operation(ir::get{el.index()}, code_numeric_type::floating_point, compound_val);
      });
}

ir::value_ptr ir_form_visitor::operator()(const conditional& cond) {
  const ir::value_ptr condition = operator()(cond.condition());
  const ir::value_ptr if_branch = operator()(cond.if_branch());
  const ir::value_ptr else_branch = operator()(cond.else_branch());

  const code_numeric_type promoted_type =
      std::max(if_branch->numeric_type(), else_branch->numeric_type());
  return push_operation(ir::cond{}, promoted_type, condition, maybe_cast(if_branch, promoted_type),
                        maybe_cast(else_branch, promoted_type));
}

ir::value_ptr ir_form_visitor::operator()(const custom_type_argument& arg) {
  return push_operation(ir::load{arg}, arg.type());
}

ir::value_ptr ir_form_visitor::operator()(const custom_type_construction& construct) {
  ir::value::operands_container operands{};
  operands.reserve(construct.size());

  // TODO: This needs a handler for nested custom types, I think?
  iterate_custom_type_fields(
      construct.type(),
      make_overloaded(
          [&](const std::size_t index, const scalar_type s) {
            operands.push_back(maybe_cast(operator()(construct.at(index)), s.numeric_type()));
          },
          [&](const std::size_t index, const matrix_type& m) {
            for (std::size_t i = 0; i < m.size(); ++i) {
              const ir::value_ptr mat_element = operator()(construct.at(index + i));
              operands.push_back(maybe_cast(mat_element, code_numeric_type::floating_point));
            }
          }));

  return push_operation(ir::construct{construct.type()}, construct.type(), std::move(operands));
}

ir::value_ptr ir_form_visitor::operator()(const external_function_invocation& invoke) {
  const external_function& f = invoke.function();

  // Generate values for every argument. Insert casts for scalars if required.
  auto operands = transform_enumerate_map<ir::value::operands_container>(
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
        return push_operation(ir::call_external_function{f}, return_type, std::move(operands));
      },
      f.return_type());
}

ir::value_ptr ir_form_visitor::operator()(const derivative&) const {
  throw type_error("Cannot generate code for expressions containing `{}`.", derivative::name_str);
}

ir::value_ptr ir_form_visitor::operator()(const float_constant& f) {
  return push_operation(ir::load{f}, code_numeric_type::floating_point);
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
    case built_in_function::ln:
      return std_math_function::log;
    case built_in_function::abs:
      return std_math_function::abs;
    case built_in_function::signum:
      return std_math_function::signum;
    case built_in_function::arctan2:
      return std_math_function::atan2;
  }
  WF_ASSERT_ALWAYS("Invalid enum value: {}", string_from_built_in_function(name));
}

ir::value_ptr ir_form_visitor::operator()(const function& func) {
  const std_math_function enum_value = std_math_function_from_built_in(func.enum_value());
  // TODO: Special case for `abs` and `signum` here.
  return push_operation(
      ir::call_std_function{enum_value}, code_numeric_type::floating_point,
      transform_map<ir::value::operands_container>(
          func, [this](const scalar_expr& expr) { return this->operator()(expr); }));
}

ir::value_ptr ir_form_visitor::operator()(const integer_constant& i) {
  return push_operation(ir::load{i}, code_numeric_type::integral);
}

ir::value_ptr ir_form_visitor::operator()(const multiplication& mul,
                                          const scalar_expr& mul_abstract) {
  if (const auto [coeff, multiplicand] = as_coeff_and_mul(mul_abstract); is_negative_one(coeff)) {
    // If the coefficient out front is -1, compute the multiplied expression and then negate it.
    const ir::value_ptr multiplicand_value = operator()(multiplicand);
    return push_operation(ir::neg{}, multiplicand_value->numeric_type(), multiplicand_value);
  }
  return convert_addition_or_multiplication(mul);
}

// Apply exponentiation by squaring to implement a power of an integer.
ir::value_ptr ir_form_visitor::exponentiate_by_squaring(ir::value_ptr base, std::size_t exponent) {
  if (exponent == 0) {
    return operator()(constants::one);
  }
  // TODO: Somewhat lazy way of handling the first iteration - use an empty optional.
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

ir::value_ptr ir_form_visitor::operator()(const power& power) {
  const ir::value_ptr base =
      maybe_cast(operator()(power.base()), code_numeric_type::floating_point);

  // Check if this exponent has a negative coefficient on it:
  if (const auto [exp_coefficient, exp_mul] = as_coeff_and_mul(power.exponent());
      is_negative_number(exp_coefficient)) {
    // Construct the reciprocal version of this power.
    const scalar_expr reciprocal = pow(power.base(), -power.exponent());
    const ir::value_ptr reciprocal_value = operator()(reciprocal);

    // Write the power as: 1 / pow(base, -exponent)
    const ir::value_ptr one = operator()(constants::one);
    constexpr code_numeric_type promoted_type = code_numeric_type::floating_point;
    return push_operation(ir::div{}, promoted_type, maybe_cast(one, promoted_type),
                          maybe_cast(reciprocal_value, promoted_type));
  }

  constexpr int max_integer_mul_exponent = 16;
  if (const integer_constant* exp_int = cast_ptr<const integer_constant>(power.exponent());
      exp_int != nullptr) {
    WF_ASSERT_GREATER_OR_EQ(exp_int->get_value(), 0, "Negative exponents were handled above");
    // Maximum exponent below which we rewrite `pow` as a series of multiplications.
    // Have not experimented with this cutoff much, but on GCC94 and Clang17, using a series of
    // multiplications is still faster even past x^32.
    if (exp_int->get_value() <= max_integer_mul_exponent) {
      return exponentiate_by_squaring(base, static_cast<std::uint64_t>(exp_int->get_value()));
    } else {
      // Just call power variant with integer exponent:
      return push_operation(ir::call_std_function{std_math_function::powi},
                            code_numeric_type::floating_point, base, operator()(power.exponent()));
    }
  } else if (const rational_constant* exp_rational =
                 cast_ptr<const rational_constant>(power.exponent());
             exp_rational != nullptr) {
    WF_ASSERT_GREATER_OR_EQ(exp_rational->numerator(), 0, "rational = {}", *exp_rational);

    // If the denominator is 1/2 and the exponent is small, it is faster to do power
    // exponentiation followed by sqrt. This is not the case for cbrt, where pow() is the same
    // approximate performance.
    if (exp_rational->denominator() == 2 && exp_rational->numerator() <= max_integer_mul_exponent) {
      const ir::value_ptr sqrt = push_operation(ir::call_std_function{std_math_function::sqrt},
                                                code_numeric_type::floating_point, base);
      return exponentiate_by_squaring(sqrt, static_cast<std::uint64_t>(exp_rational->numerator()));
    }
  }

  // TODO: Support (int ** int) powers?
  const ir::value_ptr exponent = operator()(power.exponent());
  return push_operation(ir::call_std_function{std_math_function::powf},
                        code_numeric_type::floating_point, base,
                        maybe_cast(exponent, code_numeric_type::floating_point));
}

ir::value_ptr ir_form_visitor::operator()(const rational_constant& r) {
  return push_operation(ir::load{r}, code_numeric_type::floating_point);
}

ir::value_ptr ir_form_visitor::operator()(const relational& relational) {
  const ir::value_ptr left = operator()(relational.left());
  const ir::value_ptr right = operator()(relational.right());
  const code_numeric_type promoted_type = std::max(left->numeric_type(), right->numeric_type());
  return push_operation(ir::compare{relational.operation()}, code_numeric_type::boolean,
                        maybe_cast(left, promoted_type), maybe_cast(right, promoted_type));
}

static code_numeric_type numeric_type_from_constant(const symbolic_constant& c) {
  switch (c.name()) {
    case symbolic_constant_enum::euler:
    case symbolic_constant_enum::pi:
      return code_numeric_type::floating_point;
    case symbolic_constant_enum::boolean_true:
    case symbolic_constant_enum::boolean_false:
      return code_numeric_type::boolean;
  }
  WF_ASSERT_ALWAYS("Unhandled symbolic constant: {}", string_from_symbolic_constant(c.name()));
}

ir::value_ptr ir_form_visitor::operator()(const symbolic_constant& c) {
  return push_operation(ir::load{c}, numeric_type_from_constant(c));
}

ir::value_ptr ir_form_visitor::operator()(const undefined&) const {
  throw type_error("Cannot generate code with expressions containing {}", undefined::name_str);
}

ir::value_ptr ir_form_visitor::operator()(const variable& var) {
  return push_operation(ir::load{var}, code_numeric_type::floating_point);
}

ir::value_ptr ir_form_visitor::operator()(const compound_expr& expr) {
  if (const auto it = computed_compound_values_.find(expr); it != computed_compound_values_.end()) {
    return it->second;
  }
  ir::value_ptr val = visit(expr, *this);
  computed_compound_values_.emplace(expr, val);
  return val;
}

ir::value_ptr ir_form_visitor::operator()(const matrix_expr& m) {
  WF_ASSERT(m.is_type<matrix>(), "TODO: Generalize when we add more matrix expressions.");
  const matrix_type mat_type{m.rows(), m.cols()};
  return push_operation(
      ir::construct(mat_type), mat_type,
      transform_map<ir::value::operands_container>(m.as_matrix(), [this](const scalar_expr& arg) {
        return maybe_cast(this->operator()(arg), code_numeric_type::floating_point);
      }));
}

// Check if a value has been computed. If not, convert it and return the result.
ir::value_ptr ir_form_visitor::operator()(const scalar_expr& expr) {
  if (const auto it = computed_values_.find(expr); it != computed_values_.end()) {
    return it->second;
  }
  ir::value_ptr val = visit(expr, *this);
  computed_values_.emplace(expr, val);
  return val;
}

template <typename OpType, typename Type, typename... Args>
ir::value_ptr ir_form_visitor::push_operation(OpType&& op, Type type, Args&&... args) {
  if constexpr (std::is_same_v<Type, code_numeric_type>) {
    return create_operation(output_graph_.values_, output_block_, std::forward<OpType>(op),
                            scalar_type(type), std::forward<Args>(args)...);
  } else {
    return create_operation(output_graph_.values_, output_block_, std::forward<OpType>(op),
                            std::move(type), std::forward<Args>(args)...);
  }
}

// Handler for additions and multiplications:
template <typename T>
ir::value_ptr ir_form_visitor::convert_addition_or_multiplication(const T& op) {
  // Sort first by frequency of occurrence, then in ambiguous cases by expression order:
  multiplication::container_type expressions{op.begin(), op.end()};
  std::sort(expressions.begin(), expressions.end(),
            [&](const scalar_expr& a, const scalar_expr& b) {
              const operation_term_counts::count_container& count_table = get_count_table<T>();
              const std::size_t count_a = count_table.at(a);
              const std::size_t count_b = count_table.at(b);
              if (count_a > count_b) {
                return true;
              } else if (count_a < count_b) {
                return false;
              } else {
                return expression_order_struct{}(a, b);
              }
            });

  // first recursively transform all the inputs
  const std::vector<ir::value_ptr> args = transform_map<std::vector>(expressions, *this);

  code_numeric_type promoted_type = code_numeric_type::integral;
  for (const ir::value_ptr v : args) {
    promoted_type = std::max(promoted_type, v->numeric_type());
  }

  // then create multiplications or adds for this expression:
  ir::value_ptr prev_result = maybe_cast(args[0], promoted_type);
  for (std::size_t i = 1; i < args.size(); ++i) {
    if constexpr (std::is_same_v<T, multiplication>) {
      prev_result =
          push_operation(ir::mul{}, promoted_type, prev_result, maybe_cast(args[i], promoted_type));
    } else {
      prev_result =
          push_operation(ir::add{}, promoted_type, prev_result, maybe_cast(args[i], promoted_type));
    }
  }
  return prev_result;
}

ir::value_ptr ir_form_visitor::maybe_cast(ir::value_ptr input, code_numeric_type output_type) {
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
  } else {
    return input;
  }
}

template <typename T>
constexpr const operation_term_counts::count_container& ir_form_visitor::get_count_table()
    const noexcept {
  if constexpr (std::is_same_v<T, multiplication>) {
    return counts_.muls;
  } else {
    return counts_.adds;
  }
}

mul_add_count_visitor::mul_add_count_visitor() {
  adds_.reserve(100);
  muls_.reserve(100);
  visited_.reserve(100);
}

void mul_add_count_visitor::count_group_expressions(const expression_group& group) {
  for (const scalar_expr& expr : group.expressions) {
    visit(expr, *this);
  }
}

void mul_add_count_visitor::operator()(const scalar_expr& x) { return visit(x, *this); }
void mul_add_count_visitor::operator()(const matrix_expr& m) {
  for (const scalar_expr& x : m.as_matrix()) {
    visit(x, *this);
  }
}

void mul_add_count_visitor::operator()(const compound_expr& x) {
  auto visitor = make_overloaded(
      [this](const external_function_invocation& invoke) {
        for (const auto& arg : invoke) {
          std::visit(*this, arg);
        }
      },
      [this](const custom_type_construction& construct) {
        for (const auto& arg : construct) {
          operator()(arg);
        }
      },
      [](const custom_type_argument&) constexpr {});
  return visit(x, visitor);
}

template <typename T>
void mul_add_count_visitor::operator()(const T& concrete) {
  if constexpr (!T::is_leaf_node) {
    // If this is an add or a mul, record the incoming edges to children.
    if constexpr (std::is_same_v<T, addition>) {
      for (const scalar_expr& child : concrete) {
        adds_[child]++;
      }
    } else if constexpr (std::is_same_v<T, multiplication>) {
      for (const scalar_expr& child : concrete) {
        muls_[child]++;
      }
    }

    if constexpr (std::is_same_v<T, compound_expression_element>) {
      operator()(concrete.provenance());
    } else {
      // Recurse:
      for (const scalar_expr& child : concrete) {
        if (!visited_.count(child)) {
          visited_.insert(child);
          visit(child, *this);
        }
      }
    }
  }
}

operation_term_counts mul_add_count_visitor::take_counts() && {
  operation_term_counts counts{};
  counts.muls = std::move(muls_);
  counts.adds = std::move(adds_);
  return counts;
}

}  // namespace wf
