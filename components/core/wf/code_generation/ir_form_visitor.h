// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <unordered_map>

#include "wf/code_generation/control_flow_graph.h"
#include "wf/code_generation/ir_block.h"
#include "wf/code_generation/ir_consumer_vector.h"
#include "wf/expression.h"
#include "wf/expression_cache.h"

namespace wf {
class control_flow_graph;  // Forward declare.

// Sort all additions and multiplications into canonical order.
// We do this so that the code-generation is invariant to changes in our hash functions - otherwise
// changing hash functions can cause unnecessary downstream churn.
class expression_sorter {
 public:
  template <typename T, typename X>
  X operator()(const T& concrete, const X& expr);

  // Accept abstract expression types.
  template <typename X, typename = enable_if_inherits_expression_base_t<X>>
  X operator()(const X& expr);

  any_expression operator()(const any_expression& expr);

  // Sort an expression. `X` may be scalar_expr, matrix_expr, compound_expr, etc.
  template <typename X>
  X sort_expression(const X& expr);

 private:
  expression_cache cache_;
};

// Visitor for converting an expression tree into intermediate representation.
// This visitor accepts expression types, and returns IR values. While doing the conversion, we look
// for opportunities to optimize. For the most part, this consists of identifying duplicate
// expressions and computing them only once in the IR representation.
class ir_form_visitor {
 public:
  // Construct with output graph.
  explicit ir_form_visitor(control_flow_graph& output_graph);

  ir::value_ptr operator()(const addition& add);
  ir::value_ptr operator()(const boolean_constant& b);
  ir::value_ptr operator()(const complex_infinity&) const;
  ir::value_ptr operator()(const compound_expression_element& el);
  ir::value_ptr operator()(const conditional& cond);
  ir::value_ptr operator()(const custom_type_argument& arg);
  ir::value_ptr operator()(const custom_type_construction& construct);
  ir::value_ptr operator()(const external_function_invocation& invoke);
  ir::value_ptr operator()(const derivative&) const;
  ir::value_ptr operator()(const float_constant& f);
  ir::value_ptr operator()(const function_argument_variable& var);
  ir::value_ptr operator()(const built_in_function_invocation& func);
  ir::value_ptr operator()(const imaginary_unit&) const;
  ir::value_ptr operator()(const integer_constant& i);
  ir::value_ptr operator()(const iverson_bracket& bracket);
  ir::value_ptr operator()(const matrix& mat);
  ir::value_ptr operator()(const multiplication& mul);
  ir::value_ptr operator()(const power& power);
  ir::value_ptr operator()(const rational_constant& r);
  ir::value_ptr operator()(const relational& relational);
  ir::value_ptr operator()(const stop_derivative& nd);
  ir::value_ptr operator()(const substitution& subs) const;
  ir::value_ptr operator()(const symbolic_constant& constant);
  ir::value_ptr operator()(const symbolic_function_invocation& invocation) const;
  ir::value_ptr operator()(const undefined&) const;
  ir::value_ptr operator()(const unevaluated& u);
  ir::value_ptr operator()(const unique_variable& var) const;
  ir::value_ptr operator()(const variable& var);

  // Apply to any expression.
  template <typename T, typename = enable_if_inherits_expression_base_t<T>>
  ir::value_ptr operator()(const T& expr);

  // Add a new output value to the function. This will recursively traverse all the required
  // sub-expressions and add them to graph of values. Values that have already been computed and
  // cached will be reused.
  void add_output_value(const output_key& key, const any_expression& expr,
                        const type_variant& type);

 private:
  template <typename OpType, typename Type, typename... Args>
  ir::value_ptr push_operation(OpType&& op, Type type, Args&&... args);

  // Convert sequence of expressions into `ir::add` or `ir::mul`.
  template <typename T, typename Container>
  ir::value_ptr create_add_or_mul_with_operands(Container args);

  // Wrap `input` in a cast operation if it does have type `output_type`.
  ir::value_ptr maybe_cast(ir::value_ptr input, numeric_primitive_type output_type);

  // Apply exponentiation by squaring to implement a power of an integer.
  ir::value_ptr exponentiate_by_squaring(ir::value_ptr base, std::size_t exponent);

  // Check if a power should be converted to a series of multiplications.
  // If this is the case, return the base (which is to be multiplied) and the integral power.
  std::optional<std::tuple<ir::value_ptr, std::size_t>> pow_extract_base_and_integer_exponent(
      const power& p);

  // The IR we are writing to as we convert.
  control_flow_graph& output_graph_;
  ir::block_ptr output_block_;

  template <typename T>
  using cache_map_type =
      std::unordered_map<T, ir::value_ptr, hash_struct<T>, is_identical_struct<T>>;

  // Map of expression -> IR value. We catch duplicates as we create the IR code, which greatly
  // speeds up manipulation of the code later.
  wf::expression_map_tuple<cache_map_type> cache_;

  // Sorter is stored in this converter so it can cache over multiple output expressions.
  expression_sorter sorter_{};

  // Hash tuple of [value, type]
  struct hash_value_and_type {
    std::size_t operator()(const std::tuple<ir::value_ptr, numeric_primitive_type>& pair) const {
      const auto [value, type] = pair;
      return hash_combine(value->name(), static_cast<std::size_t>(type));
    }
  };

  // Test tuple of [value, type] for equality.
  struct value_and_type_eq {
    bool operator()(const std::tuple<ir::value_ptr, numeric_primitive_type>& a,
                    const std::tuple<ir::value_ptr, numeric_primitive_type>& b) const {
      return std::get<0>(a)->name() == std::get<0>(b)->name() && std::get<1>(a) == std::get<1>(b);
    }
  };

  // We maintain a separate cache of casts. Casts don't appear in the math tree, so we cannot
  // store them in as `scalar_expr` in the computed_values_ map. This is a mapping from [value,
  // type] to the cast (if it exists already).
  std::unordered_map<std::tuple<ir::value_ptr, numeric_primitive_type>, ir::value_ptr,
                     hash_value_and_type, value_and_type_eq>
      cached_casts_;
};

}  // namespace wf
