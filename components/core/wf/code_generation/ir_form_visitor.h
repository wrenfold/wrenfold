// Copyright 2024 Gareth Cross
#pragma once
#include <unordered_map>
#include <unordered_set>

#include "wf/code_generation/flat_ir.h"
#include "wf/code_generation/ir_block.h"
#include "wf/expression.h"

namespace wf {
class control_flow_graph;  // Forward declare.

struct operation_term_counts {
  // The key in this table is a sub-expression (a term) in a multiplication or addition.
  // The value is the # of times that term appears in a unique addition or multiplication sequence.
  using count_container = std::unordered_map<scalar_expr, std::size_t, hash_struct<scalar_expr>,
                                             is_identical_struct<scalar_expr>>;

  count_container muls;
  count_container adds;
};

// Visitor for converting an expression tree into intermediate representation.
// This visitor accepts expression types, and returns IR values.
class ir_form_visitor {
 public:
  // Construct with output graph.
  ir_form_visitor(control_flow_graph& output_graph, operation_term_counts counts);

  ir::value_ptr operator()(const addition& add, const scalar_expr& add_abstract);
  ir::value_ptr operator()(const cast_bool& cast);
  ir::value_ptr operator()(const complex_infinity&) const;
  ir::value_ptr operator()(const compound_expression_element& el);
  ir::value_ptr operator()(const conditional& cond);
  ir::value_ptr operator()(const custom_type_argument& arg);
  ir::value_ptr operator()(const custom_type_construction& construct);
  ir::value_ptr operator()(const external_function_invocation& invoke);
  ir::value_ptr operator()(const derivative&) const;
  ir::value_ptr operator()(const float_constant& f);
  ir::value_ptr operator()(const function& func);
  ir::value_ptr operator()(const integer_constant& i);
  ir::value_ptr operator()(const multiplication& mul, const scalar_expr& mul_abstract);
  ir::value_ptr operator()(const power& power);
  ir::value_ptr operator()(const rational_constant& r);
  ir::value_ptr operator()(const relational& relational);
  ir::value_ptr operator()(const symbolic_constant& constant);
  ir::value_ptr operator()(const undefined&) const;
  ir::value_ptr operator()(const variable& var);

  ir::value_ptr operator()(const compound_expr& expr);
  ir::value_ptr operator()(const matrix_expr& m);
  ir::value_ptr operator()(const scalar_expr& expr);

  // Compute the value for the specified expression. If required, cast it to the output type.
  ir::value_ptr apply_output_value(const scalar_expr& expr,
                                   const code_numeric_type desired_output_type) {
    return maybe_cast(operator()(expr), desired_output_type);
  }

 private:
  template <typename OpType, typename Type, typename... Args>
  ir::value_ptr push_operation(OpType&& op, Type type, Args&&... args);

  template <typename T>
  ir::value_ptr convert_addition_or_multiplication(const T& op);

  // Wrap `input` in a cast operation if it does have type `output_type`.
  ir::value_ptr maybe_cast(ir::value_ptr input, code_numeric_type output_type);

  template <typename T>
  constexpr const operation_term_counts::count_container& get_count_table() const noexcept;

  // Apply exponentiation by squaring to implement a power of an integer.
  ir::value_ptr exponentiate_by_squaring(ir::value_ptr base, uint64_t exponent);

  // The IR we are writing to as we convert.
  control_flow_graph& output_graph_;
  ir::block_ptr output_block_;

  // Map of expression -> IR value. We catch duplicates as we create the IR code, which greatly
  // speeds up manipulation of the code later.
  std::unordered_map<scalar_expr, ir::value_ptr, hash_struct<scalar_expr>,
                     is_identical_struct<scalar_expr>>
      computed_values_;

  // Cache map for compound values.
  std::unordered_map<compound_expr, ir::value_ptr, hash_struct<compound_expr>,
                     is_identical_struct<compound_expr>>
      computed_compound_values_;

  // Hash tuple of [value, type]
  struct hash_value_and_type {
    std::size_t operator()(const std::tuple<ir::value_ptr, code_numeric_type>& pair) const {
      const auto [value, type] = pair;
      return hash_combine(value->name(), static_cast<std::size_t>(type));
    }
  };

  // Test tuple of [value, type] for equality.
  struct value_and_type_eq {
    bool operator()(const std::tuple<ir::value_ptr, code_numeric_type>& a,
                    const std::tuple<ir::value_ptr, code_numeric_type>& b) const {
      return std::get<0>(a)->name() == std::get<0>(b)->name() && std::get<1>(a) == std::get<1>(b);
    }
  };

  // We maintain a separate cache of casts. Casts don't appear in the math tree, so we cannot
  // store them in as `scalar_expr` in the computed_values_ map. This is a mapping from [value,
  // type] to the cast (if it exists already).
  std::unordered_map<std::tuple<ir::value_ptr, code_numeric_type>, ir::value_ptr,
                     hash_value_and_type, value_and_type_eq>
      cached_casts_;

  operation_term_counts counts_;
};

// Count incidences of unique multiplications and additions.
// We use the count during conversion to IR in order to select the order of operations for additions
// and multiplications.
struct mul_add_count_visitor {
  mul_add_count_visitor();

  // Visit every expression in the provided expression group.
  void count_group_expressions(const expression_group& group);

  void operator()(const compound_expr& x);
  void operator()(const scalar_expr& x);
  void operator()(const matrix_expr& m);

  template <typename T>
  void operator()(const T& concrete);

  // Return final counts of how many times each term appears in a unique addition
  // or multiplication.
  operation_term_counts take_counts() &&;

 private:
  // A map of all expressions that appear as terms in an addition or multiplication.
  // The key is the term, and the value is a count of inbound edges.
  operation_term_counts::count_container adds_{};
  operation_term_counts::count_container muls_{};

  // Track which nodes we have already visited, so that we do not double count repeated operations.
  std::unordered_set<scalar_expr, hash_struct<scalar_expr>, is_identical_struct<scalar_expr>>
      visited_;
};

}  // namespace wf
