// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/control_flow_graph.h"

#include <algorithm>
#include <queue>
#include <unordered_set>
#include <vector>

#include "wf/code_generation/factorizer.h"
#include "wf/code_generation/ir_block.h"
#include "wf/code_generation/ir_control_flow_converter.h"
#include "wf/code_generation/ir_form_visitor.h"
#include "wf/code_generation/ir_types.h"
#include "wf/code_generation/ir_value.h"
#include "wf/common_visitors.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"
#include "wf/utility/assertions.h"
#include "wf/utility/hashing.h"
#include "wf/utility/scoped_trace.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/flat_hash_map.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {
namespace ir {

template <typename Container>
static void format_operands(std::string& output, const Container& operands,
                            const std::size_t width) {
  for (auto it = operands.begin(); it != operands.end(); ++it) {
    const ir::value_ptr& val = *it;
    fmt::format_to(std::back_inserter(output), "v{:0>{}}", val->name(), width);
    if (std::next(it) != operands.end()) {
      output += ", ";
    }
  }
}

template <typename T>
struct format_op_args_struct {
  template <typename Container>
  void operator()(std::string& output, const T&, const Container& operands,
                  const std::size_t width) {
    format_operands(output, operands, width);
  }
};

template <>
struct format_op_args_struct<ir::call_external_function> {
  template <typename Container>
  void operator()(std::string& output, const ir::call_external_function& func,
                  const Container& operands, const std::size_t width) {
    output += func.function().name();
    if (!operands.empty()) {
      output += ", ";
      format_operands(output, operands, width);
    }
  }
};

template <>
struct format_op_args_struct<ir::construct> {
  void operator()(std::string& output, const matrix_type& m) const {
    fmt::format_to(std::back_inserter(output), "matrix<{}, {}>", m.rows(), m.cols());
  }

  void operator()(std::string& output, const custom_type& c) const { output += c.name(); }

  template <typename Container>
  void operator()(std::string& output, const ir::construct& construct, const Container& operands,
                  const std::size_t width) {
    std::visit([&](const auto& type) { operator()(output, type); }, construct.type());
    if (!operands.empty()) {
      output += ", ";
      format_operands(output, operands, width);
    }
  }
};

template <>
struct format_op_args_struct<ir::get> {
  template <typename Container>
  void operator()(std::string& output, const ir::get& get, const Container& operands,
                  const std::size_t width) {
    format_operands(output, operands, width);
    fmt::format_to(std::back_inserter(output), ", {}", get.index());
  }
};

template <>
struct format_op_args_struct<ir::load> {
  template <typename Container>
  void operator()(std::string& output, const ir::load& load, const Container&, const std::size_t) {
    plain_formatter formatter{};
    std::visit(formatter, load.variant());
    output += formatter.take_output();
  }
};

template <>
struct format_op_args_struct<ir::output_required> {
  template <typename Container>
  void operator()(std::string& output, const ir::output_required& oreq, const Container&,
                  const std::size_t) {
    fmt::format_to(std::back_inserter(output), "{}", oreq.name());
  }
};

template <typename Container>
void format_op_args(std::string& output, const ir::operation& op, const Container& operands,
                    const std::size_t width) {
  std::visit(
      [&](const auto& op_concrete) {
        using T = std::decay_t<decltype(op_concrete)>;
        format_op_args_struct<T>{}(output, op_concrete, operands, width);
      },
      op);
}

}  // namespace ir

// Test two values for equality. The operation must be the same type, and the operands must be
// identical. This does not recursively test the operands for equality - the pointer themselves
// must match.
struct value_equality_struct {
  bool operator()(const ir::value_ptr& a, const ir::value_ptr& b) const {
    if (const bool ops_match = are_identical(a->value_op(), b->value_op()); !ops_match) {
      return false;
    }
    return a->operands_match(b);
  }
};

control_flow_graph::control_flow_graph(const std::vector<expression_group>& groups,
                                       const std::optional<optimization_params>& params) {
  WF_FUNCTION_TRACE();
  blocks_.push_back(std::make_unique<ir::block>(0));

  ir_form_visitor visitor{*this};
  for (const expression_group& group : groups) {
    // Transform expressions into Values
    auto group_values = transform_map<std::vector>(group.expressions, [&](const scalar_expr& expr) {
      // TODO: Allow returning other types - derive numeric type from the group.
      return visitor.apply_output_value(expr, code_numeric_type::floating_point);
    });

    // Then create a sink to consume these values, the `save` operation is the sink:
    create_operation(values_, first_block(), ir::save{group.key}, ir::void_type{},
                     std::move(group_values));
  }

#ifdef WF_DEBUG
  assert_invariants();
#endif

  if (params) {
    apply_simplifications(*params);
  }
}

void control_flow_graph::apply_simplifications(const optimization_params& p) {
  WF_FUNCTION_TRACE();

  eliminate_duplicates();
  factorize_sums(p.factorization_passes);

  if (p.binarize_operations) {
    for (const auto& block : blocks_) {
      binarize_operations<ir::muln, ir::mul>(block.get());
      binarize_operations<ir::addn, ir::add>(block.get());
    }
#ifdef WF_DEBUG
    assert_invariants();
#endif

    eliminate_duplicates();
  }

  for (const auto& block : blocks_) {
    insert_negations(block.get());
  }
  eliminate_duplicates();
}

// TODO: Instead of specifying fixed passes, just put additions in a queue and run until completion.
// `num_passes` is a hack for now to get this merged.
void control_flow_graph::factorize_sums(const std::size_t num_passes) {
  for (std::size_t pass = 0; pass < num_passes; ++pass) {
    for (const auto& block : blocks_) {
      factorize_sums_in_block(block.get());
      merge_multiplications_in_block(block.get());
    }
    eliminate_needless_copies();
    eliminate_duplicates();
  }
#ifdef WF_DEBUG
  assert_invariants();
#endif
}

control_flow_graph control_flow_graph::convert_conditionals_to_control_flow() && {
  WF_FUNCTION_TRACE();
  return ir_control_flow_converter{std::move(*this)}.convert();
}

// A hash-set of values:
using value_table =
    std::unordered_set<ir::value_ptr, hash_struct<ir::value_ptr>, value_equality_struct>;

// Traverse all the values in `values` and eliminate duplicates.
inline void local_value_numbering(const absl::Span<const ir::value_ptr>& values,
                                  value_table& table) {
  WF_FUNCTION_TRACE();
  for (const ir::value_ptr val : values) {
    // Then see if this operation already exists in the map:
    if (auto [it, was_inserted] = table.insert(val); !was_inserted) {
      // Propagate the copy:
      if (val->is_consumed_by_phi()) {
        // If this value feeds a phi function, we need to keep it, but turn it into a copy:
        val->set_operation(ir::copy{}, (*it)->type(), *it);
      } else {
        val->replace_with(*it);
      }
    }
  }
}

inline bool remove_if_unused(const ir::value_ptr v) {
  if (v->is_unused()) {
    v->remove();
    return true;
  }
  return false;
}

// ReSharper disable once CppMemberFunctionMayBeConst
void control_flow_graph::eliminate_duplicates() {
  WF_FUNCTION_TRACE();

  value_table table{};
  table.reserve(values_.size());
  for (const auto& block : blocks_) {
    table.clear();
    std::vector<ir::value_ptr> operations = block->operations();
    local_value_numbering(operations, table);
    reverse_remove_if(operations, &remove_if_unused);
    block->set_operations(std::move(operations));
  }
}

// ReSharper disable once CppMemberFunctionMayBeConst
void control_flow_graph::eliminate_needless_copies() {
  WF_FUNCTION_TRACE();
  for (const auto& block : blocks_) {
    auto operations = block->operations();
    remove_if(operations, [&](const ir::value_ptr v) {
      // A copy is useless if we are duplicating a value in our current block:
      if (v->is_op<ir::copy>() && v->first_operand()->parent() == v->parent()) {
        v->replace_with(v->first_operand());
        v->remove();
        return true;
      }
      return false;
    });
    block->set_operations(std::move(operations));
  }
}

static void topological_sort_values(const ir::const_block_ptr block,
                                    std::vector<ir::value_ptr>& operations) {
  WF_FUNCTION_TRACE();
  const std::size_t initial_size = operations.size();

  // We will de-queue values in increasing order of their names.
  struct reverse_order_values_by_name {
    bool operator()(const ir::const_value_ptr a, const ir::const_value_ptr b) const noexcept {
      return a->name() > b->name();
    }
  };

  // Queue every value that does not have an operand:
  std::priority_queue<ir::value_ptr, std::deque<ir::value_ptr>, reverse_order_values_by_name>
      queue{};
  for (const ir::value_ptr val : operations) {
    if (const bool no_inputs_in_block = none_of(
            val->operands(), [&](const ir::const_value_ptr v) { return v->parent() == block; });
        no_inputs_in_block) {
      queue.push(val);
    }
  }

  // TODO: replace this with a flat vector mapping from ID -> visited
  absl::flat_hash_set<ir::const_value_ptr> visited{};
  visited.reserve(block->size());

  operations.clear();
  while (!queue.empty()) {
    const ir::value_ptr top = queue.top();
    queue.pop();

    if (const auto [_, was_inserted] = visited.insert(top); !was_inserted) {
      continue;
    }

    operations.push_back(top);

    for (const ir::value_ptr c : top->consumers()) {
      if (c->parent() != block) {
        continue;
      }
      if (const bool all_operands_visited = all_of(
              c->operands(), [&](const ir::const_value_ptr v) { return visited.contains(v); });
          all_operands_visited) {
        queue.push(c);
      }
    }
  }

  WF_ASSERT_EQ(initial_size, operations.size(), "operations: [{}]", fmt::join(operations, ", "));
}

using value_pair = std::tuple<ir::value_ptr, ir::value_ptr>;

inline auto make_value_pair(ir::value_ptr a, ir::value_ptr b) {
  if (a->name() < b->name()) {
    return std::make_tuple(a, b);
  } else {
    return std::make_tuple(b, a);
  }
}

template <>
struct hash_struct<value_pair> {
  constexpr std::size_t operator()(const value_pair& p) const noexcept {
    return static_cast<std::size_t>(std::get<0>(p)->name()) << 32 |
           static_cast<std::size_t>(std::get<1>(p)->name());
  }
};

// Order pairs by score then by their names.
// We reverse the order of names to that lower names are taken first when finding the max element.
struct order_value_pair_by_occurrence_then_name {
  constexpr bool operator()(const std::pair<value_pair, std::size_t>& a,
                            const std::pair<value_pair, std::size_t>& b) const noexcept {
    if (a.second < b.second) {
      return true;
    } else if (a.second > b.second) {
      return false;
    }
    return std::make_tuple(std::get<0>(a.first)->name(), std::get<1>(a.first)->name()) >
           std::make_tuple(std::get<0>(b.first)->name(), std::get<1>(b.first)->name());
  }
};

// Determine the unique values in sorted container `operands`, and write them to output iterator
// `output`.
template <typename Container, typename OutputContainer>
void count_operand_multiplicity(const Container& operands, OutputContainer& output) {
  WF_ASSERT(std::is_sorted(operands.begin(), operands.end(),
                           [](auto a, auto b) { return a->name() < b->name(); }),
            "operands: [{}]", fmt::join(operands, ", "));

  output.clear();
  for (auto it = operands.begin(); it != operands.end();) {
    const ir::value_ptr val = *it;

    // Find the next unique operand:
    const auto next =
        std::find_if(it, operands.end(), [val](const ir::const_value_ptr v) { return v != val; });
    const std::size_t count = static_cast<std::size_t>(std::distance(it, next));

    // Save the operand and its count:
    output.emplace_back(val, count);
    it = next;
  }
}

// TODO: This method of counting overcounts some times of pairs.
// For example, in the multiplication:
//  v0 = x*x*x*x*y
// There are two instances of x*x, and one instance of x*y.
// We should not double count all the permutions of `x`, but we presently do.
template <typename OpType, typename Container>
static auto count_operand_pairs(
    const Container& container,
    std::unordered_map<value_pair, std::size_t, hash_struct<value_pair>>& counts) {
  std::vector<std::tuple<ir::value_ptr, std::size_t>> operands_with_multiplicities{};
  for (const ir::const_value_ptr val : container) {
    if (!val->is_op<OpType>() || val->is_unused()) {
      continue;
    }

    count_operand_multiplicity(val->operands(), operands_with_multiplicities);

    for (std::size_t i = 0; i < operands_with_multiplicities.size(); ++i) {
      const auto [a, a_count] = operands_with_multiplicities[i];
      if (a_count > 1) {
        // For example: x*x*x*x (count of 4) admits two unique x*x multiplications.
        // x*x*x*x*x*x (count of 6) admits three: (x*x)*(x*x)*(x*x)
        // x*x*x (count of three) admits only one.
        counts[make_value_pair(a, a)] += a_count / 2;
      }

      for (std::size_t j = i + 1; j < operands_with_multiplicities.size(); ++j) {
        const auto [b, b_count] = operands_with_multiplicities[j];
        WF_ASSERT(a != b);  //  These should be unique after `count_operand_multiplicity`.

        // Take the min. For example: x*x*y can form one x*y multiplication, not two.
        counts[make_value_pair(a, b)] += std::min(a_count, b_count);
      }
    }
  }
}

// Given two values a & b, find the consumers they share (filtering on OpType).
template <typename OpType>
static void find_shared_consumers(const ir::const_value_ptr a, const ir::const_value_ptr b,
                                  std::vector<ir::value_ptr>& outputs) {
  outputs.clear();
  for (const auto consumer_a : a->consumers()) {
    // `a` could match `b`, in which case just take everything of type `OpType`.
    if (consumer_a->is_op<OpType>() && (a == b || consumer_a->has_operand(b))) {
      outputs.push_back(consumer_a);
    }
  }
  std::sort(outputs.begin(), outputs.end(), [](auto x, auto y) { return x->name() < y->name(); });
  outputs.erase(std::unique(outputs.begin(), outputs.end()), outputs.end());
}

// Cound the number of times that pair (a, b) occurs in `operands`.
template <typename Container>
std::size_t count_pair(const Container& operands, const ir::const_value_ptr a,
                       const ir::const_value_ptr b) {
  if (a != b) {
    return std::min(std::count(operands.begin(), operands.end(), a),
                    std::count(operands.begin(), operands.end(), b));
  } else {
    return std::count(operands.begin(), operands.end(), a) / 2;
  }
}

// Get the consumers (with no repititions).
template <typename Container>
void get_unique_consumers(const ir::const_value_ptr v, Container& output) {
  output.clear();
  // For loop here because clang won't accept assign() for some reason.
  for (const auto& consumer : v->consumers()) {
    output.push_back(consumer);
  }
  std::sort(output.begin(), output.end(), [](auto x, auto y) { return x->name() < y->name(); });
  output.erase(std::unique(output.begin(), output.end()), output.cend());
}

// TODO: It actually doesn't make sense to call this on a per-block basis, since it assumes
// the whole function is in scope. We run it before other blocks are created, so it works. The
// interface is a bit weird/misleading though.
//
// NOTE: This algorithm is approximate because we count all the pairs once at the start, and then
// put them into the priority queue. Pair counts aren't edited as we edit the graph of operations,
// except to insert new elements into the queue. That said, I have found it does not drop the
// operation count that much to actually "edit" items in the priority queue, and it woul raise the
// computational complexity of this method to do so.
template <typename Src, typename Dst>
void control_flow_graph::binarize_operations(const ir::block_ptr block) {
  WF_FUNCTION_TRACE();

  std::unordered_map<value_pair, std::size_t, hash_struct<value_pair>> pair_counts{};
  pair_counts.reserve(values_.size());

  using queue_element = std::pair<value_pair, std::size_t>;
  std::priority_queue<queue_element, std::vector<queue_element>,
                      order_value_pair_by_occurrence_then_name>
      queue{};

  // Count pairs in the n-ary operations, queue anything that shows up more than once.
  count_operand_pairs<Src>(block->operations(), pair_counts);
  for (const auto& pair : pair_counts) {
    if (pair.second > 1) {
      queue.push(pair);
    }
  }

  // We'll append to this, then sort it at the end:
  std::vector<ir::value_ptr> operations = block->operations();

  // Some storage we reuse:
  std::vector<ir::value_ptr> shared_consumers{};
  std::vector<ir::value_ptr> unique_consumers{};
  std::vector<std::tuple<ir::value_ptr, std::size_t>> operands_with_multiplicities{};
  while (!queue.empty()) {
    const queue_element top = queue.top();
    queue.pop();
    const auto [a, b] = std::get<0>(top);

    // We can process this pair.
    // Find all the consumers of type `Src` that use both `a` and `b`.
    find_shared_consumers<Src>(a, b, shared_consumers);

    // Create a new value and put it in operations. We don't care about order here, because we
    // reorder topologically at the bottom of this method.
    const ir::value_ptr new_value = push_value(block, Dst{}, a->type(), a, b);
    operations.push_back(new_value);

    // We insert new counts into `pair_counts` as we update values that use (a ,b).
    pair_counts.clear();
    for (const ir::value_ptr c : shared_consumers) {
      const std::size_t count = count_pair(c->operands(), a, b);
      if (count < 1) {
        continue;
      }

      if (c->num_operands() > 2) {
        // Replace the pair (a, b) with the new value `count` times.
        c->replace_operand_pair(a, b, count, new_value);

        // Count all the operands:
        count_operand_multiplicity(c->operands(), operands_with_multiplicities);
        for (const auto& [operand, operand_count] : operands_with_multiplicities) {
          if (operand != new_value) {
            pair_counts[make_value_pair(operand, new_value)] += std::min(operand_count, count);
          }
        }
        if (count > 1) {
          pair_counts[make_value_pair(new_value, new_value)] += count / 2;
        }
      } else if (c->is_op<Src>()) {
        // This operation has been reduced to a binary operation already, just replace it.
        WF_ASSERT_EQ(2, c->num_operands());

        // Anything that directly consumed `c` must have its count updated.
        // As `c` becomes `new_value`, insert new counts for products/sums of values with
        // `new_value`.
        get_unique_consumers(c, unique_consumers);

        for (const ir::const_value_ptr downstream_consumer : unique_consumers) {
          if (downstream_consumer->is_op<Src>()) {
            count_operand_multiplicity(downstream_consumer->operands(),
                                       operands_with_multiplicities);
            for (const auto& [operand, operand_count] : operands_with_multiplicities) {
              if (operand != c) {
                pair_counts[make_value_pair(operand, new_value)] += std::min(operand_count, count);
              } else if (operand_count > 1) {
                pair_counts[make_value_pair(new_value, new_value)] += operand_count / 2;
              }
            }
          }
        }
        c->replace_with(new_value);
      } else {
        WF_ASSERT_ALWAYS("Value has wrong number of operands ({}): {} ({}) -> [{}]",
                         c->num_operands(), c, c->op_name(), fmt::join(c->operands(), ", "));
      }

      for (const auto& pair : pair_counts) {
        if (pair.second > 1) {
          queue.push(pair);
        }
      }
    }
  }

  // Record size here since we will push back:
  const std::size_t operations_size = operations.size();
  for (std::size_t n = 0; n < operations_size; ++n) {
    const ir::value_ptr v = operations[n];
    if (!v->is_op<Src>()) {
      continue;
    }

    if (v->num_operands() > 2) {
      // Convert the remaining terms into multiplications in left -> right order.
      ir::value_ptr result = v->first_operand();
      for (std::size_t i = 1; i < v->num_operands(); ++i) {
        result = push_value(block, Dst{}, v->type(), result, v->operator[](i));
        operations.push_back(result);
      }
      v->replace_with(result);
    } else {
      WF_ASSERT_EQ(2, v->num_operands());
      // We are leveraging the fact that operand_ptr can be implicitly casted to value_ptr here.
      v->set_operation(Dst{}, v->type(), v->operands());
    }
  }

  reverse_remove_if(operations, &remove_if_unused);

  // Operations aren't in order anymore since we used push_back.
  // Topologically sort them back into an order that respects dependencies.
  topological_sort_values(block, operations);

  block->set_operations(std::move(operations));
  block->remove_unused_operations();  //  TODO: Can probably remove this.
}

template <typename In, typename Out1, typename Out2>
void separate_muls(const In& in, Out1& muls, Out2& non_muls) {
  muls.clear();
  non_muls.clear();
  for (const ir::value_ptr v : in) {
    if (v->is_op<ir::mul, ir::muln>()) {
      muls.emplace_back(v);
    } else {
      non_muls.push_back(v);
    }
  }
}

class variable_index_assignor {
 public:
  using bitset_container_type = absl::InlinedVector<factor_bits, 16>;

  bitset_container_type initialize(const absl::Span<const ir::value_ptr> muls) {
    if (muls.size() > factorizer_params::MAX_VARS_OR_TERMS) {
      return {};
    }

    // Traverse all the variables in the provided products.
    // Assign them each an index starting at zero, and store the mapping from index -> ptr.
    operand_name_to_index_.clear();
    index_to_operand_.clear();
    for (const ir::const_value_ptr mul : muls) {
      for (const ir::value_ptr operand : mul->operands()) {
        if (const auto [_, was_inserted] =
                operand_name_to_index_.emplace(operand->name(), index_to_operand_.size());
            was_inserted) {
          index_to_operand_.push_back(operand);
        }
      }
    }

    if (operand_name_to_index_.size() > factorizer_params::MAX_VARS_OR_TERMS) {
      return {};
    }

    // Now create a bitset for each term in the factorization.
    // Fill the bits corresponding to variables that eppear
    return transform_map<bitset_container_type>(muls, [this](const ir::const_value_ptr mul) {
      factor_bits mask{};
      for (const ir::const_value_ptr operand : mul->operands()) {
        mask.set(operand_name_to_index_.at(operand->name()));
      }
      return mask;
    });
  }

  std::size_t num_variables() const noexcept { return operand_name_to_index_.size(); }

  template <typename Container>
  void get_variables_from_bitset(const factor_bits& vars, Container& out) const {
    out.clear();
    for (std::size_t i = 0; i < num_variables(); ++i) {
      if (vars[i]) {
        out.push_back(index_to_operand_.at(i));
      }
    }
  }

  bool variable_was_factored_out(const factor_bits& vars, const ir::const_value_ptr val) const {
    const auto it = operand_name_to_index_.find(val->name());
    WF_ASSERT(it != operand_name_to_index_.end());
    return vars[it->second];
  }

 private:
  absl::flat_hash_map<std::uint32_t, std::size_t> operand_name_to_index_{};
  std::vector<ir::value_ptr> index_to_operand_{};
};

ir::value_ptr control_flow_graph::factorize_sum_of_products(
    const variable_index_assignor& index_assignor, const factorization& fac,
    const ir::block_ptr block, const absl::Span<const ir::value_ptr> muls,
    const absl::Span<const ir::value_ptr> non_muls, std::vector<ir::value_ptr>& operations_out) {
  WF_ASSERT(!muls.empty());
  const auto value_type = scalar_type(muls.front()->numeric_type());

  absl::InlinedVector<ir::value_ptr, 8> mul_args{};
  absl::InlinedVector<ir::value_ptr, 8> factored_sum_args{};
  for (const auto& step : fac.steps()) {
    const auto& [vars, fac_terms] = step;
    WF_ASSERT_GE(vars.count(), 1);
    WF_ASSERT_GE(fac_terms.count(), 2);

    // Get the variables that were factored out:
    index_assignor.get_variables_from_bitset(vars, mul_args);

    // now create an addition from the remaining terms:
    absl::InlinedVector<ir::value_ptr, 8> sum_args{};
    for (std::size_t j = 0; j < muls.size(); ++j) {
      if (!fac_terms[j]) {
        continue;
      }

      // Copy the operands vector, and drop any thing were factored out:
      auto operands = muls[j]->operands();
      WF_ASSERT(std::is_sorted(operands.begin(), operands.end(),
                               [](auto a, auto b) { return a->name() < b->name(); }));

      // Find the unique subset, and extract the duplicates into `remaining_operands`:
      absl::InlinedVector<ir::value_ptr, 8> remaining_operands;
      operands.erase(unique_and_extract_duplicates(operands.begin(), operands.end(),
                                                   std::back_inserter(remaining_operands)),
                     operands.cend());

      // Now take any operands that were _not_ factored out.
      std::copy_if(operands.begin(), operands.end(), std::back_inserter(remaining_operands),
                   [&](const ir::const_value_ptr v) {
                     return !index_assignor.variable_was_factored_out(std::get<0>(step), v);
                   });

      if (remaining_operands.empty()) {
        // All the variables in this product were factored out, so insert a one.
        const ir::value_ptr one = push_value(block, ir::load{integer_constant(1)},
                                             scalar_type(code_numeric_type::integral));
        operations_out.push_back(one);
        const ir::value_ptr one_casted = maybe_cast(one, value_type.numeric_type());
        if (one_casted != one) {
          operations_out.push_back(one_casted);
        }
        sum_args.push_back(one_casted);
      } else if (remaining_operands.size() == 1) {
        // There is one variable left that was not factored out.
        sum_args.push_back(remaining_operands.front());
      } else {
        const ir::value_ptr remaining_vars_mul =
            push_value(block, ir::muln{}, value_type, remaining_operands);
        operations_out.push_back(remaining_vars_mul);
        sum_args.push_back(remaining_vars_mul);
      }
    }

    WF_ASSERT_GE(sum_args.size(), 2);

    const ir::value_ptr sum = push_value(block, ir::addn{}, value_type, sum_args);
    operations_out.push_back(sum);

    // Then multiply the sum by the extracted factor:
    mul_args.push_back(sum);
    WF_ASSERT_GE(mul_args.size(), 2);

    const ir::value_ptr prod = push_value(block, ir::muln{}, value_type, mul_args);
    operations_out.push_back(prod);
    factored_sum_args.push_back(prod);
  }

  // Add any of the terms from the original sum that were not multiplications:
  factored_sum_args.insert(factored_sum_args.cend(), non_muls.begin(), non_muls.end());

  // And put back any terms we failed to include in a factorization:
  const auto unfactored_terms = fac.unfactored_terms(muls.size());
  for (std::size_t i = 0; i < muls.size(); ++i) {
    if (unfactored_terms[i]) {
      factored_sum_args.push_back(muls[i]);
    }
  }

  // Finally sum it all together:
  WF_ASSERT(!factored_sum_args.empty());
  if (factored_sum_args.size() == 1) {
    return factored_sum_args.front();
  }
  const auto result = push_value(block, ir::addn{}, value_type, factored_sum_args);
  operations_out.push_back(result);
  return result;
}

void control_flow_graph::factorize_sums_in_block(const ir::block_ptr block) {
  WF_FUNCTION_TRACE();

  // We'll copy and modify this vector:
  auto operations = block->operations();

  // Used to split out multiplication from non-multiplication terms.
  std::vector<ir::value_ptr> muls{};
  std::vector<ir::value_ptr> non_muls{};
  variable_index_assignor index_assignor{};

  for (const ir::value_ptr add : block->operations()) {
    if (!add->is_op<ir::add, ir::addn>()) {
      continue;
    }

    // Pull out arguments that are multiplications:
    separate_muls(add->operands(), muls, non_muls);
    if (muls.size() <= 1) {
      continue;
    }

    // Assign indices to the args:
    const auto term_bitsets = index_assignor.initialize(muls);
    if (term_bitsets.empty()) {
      // This sum is too large.
      continue;
    }

    // Compute factorizations:
    constexpr std::size_t branching_factor = 4;
    const auto factorizations = compute_ranked_factorizations(
        term_bitsets, index_assignor.num_variables(), branching_factor);

    if (factorizations.empty()) {
      continue;
    }

    // TODO: For now we take the highest scoring one, but there might be something smarter we
    // could do here by considering the whole graph.
    const ir::value_ptr factorized_sum = factorize_sum_of_products(
        index_assignor, factorizations.front(), block, muls, non_muls, operations);

    add->replace_with(factorized_sum);
  }

  reverse_remove_if(operations, &remove_if_unused);
  topological_sort_values(block, operations);
  block->set_operations(std::move(operations));
}

// ReSharper disable once CppMemberFunctionMayBeStatic
void control_flow_graph::merge_multiplications_in_block(const ir::block_ptr block) {
  WF_FUNCTION_TRACE();

  // Lambda that evalutes to true for multiplications.
  const auto is_mul = [](const ir::const_value_ptr v) { return v->is_op<ir::mul, ir::muln>(); };

  std::vector<ir::value_ptr> mul_args{};
  for (const ir::value_ptr val : block->operations()) {
    if (!is_mul(val)) {
      continue;
    }

    mul_args.clear();
    for (const ir::value_ptr operand : val->operands()) {
      if (is_mul(operand)) {
        mul_args.insert(mul_args.end(), operand->operands().begin(), operand->operands().end());
      } else {
        mul_args.push_back(operand);
      }
    }

    // Check if this multiplication had any multiplications as its arguments:
    if (mul_args.size() > val->num_operands()) {
      // Replace it with a larger multiplication:
      val->set_operation(ir::muln{}, val->type(), mul_args);
    }
  }
  block->remove_unused_operations();
}

inline bool value_is_negative_one(const ir::const_value_ptr val) {
  if (const ir::load* load = std::get_if<ir::load>(&val->value_op()); load != nullptr) {
    if (const integer_constant* constant = std::get_if<integer_constant>(&load->variant());
        constant != nullptr && constant->value() == -1) {
      return true;
    }
  } else if (val->is_op<ir::cast>() && value_is_negative_one(val->first_operand())) {
    return true;
  }
  return false;
}

void control_flow_graph::insert_negations(const ir::block_ptr block) {
  WF_FUNCTION_TRACE();

  std::vector<ir::value_ptr> operations_out = block->operations();
  for (const ir::value_ptr val : block->operations()) {
    if (!val->is_op<ir::mul, ir::muln>()) {
      continue;
    }

    // Copy out all the operands that are not negative one.
    absl::InlinedVector<ir::value_ptr, 8> other_operands{};
    std::copy_if(val->operands().begin(), val->operands().end(), std::back_inserter(other_operands),
                 [](const ir::const_value_ptr x) { return !value_is_negative_one(x); });

    const std::size_t negation_count = val->num_operands() - other_operands.size();
    const bool is_negative = negation_count & 1;
    if (negation_count == 0 || other_operands.empty()) {
      // For now just skip if there are no other operands - this case should not occur in
      // practice.
      continue;
    }

    const ir::value_ptr positive_value = std::invoke([&] {
      if (other_operands.size() == 1) {
        return other_operands.front();
      } else if (other_operands.size() == 2) {
        // Otherwise insert a multiplcation:
        return push_value(block, ir::mul{}, val->type(), other_operands);
      } else {
        return push_value(block, ir::muln{}, val->type(), other_operands);
      }
    });

    if (other_operands.size() > 1) {
      operations_out.push_back(positive_value);
    }

    if (is_negative) {
      const ir::value_ptr negated_value = push_value(block, ir::neg{}, val->type(), positive_value);
      operations_out.push_back(negated_value);
      val->replace_with(negated_value);
    } else {
      val->replace_with(positive_value);
    }
  }

  reverse_remove_if(operations_out, &remove_if_unused);
  topological_sort_values(block, operations_out);
  block->set_operations(std::move(operations_out));
}

std::string control_flow_graph::to_string() const {
  const std::size_t width = value_print_width();
  std::string output{};

  // size of the left column, so we can align things
  const std::size_t left_column_width = fmt::formatted_size("  v{:0>{}} <- ", 0, width);

  for (const std::unique_ptr<ir::block>& block : blocks_) {
    fmt::format_to(std::back_inserter(output), "{}:", ir::block_ptr{block.get()});
    output += "\n";

    for (const ir::value_ptr& code : block->operations()) {
      // Print the value name:
      if (code->num_consumers() > 0) {
        fmt::format_to(std::back_inserter(output), "  v{:0>{}} <- ", code->name(), width);
      } else {
        output.append(left_column_width, ' ');
      }

      // Print the instruction name:
      constexpr int operation_width = 4;
      fmt::format_to(std::back_inserter(output), "{:>{}} ",
                     std::visit([](const auto& op) { return op.to_string(); }, code->value_op()),
                     operation_width);
      format_op_args(output, code->value_op(), code->operands(), width);
      output += "\n";
    }

    if (!block->has_no_descendents()) {
      output.append(left_column_width, ' ');
      fmt::format_to(std::back_inserter(output), "jump {}\n",
                     fmt::join(block->descendants(), ", "));
    }
  }
  if (!output.empty()) {
    output.pop_back();
  }
  return output;
}

constexpr std::size_t compute_print_width(std::size_t num_assignments) noexcept {
  std::size_t width = 1;
  for (; num_assignments > 0; ++width) {
    num_assignments /= 10;
  }
  return width;
}

std::size_t control_flow_graph::value_print_width() const {
  const uint32_t highest_value_name = values_.empty() ? 0 : values_.back()->name();
  return compute_print_width(highest_value_name);
}

std::size_t control_flow_graph::num_operations() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](const std::size_t total, const ir::block::unique_ptr& b) {
                           return total + b->count_operation([](const ir::const_value_ptr v) {
                             return !v->is_op<ir::jump_condition, ir::save, ir::load, ir::copy>();
                           });
                         });
}

std::size_t control_flow_graph::num_conditionals() const {
  return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                         [](const std::size_t total, const ir::block::unique_ptr& b) {
                           return total + b->count_operation([](const ir::const_value_ptr v) {
                             return v->is_op<ir::jump_condition, ir::cond>();
                           });
                         });
}

ir::block_ptr control_flow_graph::first_block() const {
  const auto it =
      std::find_if(blocks_.begin(), blocks_.end(),
                   [](const ir::block::unique_ptr& block) { return block->has_no_ancestors(); });
  WF_ASSERT(it != blocks_.end(), "There must be an entry block");
  return ir::block_ptr{it->get()};
}

std::size_t control_flow_graph::count_function(const std_math_function enum_value) const noexcept {
  return count_operation(
      [&](const ir::call_std_function func) { return func.name() == enum_value; });
}

static operation_counts count_all_operations(
    const ir::const_block_ptr block, std::unordered_set<ir::const_block_ptr>& non_traversable) {
  operation_counts counts{};
  if (non_traversable.count(block)) {
    return counts;
  }

  counts[operation_count_label::add] = block->count_operation<ir::add>();
  counts[operation_count_label::branch] = block->count_operation<ir::jump_condition>();
  counts[operation_count_label::call] = block->count_operation<ir::call_std_function>() +
                                        block->count_operation<ir::call_external_function>();
  counts[operation_count_label::compare] = block->count_operation<ir::compare>();
  counts[operation_count_label::divide] = block->count_operation<ir::div>();
  counts[operation_count_label::multiply] = block->count_operation<ir::mul>();
  counts[operation_count_label::negate] = block->count_operation<ir::neg>();

  if (const auto& descendants = block->descendants(); !descendants.empty()) {
    if (const ir::const_value_ptr last_op = block->last_operation();
        !last_op->is_op<ir::jump_condition>()) {
      WF_ASSERT_EQ(1, descendants.size());
      counts.increment(count_all_operations(descendants[0], non_traversable));
    } else {
      WF_ASSERT_EQ(2, descendants.size());

      // Determine where the two branches eventually reconnect:
      const ir::const_block_ptr merge =
          ir::find_merge_point(descendants[0], descendants[1], ir::search_direction::downwards);
      non_traversable.insert(merge);

      const auto counts_left = count_all_operations(descendants[0], non_traversable);
      const auto counts_right = count_all_operations(descendants[1], non_traversable);

      // Take the maximum in each type of operation.
      // This isn't really reflective of what would execute, but probably a bit better than double
      // counting.
      counts.increment(counts_left.max(counts_right));

      // Continue counting from after the merge point:
      non_traversable.erase(merge);
      counts.increment(count_all_operations(merge, non_traversable));
    }
  }
  return counts;
}

operation_counts control_flow_graph::compute_operation_counts() const {
  std::unordered_set<ir::const_block_ptr> non_traversable_set{};
  return count_all_operations(first_block(), non_traversable_set);
}

void control_flow_graph::assert_invariants() const {
  std::unordered_set<ir::const_value_ptr> all_used_values{};
  for (const auto& v : values_) {
    if (!v->is_unused()) {
      all_used_values.emplace(v.get());
    }
  }

  for (const auto& block : blocks_) {
    WF_ASSERT(block);
    for (const ir::value_ptr op : block->operations()) {
      WF_ASSERT(all_used_values.count(op.get()));
      WF_ASSERT(op->parent().get() == block.get());
    }
  }

  // check the invariants:
  for (const auto& v : values_) {
    WF_ASSERT(v);
    for (const auto operand : v->operands()) {
      WF_ASSERT(all_used_values.count(operand.get()));
      WF_ASSERT(contains(operand->consumers(), ir::value_ptr{v.get()}));
    }
    for (const auto consumer : v->consumers()) {
      WF_ASSERT(all_used_values.count(consumer.get()));
      WF_ASSERT(contains(consumer->operands(), ir::value_ptr{v.get()}));
    }
  }
}

template <typename T, typename... Args>
ir::value_ptr control_flow_graph::push_value(ir::block_ptr block, T op, ir::value::types type,
                                             Args&&... args) {
  const uint32_t name = values_.empty() ? 0 : values_.back()->name() + 1;
  auto value = std::make_unique<ir::value>(name, block, std::move(op), std::move(type),
                                           std::forward<Args>(args)...);
  const ir::value_ptr result{value.get()};
  values_.push_back(std::move(value));
  return result;
}

ir::value_ptr control_flow_graph::maybe_cast(const ir::value_ptr val,
                                             const code_numeric_type type) {
  if (val->numeric_type() == type) {
    return val;
  } else {
    return push_value(val->parent(), ir::cast{type}, scalar_type(type), val);
  }
}

control_flow_graph::control_flow_graph(std::vector<ir::block::unique_ptr> blocks,
                                       std::vector<ir::value::unique_ptr> values)
    : blocks_(std::move(blocks)), values_(std::move(values)) {}

}  // namespace wf
