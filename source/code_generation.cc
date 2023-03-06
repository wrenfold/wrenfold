// Copyright 2023 Gareth Cross
#include "code_generation.h"

#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "expressions/all_expressions.h"
#include "hashing.h"
#include "visitor_impl.h"

namespace math {
namespace ssa {

struct PairCountVisitor {
  constexpr static VisitorPolicy Policy = VisitorPolicy::NoError;
  using ReturnType = void;

  struct PairHash {
    std::size_t operator()(const std::pair<Expr, Expr>& pair) const {
      return HashCombine(HashExpression(pair.first), HashExpression(pair.second));
    }
  };

  struct PairEquality {
    bool operator()(const std::pair<Expr, Expr>& a, const std::pair<Expr, Expr>& b) const {
      return a.first.IsIdenticalTo(b.first) && a.second.IsIdenticalTo(b.second);
    }
  };

  void Apply(const Multiplication& mul) {
    ASSERT_GREATER_OR_EQ(mul.Arity(), 2, "Multiplication must have at least two args");
    for (const Expr& operand : mul) {
      counts[operand]++;
      VisitStruct(operand, *this);
    }
    // generate pairs of expressions:
    for (auto i = mul.begin(); i != mul.end(); ++i) {
      for (auto j = std::next(i); j != mul.end(); ++j) {
        auto [it, was_inserted] = mul_counts.emplace(std::make_pair(*i, *j), 1);
        if (!was_inserted) {
          it->second += 1;
        }
      }
    }
  }

  void Apply(const Addition& add) {
    ASSERT_GREATER_OR_EQ(add.Arity(), 2, "Multiplication must have at least two args");
    for (const Expr& operand : add) {
      counts[operand]++;
      VisitStruct(operand, *this);
    }

    // generate pairs of expressions:
    for (auto i = add.begin(); i != add.end(); ++i) {
      for (auto j = std::next(i); j != add.end(); ++j) {
        auto [it, was_inserted] = add_counts.emplace(std::make_pair(*i, *j), 1);
        if (!was_inserted) {
          it->second += 1;
        }
      }
    }
  }

  std::unordered_map<Expr, std::size_t, HashObject, ExprEquality> counts;
  std::unordered_map<std::pair<Expr, Expr>, std::size_t, PairHash, PairEquality> mul_counts;
  std::unordered_map<std::pair<Expr, Expr>, std::size_t, PairHash, PairEquality> add_counts;
};

// Visitor for converting an expression tree into static-single-assignment form.
struct SSAFormVisitor {
  constexpr static VisitorPolicy Policy = VisitorPolicy::CompileError;
  using ReturnType = Operand;

  // Construct with an output array that we write into.
  explicit SSAFormVisitor(std::unordered_map<std::size_t, ssa::OperationVariant>& output,
                          const PairCountVisitor& pair_count)
      : operations_(output), pair_counts(pair_count) {
    if (operations_.empty()) {
      insertion_point_ = 0;
    } else {
      insertion_point_ =
          std::max_element(operations_.begin(), operations_.end(),
                           [&](const auto& a, const auto& b) { return a.first < b.first; })
              ->first +
          1;
    }
  }

  template <typename T, typename... Args>
  Operand PushOperation(Args&&... args) {
    T operation{std::forward<Args>(args)...};
    const std::size_t value_index = insertion_point_;
    insertion_point_++;
    operations_.emplace(value_index, std::move(operation));
    return value_index;
  }

  template <typename Derived>
  Operand Apply(const NAryOp<Derived>& op) {
    static_assert(std::is_same_v<Derived, Addition> || std::is_same_v<Multiplication, Derived>);

    std::vector<std::pair<Expr, Expr>> pairs{};
    pairs.reserve(op.Arity());
    for (auto i = op.begin(); i != op.end(); ++i) {
      for (auto j = std::next(i); j != op.end(); ++j) {
        pairs.emplace_back(*i, *j);
      }
    }

    // Put the thing w/ the highest count in the first cell:
    std::vector<Expr> expressions{op.begin(), op.end()};
    std::nth_element(expressions.begin(), expressions.begin(), expressions.end(),
                     [&](const Expr& a, const Expr& b) {
                       return pair_counts.counts.at(a) > pair_counts.counts.at(b);
                     });

    const auto make_pair = [](const Expr& a, const Expr& b) {
      auto result = std::make_pair(a, b);
      if (VisitBinaryStruct(result.first, result.second, OrderVisitor{}) !=
          OrderVisitor::RelativeOrder::LessThan) {
        std::swap(result.first, result.second);
      }
      return result;
    };

    // then pick the rest to obtain max pair count
    for (auto it = std::next(expressions.begin()); it != expressions.end(); ++it) {
      const auto prev = std::prev(it);
      std::nth_element(it, it, expressions.end(), [&](const Expr& a, const Expr& b) {
        if constexpr (std::is_same_v<Derived, Multiplication>) {
          return pair_counts.mul_counts.at(make_pair(*prev, a)) >
                 pair_counts.mul_counts.at(make_pair(*prev, b));
        } else {
          return pair_counts.add_counts.at(make_pair(*prev, a)) >
                 pair_counts.add_counts.at(make_pair(*prev, b));
        }
      });
    }

    // first recursively transform all the inputs
    std::vector<Operand> args;
    args.reserve(op.Arity());
    std::transform(expressions.begin(), expressions.end(), std::back_inserter(args),
                   [this](const Expr& expr) { return VisitStruct(expr, *this); });
    ASSERT(!args.empty());

    // now iterate and build new assignments
    Operand prev_result = args[0];
    for (std::size_t i = 1; i < args.size(); ++i) {
      if constexpr (std::is_same_v<Derived, Multiplication>) {
        prev_result = PushOperation<Mul>(prev_result, args[i]);
      } else {
        prev_result = PushOperation<Add>(prev_result, args[i]);
      }
    }
    return prev_result;
  }

  Operand Apply(const Expr& input_expression, const Constant&) { return input_expression; }

  Operand Apply(const Matrix&) { throw TypeError("Cannot evaluate this on a matrix."); }

  Operand Apply(const UnaryFunction& func) {
    Operand arg = VisitStruct(func.Arg(), *this);
    return PushOperation<CallUnaryFunc>(func.Func(), std::move(arg));
  }

  Operand Apply(const Expr& input_expression, const Integer&) { return input_expression; }
  Operand Apply(const Expr& input_expression, const Float&) { return input_expression; }

  Operand Apply(const Power& pow) {
    Operand base = VisitStruct(pow.Base(), *this);
    Operand exponent = VisitStruct(pow.Exponent(), *this);
    return PushOperation<Pow>(std::move(base), std::move(exponent));
  }

  Operand Apply(const Expr& input_expression, const Rational&) { return input_expression; }
  Operand Apply(const Expr& input_expression, const Variable&) { return input_expression; }

  std::size_t LastInserted() const {
    ASSERT_GREATER(insertion_point_, 0);
    return insertion_point_ - 1;
  }

 private:
  std::unordered_map<std::size_t, ssa::OperationVariant>& operations_;
  const PairCountVisitor& pair_counts;
  std::size_t insertion_point_;
};

inline void FormatOperand(std::string& output, const Operand& operand, const std::size_t width) {
  std::visit(
      [&](const auto& operand) {
        using T = std::decay_t<decltype(operand)>;
        if constexpr (std::is_same_v<T, std::size_t>) {
          fmt::format_to(std::back_inserter(output), "v{:0>{}}", operand, width);
        } else {
          // input value of type `Expr`
          output += operand.ToString();
        }
      },
      operand);
}

// Alignment of operations when printed:
constexpr int OperationWidth = 4;

template <typename Op>
inline void FormatOpWithArgs(std::string& output, const Op& op, const std::size_t width) {
  fmt::format_to(std::back_inserter(output), "{:>{}} ", op.ToString(), OperationWidth);
  for (auto it = op.args.begin(); it != op.args.end(); ++it) {
    FormatOperand(output, *it, width);
    if (std::next(it) != op.args.end()) {
      output += ", ";
    }
  }
}

inline void FormatOp(std::string& output, const Add& add, const std::size_t width) {
  FormatOpWithArgs(output, add, width);
}

inline void FormatOp(std::string& output, const Mul& mul, const std::size_t width) {
  FormatOpWithArgs(output, mul, width);
}

inline void FormatOp(std::string& output, const Pow& pow, const std::size_t width) {
  FormatOpWithArgs(output, pow, width);
}

inline void FormatOp(std::string& output, const Load& load, const std::size_t width) {
  FormatOpWithArgs(output, load, width);
}

inline void FormatOp(std::string& output, const CallUnaryFunc& f, const std::size_t width) {
  FormatOpWithArgs(output, f, width);
}

template <typename Container>
inline std::vector<Expr> ExtractArgExpressions(const Container& args,
                                               const std::vector<Expr>& expressions) {
  std::vector<Expr> inputs{};
  inputs.reserve(args.size());
  std::transform(args.begin(), args.end(), std::back_inserter(inputs), [&](const Operand& operand) {
    return std::visit(
        [&](const auto& x) -> Expr {
          using T = std::decay_t<decltype(x)>;
          if constexpr (std::is_same_v<T, std::size_t>) {
            ASSERT_LESS(x, expressions.size());
            return expressions[x];
          } else {
            return x;
          }
        },
        operand);
  });
  return inputs;
}

inline std::size_t HashOperand(const Operand& operand) {
  return std::visit(
      [](const auto& operand) -> std::size_t {
        using T = std::decay_t<decltype(operand)>;
        if constexpr (std::is_same_v<T, std::size_t>) {
          return std::hash<std::size_t>{}(operand);
        } else {
          return HashExpression(operand);
        }
      },
      operand);
}

inline bool OperandsAreEqual(const Operand& a, const Operand& b) {
  return std::visit(
      [](const auto& a, const auto& b) -> bool {
        using A = std::decay_t<decltype(a)>;
        using B = std::decay_t<decltype(b)>;
        if constexpr (std::is_same_v<A, B>) {
          if constexpr (std::is_same_v<A, std::size_t>) {
            return a == b;
          } else {
            return a.IsIdenticalTo(b);  //  Expr
          }
        } else {
          return false;
        }
      },
      a, b);
}

template <typename T>
struct TypeListFromVariant;

template <typename... Ts>
struct TypeListFromVariant<std::variant<Ts...>> {
  using List = TypeList<Ts...>;
};

template <typename T>
std::size_t HashOpWithArgs(const T& op) {
  std::size_t seed = IndexOfType<T, TypeListFromVariant<OperationVariant>::List>::Value;
  for (const Operand& operand : op.args) {
    seed = HashCombine(seed, HashOperand(operand));
  }
  return seed;
}

std::size_t HashOp(const Add& addition) { return HashOpWithArgs(addition); }

std::size_t HashOp(const Mul& mul) { return HashOpWithArgs(mul); }

std::size_t HashOp(const Pow& pow) { return HashOpWithArgs(pow); }

std::size_t HashOp(const Load& load) { return HashOpWithArgs(load); }

std::size_t HashOp(const CallUnaryFunc& call) {
  return HashCombine(HashOpWithArgs(call), static_cast<std::size_t>(call.name));
}

struct OperationHasher {
  std::size_t operator()(const OperationVariant& operation) const {
    return std::visit([](const auto& op) -> std::size_t { return HashOp(op); }, operation);
  }
};

struct OperationEquality {
  bool operator()(const OperationVariant& a, const OperationVariant& b) const {
    if (a.index() != b.index()) {
      return false;
    }
    return std::visit(
        [&](const auto& a, const auto& b) -> bool {
          using A = std::decay_t<decltype(a)>;
          using B = std::decay_t<decltype(b)>;
          if constexpr (std::is_same_v<A, B>) {
            // a and b are the same type, so we can compare them:
            const bool args_equal =
                std::equal(a.args.begin(), a.args.end(), b.args.begin(), &OperandsAreEqual);
            if (!args_equal) {
              return false;
            }
            // TODO: temporary hack
            if constexpr (std::is_same_v<A, CallUnaryFunc>) {
              return a.name == b.name;
            }
            return true;
          } else {
            return false;
          }
        },
        a, b);
  }
};

}  // namespace ssa

//  a,b,c,d
//
//  len 2: [a,b]  [a,c]  [a,d]
//         [b,c]  [b,d]
//         [c,d]
//
//  len 3: [a,b,c]  [a,b,d]
//         [a,c,d]
//         [b,c,d]

// a,b,c,d,e
//
//
// len 2: [a,b]  [a,c]  [a,d]  [a,e]
//        [b,c]  [b,d]  [b,e]
//        [c,d]  [c,e]
//        [d,e]
//
// len 3: [a,b,c]  [a,b,d]  [a,b,e]
//        [a,c,d]  [a,c,e]
//        [a,d,e]
//        [b,c,d]  [b,c,e]
//        [b,d,e]
//        [c,d,e]
//
// len 4: [a,b,c,d] [a,b,c,e]
//        [a,c,d,e]

template <typename Container, typename Handler>
void CreatePermutations(const Container& container, const std::size_t start,
                        std::vector<Expr>& buffer, Handler handler) {
  buffer.push_back(container[start]);
  if (buffer.size() > 1) {
    handler(const_cast<const std::vector<Expr>&>(buffer));
  }
  for (std::size_t index = start + 1; index < container.size(); ++index) {
    CreatePermutations(container, index, buffer, handler);
  }
  buffer.pop_back();
}

template <typename Container, typename Handler>
void CreatePermutations(const Container& container, Handler handler) {
  std::vector<Expr> outputs;
  outputs.reserve(10);
  for (std::size_t start = 0; start < container.size(); ++start) {
    CreatePermutations(container, start, outputs, handler);
  }
}

struct CountOperationsVisitor {
  constexpr static VisitorPolicy Policy = VisitorPolicy::CompileError;
  using ReturnType = std::size_t;

  std::size_t Apply(const Addition& add) const {
    return std::accumulate(add.begin(), add.end(), static_cast<std::size_t>(0),
                           [](std::size_t total, const Expr& expr) {
                             return total + VisitStruct(expr, CountOperationsVisitor{});
                           }) +
           (add.Arity() - 1);
  }

  constexpr std::size_t Apply(const Constant&) const { return 0; }

  std::size_t Apply(const Matrix& mat) const {
    return std::accumulate(mat.begin(), mat.end(), static_cast<std::size_t>(0),
                           [](std::size_t total, const Expr& expr) {
                             return total + VisitStruct(expr, CountOperationsVisitor{});
                           });
  }

  std::size_t Apply(const Multiplication& mul) const {
    return std::accumulate(mul.begin(), mul.end(), static_cast<std::size_t>(0),
                           [](std::size_t total, const Expr& expr) {
                             return total + VisitStruct(expr, CountOperationsVisitor{});
                           }) +
           (mul.Arity() - 1);
  }

  std::size_t Apply(const UnaryFunction& func) const {
    return VisitStruct(func.Arg(), CountOperationsVisitor{}) + 1;
  }

  constexpr std::size_t Apply(const Integer&) const { return 0; }
  constexpr std::size_t Apply(const Float&) const { return 0; }

  std::size_t Apply(const Power& pow) const {
    return VisitStruct(pow.Base(), CountOperationsVisitor{}) +
           VisitStruct(pow.Exponent(), CountOperationsVisitor{}) + 1;
  }

  constexpr std::size_t Apply(const Rational&) const { return 1; }
  constexpr std::size_t Apply(const Variable&) const { return 0; }
};

struct SequenceCountVisitor {
  constexpr static VisitorPolicy Policy = VisitorPolicy::NoError;
  using ReturnType = void;

  void Apply(const Multiplication& mul) {
    ASSERT_GREATER_OR_EQ(mul.Arity(), 2, "Multiplication must have at least two args");
    for (const Expr& operand : mul) {
      VisitStruct(operand, *this);
    }

    // create permutations of the args
    // TODO: we should build the hashes recursively here as we make the permutations
    CreatePermutations(mul.Args(), [&](const std::vector<Expr>& args) {
      // inefficient, but create a new expression
      Expr reduced_mul = Multiplication::FromOperands(args);
      auto [it, _] = counts.emplace(std::move(reduced_mul), 0);
      it->second++;
    });
  }

  void Apply(const Addition& add) {
    ASSERT_GREATER_OR_EQ(add.Arity(), 2, "Multiplication must have at least two args");
    for (const Expr& operand : add) {
      VisitStruct(operand, *this);
    }

    CreatePermutations(add.Args(), [&](const std::vector<Expr>& args) {
      Expr reduced_add = Addition::FromOperands(args);
      auto [it, _] = counts.emplace(std::move(reduced_add), 0);
      it->second++;
    });
  }

  std::unordered_map<Expr, std::size_t, HashObject, ExprEquality> counts;
};

std::size_t CreateSSA(const Expr& expression,
                      std::unordered_map<std::size_t, ssa::OperationVariant>& output) {
  // count sub-expression frequency
  SequenceCountVisitor sequence_counter{};
  VisitStruct(expression, sequence_counter);

  ssa::PairCountVisitor pair_visitor{};
  VisitStruct(expression, pair_visitor);

  ssa::SSAFormVisitor visitor{output, pair_visitor};
  VisitStruct(expression, visitor);
  return visitor.LastInserted();
}

inline constexpr std::size_t GetPrintWidth(std::size_t num_assignments) {
  std::size_t width = 1;
  for (; num_assignments > 0; ++width) {
    num_assignments /= 10;
  }
  return width;
}

template <typename Container>
inline std::vector<Expr> ExtractArgExpressions2(
    const Container& args,
    const std::unordered_map<std::size_t, ssa::OperationVariant>& expressions,
    std::unordered_map<std::size_t, Expr>& output_map) {
  std::vector<Expr> inputs{};
  inputs.reserve(args.size());
  std::transform(args.begin(), args.end(), std::back_inserter(inputs),
                 [&](const ssa::Operand& operand) {
                   return std::visit(
                       [&](const auto& x) -> Expr {
                         using T = std::decay_t<decltype(x)>;
                         if constexpr (std::is_same_v<T, std::size_t>) {
                           auto it = output_map.find(x);
                           if (it == output_map.end()) {
                             Expr expression = ExpressionFromSSA(expressions, x, output_map);
                             it = output_map.emplace(x, std::move(expression)).first;
                           }
                           return it->second;
                         } else {
                           return x;
                         }
                       },
                       operand);
                 });
  return inputs;
}
inline Expr CreateExpressionForOp(
    const ssa::Add& add, const std::unordered_map<std::size_t, ssa::OperationVariant>& expressions,
    std::unordered_map<std::size_t, Expr>& output_map) {
  return Addition::FromOperands(ExtractArgExpressions2(add.args, expressions, output_map));
}

inline Expr CreateExpressionForOp(
    const ssa::Mul& mul, const std::unordered_map<std::size_t, ssa::OperationVariant>& expressions,
    std::unordered_map<std::size_t, Expr>& output_map) {
  return Multiplication::FromOperands(ExtractArgExpressions2(mul.args, expressions, output_map));
}

inline Expr CreateExpressionForOp(
    const ssa::Pow& pow, const std::unordered_map<std::size_t, ssa::OperationVariant>& expressions,
    std::unordered_map<std::size_t, Expr>& output_map) {
  std::vector<Expr> inputs = ExtractArgExpressions2(pow.args, expressions, output_map);
  ASSERT_EQUAL(2, inputs.size());
  return Power::Create(inputs[0], inputs[1]);
}

inline Expr CreateExpressionForOp(
    const ssa::CallUnaryFunc& func,
    const std::unordered_map<std::size_t, ssa::OperationVariant>& expressions,
    std::unordered_map<std::size_t, Expr>& output_map) {
  std::vector<Expr> inputs = ExtractArgExpressions2(func.args, expressions, output_map);
  ASSERT_EQUAL(1, inputs.size());
  return CreateUnaryFunction(func.name, inputs.front());
}

inline Expr CreateExpressionForOp(
    const ssa::Load& load,
    const std::unordered_map<std::size_t, ssa::OperationVariant>& expressions,
    std::unordered_map<std::size_t, Expr>& output_map) {
  std::vector<Expr> inputs = ExtractArgExpressions2(load.args, expressions, output_map);
  ASSERT_EQUAL(1, inputs.size());
  return inputs.front();
}

Expr ExpressionFromSSA(const std::unordered_map<std::size_t, ssa::OperationVariant>& operations,
                       const std::size_t value_index,
                       std::unordered_map<std::size_t, Expr>& output_map) {
  auto it = operations.find(value_index);
  ASSERT(it != operations.end(), "Missing expression for value: {}", value_index);
  return std::visit(
      [&](const auto& op) { return CreateExpressionForOp(op, operations, output_map); },
      it->second);
}

Expr ExpressionFromSSA(const std::unordered_map<std::size_t, ssa::OperationVariant>& operations,
                       const std::size_t value_index) {
  std::unordered_map<std::size_t, Expr> output_map{};
  output_map.reserve(operations.size());
  return ExpressionFromSSA(operations, value_index, output_map);
}

std::string FormatSSA(const std::unordered_map<std::size_t, ssa::OperationVariant>& assignments) {
  // # of assignments determines # of digits in the output (align everything in columns).

  std::vector<std::size_t> keys;
  keys.reserve(assignments.size());
  std::transform(assignments.begin(), assignments.end(), std::back_inserter(keys),
                 [&](const auto& pair) { return pair.first; });
  std::sort(keys.begin(), keys.end());

  const auto max_index = keys.back();

  const std::size_t width = GetPrintWidth(max_index);
  std::string output{};
  for (const std::size_t v : keys) {
    fmt::format_to(std::back_inserter(output), "v{:0>{}} <- ", v, width);
    std::visit([&](const auto& op) { ssa::FormatOp(output, op, width); }, assignments.at(v));
    output += "\n";
  }
  output.pop_back();  //  remove final newline
  return output;
}

inline void PropagateCopiedOperands(
    ssa::OperationVariant& op,
    const std::unordered_map<std::size_t, ssa::OperationVariant>& operations) {
  std::visit(
      [&](auto& op) {
        for (ssa::Operand& operand : op.args) {
          if (!std::holds_alternative<std::size_t>(operand)) {
            continue;
          }
          auto it = operations.find(std::get<std::size_t>(operand));
          ASSERT(it != operations.end(), "Missing operation for value: {}",
                 std::get<std::size_t>(operand));
          if (std::holds_alternative<ssa::Load>(it->second)) {
            // "De-reference" by replacing this operand w/ the argument to the load.
            operand = std::get<ssa::Load>(it->second).args[0];
          }
        }

        // todo: gross
        if (std::is_same_v<std::decay_t<decltype(op)>, ssa::Add> ||
            std::is_same_v<std::decay_t<decltype(op)>, ssa::Mul>) {
          ssa::OrderOperands(op.args[0], op.args[1]);
        }
      },
      op);
}

void PropagateCopies(std::unordered_map<std::size_t, ssa::OperationVariant>& operations) {
  for (auto it = operations.begin(); it != operations.end(); ++it) {
    PropagateCopiedOperands(it->second, operations);
  }
}

void StripUnusedValues(std::unordered_map<std::size_t, ssa::OperationVariant>& operations,
                       const std::vector<std::size_t>& output_expressions) {
  std::unordered_set<std::size_t> used_values;
  used_values.reserve(operations.size());
  std::copy(output_expressions.begin(), output_expressions.end(),
            std::inserter(used_values, used_values.end()));

  for (const auto& pair : operations) {
    std::visit(
        [&](const auto& op) {
          for (const ssa::Operand& operand : op.args) {
            if (std::holds_alternative<std::size_t>(operand)) {
              used_values.insert(std::get<std::size_t>(operand));
            }
          }
        },
        pair.second);
  }

  for (auto it = operations.begin(); it != operations.end();) {
    if (used_values.count(it->first)) {
      ++it;
    } else {
      it = operations.erase(it);
    }
  }
}

void EliminateDuplicates(std::unordered_map<std::size_t, ssa::OperationVariant>& operations,
                         const std::vector<std::size_t>& output_expressions) {
  // Hash map from operation to value
  std::unordered_map<ssa::OperationVariant, std::size_t, ssa::OperationHasher,
                     ssa::OperationEquality>
      value_for_variant;
  value_for_variant.reserve(operations.size());

  // determine traversal order:
  std::vector<std::size_t> ordered_keys;
  ordered_keys.reserve(operations.size());
  std::transform(operations.begin(), operations.end(), std::back_inserter(ordered_keys),
                 [](const auto& pair) { return pair.first; });
  std::sort(ordered_keys.begin(), ordered_keys.end());

  for (const std::size_t value : ordered_keys) {
    auto it = operations.find(value);
    ASSERT(it != operations.end());

    PropagateCopiedOperands(it->second, operations);

    const auto [existing_it, was_inserted] = value_for_variant.emplace(it->second, it->first);
    if (!was_inserted) {
      // This operation already exists in the map, so replace this w/ a copy:
      it->second = ssa::Load(existing_it->second);
    }
  }

  StripUnusedValues(operations, output_expressions);
}

std::vector<Expr> IdentifySequenceCounts(const Expr& root) {
  SequenceCountVisitor visitor{};
  VisitStruct(root, visitor);

  std::vector<std::tuple<Expr, std::size_t, std::size_t>> counts;
  std::transform(visitor.counts.begin(), visitor.counts.end(), std::back_inserter(counts),
                 [](const std::pair<Expr, std::size_t>& expr_and_occurrences) {
                   const std::size_t ops =
                       VisitStruct(expr_and_occurrences.first, CountOperationsVisitor{});
                   return std::make_tuple(expr_and_occurrences.first, expr_and_occurrences.second,
                                          ops * expr_and_occurrences.second);
                 });

  // Delete anything seen only once
  auto it = std::remove_if(counts.begin(), counts.end(),
                           [](const auto& x) { return std::get<1>(x) == 1; });
  counts.erase(it, counts.end());

  // sort in descending order of op count
  std::sort(counts.begin(), counts.end(),
            [](const auto& a, const auto& b) { return std::get<2>(a) > std::get<2>(b); });

  for (const auto& [expr, count, op_count] : counts) {
    fmt::print("count = {} ({}), expr = {}\n", count, op_count, expr.ToString());
  }

  // replace the expressions:
  std::vector<Expr> outputs;
  Expr input = root;
  for (const auto& tuple : counts) {
    Expr variable{fmt::format("$temp{:03}", outputs.size())};
    Expr output = input.Subs(std::get<0>(tuple), variable);
    if (!input.IsIdenticalTo(output)) {
      outputs.push_back(std::get<0>(tuple));
    }
    input = output;
  }
  outputs.push_back(input);
  return outputs;
}

}  // namespace math
