// Copyright 2023 Gareth Cross
#include "code_generation/ast.h"

#include <unordered_set>

#include "code_generation/ir_builder.h"
#include "code_generation/ir_types.h"
#include "expressions/all_expressions.h"
#include "visitor_impl.h"

namespace math {

inline void GetConditionalOutputs(const ir::ValuePtr v, std::vector<ir::ValuePtr>& outputs) {
  bool all_phi_consumers = true;
  for (ir::ValuePtr consumer : v->Consumers()) {
    if (consumer->IsPhi()) {
      GetConditionalOutputs(consumer, outputs);
    } else {
      all_phi_consumers = false;
    }
  }
  if (!all_phi_consumers) {
    // There are no consumers of this phi function that not phi functions, so we need to declare
    // a variable for this value.
    outputs.push_back(v);
  }
}

struct AstBuilder {
  AstBuilder(std::size_t value_width, const ast::FunctionSignature& signature)
      : value_width_(value_width), signature_(signature) {}

  ast::FunctionDefinition CreateFunction(ir::BlockPtr block) {
    ProcessBlock(block);
    return ast::FunctionDefinition{signature_, std::move(operations_)};
  }

  void ProcessBlock(ir::BlockPtr block) {
    ASSERT(!block->IsEmpty());
    if (stop_set_.count(block)) {
      return;
    }
    operations_.reserve(operations_.capacity() + block->operations.size());

    // We want to place any `ir::Save` operations at the end before conditional logic, so separate
    // those:
    std::vector<ir::ValuePtr> save_values{};
    save_values.reserve(block->operations.size());

    std::vector<ast::AssignTemporary> phi_assignments{};
    phi_assignments.reserve(block->operations.size());

    for (const ir::ValuePtr value : block->operations) {
      //      const ir::ValuePtr value = block->operations[i];
      if (value->Is<ir::Save>()) {
        save_values.push_back(value);
      } else if (value->IsPhi()) {
        // Do nothing.
      } else if (value->Is<ir::JumpCondition>()) {
        // Do nothing.
      } else {
        std::vector<ir::ValuePtr> conditional_outputs{};
        bool all_phi_consumers = true;
        for (ir::ValuePtr consumer : value->Consumers()) {
          if (consumer->IsPhi()) {
            GetConditionalOutputs(consumer, conditional_outputs);
          } else {
            all_phi_consumers = false;
          }
        }

        ast::VariantPtr rhs;
        if (!all_phi_consumers || conditional_outputs.size() > 1) {
          if (ShouldInlineConstant(value)) {
            rhs = MakeArgPtr(value);
          } else {
            Emplace<ast::Declaration>(FormatTemporary(value),
                                      ast::ScalarType(value->DetermineType()), VisitMakePtr(value));
            rhs = MakeVariableRefPtr(value);
          }
        } else {
          rhs = VisitMakePtr(value);
        }

        for (ir::ValuePtr consumer : conditional_outputs) {
          phi_assignments.emplace_back(FormatTemporary(consumer), rhs);
        }
      }
    }

    std::sort(phi_assignments.begin(), phi_assignments.end(),
              [](const auto& a, const auto& b) { return a.left < b.left; });
    std::copy(std::make_move_iterator(phi_assignments.begin()),
              std::make_move_iterator(phi_assignments.end()), std::back_inserter(operations_));
    phi_assignments.clear();

    // insert outputs
    for (const ir::ValuePtr save_value : save_values) {
      const ir::Save& save = save_value->As<ir::Save>();
      const OutputKey key = save.key;

      std::vector<ast::Variant> args{};
      args.reserve(save_value->NumOperands());
      for (const ir::ValuePtr v : save_value->Operands()) {
        args.emplace_back(MakeVariableRef(v));
      }

      if (key.usage == ExpressionUsage::ReturnValue) {
        Emplace<ast::ConstructReturnValue>(
            key.arg_position, signature_.return_values.at(key.arg_position), std::move(args));
      } else {
        Emplace<ast::AssignOutputArgument>(signature_.arguments.at(key.arg_position),
                                           std::move(args));
      }
    }

    if (block->descendants.empty()) {
      return;
    }

    ir::ValuePtr last_op = block->operations.back();
    if (!last_op->Is<ir::JumpCondition>()) {
      // just keep appending:
      ASSERT_EQUAL(1, block->descendants.size());
      ProcessBlock(block->descendants[0]);
    } else {
      ASSERT(last_op->Is<ir::JumpCondition>());
      ASSERT_EQUAL(2, block->descendants.size());

      // Find phi functions:
      const ir::BlockPtr merge_point =
          FindMergePoint(block->descendants[0], block->descendants[1], SearchDirection::Downwards);
      stop_set_.insert(merge_point);

      for (ir::ValuePtr maybe_phi : merge_point->operations) {
        if (maybe_phi->IsPhi()) {
          const bool no_declaration =
              maybe_phi->AllConsumersSatisfy([](ir::ValuePtr v) { return v->IsPhi(); });
          if (no_declaration) {
            continue;
          }
          // We should declare this variable prior to entering the branch:
          Emplace<ast::Declaration>(FormatTemporary(maybe_phi),
                                    ast::ScalarType(maybe_phi->DetermineType()));
        }
      }

      // move aside the contents of this block, as we are about to descend the branches:
      std::vector<ast::Variant> operations_stashed = std::move(operations_);
      operations_.clear();

      // Process the true branch and save the output:
      ProcessBlock(block->descendants[0]);  //  append to operations_
      std::vector<ast::Variant> operations_true = std::move(operations_);
      operations_.clear();

      // Process the right branch
      ProcessBlock(block->descendants[1]);
      std::vector<ast::Variant> operations_false = std::move(operations_);
      operations_ = std::move(operations_stashed);  //  Put back operations for the current block.

      // Create a conditional
      Emplace<ast::Branch>(ast::VariableRef{FormatTemporary(last_op->Front())},
                           std::move(operations_true), std::move(operations_false));

      stop_set_.erase(merge_point);
      ProcessBlock(merge_point);
    }
  }

  std::string FormatTemporary(const ir::Value& val) const {
    return fmt::format("v{:0>{}}", val.Name(), value_width_);
  }

  std::string FormatTemporary(const ir::ValuePtr val) const { return FormatTemporary(*val); }

  ast::Variant VisitValue(ir::ValuePtr val) {
    return std::visit(
        [this, &val](const auto& op) -> ast::Variant {
          // These types should never get converted to AST:
          using T = std::decay_t<decltype(op)>;
          using ExcludedTypes = TypeList<ir::JumpCondition, ir::Save, ir::Cond, ir::Phi>;
          if constexpr (ContainsType<T, ExcludedTypes>) {
            throw TypeError("Type cannot be converted to AST: {}", typeid(T).name());
          } else {
            return this->operator()(*val, op);
          }
        },
        val->Op());
  }

  ast::VariantPtr VisitMakePtr(ir::ValuePtr val) {
    return std::make_shared<const ast::Variant>(VisitValue(val));
  }

  // Return true if the specified value should be inlined instead of declared as a variable.
  bool ShouldInlineConstant(const ir::ValuePtr val) const {
    return OverloadedVisit(
        val->Op(), [](const ir::Load& load) { return load.expr.Is<Integer, Float, Constant>(); },
        [&](const ir::Cast&) { return ShouldInlineConstant(val->Front()); },
        [](auto&&) constexpr { return false; });
  }

  ast::Variant MakeVariableRef(const ir::ValuePtr val) const {
    return ast::VariableRef{FormatTemporary(val)};
  }

  ast::VariantPtr MakeVariableRefPtr(const ir::ValuePtr val) const {
    return std::make_shared<const ast::Variant>(MakeVariableRef(val));
  }

  ast::Variant MakeArg(const ir::ValuePtr val) {
    if (ShouldInlineConstant(val)) {
      return VisitValue(val);
    }
    return MakeVariableRef(val);
  }

  ast::VariantPtr MakeArgPtr(const ir::ValuePtr val) {
    return std::make_shared<const ast::Variant>(MakeArg(val));
  }

  template <typename T, typename... Args>
  void Emplace(Args&&... args) {
    operations_.emplace_back(T{std::forward<Args>(args)...});
  }

  ast::Variant operator()(const ir::Value& val, const ir::Add&) {
    return ast::Add{MakeArgPtr(val[0]), MakeArgPtr(val[1])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::CallUnaryFunc& func) {
    return ast::Call{func.name, MakeArg(val[0])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Cast& cast) {
    return ast::Cast{cast.destination_type, MakeArgPtr(val[0])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Compare& compare) {
    return ast::Compare{compare.operation, MakeArgPtr(val[0]), MakeArgPtr(val[1])};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Copy&) { return MakeArg(val.Front()); }

  ast::Variant operator()(const ir::Value& val, const ir::Mul&) {
    return ast::Multiply{MakeArgPtr(val[0]), MakeArgPtr(val[1])};
  }

  ast::Variant operator()(const ir::Value&, const ir::Load& load) {
    return Visit(load.expr, [this](const auto& inner) -> ast::Variant {
      using T = std::decay_t<decltype(inner)>;
      if constexpr (std::is_same_v<T, Integer>) {
        return ast::IntegerConstant{inner.GetValue()};
      } else if constexpr (std::is_same_v<T, Float>) {
        return ast::FloatConstant{inner.GetValue()};
      } else if constexpr (std::is_same_v<T, Rational>) {
        return ast::FloatConstant{static_cast<Float>(inner).GetValue()};
      } else if constexpr (std::is_same_v<T, Variable>) {
        return ast::VariableRef{inner.GetName()};
      } else if constexpr (std::is_same_v<T, FunctionArgument>) {
        const auto element_index = static_cast<index_t>(inner.ElementIndex());
        return ast::InputValue{signature_.arguments[inner.ArgIndex()], element_index};
      } else {
        throw TypeError("Invalid type in code generation expression: {}", T::NameStr);
      }
    });
  }

  ast::Variant operator()(const ir::Value&, const ir::OutputRequired& oreq) {
    return ast::OutputExists{signature_.arguments[oreq.arg_position]};
  }

  ast::Variant operator()(const ir::Value& val, const ir::Pow&) {
    return ast::Call{BinaryFunctionName::Pow, MakeArg(val[0]), MakeArg(val[1])};
  }

 private:
  std::size_t value_width_;
  const ast::FunctionSignature& signature_;

  std::vector<ast::Variant> operations_;
  std::unordered_set<ir::BlockPtr> stop_set_;
};

namespace ast {
ast::FunctionDefinition CreateAST(const math::OutputIr& ir, const FunctionSignature& signature) {
  AstBuilder builder(ir.ValuePrintWidth(), signature);
  return builder.CreateFunction(ir.FirstBlock());
}
}  // namespace ast
}  // namespace math
