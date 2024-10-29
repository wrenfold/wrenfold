// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/external_function.h"

#include "wf/code_generation/ast_formatters.h"
#include "wf/expression_visitor.h"
#include "wf/utility/overloaded_visit.h"

namespace wf {

std::shared_ptr<const external_function::impl> external_function::impl::create(
    std::string name, std::vector<argument> arguments, type_variant return_type) {
  impl result{std::move(name), std::move(arguments), std::move(return_type), 0};
  result.hash = hash_string_fnv(result.name);
  result.hash = hash_all(result.hash, result.arguments);
  result.hash = hash_args(result.hash, result.return_type);
  return std::make_shared<impl>(std::move(result));
}

static void assert_argument_names_are_unique(const std::vector<argument>& args) {
  if (args.empty()) {
    return;
  }
  auto names = transform_map<absl::InlinedVector<std::string_view, 8>>(
      args, [](const argument& arg) -> std::string_view { return arg.name(); });
  std::sort(names.begin(), names.end());

  for (auto it = names.begin(); std::next(it) != names.end(); ++it) {
    WF_ASSERT(*it != *std::next(it), "External function has duplicated argument name: {}", *it);
  }
}

external_function::external_function(std::string name, std::vector<argument> arguments,
                                     type_variant return_type)
    : impl_(impl::create(std::move(name), std::move(arguments), std::move(return_type))) {
  // Check that argument names are unique:
  assert_argument_names_are_unique(impl_->arguments);
}

std::optional<std::size_t> external_function::arg_position(const std::string_view name) const {
  if (const auto it = std::find_if(impl_->arguments.begin(), impl_->arguments.end(),
                                   [&](const argument& arg) { return arg.name() == name; });
      it != impl_->arguments.end()) {
    return std::distance(impl_->arguments.begin(), it);
  }
  return std::nullopt;
}

// Resolve the actual type of a compound expression:
static type_variant get_compound_type(const compound_expr& expr) {
  return visit(expr, make_overloaded(
                         [](const external_function_invocation& invocation) -> type_variant {
                           return invocation.function().return_type();
                         },
                         [](const custom_type_argument& arg) -> type_variant { return arg.type(); },
                         [](const custom_type_construction& construct) -> type_variant {
                           return construct.type();
                         }));
}

template <typename A, typename B>
bool types_match(const A& a, const B& b) {
  if constexpr (std::is_same_v<A, B>) {
    return are_identical(a, b);
  } else {
    return false;
  }
}

any_expression external_function::create_invocation(std::vector<any_expression> args) const {
  WF_ASSERT_EQ(num_arguments(), args.size(), "Wrong number of arguments for function `{}`.",
               name());

  for (std::size_t i = 0; i < args.size(); ++i) {
    // Determine what type we were passed:
    const type_variant passed_type = overloaded_visit(
        args[i],
        [&](const scalar_expr&) -> type_variant {
          // TODO: Check numeric type here.
          return scalar_type(numeric_primitive_type::floating_point);
        },
        [&](const boolean_expr&) -> type_variant {
          return scalar_type(numeric_primitive_type::boolean);
        },
        [&](const matrix_expr& matrix) -> type_variant {
          return matrix_type(matrix.rows(), matrix.cols());
        },
        [&](const compound_expr& expr) { return get_compound_type(expr); });

    // Now we need to check this against the expected type:
    std::visit(
        [&](const auto& expected, const auto& actual) {
          if (!types_match(expected, actual)) {
            throw type_error(
                "Argument `{}` of function `{}` expects expression of type {} but was provided "
                "with {}.",
                impl_->arguments[i].name(), name(), expected, actual);
          }
        },
        impl_->arguments[i].type(), passed_type);
  }

  // Create invocation expression:
  compound_expr invocation{std::in_place_type_t<external_function_invocation>{}, *this,
                           std::move(args)};

  // Formulate an expression that matches our return type:
  return overloaded_visit(
      return_type(),
      [&](const scalar_type) -> any_expression {
        return scalar_expr(std::in_place_type_t<compound_expression_element>{},
                           std::move(invocation), 0);
      },
      [&](const matrix_type& mat) -> any_expression {
        return matrix_expr::create(mat.rows(), mat.cols(),
                                   create_expression_elements(invocation, mat.size()));
      },
      [&](const custom_type&) -> any_expression { return std::move(invocation); });
}

bool is_identical_struct<external_function>::operator()(const external_function& a,
                                                        const external_function& b) const noexcept {
  if (a.has_same_address(b)) {
    return true;
  }
  return a.name() == b.name() && are_identical(a.arguments(), b.arguments()) &&
         are_identical(a.return_type(), b.return_type());
}

}  // namespace wf
