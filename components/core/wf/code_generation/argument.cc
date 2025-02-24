// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/argument.h"

#include "wf/code_generation/type_registry.h"

namespace wf {

argument::argument(const std::string_view name, type_variant type,
                   const argument_direction direction, const std::size_t index)
    : impl_(std::make_shared<const impl>(
          impl{std::string(name), std::move(type), direction, index})) {}

std::variant<scalar_expr, matrix_expr, compound_expr> argument::create_symbolic_input() const {
  return std::visit(
      [&](const auto& type_concrete) -> std::variant<scalar_expr, matrix_expr, compound_expr> {
        return detail::create_function_input(type_concrete, name());
      },
      type());
}

std::size_t hash_struct<argument>::operator()(const argument& arg) const noexcept {
  std::size_t hash = hash_string_fnv(arg.name());
  hash = hash_args(hash, arg.type());
  hash = hash_combine(hash, static_cast<std::size_t>(arg.direction()));
  return hash_combine(hash, arg.index());
}

bool is_identical_struct<argument>::operator()(const argument& a, const argument& b) const {
  if (a.has_same_address(b)) {
    return true;
  }
  return a.name() == b.name() && are_identical(a.type(), b.type()) &&
         are_identical(a.direction(), b.direction()) && are_identical(a.index(), b.index());
}

}  // namespace wf
