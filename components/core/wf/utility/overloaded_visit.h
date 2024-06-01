// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <variant>

namespace wf {

namespace detail {
template <class... Ts>
struct overloaded_struct : Ts... {
  using Ts::operator()...;
};

// A user-defined deduction guide for `overloaded_struct`.
template <class... Ts>
overloaded_struct(Ts...) -> overloaded_struct<Ts...>;
}  // namespace detail

// Create a struct w/ multiple operator() methods (determined by `funcs`).
template <typename... Funcs>
auto make_overloaded(Funcs&&... funcs) {
  return detail::overloaded_struct{std::forward<Funcs>(funcs)...};
}

// Visit a variant w/ the lambdas `funcs`. Whichever lambda matches the variant type is invoked.
// If the last lambda is `auto`, it will match any remaining types.
template <typename... Funcs, typename Variant>
auto overloaded_visit(Variant&& var, Funcs&&... funcs) {
  return std::visit(make_overloaded(std::forward<Funcs>(funcs)...), std::forward<Variant>(var));
}

}  // namespace wf
