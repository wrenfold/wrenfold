// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/code_generation/ast.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

// Search all the types [T, Ts...] to see if one of them matches `type`.
// If `T` matches `type`, we return its type_index. Otherwise, we continue the search.
template <typename T, typename... Ts>
std::optional<std::type_index> match_ast_type(const py::type& type) {
  if (const py::type candidate = py::type::of<T>(); type.is(candidate)) {
    return std::type_index{typeid(T)};
  }
  if constexpr (sizeof...(Ts) > 0) {
    return match_ast_type<Ts...>(type);
  } else {
    return std::nullopt;
  }
}

template <typename T>
struct match_ast_type_struct;
template <typename... Ts>
struct match_ast_type_struct<std::variant<Ts...>> {
  auto operator()(const py::type& type) const { return match_ast_type<Ts...>(type); }
};

//
template <typename Generator>
class wrapped_code_generator {
 public:
  using handler_type = std::function<void()>;

  // Register a handler
  void register_handler(const py::type& type, const handler_type& function) {
    std::optional<std::type_index> type_index = match_ast_type_struct<ast::variant>{}(type);
    if (type_index.has_value()) {
      auto [it, was_inserted] = custom_handlers_.emplace(*type_index, function);
      if (!was_inserted) {
        it->second = function;
      }
    } else {
      throw type_error("Provided type `{}` is not a valid ast type.", py::repr(type));
    }
  }

  void apply(int formatter, const ast::variant& var) const {
    py::weakref std::visit(
        [&](const auto& x) {
          using T = std::decay_t<decltype(x)>;
          if (auto it = custom_handlers_.find(typeid(T)); it != custom_handlers_.end()) {
            // Delegate to python:
          } else {
            //
          }
        },
        var);
  }

 private:
  Generator generator_;

  std::unordered_map<std::type_index, handler_type> custom_handlers_;
};

void wrap_code_formatting_operations(py::module_&) {}

}  // namespace wf
