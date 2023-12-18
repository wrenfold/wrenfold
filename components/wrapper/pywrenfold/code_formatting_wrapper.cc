// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/functional.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/code_generation/ast.h"
#include "wf/code_generation/cpp_code_generator.h"
#include "wf/code_generation/rust_code_generator.h"

#include <wf/code_generation/function_definition.h>

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

// Unpack the variadic std::variant template and invoke `match_ast_type`.
template <typename T>
struct match_ast_type_struct;
template <typename... Ts>
struct match_ast_type_struct<std::variant<Ts...>> {
  auto operator()(const py::type& type) const { return match_ast_type<Ts...>(type); }
};

// Wrap a C++ generator class, and allow overriding formatting logic of specific types from
// python. We maintain a mapping from std::type_index --> std::function, where the std::function is
// a callable object that pybind11 gave us. The type_index corresponds to one of the members of
// ast::variant. Whenever we format an element of syntax, we check if a pyhon-defined override has
// been specified for that particular type. If it has, we delegate to the user python code -
// otherwise the C++ implemenetation is invoked. Because we have overriden `Generator::apply`, any
// formatting calls made by the base class can be intercepted. This does unfortunately mean that
// C++ can call into python, which calls into C++, etc...
template <typename Generator>
class wrapped_generator : public Generator {
 public:
  using handler_type = std::function<std::string(py::object)>;

  // Construct base type with the provided arguments.
  template <typename... Args>
  explicit wrapped_generator(Args&&... args) : Generator(std::forward<Args>(args)...) {}

  // Register a handler for a particular type.
  void register_handler(const py::type& type, const handler_type& function) {
    // Determine which member of ast::variant is being overriden here.
    if (const std::optional<std::type_index> type_index =
            match_ast_type_struct<ast::variant>{}(type);
        type_index.has_value()) {
      if (auto [it, was_inserted] = custom_handlers_.emplace(*type_index, function);
          !was_inserted) {
        it->second = function;
      }
    } else {
      const py::str repr = py::repr(type);
      throw type_error("Provided type `{}` is not a valid ast type.",
                       py::cast<std::string_view>(repr));
    }
  }

  // Discard all handlers.
  // This is done explicitly to make sure pybind doesn't hold any strong references
  void clear_handlers() { custom_handlers_.clear(); }

  // Override the C++ implementation, and call the user-provided custom handler.
  std::string apply(const ast::variant& var) const final {
    if (custom_handlers_.empty()) {
      return std::visit(static_cast<const Generator&>(*this), var);
    }
    return std::visit(
        [&](const auto& x) -> std::string {
          using T = std::decay_t<decltype(x)>;
          if (const auto it = custom_handlers_.find(typeid(T)); it != custom_handlers_.end()) {
            // Delegate to python formatter.
            // Doing a copy here feels a bit wasteful, but it is difficult to reason about what the
            // user callback might do with the provided object. Note that the default policy of
            // py::cast is to create a weak reference:
            //  https://github.com/pybind/pybind11/issues/287#issuecomment-233373721
            return it->second(py::cast(x, py::return_value_policy::copy));
          } else {
            // Call the base class implementation directly.
            return static_cast<const Generator&>(*this)(x);
          }
        },
        var);
  }

  // Given a function definition, return the generated code as a string.
  std::string generate(const function_definition::shared_ptr& definition) const {
    WF_ASSERT(definition);
    return Generator::generate_code(definition->signature(), definition->ast());
  }

 private:
  std::unordered_map<std::type_index, handler_type> custom_handlers_;
};

// This struct expands over all the types in `ast::variant` and exposes operator() for
// all of them via pybind11.
template <typename T = ast::variant>
struct register_ast_operators_struct;
template <typename... Ts>
struct register_ast_operators_struct<std::variant<Ts...>> {
  template <typename T, typename PyClass>
  static void register_operator(PyClass& klass) {
    // TODO: Get a pretty type name here.
    // Static const so that we are certain pybind11 isn't taking an invalid weak reference here.
    static const std::string format = fmt::format("Format ast type `{}`.", typeid(T).name());
    // Expose operator() on the generator type under the name `apply`.
    // Note that we call the concrete non-virtual implementation here, which cannot immediately
    // recurse back into python.
    using underlying_type = typename PyClass::type;
    const auto ptr =
        static_cast<std::string (underlying_type::*)(const T&) const>(&underlying_type::operator());
    klass.def("apply", ptr, py::arg("a"), py::doc(format.c_str()));
  }

  template <typename PyClass>
  void operator()(PyClass& klass) {
    // Expand over all the types `Ts`:
    (register_operator<Ts, PyClass>(klass), ...);
  }
};

// Wrap generator of type `T`.
template <typename T, typename... CtorArgs>
static auto wrap_code_generator(py::module_& m, const std::string_view name) {
  using wrapped_generator = wrapped_generator<T>;
  py::class_ gen =
      py::class_<wrapped_generator>(m, name.data())
          .def(py::init<CtorArgs...>())
          .def("register_handler", &wrapped_generator::register_handler, py::arg("t"),
               py::arg("function"), py::doc("Register a custom handler for a particular ast type."))
          .def("clear_handlers", &wrapped_generator::clear_handlers,
               py::doc("Destroy strong references to python lambdas/functions by removing all "
                       "handlers."))
          .def("generate", &wrapped_generator::generate, py::arg("definition"),
               py::doc("Generate code for the provided definition."));
  // Wrap all the operator() methods.
  register_ast_operators_struct{}(gen);
  return gen;
}

void wrap_code_formatting_operations(py::module_& m) {
  wrap_code_generator<cpp_code_generator>(m, "CppGenerator");
  wrap_code_generator<rust_code_generator>(m, "RustGenerator");
}

}  // namespace wf
