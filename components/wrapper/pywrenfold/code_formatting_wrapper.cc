// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/functional.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/code_generation/ast.h"
#include "wf/code_generation/cpp_code_generator.h"
#include "wf/code_generation/rust_code_generator.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

// Define the override method in the trampoline class:
// https://pybind11.readthedocs.io/en/stable/advanced/classes.html
#define IMPL_VIRTUAL_OPERATOR(Base, T)                                                       \
  virtual std::string operator()(const T& arg) const override {                              \
    static const std::string method_name = fmt::format("format_{}", T::snake_case_name_str); \
    PYBIND11_OVERRIDE_NAME(std::string, Base, method_name.c_str(), operator(), arg);         \
  }

// A trampoline class that inherits from a C++ code-generator, and allows pybind11 to check for
// derived implementations in python.
template <typename Base>
class generator_trampoline : public Base {
 public:
  using Base::Base;

  // Try as I might, I can't find a way around this ugly macro.
  // We can't iterate at compile time over the types, and using the multiple-base class trick
  // appears to cause UB with pybind11.
  IMPL_VIRTUAL_OPERATOR(Base, argument)
  IMPL_VIRTUAL_OPERATOR(Base, ast::add)
  IMPL_VIRTUAL_OPERATOR(Base, ast::assign_output_argument)
  IMPL_VIRTUAL_OPERATOR(Base, ast::assign_temporary)
  IMPL_VIRTUAL_OPERATOR(Base, ast::branch)
  IMPL_VIRTUAL_OPERATOR(Base, ast::call)
  IMPL_VIRTUAL_OPERATOR(Base, ast::cast)
  IMPL_VIRTUAL_OPERATOR(Base, ast::comment)
  IMPL_VIRTUAL_OPERATOR(Base, ast::compare)
  IMPL_VIRTUAL_OPERATOR(Base, ast::construct_matrix)
  IMPL_VIRTUAL_OPERATOR(Base, ast::construct_custom_type)
  IMPL_VIRTUAL_OPERATOR(Base, ast::declaration)
  IMPL_VIRTUAL_OPERATOR(Base, ast::divide)
  IMPL_VIRTUAL_OPERATOR(Base, ast::float_literal)
  IMPL_VIRTUAL_OPERATOR(Base, ast::function_definition)
  IMPL_VIRTUAL_OPERATOR(Base, ast::function_signature2)
  IMPL_VIRTUAL_OPERATOR(Base, ast::integer_literal)
  IMPL_VIRTUAL_OPERATOR(Base, ast::multiply)
  IMPL_VIRTUAL_OPERATOR(Base, ast::negate)
  IMPL_VIRTUAL_OPERATOR(Base, ast::optional_output_branch)
  IMPL_VIRTUAL_OPERATOR(Base, ast::read_input_matrix)
  IMPL_VIRTUAL_OPERATOR(Base, ast::read_input_scalar)
  IMPL_VIRTUAL_OPERATOR(Base, ast::read_input_struct)
  IMPL_VIRTUAL_OPERATOR(Base, ast::return_type_annotation)
  IMPL_VIRTUAL_OPERATOR(Base, ast::return_value)
  IMPL_VIRTUAL_OPERATOR(Base, ast::special_constant)
  IMPL_VIRTUAL_OPERATOR(Base, ast::variable_ref)
};

// Convert std::variant to type list.
template <typename T>
struct type_list_from_variant;
template <typename T>
using type_list_from_variant_t = typename type_list_from_variant<T>::type;
template <typename... Ts>
struct type_list_from_variant<std::variant<Ts...>> {
  using type = type_list<Ts...>;
};

// Create a combined type list of all types that can be overriden
using all_wrapped_ast_types =
    concatenate_type_lists_t<type_list_from_variant_t<ast::variant>,
                             type_list<argument, ast::function_definition, ast::function_signature2,
                                       ast::return_type_annotation>>;

// This struct expands over all the types in `ast::variant` and exposes operator() for
// all of them via pybind11.
template <typename T = all_wrapped_ast_types>
struct register_operators_struct;
template <typename... Ts>
struct register_operators_struct<type_list<Ts...>> {
  template <typename T, typename PyClass>
  static void register_operator(PyClass& klass) {
    // Static const so that we are certain pybind11 isn't taking an invalid weak reference here.
    static const std::string method = fmt::format("format_{}", T::snake_case_name_str);
    static const std::string doc = fmt::format("Format ast type `{}`.", T::snake_case_name_str);
    // Expose operator() on the generator.
    // underlying_type will be cpp_code_generator, rust_code_generator, etc...
    using underlying_type = typename PyClass::type;
    klass.def(
        method.c_str(),
        [](const underlying_type& self, const T& arg) -> std::string { return self(arg); },
        py::arg("element"), py::doc(doc.c_str()));
    // We also register under an overloaded name:
    klass.def(
        "format",
        [](const underlying_type& self, const T& arg) -> std::string { return self(arg); },
        py::arg("element"), py::doc(doc.c_str()));
  }

  template <typename PyClass>
  void operator()(PyClass& klass) {
    // Expand over all the types `Ts`:
    (register_operator<Ts, PyClass>(klass), ...);
  }
};

// Generate multiple definitions.
template <typename Generator>
std::string generate_multiple(const Generator& generator,
                              const std::vector<ast::function_definition>& definitions) {
  if (definitions.empty()) {
    return "";
  }
  auto it = definitions.begin();
  std::string output = std::invoke(generator, *it);
  for (++it; it != definitions.end(); ++it) {
    output.append("\n\n");
    output += std::invoke(generator, *it);
  }
  return output;
}

// Wrap generator of type `T`.
template <typename T, typename... CtorArgs>
static auto wrap_code_generator(py::module_& m, const std::string_view name) {
  py::class_ klass =
      py::class_<T, generator_trampoline<T>>(m, name.data())
          .def(py::init<CtorArgs...>())
          .def(
              "generate",
              [](const T& self, const ast::function_definition& definition) {
                return std::invoke(self, definition);
              },
              py::arg("definition"), py::doc("Generate code for the provided definition."))
          .def("generate", &generate_multiple<T>, py::arg("definitions"),
               py::doc("Generate code for multiple definitions."));
  // Wrap all the operator() methods.
  register_operators_struct{}(klass);
  return klass;
}

// Search all the types [T, Ts...] to see if one of them matches `type`.
// If `T` matches `type`, we return its type_index. Otherwise, we continue the search.
// TODO: Cache this result instead of doing linear search.
template <typename T, typename... Ts>
bool is_formattable_type(const py::type& type) {
  if (const py::type candidate = py::type::of<T>(); type.is(candidate)) {
    return true;
  }
  if constexpr (sizeof...(Ts) > 0) {
    return is_formattable_type<Ts...>(type);
  } else {
    return false;
  }
}
// Unpack the variadic std::variant template and invoke `is_formattable_type`.
template <typename T = all_wrapped_ast_types>
struct is_formattable_type_struct;
template <typename... Ts>
struct is_formattable_type_struct<type_list<Ts...>> {
  auto operator()(const py::type& type) const { return is_formattable_type<Ts...>(type); }
};

void wrap_code_formatting_operations(py::module_& m) {
  wrap_code_generator<cpp_code_generator>(m, "CppGenerator");
  wrap_code_generator<rust_code_generator>(m, "RustGenerator");
  m.def(
      "is_formattable_type",
      [](const py::type& type) { return is_formattable_type_struct<>{}(type); }, py::arg("t"),
      py::doc("Check if the provided type is formattable with a code generator."));
}

}  // namespace wf
