// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <nanobind/nanobind.h>
#include <nanobind/stl/function.h>
#include <nanobind/stl/string.h>
#include <nanobind/stl/string_view.h>
#include <nanobind/stl/vector.h>

#include "docs/code_formatting_wrapper.h"
#include "wf/code_generation/ast.h"
#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/cpp_code_generator.h"
#include "wf/code_generation/python_code_generator.h"
#include "wf/code_generation/rust_code_generator.h"
#include "wf/utility/type_list.h"

namespace py = nanobind;
using namespace py::literals;

namespace wf {

// clang-format off
#define WF_APPLY_MACRO_TO_FORMATTABLE_TYPES(stamp) \
  stamp(ast::add) \
  stamp(ast::assign_temporary) \
  stamp(ast::assign_output_matrix) \
  stamp(ast::assign_output_scalar) \
  stamp(ast::assign_output_struct) \
  stamp(ast::boolean_literal) \
  stamp(ast::branch) \
  stamp(ast::call_external_function) \
  stamp(ast::call_std_function) \
  stamp(ast::cast) \
  stamp(ast::comment) \
  stamp(ast::compare) \
  stamp(ast::construct_custom_type) \
  stamp(ast::construct_matrix) \
  stamp(ast::declaration) \
  stamp(ast::divide) \
  stamp(ast::float_literal) \
  stamp(ast::get_argument) \
  stamp(ast::get_field) \
  stamp(ast::get_matrix_element) \
  stamp(ast::integer_literal) \
  stamp(ast::multiply) \
  stamp(ast::negate) \
  stamp(ast::optional_output_branch) \
  stamp(ast::parenthetical) \
  stamp(ast::special_constant) \
  stamp(ast::return_object) \
  stamp(ast::ternary) \
  stamp(ast::variable_ref) \
  stamp(ast::function_definition) \
  stamp(ast::function_signature) \
  stamp(custom_type) \
  stamp(matrix_type) \
  stamp(scalar_type)  // clang-format on

// Macro used to declare an override for a formattable type.
// We invoke maybe_override, which is part of wrapper_generator<T>
#define WF_CALL_MAYBE_OVERRIDE(T) \
  std::string operator()(const T& element) const override final { return maybe_override(element); }

// Macro used to declare methods on `base_code_generator`.
#define WF_DECLARE_UNIMPLEMENTED(T)                                                          \
  virtual std::string operator()(const T&) const {                                           \
    throw type_error("Missing override for type `{}`: format_{}", ast::camel_case_name<T>(), \
                     T::snake_case_name_str);                                                \
  }

// pybind11 has issues with combining recursion and inheritance.
// For example: https://github.com/pybind/pybind11/issues/1552
//
// NOTE: Unclear if this issue above persists after the switch to nanobind, but I am unlikely
// to change this implementation too much without good cause.
//
// The root of the issue AFAIK is that pybind determines whether to call `self.method` or
// `super().method` by inspecting the python stack in order to check if the overriden method is
// calling itself. This makes formatting a recursive structure somewhat tricky. For instance,
// consider the expression `cos(sqrt(x))`. This entails the `format_call` function calling itself in
// order to obtain the formatted argument to `cos`. nanobind incorrectly detects this as attempt to
// call the super method in C++, rather than respecting the user override. This behavior appears to
// differ in `ipdb` vs regular python (maybe since the call stack is altered by the presence of the
// debugger).
//
// To work around this, we manually check for overrides (in a similar manner to nanobind, by using
// getattr). Rather than trying to automatically select `super` or `self`, we expose both methods
// under different names.
template <typename Base>
class wrapped_generator : public Base {
 public:
  using generator_base = Base;

  template <typename... CtorArgs>
  explicit wrapped_generator(CtorArgs&&... args) : Base(std::forward<CtorArgs>(args)...) {}

  // Implement all the virtual methods from `Base`:
  WF_APPLY_MACRO_TO_FORMATTABLE_TYPES(WF_CALL_MAYBE_OVERRIDE)

  // Check if a python class derived from this one implements a formatting operator for type `T`.
  // If it does, we call the derived class implementation. Otherwise, call Base::operator().
  //
  // This method is loosely inspired by nanobind get_override.
  template <typename T>
  std::string maybe_override(const T& element) const {
    /* scope for acquiring the GIL */ {
      py::gil_scoped_acquire gil;
      if (const py::object func = get_override<T>(); static_cast<bool>(func) && !func.is_none()) {
        try {
          using std_function = std::function<std::string(const T&)>;
          const std_function typed_func = py::cast<std_function>(func);
          return typed_func(element);
        } catch (const py::cast_error& err) {
          throw type_error(
              "Failed while casting formatter `format_{snake}` to std::function<std::string(const "
              "{snake}&)>. The python method should have the signature: format_{snake}(element: "
              "{camel}) -> str\n"
              "nanobind error: {err}",
              fmt::arg("snake", T::snake_case_name_str),
              fmt::arg("camel", ast::camel_case_name<T>()), fmt::arg("err", err.what()));
        }
      }
    }
    // Fall back to base class implementation.
    return static_cast<const Base&>(*this).Base::operator()(element);
  }

  // Invoke the base class operator() and check for infinite recursions.
  template <typename T>
  std::string invoke_with_guard(const T& element) const {
    recursion_guard guard{recursions_, element};
    return static_cast<const Base*>(this)->operator()(element);
  }

 private:
  // Search the derived class for a formatting method that accepts type `T`.
  // Return it as a py::object (which will be empty if we cannot find an appropriate method).
  // First check the cache to see if we can shortcut to None if the attribute does not exist.
  template <typename T>
  py::object get_override() const {
    if (const auto it = has_override_.find(typeid(T)); it != has_override_.end()) {
      if (it->second) {
        return getattr_override<T>();
      } else {
        return py::none();
      }
    }
    py::object override = getattr_override<T>();
    has_override_.emplace(typeid(T), static_cast<bool>(override));
    return override;
  }

  // Check if our derived type has an appropriate override for type `T`.
  // If it does, stash it in our cache. If not, place an empty entry into the cache so that we do
  // not repeat this work.
  template <typename T>
  py::object getattr_override() const {
    static const std::string method_name = fmt::format("format_{}", T::snake_case_name_str);
    const py::object maybe_method = py::getattr(py::cast(*this, nanobind::rv_policy::reference),
                                                method_name.c_str(), py::none());
    return maybe_method;
  }

  // We need mutable here because operator() is const in the C++ classes.
  // This should be thread safe since the GIL prevents access from multiple threads.
  // I'd prefer to save weak handles to the underlying functions, but I can't quite figure out how
  // to do that in nanobind without hitting invalid access. For now just cache whether the override
  // exists.
  mutable std::unordered_map<std::type_index, bool> has_override_;

  // Store the recursion count for each type.
  using recursion_counters = std::unordered_map<std::type_index, int32_t>;
  mutable recursion_counters recursions_;

  // Because we know the ast tree is not very deep, we can catch infinite recursions by counting
  // recursions and throwing when we pass a relatively low threshold. This type increases the count
  // on construction, and decreases it on destruction.
  class recursion_guard {
   public:
    template <typename T>
    explicit recursion_guard(recursion_counters& counters, const T&)
        : counter_(counters[typeid(T)]) {
      if (constexpr std::int32_t max_recursions = 32; counter_ + 1 == max_recursions) {
        throw std::runtime_error(
            fmt::format("Recursed {} times while formatting type `{}`. It is possible that a "
                        "formatting override specified in python is calling itself indirectly. "
                        "Instead of calling format(), you likely intended to call super_format().",
                        max_recursions, ast::camel_case_name<T>()));
      }
      ++counter_;
    }

    ~recursion_guard() { counter_ = std::max(counter_ - 1, 0); }

   private:
    std::int32_t& counter_;
  };
};

// Declare a base code generator. By default it does no formatting at all, and throws on every type
// we give it. The user is responsible for implementing all formatting methods in python.
// ReSharper disable once CppClassCanBeFinal
class base_code_generator {
 public:
  virtual ~base_code_generator() = default;
  WF_APPLY_MACRO_TO_FORMATTABLE_TYPES(WF_DECLARE_UNIMPLEMENTED);
};

template <typename T>
struct register_one_format_operator {
  // PyClass is the py::class_ type.
  template <typename PyClass>
  void operator()(PyClass& klass, const std::string_view module_name) const {
    // Static const so that we are certain nanobind isn't taking an invalid weak reference here.
    static const std::string doc =
        fmt::format("Format type :class:`wrenfold.{}.{}`.", module_name, ast::camel_case_name<T>());
    static const std::string super_doc = doc + " Invokes the wrapped base class implementation.";

    // Expose operator() on the generator.
    // underlying_base_type will be cpp_code_generator, etc...
    using wrapped_type = typename PyClass::Type;
    using underlying_base_type = typename wrapped_type::generator_base;

    // Register under an overloaded name.
    // This is so we can just call format(...) in python, and have nanobind automatically
    // do the overload resolution.
    klass.def(
        "format",
        [](const wrapped_type& self, const T& arg) -> std::string {
          return self.invoke_with_guard(arg);
        },
        py::arg("element"), doc.c_str());

    // super_format always calls the base class implementation.
    klass.def(
        "super_format",
        [](const wrapped_type& self, const T& arg) -> std::string {
          return self.underlying_base_type::operator()(arg);
        },
        py::arg("element"), super_doc.c_str());
  }
};

// This struct expands over all the types in `ast::ast_element` and exposes operator() for
// all of them via nanobind.
template <typename T>
struct register_all_format_operators;
template <typename... Ts>
struct register_all_format_operators<type_list<Ts...>> {
  template <typename PyClass>
  void operator()(PyClass& klass, const std::string_view module_name) {
    // Expand over all the types `Ts`:
    (register_one_format_operator<Ts>{}(klass, module_name), ...);
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
  std::string output = generator(*it);
  for (++it; it != definitions.end(); ++it) {
    output.append("\n\n");
    output += generator(*it);
  }
  return output;
}

// Wrap generator of type `T`.
template <typename T, typename... CtorArgs>
static auto wrap_code_generator(py::module_& m, const std::string_view name) {
  py::class_ klass =
      py::class_<wrapped_generator<T>>(m, name.data())
          .def(
              "generate",
              [](const wrapped_generator<T>& self, const ast::function_definition& definition) {
                return static_cast<const T&>(self)(definition);
              },
              py::arg("definition"), "Generate code for the provided definition.")
          .def(
              "generate",
              [](const wrapped_generator<T>& self,
                 const std::vector<ast::function_definition>& definitions) {
                return generate_multiple(static_cast<const T&>(self), definitions);
              },
              py::arg("definition"), "Generate code for multiple definitions.");

  // Wrap all the operator() methods.
  register_all_format_operators<
      type_list_concatenate_t<ast::ast_element_types, type_list<ast::function_signature>>>{}(klass,
                                                                                             "ast");
  register_all_format_operators<type_list_from_variant_t<type_variant>>{}(klass, "type_info");
  return klass;
}

void wrap_code_formatting_operations(py::module_& m) {
  py::enum_<cpp_matrix_type_behavior>(m, "CppMatrixTypeBehavior")
      .value(
          "GenericSpan", cpp_matrix_type_behavior::generic_span,
          "Generate functions that accept generic/templated arguments for which a specialization "
          "of ``wf::convert_to_span`` is defined.")
      .value("Eigen", cpp_matrix_type_behavior::eigen,
             "Generate functions that accept and return Eigen::Matrix.");

  wrap_code_generator<cpp_code_generator>(m, "CppGenerator")
      .def(py::init<cpp_matrix_type_behavior>(),
           py::arg("matrix_args") = cpp_matrix_type_behavior::generic_span)
      .def("apply_preamble", &cpp_code_generator::apply_preamble, py::arg("code"),
           py::arg("namespace"), py::arg("imports") = "",
           "Apply a preamble that incorporates necessary runtime includes.")
      .def_prop_ro("matrix_type_behavior",
                   [](const wf::wrapped_generator<cpp_code_generator>& self) {
                     return self.matrix_type_behavior();
                   })
      .doc() = "Generates C++ code.";

  wrap_code_generator<rust_code_generator>(m, "RustGenerator")
      .def(py::init<>())
      .def_static("apply_preamble", &rust_code_generator::apply_preamble, py::arg("code"),
                  "Apply a preamble to generated code.")
      .doc() = "Generates Rust code.";

  wrap_code_generator<base_code_generator>(m, "BaseGenerator").def(py::init<>()).doc() =
      "Abstract base class for generators. The user may inherit from this in python when writing "
      "a new generator from scratch.";

  py::enum_<python_generator_target>(m, "PythonGeneratorTarget")
      .value("NumPy", python_generator_target::numpy, "Target the NumPy API.")
      .value("PyTorch", python_generator_target::pytorch, "Target the PyTorch API.")
      .value("JAX", python_generator_target::jax, "Target the JAX API.");

  py::enum_<python_generator_float_width>(m, "PythonGeneratorFloatWidth")
      .value("Float32", python_generator_float_width::float32,
             "Float arrays/tensors will be interpreted as float32.")
      .value("Float64", python_generator_float_width::float64,
             "Float arrays/tensors will be interpreted as float64.");

  wrap_code_generator<python_code_generator>(m, "PythonGenerator")
      .def(py::init<python_generator_target, python_generator_float_width, int, bool>(),
           py::arg("target") = python_generator_target::numpy,
           py::arg("float_width") = python_generator_float_width::float64,
           py::arg("indentation") = 4, py::arg("use_output_arguments") = false,
           docstrings::python_generator_constructor.data())
      .def_prop_ro(
          "target",
          [](const wf::wrapped_generator<python_code_generator>& self) { return self.target(); },
          "The API that the python generator targets.")
      .def_prop_ro(
          "float_width",
          [](const wf::wrapped_generator<python_code_generator>& self) {
            return self.float_width();
          },
          "Float precision applied to all NumPy arrays and tensors.")
      .def_prop_ro(
          "indentation",
          [](const wf::wrapped_generator<python_code_generator>& self) {
            return self.indentation();
          },
          "Amount of spaces used to indent nested scopes.")
      .def("apply_preamble", &python_code_generator::apply_preamble, py::arg("code"),
           "Apply a preamble to generated code.")
      .doc() = "Generates Python code. Can target NumPy, PyTorch, or JAX.";
}

}  // namespace wf
