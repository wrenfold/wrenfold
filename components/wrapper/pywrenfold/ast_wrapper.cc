// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <any>

#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/code_generation/ast_conversion.h"
#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/ast_visitor.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/types/span.h>
WF_END_THIRD_PARTY_INCLUDES

namespace py = pybind11;
using namespace py::literals;

// We make this type opaque and wrap it manually below. That way we can cast elements to their
// underlying type in the `next()` impl, and attach lifetime of iterators to the vector using
// py::keep_alive.
using ast_element_span = absl::Span<const wf::ast::ast_element>;
PYBIND11_MAKE_OPAQUE(ast_element_span)

namespace wf {

// Construct class_ wrapper for an AST type. Name is derived automatically.
template <typename T>
auto wrap_ast_type(py::module_& m) {
  auto klass = py::class_<T>(m, ast::camel_case_name<T>());
  if constexpr (ast::is_formattable<T>::value) {
    klass.def("__repr__", [](const T& obj) -> std::string {
      // Handled by the formatters in ast_formatters.h
      return fmt::format("{}", obj);
    });
  }
  return klass;
}

// A std::variant<> over const-pointers to types in `ast::all_ast_types`.
using variant_of_ast_element_ptrs = variant_from_type_list_t<
    type_list_map_t<std::add_pointer_t, type_list_map_t<std::add_const_t, ast::ast_element_types>>>;

// Cast to a variant so we can return this into python and generate correct type annotations.
// We cast with `reference` policy, which is going to create a weak reference to `x`.
auto to_inner(const ast::ast_element& element) {
  return wf::ast::visit(element, [](const auto& x) -> variant_of_ast_element_ptrs { return &x; });
}

// Create a lambda that accepts `StructType` and retrieves `member`, then invokes `to_inner` to
// convert that member to a handle.
template <typename StructType>
auto to_inner_accessor(ast::ast_element StructType::*member) {
  return [member](const StructType& self) { return to_inner(self.*member); };
}

// Iterator that converts vector elements to their inner type.
class ast_span_iterator {
 public:
  static ast_span_iterator begin(const ast_element_span& vec) noexcept {
    return ast_span_iterator(vec.begin());
  }
  static ast_span_iterator end(const ast_element_span& vec) noexcept {
    return ast_span_iterator(vec.end());
  }

  bool operator==(const ast_span_iterator& other) const noexcept { return it_ == other.it_; }

  // Pre-increment.
  auto& operator++() noexcept {
    ++it_;
    return *this;
  }

  auto operator*() const { return to_inner(*it_); }

 private:
  explicit ast_span_iterator(const ast_element_span::const_iterator& it) noexcept : it_(it) {}

  ast_element_span::const_iterator it_;
};

class ast_type_map {
 public:
  // Declare wrappers for all the AST types, and insert them into `wrappers_`.
  explicit ast_type_map(py::module_& m) { initialize(m, ast::ast_element_types{}); }

  // Access the wrapper for type `T`.
  template <typename T>
  py::class_<T>& at() {
    const auto it = wrappers_.find(typeid(T));
    WF_ASSERT(it != wrappers_.end(), "Missing wrapper for type: {}", typeid(T).name());
    return std::any_cast<py::class_<T>&>(it->second);
  }

 private:
  template <typename... Ts>
  void initialize(py::module_& m, type_list<Ts...>) {
    wrappers_.reserve(sizeof...(Ts));
    (wrappers_.emplace(typeid(Ts), wrap_ast_type<Ts>(m)), ...);
  }

  std::unordered_map<std::type_index, std::any> wrappers_;
};

void wrap_ast(py::module_& m) {
  // AST element vector is wrapped as an opaque type so we don't have to deal
  // with ast_element not being default constructible.
  py::class_<ast_element_span>(m, "AstSpan")
      .def("__repr__",
           [](const ast_element_span& vec) {
             return fmt::format("AstSpan({} elements)", vec.size());
           })
      .def("__len__", [](const ast_element_span& self) { return self.size(); })
      .def(
          "__iter__",
          [](const ast_element_span& self) {
            return py::make_iterator(ast_span_iterator::begin(self), ast_span_iterator::end(self));
          },
          py::keep_alive<0, 1>())
      .def(
          "__getitem__",
          [](const ast_element_span& self, const std::int64_t index) {
            const std::size_t actual_index =
                index >= 0
                    ? static_cast<std::size_t>(index)
                    : static_cast<std::size_t>(static_cast<std::int64_t>(self.size()) + index);
            if (actual_index >= self.size()) {
              throw dimension_error("Index `{}` exceeds vector length `{}`.", actual_index,
                                    self.size());
            }
            return to_inner(self[actual_index]);
          },
          py::arg("index"), py::doc("Array access operator."), py::return_value_policy::reference,
          py::keep_alive<0, 1>())
      .doc() = "Stores a sequence of AST elements.";

  ast_type_map map{m};

  map.at<ast::add>()
      .def_property_readonly(
          "args", [](const ast::add& self) -> ast_element_span { return self.args; },
          py::return_value_policy::reference, py::keep_alive<0, 1>(),
          "Operands to the addition. There will always be more than one element.")
      .doc() = "Addition operation: ``args[0] + args[1] + ...``";

  map.at<ast::assign_temporary>()
      .def_property_readonly(
          "left", [](const ast::assign_temporary& x) { return x.left; },
          "Name of the variable to which the assignment applies.")
      .def_property_readonly("right", to_inner_accessor(&ast::assign_temporary::right),
                             py::return_value_policy::reference, py::keep_alive<0, 1>(),
                             "The value being assigned.")
      .doc() = "Assignment to a temporary variable: ``left = right``";

  map.at<ast::assign_output_matrix>()
      .def_property_readonly(
          "arg", [](const ast::assign_output_matrix& x) { return x.arg; }, "Destination argument.")
      .def_property_readonly(
          "value", [](const ast::assign_output_matrix& x) -> const auto& { return x.value; },
          py::return_value_policy::reference_internal,
          "``ConstructMatrix`` specifying values to assign.")
      .doc() = "Assign a matrix to an output argument.";

  map.at<ast::assign_output_scalar>()
      .def_property_readonly(
          "arg", [](const ast::assign_output_scalar& x) { return x.arg; }, "Destination argument.")
      .def_property_readonly("value", to_inner_accessor(&ast::assign_output_scalar::value),
                             py::return_value_policy::reference, py::keep_alive<0, 1>(),
                             "Scalar value to assign.")
      .doc() = "Assign a scalar to an output argument.";

  map.at<ast::assign_output_struct>()
      .def_property_readonly(
          "arg", [](const ast::assign_output_struct& x) { return x.arg; }, "Destination argument.")
      .def_property_readonly(
          "value", [](const ast::assign_output_struct& x) -> const auto& { return x.value; },
          py::return_value_policy::reference_internal,
          "``ConstructCustomType`` specifying values to assign.")
      .doc() = "Assign a struct to an output argument.";

  map.at<ast::boolean_literal>()
      .def_property_readonly(
          "value", [](const ast::boolean_literal& b) { return b.value; },
          "Value of the constant (True or False).")
      .doc() = "Emit a boolean literal constant.";

  map.at<ast::branch>()
      .def_property_readonly("condition", to_inner_accessor(&ast::branch::condition),
                             py::return_value_policy::reference, py::keep_alive<0, 1>(),
                             "Condition governing which branch to take.")
      .def_property_readonly(
          "if_branch", [](const ast::branch& c) -> ast_element_span { return c.if_branch; },
          py::return_value_policy::reference_internal,
          "Statements that evaluate when the condition is true.")
      .def_property_readonly(
          "else_branch", [](const ast::branch& c) -> ast_element_span { return c.else_branch; },
          py::return_value_policy::reference_internal,
          "Statements that evaluate when the condition is false.")
      .doc() = "Emit an if-else statement: ``if (condition) { ... } else { ... }``";

  map.at<ast::call_external_function>()
      .def_property_readonly("function",
                             [](const ast::call_external_function& c) { return c.function; })
      .def_property_readonly(
          "args", [](const ast::call_external_function& c) -> ast_element_span { return c.args; },
          py::return_value_policy::reference_internal)
      .doc() = "Invoke a user-provided external function.";

  map.at<ast::call_std_function>()
      .def_property_readonly(
          "function", [](const ast::call_std_function& c) { return c.function; },
          "The function being invoked.")
      .def_property_readonly(
          "args", [](const ast::call_std_function& c) -> ast_element_span { return c.args; },
          py::return_value_policy::reference_internal, "Arguments to the function.")
      .doc() = "Invoke a standard library math function.";

  map.at<ast::cast>()
      .def_property_readonly(
          "destination_type", [](const ast::cast& c) { return c.destination_type; },
          "The destination numerical type.")
      .def_property_readonly("arg", to_inner_accessor(&ast::cast::arg),
                             py::return_value_policy::reference, py::keep_alive<0, 1>(),
                             "Source value being casted.")
      .doc() = "Cast a numerical value.";

  map.at<ast::comment>()
      .def_property_readonly(
          "content", [](const ast::comment& c) { return c.content; }, "Comment as a single string.")
      .def("split_lines", &ast::comment::split_lines,
           "Split comment by newlines and return a list of strings, one per line.")
      .doc() = "Emit a comment block.";

  map.at<ast::compare>()
      .def_property_readonly("left", to_inner_accessor(&ast::compare::left),
                             py::return_value_policy::reference, py::keep_alive<0, 1>(),
                             "The left operand.")
      .def_property_readonly("right", to_inner_accessor(&ast::compare::right),
                             py::return_value_policy::reference, py::keep_alive<0, 1>(),
                             "The right operand.")
      .def_property_readonly(
          "operation", [](const ast::compare& c) { return c.operation; }, "Relational operation.")
      .doc() = "Compare two operands.";

  map.at<ast::construct_matrix>()
      .def_property_readonly(
          "type", [](const ast::construct_matrix& c) { return c.type; },
          "Describe dimensions of the matrix.")
      .def_property_readonly(
          "args", [](const ast::construct_matrix& c) -> ast_element_span { return c.args; },
          py::return_value_policy::reference_internal,
          "Contents of the matrix, in row-major order.")
      .doc() = "Construct a matrix from a list of statements.";

  map.at<ast::construct_custom_type>()
      .def_property_readonly(
          "type", [](const ast::construct_custom_type& self) { return self.type; },
          "Instance of :class:`wrenfold.codegen.CustomType` specifying which type to instantiate.")
      .def(
          "get_field_value",
          [](const ast::construct_custom_type& self,
             const std::string_view name) -> std::optional<variant_of_ast_element_ptrs> {
            if (const auto v = self.get_field_by_name(name); v.has_value()) {
              return to_inner(*v);
            }
            return std::nullopt;
          },
          py::arg("name"), py::return_value_policy::reference, py::keep_alive<0, 1>(),
          "Given the name of a field, return the statement being assigned to it (or None if the "
          "field does not exist).")
      .doc() = "Construct an instance of a user-provided type.";

  map.at<ast::declaration>()
      .def_property_readonly(
          "name", [](const ast::declaration& d) { return d.name; }, "Name of the variable.")
      .def_property_readonly(
          "type", [](const ast::declaration& d) { return d.type; }, "Type of the variable.")
      .def_property_readonly(
          "value",
          [](const ast::declaration& d) -> std::optional<variant_of_ast_element_ptrs> {
            if (d.value) {
              return to_inner(*d.value);
            }
            return std::nullopt;
          },
          py::return_value_policy::reference, py::keep_alive<0, 1>(),
          "Optional value with which the variable should be initialized.")
      .doc() = "Declare a variable, and optionally assign it a value: ``name: type = value``";

  map.at<ast::divide>()
      .def_property_readonly("left", to_inner_accessor(&ast::divide::left),
                             py::return_value_policy::reference, py::keep_alive<0, 1>(),
                             "Left operand (numerator).")
      .def_property_readonly("right", to_inner_accessor(&ast::divide::right),
                             py::return_value_policy::reference, py::keep_alive<0, 1>(),
                             "Right operand (denominator).")
      .doc() = "Division operation: ``left / right``";

  map.at<ast::float_literal>()
      .def_property_readonly(
          "value", [](const ast::float_literal& f) { return f.value; }, "Value of the constant.")
      .doc() = "Emit a floating-point literal constant.";

  map.at<ast::get_argument>()
      .def_property_readonly(
          "argument", [](const ast::get_argument& self) { return self.arg; },
          "Argument being accessed.")
      .doc() = "Reference an argument to the generated function.";

  map.at<ast::get_field>()
      .def_property_readonly("arg", to_inner_accessor(&ast::get_field::arg),
                             py::return_value_policy::reference, py::keep_alive<0, 1>(),
                             "Operand from which we wish to retrieve the specified field.")
      .def_property_readonly(
          "struct_type", [](const ast::get_field& self) { return self.type; },
          "Type of the struct.")
      .def_property_readonly(
          "field_name", [](const ast::get_field& self) { return self.field; },
          "Name of the field being accessed.")
      .doc() = "Reference a field on a struct: ``arg.field_name``";

  map.at<ast::get_matrix_element>()
      .def_property_readonly("arg", to_inner_accessor(&ast::get_matrix_element::arg),
                             py::return_value_policy::reference, py::keep_alive<0, 1>(),
                             "Operand matrix.")
      .def_property_readonly(
          "row", [](const ast::get_matrix_element& self) { return self.row; }, "Row to access.")
      .def_property_readonly(
          "col", [](const ast::get_matrix_element& self) { return self.col; }, "Column to access.")
      .doc() = "Retrieve a value from a matrix: ``arg[row, col]``";

  map.at<ast::integer_literal>()
      .def_property_readonly(
          "value", [](const ast::integer_literal& i) { return i.value; }, "Value of the constant.")
      .doc() = "Emit an integer literal constant.";

  map.at<ast::multiply>()
      .def_property_readonly(
          "args", [](const ast::multiply& self) -> ast_element_span { return self.args; },
          py::return_value_policy::reference, py::keep_alive<0, 1>(),
          "Operands to the multiplication. There will always be more than one.")
      .doc() = "Multiplication operation: ``args[0] * args[1] * ...``";

  map.at<ast::negate>()
      .def_property_readonly("arg", to_inner_accessor(&ast::negate::arg),
                             py::return_value_policy::reference, py::keep_alive<0, 1>(),
                             "Operand being negated.")
      .doc() = "Negation operation: ``-arg``";

  map.at<ast::optional_output_branch>()
      .def_property_readonly(
          "argument", [](const ast::optional_output_branch& self) { return self.arg; },
          "An optional output argument.")
      .def_property_readonly(
          "statements",
          [](const ast::optional_output_branch& self) -> ast_element_span {
            return self.statements;
          },
          "Statements that are relevant when the optional argument is present.")
      .doc() =
      "Conditionally assign values to an optional output argument: ``if (<argument exists>) { ... "
      "}``";

  map.at<ast::parenthetical>()
      .def_property_readonly("contents", to_inner_accessor(&ast::parenthetical::contents),
                             py::return_value_policy::reference, py::keep_alive<0, 1>(),
                             "Value that should be wrapped in parentheses.")
      .doc() = "Wrap an expression in parentheses.";

  map.at<ast::return_object>()
      .def_property_readonly("value", to_inner_accessor(&ast::return_object::value),
                             py::return_value_policy::reference, py::keep_alive<0, 1>(),
                             "Value or object being returned.")
      .doc() = "Return a value from the function.";

  map.at<ast::special_constant>()
      .def_property_readonly(
          "value", [](const ast::special_constant& c) { return c.value; },
          "Enum indicating the value of the constant.")
      .doc() = "Emit a mathematical constant";

  map.at<ast::variable_ref>()
      .def_property_readonly(
          "name", [](const ast::variable_ref& v) { return v.name; }, "Name of the variable.")
      .doc() = "Reference a local variable.";

  wrap_ast_type<ast::function_signature>(m)
      .def_property_readonly("return_type", &ast::function_signature::return_type,
                             "Return type of the function.")
      .def_property_readonly("name", &ast::function_signature::name, "Name of the function.")
      .def_property_readonly("arguments", &ast::function_signature::arguments, "List of arguments.")
      .doc() =
      "Emit the signature of a generated function: ``name(... arguments ...) -> "
      "return_annotation``";

  wrap_ast_type<ast::function_definition>(m)
      .def_property_readonly("signature", &ast::function_definition::signature,
                             "Function signature.")
      .def_property_readonly(
          "body",
          [](const ast::function_definition& self) -> ast_element_span { return self.body(); },
          "Statements that make up the body of the function.",
          py::return_value_policy::reference_internal)
      .doc() =
      "Define a generated function. This is the top level object of the emitted syntax tree.";
}

}  // namespace wf
