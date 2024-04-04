// Copyright 2024 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/code_generation/ast_conversion.h"
#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/ast_visitor.h"

namespace py = pybind11;
using namespace py::literals;

// We make this type opaque and wrap it manually below. That way we can cast elements to their
// underlying type in the `next()` impl, and attach lifetime of iterators to the vector using
// py::keep_alive.
using ast_element_vector = std::vector<wf::ast::ast_element>;
PYBIND11_MAKE_OPAQUE(ast_element_vector)

namespace wf {

// Construct class_ wrapper for an AST type. Name is derived automatically.
template <typename T>
auto wrap_ast_type(py::module_& m) {
  return py::class_<T>(m, ast::camel_case_name<T>())
      .def("__repr__", [](const T& obj) -> std::string {
        // Handled by the formatters in ast_formatters.h
        static_assert(ast::is_formattable<T>::value,
                      "The specified type is missing a format_ast(...) method");
        return fmt::format("{}", obj);
      });
}

// Cast to `ast::variant` so we can return this into python.
// We cast with `reference` policy, which is going to create a weak reference to `x`.
py::object to_inner(const ast::ast_element& element) {
  return wf::ast::visit(element, [](const auto& x) -> py::object {
    return py::cast(x, py::return_value_policy::reference);
  });
}

// Create a lambda that accepts `StructType` and retrieves `member`, then invokes `to_inner` to
// convert that member to a handle.
template <typename StructType>
auto to_inner_accessor(ast::ast_element StructType::*member) {
  return [member](const StructType& self) { return to_inner(self.*member); };
}

// Iterator that converts vector elements to their inner type.
class ast_vector_iterator {
 public:
  static ast_vector_iterator begin(const ast_element_vector& vec) noexcept {
    return ast_vector_iterator(vec.begin());
  }
  static ast_vector_iterator end(const ast_element_vector& vec) noexcept {
    return ast_vector_iterator(vec.end());
  }

  bool operator==(const ast_vector_iterator& other) const noexcept { return it_ == other.it_; }

  // Pre-increment.
  auto& operator++() noexcept {
    ++it_;
    return *this;
  }

  py::object operator*() const { return to_inner(*it_); }

 private:
  explicit ast_vector_iterator(ast_element_vector::const_iterator it) noexcept : it_(it) {}

  ast_element_vector::const_iterator it_;
};

void wrap_ast(py::module_& m) {
  // AST element vector is wrapped as an opaque type so we don't have to deal
  // with ast_element not being default constructible.
  py::class_<ast_element_vector>(m, "AstVector")
      .def("__repr__",
           [](const ast_element_vector& vec) {
             return fmt::format("AstVector({} elements)", vec.size());
           })
      .def("__len__", [](const ast_element_vector& vec) { return vec.size(); })
      .def(
          "__iter__",
          [](const ast_element_vector& vec) {
            return py::make_iterator(ast_vector_iterator::begin(vec),
                                     ast_vector_iterator::end(vec));
          },
          py::keep_alive<0, 1>())
      .def(
          "__getitem__",
          [](const ast_element_vector& vec, const std::size_t index) {
            if (index >= vec.size()) {
              throw dimension_error("Index `{}` exceeds vector length `{}`.", index, vec.size());
            }
            return to_inner(vec[index]);
          },
          py::arg("index"), py::doc("Array access operator."), py::keep_alive<0, 1>())
      .doc() = "Stores a sequence of AST elements.";

  wrap_ast_type<ast::add>(m)
      .def_property_readonly("left", to_inner_accessor(&ast::add::left), py::keep_alive<0, 1>(),
                             "Left operand.")
      .def_property_readonly("right", to_inner_accessor(&ast::add::right), py::keep_alive<0, 1>(),
                             "Right operand.")
      .doc() = "Addition operation: ``left + right``";

  wrap_ast_type<ast::assign_temporary>(m)
      .def_property_readonly(
          "left", [](const ast::assign_temporary& x) { return x.left; },
          "Name of the variable to which the assignment applies.")
      .def_property_readonly("right", to_inner_accessor(&ast::assign_temporary::right),
                             py::keep_alive<0, 1>(), "The value being assigned.")
      .doc() = "Assignment to a temporary variable: ``left = right``";

  wrap_ast_type<ast::assign_output_matrix>(m)
      .def_property_readonly(
          "arg", [](const ast::assign_output_matrix& x) { return x.arg; }, "Destination argument.")
      .def_property_readonly(
          "value", [](const ast::assign_output_matrix& x) -> const auto& { return x.value; },
          py::return_value_policy::reference_internal,
          "``ConstructMatrix`` specifying values to assign.")
      .doc() = "Assign a matrix to an output argument.";

  wrap_ast_type<ast::assign_output_scalar>(m)
      .def_property_readonly(
          "arg", [](const ast::assign_output_scalar& x) { return x.arg; }, "Destination argument.")
      .def_property_readonly("value", to_inner_accessor(&ast::assign_output_scalar::value),
                             py::keep_alive<0, 1>(), "Scalar value to assign.")
      .doc() = "Assign a scalar to an output argument.";

  wrap_ast_type<ast::assign_output_struct>(m)
      .def_property_readonly(
          "arg", [](const ast::assign_output_struct& x) { return x.arg; }, "Destination argument.")
      .def_property_readonly(
          "value", [](const ast::assign_output_struct& x) -> const auto& { return x.value; },
          py::return_value_policy::reference_internal,
          "``ConstructCustomType`` specifying values to assign.")
      .doc() = "Assign a struct to an output argument.";

  wrap_ast_type<ast::boolean_literal>(m)
      .def_property_readonly(
          "value", [](const ast::boolean_literal& b) { return b.value; },
          "Value of the constant (True or False).")
      .doc() = "Emit a boolean literal constant.";

  wrap_ast_type<ast::branch>(m)
      .def_property_readonly("condition", to_inner_accessor(&ast::branch::condition),
                             py::keep_alive<0, 1>(), "Condition governing which branch to take.")
      .def_property_readonly(
          "if_branch", [](const ast::branch& c) -> const auto& { return c.if_branch; },
          py::return_value_policy::reference_internal,
          "Statements that evaluate when the condition is true.")
      .def_property_readonly(
          "else_branch", [](const ast::branch& c) -> const auto& { return c.else_branch; },
          py::return_value_policy::reference_internal,
          "Statements that evaluate when the condition is false.")
      .doc() = "Emit an if-else statement: ``if (condition) { ... } else { ... }``";

  wrap_ast_type<ast::call_external_function>(m)
      .def_property_readonly("function",
                             [](const ast::call_external_function& c) { return c.function; })
      .def_property_readonly(
          "args", [](const ast::call_external_function& c) -> const auto& { return c.args; },
          py::return_value_policy::reference_internal)
      .doc() = "Invoke a user-provided external function.";

  wrap_ast_type<ast::call_std_function>(m)
      .def_property_readonly(
          "function", [](const ast::call_std_function& c) { return c.function; },
          "The function being invoked.")
      .def_property_readonly(
          "args", [](const ast::call_std_function& c) -> const auto& { return c.args; },
          py::return_value_policy::reference_internal, "Arguments to the function.")
      .doc() = "Invoke a standard library math function.";

  wrap_ast_type<ast::cast>(m)
      .def_property_readonly(
          "destination_type", [](const ast::cast& c) { return c.destination_type; },
          "The destination numerical type.")
      .def_property_readonly("arg", to_inner_accessor(&ast::cast::arg), py::keep_alive<0, 1>(),
                             "Source value being casted.")
      .doc() = "Cast a numerical value.";

  wrap_ast_type<ast::comment>(m)
      .def_property_readonly(
          "content", [](const ast::comment& c) { return c.content; }, "Comment as a single string.")
      .def("split_lines", &ast::comment::split_lines,
           "Split comment by newlines and return a list of strings, one per line.")
      .doc() = "Emit a comment block.";

  wrap_ast_type<ast::compare>(m)
      .def_property_readonly("left", to_inner_accessor(&ast::compare::left), py::keep_alive<0, 1>(),
                             "The left operand.")
      .def_property_readonly("right", to_inner_accessor(&ast::compare::right),
                             py::keep_alive<0, 1>(), "The right operand.")
      .def_property_readonly(
          "operation", [](const ast::compare& c) { return c.operation; }, "Relational operation.")
      .doc() = "Compare two operands.";

  wrap_ast_type<ast::construct_matrix>(m)
      .def_property_readonly(
          "type", [](const ast::construct_matrix& c) { return c.type; },
          "Describe dimensions of the matrix.")
      .def_property_readonly(
          "args", [](const ast::construct_matrix& c) -> const auto& { return c.args; },
          py::return_value_policy::reference_internal,
          "Contents of the matrix, in row-major order.")
      .doc() = "Construct a matrix from a list of statements.";

  wrap_ast_type<ast::construct_custom_type>(m)
      .def_property_readonly(
          "type", [](const ast::construct_custom_type& self) { return self.type; },
          "Instance of ``wrenfold.codegen.CustomType`` specifying which type to instantiate.")
      .def(
          "get_field_value",
          [](const ast::construct_custom_type& self, const std::string_view name) -> py::object {
            if (const auto v = self.get_field_by_name(name); v.has_value()) {
              return to_inner(*v);
            }
            return py::none();
          },
          py::arg("name"), py::keep_alive<0, 1>(),
          "Given the name of a field, return the statement being assigned to it (or None if the "
          "field does not exist).")
      .doc() = "Construct an instance of a user-provided type.";

  wrap_ast_type<ast::declaration>(m)
      .def_property_readonly(
          "name", [](const ast::declaration& d) { return d.name; }, "Name of the variable.")
      .def_property_readonly(
          "type", [](const ast::declaration& d) { return d.type; }, "Type of the variable.")
      .def_property_readonly(
          "value",
          [](const ast::declaration& d) -> py::object {
            if (d.value) {
              return to_inner(*d.value);
            }
            return py::none();
          },
          py::keep_alive<0, 1>(), "Optional value with which the variable should be initialized.")
      .doc() = "Declare a variable, and optionally assign it a value: ``name: type = value``";

  wrap_ast_type<ast::declaration_type_annotation>(m)
      .def_property_readonly(
          "type", [](const ast::declaration_type_annotation& d) { return d.type; },
          "Type being declared.")
      .doc() = "Annotate the type of a variable declaration.";

  wrap_ast_type<ast::divide>(m)
      .def_property_readonly("left", to_inner_accessor(&ast::divide::left), py::keep_alive<0, 1>(),
                             "Left operand (numerator).")
      .def_property_readonly("right", to_inner_accessor(&ast::divide::right),
                             py::keep_alive<0, 1>(), "Right operand (denonimator).")
      .doc() = "Division operation: ``left / right``";

  wrap_ast_type<ast::float_literal>(m)
      .def_property_readonly(
          "value", [](const ast::float_literal& f) { return f.value; }, "Value of the constant.")
      .doc() = "Emit a floating-point literal constant.";

  wrap_ast_type<ast::get_argument>(m)
      .def_property_readonly(
          "argument", [](const ast::get_argument& self) { return self.arg; },
          "Argument being accessed.")
      .doc() = "Reference an argument to the generated function.";

  wrap_ast_type<ast::get_field>(m)
      .def_property_readonly("arg", to_inner_accessor(&ast::get_field::arg), py::keep_alive<0, 1>(),
                             "Operand from which we wish to retrieve the specified field.")
      .def_property_readonly(
          "struct_type", [](const ast::get_field& self) { return self.type; },
          "Type of the struct.")
      .def_property_readonly(
          "field_name", [](const ast::get_field& self) { return self.field; },
          "Name of the field being accessed.")
      .doc() = "Reference a field on a struct: ``arg.field_name``";

  wrap_ast_type<ast::get_matrix_element>(m)
      .def_property_readonly("arg", to_inner_accessor(&ast::get_matrix_element::arg),
                             py::keep_alive<0, 1>(), "Operand matrix.")
      .def_property_readonly(
          "row", [](const ast::get_matrix_element& self) { return self.row; }, "Row to access.")
      .def_property_readonly(
          "col", [](const ast::get_matrix_element& self) { return self.col; }, "Column to access.")
      .doc() = "Retrieve a value from a matrix: ``arg[row, col]``";

  wrap_ast_type<ast::integer_literal>(m)
      .def_property_readonly(
          "value", [](const ast::integer_literal& i) { return i.value; }, "Value of the constant.")
      .doc() = "Emit an integer literal constant.";

  wrap_ast_type<ast::multiply>(m)
      .def_property_readonly("left", to_inner_accessor(&ast::multiply::left),
                             py::keep_alive<0, 1>(), "Left operand.")
      .def_property_readonly("right", to_inner_accessor(&ast::multiply::right),
                             py::keep_alive<0, 1>(), "Right operand.")
      .doc() = "Multiplication operation: ``left * right``";

  wrap_ast_type<ast::negate>(m)
      .def_property_readonly("arg", to_inner_accessor(&ast::negate::arg), py::keep_alive<0, 1>(),
                             "Operand being negated.")
      .doc() = "Negation operation: ``-arg``";

  wrap_ast_type<ast::optional_output_branch>(m)
      .def_property_readonly(
          "argument", [](const ast::optional_output_branch& self) { return self.arg; },
          "An optional output argument.")
      .def_property_readonly(
          "statements", [](const ast::optional_output_branch& self) { return self.statements; },
          "Statements that are relevant when the optional argument is present.")
      .doc() =
      "Conditionally assign values to an optional output argument: ``if (<argument exists>) { ... "
      "}``";

  wrap_ast_type<ast::return_object>(m)
      .def_property_readonly("value", to_inner_accessor(&ast::return_object::value),
                             py::keep_alive<0, 1>(), "Value or object being returned.")
      .doc() = "Return a value from the function.";

  wrap_ast_type<ast::special_constant>(m)
      .def_property_readonly(
          "value", [](const ast::special_constant& c) { return c.value; },
          "Enum indicating the value of the constant.")
      .doc() = "Emit a mathematical constant";

  wrap_ast_type<ast::variable_ref>(m)
      .def_property_readonly(
          "name", [](const ast::variable_ref& v) { return v.name; }, "Name of the variable.")
      .doc() = "Reference a local variable.";

  // Types that are not part of the ast::variant
  wrap_ast_type<ast::return_type_annotation>(m)
      .def_property_readonly(
          "type", [](const ast::return_type_annotation& ret) { return ret.type; },
          "Return type, or None if the function has no return value.")
      .doc() = "Emit the return type annotation of a generated function.";

  wrap_ast_type<ast::function_signature>(m)
      .def_property_readonly("return_annotation", &ast::function_signature::return_annotation,
                             "Return type of the function.")
      .def_property_readonly("name", &ast::function_signature::name, "Name of the function.")
      .def_property_readonly("arguments", &ast::function_signature::arguments, "List of arguments.")
      .doc() =
      "Emit the signature of a generated function: ``name(... arguments ...) -> "
      "return_annotation``";

  wrap_ast_type<ast::function_definition>(m)
      .def_property_readonly("signature", &ast::function_definition::signature,
                             "Function signature.")
      .def_property_readonly("body", &ast::function_definition::body,
                             "Statements that make up the body of the function.",
                             py::return_value_policy::reference_internal)
      .doc() =
      "Define a generated function. This is the top level object of the emitted syntax tree.";
}

}  // namespace wf
