// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/code_generation/ast_conversion.h"
#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/cpp_code_generator.h"
#include "wf/code_generation/expression_group.h"
#include "wf/code_generation/ir_builder.h"
#include "wf/code_generation/rust_code_generator.h"
#include "wf/code_generation/types.h"
#include "wf/expression.h"
#include "wf/matrix_expression.h"

namespace py = pybind11;
using namespace py::literals;

// We make this type opaque and wrap it manually below.
// This allows us to avoid problems from variant not being default constructible.
PYBIND11_MAKE_OPAQUE(wf::ast::variant_vector)

namespace wf {

template <class T>
static std::string format_ast_repr(const T& x) {
  return fmt::format("{}", x);
}

// Accept the mathematical function description, and "transpile" it into AST that can be emitted
// in another language.
ast::function_definition transpile_to_function_definition(const function_description& description) {
  flat_ir ir{description.output_expressions()};
  ir.eliminate_duplicates();
  const output_ir output_ir{std::move(ir)};
  return ast::create_ast(output_ir, description.signature());
}

custom_type init_custom_type(std::string name,
                             const std::vector<std::tuple<std::string_view, py::object>>& fields,
                             py::type python_type) {
  std::vector<field> fields_converted{};
  fields_converted.reserve(fields.size());
  std::transform(fields.begin(), fields.end(), std::back_inserter(fields_converted),
                 [](const auto& tup) {
                   // We can't use a variant in the tuple, since it can't be default constructed.
                   // Instead, we check for different types manually here.
                   const auto& [field_name, type_obj] = tup;
                   if (py::isinstance<scalar_type>(type_obj)) {
                     return field(std::string{field_name}, py::cast<scalar_type>(type_obj));
                   } else if (py::isinstance<matrix_type>(type_obj)) {
                     return field(std::string{field_name}, py::cast<matrix_type>(type_obj));
                   } else if (py::isinstance<custom_type>(type_obj)) {
                     return field(std::string{field_name}, py::cast<custom_type>(type_obj));
                   } else {
                     throw type_error("Field type must be ScalarType, MatrixType, or CustomType.");
                   }
                 });
  return custom_type(std::move(name), std::move(fields_converted),
                     std::any{std::move(python_type)});
}

void wrap_codegen_operations(py::module_& m) {
  // Stored as shared-ptr to avoid copies.
  py::class_<ast::variant_vector, std::shared_ptr<ast::variant_vector>>(m, "AstVector")
      .def("__repr__",
           [](const ast::variant_vector& vec) {
             return fmt::format("AstVector({} elements)", vec.size());
           })
      .def("__len__", [](const ast::variant_vector& vec) { return vec.size(); })
      .def(
          "__iter__",
          [](const std::vector<ast::variant>& vec) {
            return py::make_iterator(vec.begin(), vec.end());
          },
          py::keep_alive<0, 1>())
      .def(
          "__getitem__",
          [](const std::vector<ast::variant>& vec, const std::size_t index) {
            if (index >= vec.size()) {
              throw dimension_error("Index `{}` exceeds vector length `{}`.", index, vec.size());
            }
            return vec[index];
          },
          py::arg("index"), py::doc("Array access operator."));

  m.def(
      "transpile",
      [](const std::vector<function_description::shared_ptr>& descriptions) {
        // TODO: Allow this to run in parallel.
        std::vector<ast::function_definition> outputs;
        outputs.reserve(descriptions.size());
        std::transform(descriptions.begin(), descriptions.end(), std::back_inserter(outputs),
                       [](const function_description::shared_ptr& ptr) {
                         WF_ASSERT(ptr);
                         return transpile_to_function_definition(*ptr);
                       });
        return outputs;
      },
      py::arg("descriptions"),
      py::doc("Generate function definitions in AST form, given symbolic function descriptions."),
      py::return_value_policy::take_ownership);

  m.def(
      "transpile",
      [](const function_description::shared_ptr& description) {
        WF_ASSERT(description);
        return transpile_to_function_definition(*description);
      },
      py::arg("description"),
      py::doc(
          "Generate a function definition in AST form, given the symbolic function description."),
      py::return_value_policy::take_ownership);

  py::enum_<expression_usage>(m, "ExpressionUsage")
      .value("OptionalOutputArgument", expression_usage::optional_output_argument)
      .value("OutputArgument", expression_usage::output_argument)
      .value("ReturnValue", expression_usage::return_value);

  py::class_<output_key>(m, "OutputKey")
      .def(py::init<expression_usage, std::string_view>(), py::arg("usage"), py::arg("name"));

  py::class_<expression_group>(m, "ExpressionGroup")
      .def(py::init<std::vector<Expr>, output_key>(), py::arg("expressions"), py::arg("output_key"))
      .def(py::init([](const MatrixExpr& m, output_key key) {
             return expression_group(m.to_vector(), key);
           }),
           py::arg("expressions"), py::arg("output_key"));

  py::enum_<std_math_function>(m, "StdMathFunction")
      .value("Cos", std_math_function::cos)
      .value("Sin", std_math_function::sin)
      .value("Tan", std_math_function::tan)
      .value("ArcCos", std_math_function::acos)
      .value("ArcSin", std_math_function::asin)
      .value("ArcTan", std_math_function::atan)
      .value("Log", std_math_function::log)
      .value("Sqrt", std_math_function::sqrt)
      .value("Abs", std_math_function::abs)
      .value("Signum", std_math_function::signum)
      .value("Arctan2", std_math_function::atan2)
      .value("Powi", std_math_function::powi)
      .value("Powf", std_math_function::powf)
      .def(
          "to_string",
          [](std_math_function name) { return string_from_standard_library_function(name); },
          py::doc("Convert to string."));

  py::enum_<code_numeric_type>(m, "NumericType")
      .value("Bool", code_numeric_type::boolean)
      .value("Integer", code_numeric_type::integral)
      .value("Real", code_numeric_type::floating_point);

  py::enum_<relational_operation>(m, "RelationalOperation")
      .value("LessThan", relational_operation::less_than)
      .value("LessThanOrEqual", relational_operation::less_than_or_equal)
      .value("Equal", relational_operation::equal);

  py::enum_<argument_direction>(m, "ArgumentDirection")
      .value("Input", argument_direction::input)
      .value("Output", argument_direction::output)
      .value("OptionalOutput", argument_direction::optional_output);

  py::class_<scalar_type>(m, "ScalarType")
      .def(py::init<code_numeric_type>())
      .def_property_readonly("numeric_type", &scalar_type::numeric_type)
      .def("__repr__", [](scalar_type self) { return fmt::format("{}", self); });

  py::class_<matrix_type>(m, "MatrixType")
      .def(py::init<index_t, index_t>(), py::arg("rows"), py::arg("cols"))
      .def_property_readonly("num_rows", &matrix_type::rows)
      .def_property_readonly("num_cols", &matrix_type::cols)
      .def("compute_indices", &matrix_type::compute_indices)
      .def("__repr__", [](matrix_type self) { return fmt::format("{}", self); });

  py::class_<custom_type>(m, "CustomType")
      .def(py::init(&init_custom_type), py::arg("name"), py::arg("fields"), py::arg("python_type"),
           py::doc("Construct custom type from fields."))
      .def_property_readonly("name", &custom_type::name)
      .def_property_readonly("fields", &custom_type::fields, py::doc("Access fields on this type."))
      .def_property_readonly(
          "python_type",
          [](const custom_type& self) -> std::variant<py::none, py::type> {
            if (self.python_type().has_value()) {
              return std::any_cast<py::type>(self.python_type());
            }
            return py::none();
          },
          py::doc("Get the underlying python type."))
      .def("__repr__", [](const custom_type& self) {
        const py::object python_type = self.python_type().has_value()
                                           ? std::any_cast<py::type>(self.python_type())
                                           : py::none();
        const py::str repr = py::repr(python_type);
        return fmt::format("CustomType('{}', {} fields, {})", self.name(), self.size(),
                           py::cast<std::string_view>(repr));
      });

  py::class_<field>(m, "Field")
      .def_property_readonly("name", &field::name)
      .def_property_readonly("type", &field::type)
      .def("__repr__", [](const field& self) {
        return fmt::format("Field({}: {})", self.name(), self.type());
      });

  py::class_<function_signature>(m, "FunctionSignature")
      .def_property_readonly("name", &function_signature::name)
      .def("__repr__", [](const function_signature& s) {
        return fmt::format("FunctionSignature('{}', {} args)", s.name(), s.num_arguments());
      });

  py::class_<function_description, function_description::shared_ptr>(m, "FunctionDescription")
      .def(py::init<std::string>(), py::arg("name"), py::doc("Construct with string name."))
      .def_property_readonly("name", &function_description::name)
      .def("__repr__",
           [](const function_description& s) {
             return fmt::format("FunctionDescription('{}', {} args)", s.name(),
                                s.signature().num_arguments());
           })
      .def(
          "add_input_argument",
          [](function_description& self, const std::string_view name, scalar_type type) {
            return self.add_input_argument(name, type);
          },
          py::arg("name"), py::arg("type"),
          py::doc("Add a scalar input argument. Returns placeholder value to pass to the python "
                  "function."))
      .def(
          "add_input_argument",
          [](function_description& self, const std::string_view name, matrix_type type) {
            return self.add_input_argument(name, type);
          },
          py::arg("name"), py::arg("type"),
          py::doc("Add a matrix input argument. Returns placeholder value to pass to the python "
                  "function."))
      .def(
          "add_input_argument",
          [](function_description& self, const std::string_view name, const custom_type& type) {
            return self.add_input_argument(name, type);
          },
          py::arg("name"), py::arg("type"),
          py::doc("Add an input argument with a custom user-specified type."))
      .def(
          "add_output_argument",
          [](function_description& self, const std::string_view name, const bool is_optional,
             const Expr& value) {
            self.add_output_argument(name, scalar_type(code_numeric_type::floating_point),
                                     is_optional, {value});
          },
          py::arg("name"), py::arg("is_optional"), py::arg("value"))
      .def(
          "add_output_argument",
          [](function_description& self, const std::string_view name, const bool is_optional,
             const MatrixExpr& value) {
            self.add_output_argument(name, matrix_type(value.rows(), value.cols()), is_optional,
                                     value.to_vector());
          },
          py::arg("name"), py::arg("is_optional"), py::arg("value"))
      .def(
          "add_output_argument",
          [](function_description& self, const std::string_view name, const bool is_optional,
             const custom_type& custom_type, std::vector<Expr> expressions) {
            self.add_output_argument(name, custom_type, is_optional, std::move(expressions));
          },
          py::arg("name"), py::arg("is_optional"), py::arg("custom_type"), py::arg("expressions"),
          py::doc("Record an output argument of custom type."))
      .def(
          "set_return_value",
          [](function_description& self, const Expr& value) {
            self.set_return_value(scalar_type(code_numeric_type::floating_point), {value});
          },
          py::arg("value"))
      .def(
          "set_return_value",
          [](function_description& self, const MatrixExpr& value) {
            self.set_return_value(matrix_type(value.rows(), value.cols()), value.to_vector());
          },
          py::arg("value"))
      .def(
          "set_return_value",
          [](function_description& self, const custom_type& custom_type,
             std::vector<Expr> expressions) {
            self.set_return_value(custom_type, std::move(expressions));
          },
          py::arg("custom_type"), py::arg("expressions"));

  py::class_<argument>(m, "Argument")
      .def_property_readonly("name", &argument::name)
      .def_property_readonly("type", &argument::type)
      .def_property_readonly("direction", &argument::direction)
      .def_property_readonly("is_optional", &argument::is_optional)
      .def("__repr__", [](const argument& self) {
        return fmt::format("Argument({}: {})", self.name(), self.type());
      });

  // AST types are below:
  // --------------------

  py::class_<ast::add>(m, "Add")
      .def_property_readonly("left", [](const ast::add& x) { return *x.left; })
      .def_property_readonly("right", [](const ast::add& x) { return *x.right; })
      .def("__repr__", &format_ast_repr<ast::add>);

  py::class_<ast::assign_temporary>(m, "AssignTemporary")
      .def_property_readonly("left", [](const ast::assign_temporary& x) { return x.left; })
      .def_property_readonly("right",
                             [](const ast::assign_temporary& x) {
                               WF_ASSERT(x.right);
                               return *x.right;
                             })
      .def("__repr__", &format_ast_repr<ast::assign_temporary>);

  py::class_<ast::assign_output_argument>(m, "AssignOutputArgument")
      .def_property_readonly("argument", [](const ast::assign_output_argument& x) { return x.arg; })
      .def_property_readonly("values",
                             [](const ast::assign_output_argument& x) { return x.values; })
      .def("__repr__", &format_ast_repr<ast::assign_output_argument>);

  py::class_<ast::branch>(m, "Branch")
      .def_property_readonly("condition", [](const ast::branch& c) { return *c.condition; })
      .def_property_readonly("if_branch", [](const ast::branch& c) { return c.if_branch; })
      .def_property_readonly("else_branch", [](const ast::branch& c) { return c.else_branch; })
      .def("__repr__", &format_ast_repr<ast::branch>);

  py::class_<ast::call>(m, "Call")
      .def_property_readonly("function", [](const ast::call& c) { return c.function; })
      .def_property_readonly("args", [](const ast::call& c) { return c.args; })
      .def("__repr__", &format_ast_repr<ast::call>);

  py::class_<ast::cast>(m, "Cast")
      .def_property_readonly("destination_type",
                             [](const ast::cast& c) { return c.destination_type; })
      .def_property_readonly("arg",
                             [](const ast::cast& c) {
                               WF_ASSERT(c.arg);
                               return *c.arg;
                             })
      .def("__repr__", &format_ast_repr<ast::cast>);

  py::class_<ast::comment>(m, "Comment")
      .def_property_readonly("content", [](const ast::comment& c) { return c.content; })
      .def("split_lines", &ast::comment::split_lines,
           py::doc("Split comment by newlines and return a list of strings, one per line."));

  py::class_<ast::compare>(m, "Compare")
      .def_property_readonly("left",
                             [](const ast::compare& c) {
                               WF_ASSERT(c.left);
                               return *c.left;
                             })
      .def_property_readonly("right",
                             [](const ast::compare& c) {
                               WF_ASSERT(c.right);
                               return *c.right;
                             })
      .def_property_readonly("operation", [](const ast::compare& c) { return c.operation; })
      .def("__repr__", &format_ast_repr<ast::compare>);

  py::class_<ast::construct_matrix>(m, "ConstructReturnValue")
      .def_property_readonly("type", [](const ast::construct_matrix& c) { return c.type; })
      .def_property_readonly("args", [](const ast::construct_matrix& c) { return c.args; })
      .def("__repr__", &format_ast_repr<ast::construct_matrix>);

  py::class_<ast::construct_custom_type>(m, "ConstructCustomType")
      .def_property_readonly("type",
                             [](const ast::construct_custom_type& self) { return self.type; })
      .def_property_readonly(
          "field_values",
          [](const ast::construct_custom_type& self) -> const auto& { return self.field_values; })
      .def("__repr__", &format_ast_repr<ast::construct_custom_type>);

  py::class_<ast::declaration>(m, "Declaration")
      .def_property_readonly("name", [](const ast::declaration& d) { return d.name; })
      .def_property_readonly("type", [](const ast::declaration& d) { return d.type; })
      .def_property_readonly("value",
                             [](const ast::declaration& d) -> std::optional<ast::variant> {
                               if (d.value) {
                                 return *d.value;
                               } else {
                                 return std::nullopt;
                               }
                             })
      .def("__repr__", &format_ast_repr<ast::declaration>);

  py::class_<ast::divide>(m, "Divide")
      .def_property_readonly("left",
                             [](const ast::divide& x) {
                               WF_ASSERT(x.left);
                               return *x.left;
                             })
      .def_property_readonly("right", [](const ast::divide& x) {
        WF_ASSERT(x.right);
        return *x.right;
      });

  py::class_<ast::float_literal>(m, "FloatLiteral")
      .def_property_readonly("value", [](const ast::float_literal& f) { return f.value; })
      .def("__repr__", &format_ast_repr<ast::float_literal>);

  py::class_<ast::integer_literal>(m, "IntegerLiteral")
      .def_property_readonly("value", [](const ast::integer_literal& i) { return i.value; })
      .def("__repr__", &format_ast_repr<ast::integer_literal>);

  py::class_<ast::multiply>(m, "Multiply")
      .def_property_readonly("left",
                             [](const ast::multiply& x) {
                               WF_ASSERT(x.left);
                               return *x.left;
                             })
      .def_property_readonly("right",
                             [](const ast::multiply& x) {
                               WF_ASSERT(x.right);
                               return *x.right;
                             })
      .def("__repr__", &format_ast_repr<ast::multiply>);

  py::class_<ast::negate>(m, "Negate").def_property_readonly("arg", [](const ast::negate& x) {
    WF_ASSERT(x.arg);
    return *x.arg;
  });

  py::class_<ast::optional_output_branch>(m, "OptionalOutputBranch")
      .def_property_readonly("argument",
                             [](const ast::optional_output_branch& self) { return self.arg; })
      .def_property_readonly(
          "statements", [](const ast::optional_output_branch& self) { return self.statements; })
      .def("__repr__", &format_ast_repr<ast::optional_output_branch>);

  py::class_<ast::read_input_matrix>(m, "ReadInputMatrix")
      .def_property_readonly("argument",
                             [](const ast::read_input_matrix& self) { return self.arg; })
      .def_property_readonly("row", [](const ast::read_input_matrix& self) { return self.row; })
      .def_property_readonly("col", [](const ast::read_input_matrix& self) { return self.col; })
      .def("__repr__", &format_ast_repr<ast::read_input_matrix>);

  py::class_<ast::read_input_scalar>(m, "ReadInputScalar")
      .def_property_readonly("argument",
                             [](const ast::read_input_scalar& self) { return self.arg; })
      .def("__repr__", &format_ast_repr<ast::read_input_scalar>);

  py::class_<ast::read_input_struct>(m, "ReadInputStruct")
      .def_property_readonly("argument",
                             [](const ast::read_input_struct& self) { return self.arg; })
      .def_property_readonly(
          "access_sequence",
          [](const ast::read_input_struct& self) { return self.access_sequence; })
      .def("__repr__", &format_ast_repr<ast::read_input_struct>);

  py::class_<ast::return_value>(m, "ReturnValue")
      .def_property_readonly(
          "value", [](const ast::return_value& c) { return c.value; },
          py::doc("Value or object being returned."));

  py::class_<ast::special_constant>(m, "SpecialConstant")
      .def_property_readonly(
          "value", [](const ast::special_constant& c) { return c.value; },
          py::doc("Enum indicating the value of the constant"));

  py::class_<ast::variable_ref>(m, "VariableRef")
      .def_property_readonly("name", [](const ast::variable_ref& v) { return v.name; })
      .def("__repr__", &format_ast_repr<ast::variable_ref>);

  // Types that are not part of the ast::variant
  py::class_<ast::function_definition>(m, "FunctionDefinition")
      .def_property_readonly("signature", &ast::function_definition::signature)
      .def_property_readonly("body",  // TODO: Don't copy this on return.
                             &ast::function_definition::body);

  py::class_<ast::function_signature2>(m, "FunctionSignature2")
      .def_property_readonly("return_type",
                             [](const ast::function_signature2& self) { return self.return_type; })
      .def_property_readonly("name", [](const ast::function_signature2& self) { return self.name; })
      .def_property_readonly("arguments",
                             [](const ast::function_signature2& self) { return self.arguments; });

  py::class_<field_access>(m, "FieldAccess")
      .def_property_readonly(
          "type", [](const field_access& self) { return self.type(); },
          py::doc("The type of the struct we are accessing."))
      .def_property_readonly("field_name", &field_access::field_name,
                             py::doc("Name of the field being accessed."))
      .def_property_readonly(
          "field_type",
          [](const field_access& self) {
            const field* field = self.type().field_by_name(self.field_name());
            WF_ASSERT(field != nullptr, "Missing field: {}", self.field_name());
            return field->type();
          },
          py::doc("The type of the field being accessed."))
      .def("__repr__", [](const field_access& self) {
        return fmt::format("FieldAccess({})", self.field_name());
      });

  py::class_<matrix_access>(m, "MatrixAccess")
      .def_property_readonly("indices", &matrix_access::indices, py::doc("Access "))
      .def_property_readonly("row", &matrix_access::row, py::doc("Row index."))
      .def_property_readonly("col", &matrix_access::col, py::doc("Col index."))
      .def("__repr__", [](const matrix_access& self) {
        return fmt::format("MatrixAccess({}, {})", self.row(), self.col());
      });
}

}  // namespace wf
