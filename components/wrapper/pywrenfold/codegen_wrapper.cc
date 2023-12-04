// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/code_generation/ast.h"
#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/cpp_code_generator.h"
#include "wf/code_generation/expression_group.h"
#include "wf/code_generation/ir_builder.h"
#include "wf/code_generation/rust_code_generator.h"
#include "wf/expression.h"
#include "wf/matrix_expression.h"

namespace py = pybind11;
using namespace py::literals;

// We make this type opaque and wrap it manually below.
// This allows us to avoid problems from variant not being default constructible.
PYBIND11_MAKE_OPAQUE(std::vector<wf::ast::variant>)

namespace wf {

// Pybind11 requires that std::variant be default-constructible.
// We have to allow monostate to achieve this.
static ast::argument_type type_from_default_constructible_variant(
    const std::variant<std::monostate, ast::scalar_type, ast::matrix_type>& variant) {
  return std::visit(
      [](const auto& element) -> ast::argument_type {
        using T = std::decay_t<decltype(element)>;
        if constexpr (std::is_same_v<T, std::monostate>) {
          throw type_error("`type` cannot be None");
        } else {
          return element;
        }
      },
      variant);
}

template <class T>
static std::string format_ast(const T& x) {
  return fmt::format("{}", x);
}

void wrap_codegen_operations(py::module_& m) {
  m.def(
      "create_function_argument",
      [](std::size_t index) { return variable::create_function_argument(index, 0); },
      py::arg("index"));
  m.def(
      "create_matrix_function_argument",
      [](std::size_t index, index_t rows, index_t cols) {
        std::vector<Expr> expressions{};
        expressions.reserve(static_cast<std::size_t>(rows * cols));
        for (std::size_t i = 0; i < rows * cols; ++i) {
          expressions.push_back(variable::create_function_argument(index, i));
        }
        return MatrixExpr::create(rows, cols, std::move(expressions));
      },
      py::arg("index"), py::arg("rows"), py::arg("cols"));

  // Stored as shared-ptr to avoid copies.
  py::class_<std::vector<ast::variant>, std::shared_ptr<std::vector<ast::variant>>>(m, "AstVector")
      .def("__repr__",
           [](const std::vector<ast::variant>& vec) {
             return fmt::format("AstVector({} elements)", vec.size());
           })
      .def("__len__", [](const std::vector<ast::variant>& vec) { return vec.size(); })
      .def(
          "__iter__",
          [](const std::vector<ast::variant>& vec) {
            return py::make_iterator(vec.begin(), vec.end());
          },
          py::keep_alive<0, 1>());

  m.def(
      "generate_func",
      [](const ast::function_signature& signature,
         const std::vector<expression_group>& expressions) {
        flat_ir ir{expressions};
        ir.eliminate_duplicates();
        output_ir output_ir{std::move(ir)};
        return ast::create_ast(output_ir, signature);
      },
      py::arg("signature"), py::arg("expressions"),
      py::doc("Generate function body AST from signature and output expressions."),
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
      .value("Real", code_numeric_type::floating_point)
      .value("Complex", code_numeric_type::complex);

  py::enum_<relational_operation>(m, "RelationalOperation")
      .value("LessThan", relational_operation::less_than)
      .value("LessThanOrEqual", relational_operation::less_than_or_equal)
      .value("Equal", relational_operation::equal);

  py::enum_<ast::argument_direction>(m, "ArgumentDirection")
      .value("Input", ast::argument_direction::input)
      .value("Output", ast::argument_direction::output)
      .value("OptionalOutput", ast::argument_direction::optional_output);

  py::class_<ast::scalar_type>(m, "ScalarType")
      .def(py::init<code_numeric_type>())
      .def_property_readonly("numeric_type", &ast::scalar_type::numeric_type)
      .def("__repr__", &format_ast<ast::scalar_type>);

  py::class_<ast::matrix_type>(m, "MatrixType")
      .def(py::init<index_t, index_t>(), py::arg("rows"), py::arg("cols"))
      .def_property_readonly("num_rows", &ast::matrix_type::rows)
      .def_property_readonly("num_cols", &ast::matrix_type::cols)
      .def("compute_indices", &ast::matrix_type::compute_indices)
      .def("__repr__", &format_ast<ast::matrix_type>);

  py::class_<ast::variable_ref>(m, "VariableRef")
      .def_property_readonly("name", [](const ast::variable_ref& v) { return v.name; })
      .def("__repr__", &format_ast<ast::variable_ref>);

  py::class_<ast::function_signature>(m, "FunctionSignature")
      .def(py::init<std::string>(), py::arg("name"), py::doc("Construct with string name."))
      .def_property_readonly("name",
                             [](const ast::function_signature& s) { return s.function_name; })
      .def("__repr__",
           [](const ast::function_signature& s) {
             return fmt::format("FunctionSignature('{}', {} args)", s.function_name,
                                s.arguments.size());
           })
      .def(
          "add_argument",
          [](ast::function_signature& self, const std::string_view name,
             const std::variant<std::monostate, ast::scalar_type, ast::matrix_type>& type,
             ast::argument_direction direction) {
            return self.add_argument(name, type_from_default_constructible_variant(type),
                                     direction);
          },
          py::arg("name"), py::arg("type"), py::arg("direction"))
      .def(
          "set_return_type",
          [](ast::function_signature& self,
             const std::variant<std::monostate, ast::scalar_type, ast::matrix_type>& type) {
            self.return_value = type_from_default_constructible_variant(type);
          },
          py::arg("type"))
      .def_property_readonly("return_type",
                             [](const ast::function_signature& self) { return self.return_value; })
      .def_property_readonly("arguments",
                             [](const ast::function_signature& self) { return self.arguments; });

  // Use std::shared_ptr to store argument, since this is what ast::function_signature uses.
  // If we don't do this, we might free something incorrectly when accessing arguments.
  py::class_<ast::argument, std::shared_ptr<ast::argument>>(m, "Argument")
      .def_property_readonly("name", &ast::argument::name)
      .def_property_readonly("type", &ast::argument::type)
      .def_property_readonly("is_optional", &ast::argument::is_optional)
      .def("__repr__",
           [](const ast::argument& self) { return fmt::format("Argument('{}')", self.name()); });

  py::class_<ast::add>(m, "Add")
      .def_property_readonly("left", [](const ast::add& x) { return *x.left; })
      .def_property_readonly("right", [](const ast::add& x) { return *x.right; })
      .def("__repr__", &format_ast<ast::add>);

  py::class_<ast::assign_temporary>(m, "AssignTemporary")
      .def_property_readonly("left", [](const ast::assign_temporary& x) { return x.left; })
      .def_property_readonly("right",
                             [](const ast::assign_temporary& x) {
                               WF_ASSERT(x.right);
                               return *x.right;
                             })
      .def("__repr__", &format_ast<ast::assign_temporary>);

  py::class_<ast::assign_output_argument>(m, "AssignOutputArgument")
      .def_property_readonly("argument",
                             [](const ast::assign_output_argument& x) {
                               WF_ASSERT(x.arg);
                               return *x.arg;
                             })
      .def_property_readonly("values",
                             [](const ast::assign_output_argument& x) { return x.values; })
      .def("__repr__", &format_ast<ast::assign_output_argument>);

  py::class_<ast::branch>(m, "Branch")
      .def_property_readonly("condition", [](const ast::branch& c) { return *c.condition; })
      .def_property_readonly("if_branch", [](const ast::branch& c) { return c.if_branch; })
      .def_property_readonly("else_branch", [](const ast::branch& c) { return c.else_branch; })
      .def("__repr__", &format_ast<ast::branch>);

  py::class_<ast::call>(m, "Call")
      .def_property_readonly("function", [](const ast::call& c) { return c.function; })
      .def_property_readonly("args", [](const ast::call& c) { return c.args; })
      .def("__repr__", &format_ast<ast::call>);

  py::class_<ast::cast>(m, "Cast")
      .def_property_readonly("destination_type",
                             [](const ast::cast& c) { return c.destination_type; })
      .def_property_readonly("arg",
                             [](const ast::cast& c) {
                               WF_ASSERT(c.arg);
                               return *c.arg;
                             })
      .def("__repr__", &format_ast<ast::cast>);

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
      .def("__repr__", &format_ast<ast::compare>);

  py::class_<ast::construct_return_value>(m, "ConstructReturnValue")
      .def_property_readonly("type", [](const ast::construct_return_value& c) { return c.type; })
      .def_property_readonly("args", [](const ast::construct_return_value& c) { return c.args; })
      .def("__repr__", &format_ast<ast::construct_return_value>);

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
      .def("__repr__", &format_ast<ast::declaration>);

  py::class_<ast::float_literal>(m, "FloatConstant")
      .def_property_readonly("value", [](const ast::float_literal& f) { return f.value; })
      .def("__repr__", &format_ast<ast::float_literal>);

  py::class_<ast::input_value>(m, "InputValue")
      .def_property_readonly("argument", [](const ast::input_value& v) { return v.arg; })
      .def_property_readonly("element", [](const ast::input_value& v) { return v.element; })
      .def("__repr__", &format_ast<ast::input_value>);

  py::class_<ast::integer_literal>(m, "IntegerConstant")
      .def_property_readonly("value", [](const ast::integer_literal& i) { return i.value; })
      .def("__repr__", &format_ast<ast::integer_literal>);

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
      .def("__repr__", &format_ast<ast::multiply>);

  py::class_<ast::optional_output_branch>(m, "OptionalOutputBranch")
      .def_property_readonly("argument", [](const ast::optional_output_branch& b) { return b.arg; })
      .def_property_readonly("statements",
                             [](const ast::optional_output_branch& b) { return b.statements; })
      .def("__repr__", &format_ast<ast::optional_output_branch>);

  m.def(
      "generate_cpp",
      [](const ast::function_signature& signature, const std::vector<ast::variant>& ast)
          -> std::string { return cpp_code_generator{}.generate_code(signature, ast); },
      "signature"_a, "ast"_a,
      py::doc("Generate C++ code from the given function signature and expressions."));

  m.def(
      "generate_rust",
      [](const ast::function_signature& signature, const std::vector<ast::variant>& ast)
          -> std::string { return rust_code_generator{}.generate_code(signature, ast); },
      "signature"_a, "ast"_a,
      py::doc("Generate Rust code from the given function signature and expressions."));
}

}  // namespace wf
