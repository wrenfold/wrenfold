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
// This allows us to avoid problems from Variant not being default constructible.
PYBIND11_MAKE_OPAQUE(std::vector<math::ast::Variant>)

namespace math {

// Pybind11 requires that std::variant be default-constructible.
// We have to allow monostate to achieve this.
static ast::Type type_from_default_constructible_variant(
    const std::variant<std::monostate, ast::ScalarType, ast::MatrixType>& variant) {
  return std::visit(
      [](const auto& element) -> ast::Type {
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
      [](std::size_t index) { return Variable::create_function_argument(index, 0); },
      py::arg("index"));
  m.def(
      "create_matrix_function_argument",
      [](std::size_t index, index_t rows, index_t cols) {
        std::vector<Expr> expressions{};
        expressions.reserve(static_cast<std::size_t>(rows * cols));
        for (std::size_t i = 0; i < rows * cols; ++i) {
          expressions.push_back(Variable::create_function_argument(index, i));
        }
        return MatrixExpr::create(rows, cols, std::move(expressions));
      },
      py::arg("index"), py::arg("rows"), py::arg("cols"));

  // Stored as shared-ptr to avoid copies.
  py::class_<std::vector<ast::Variant>, std::shared_ptr<std::vector<ast::Variant>>>(m, "AstVector")
      .def("__repr__",
           [](const std::vector<ast::Variant>& vec) {
             return fmt::format("AstVector({} elements)", vec.size());
           })
      .def("__len__", [](const std::vector<ast::Variant>& vec) { return vec.size(); })
      .def(
          "__iter__",
          [](const std::vector<ast::Variant>& vec) {
            return py::make_iterator(vec.begin(), vec.end());
          },
          py::keep_alive<0, 1>());

  m.def(
      "generate_func",
      [](const ast::FunctionSignature& signature, const std::vector<ExpressionGroup>& expressions) {
        FlatIr ir{expressions};
        ir.eliminate_duplicates();
        OutputIr output_ir{std::move(ir)};
        return ast::create_ast(output_ir, signature);
      },
      py::arg("signature"), py::arg("expressions"),
      py::doc("Generate function body AST from signature and output expressions."),
      py::return_value_policy::take_ownership);

  py::enum_<ExpressionUsage>(m, "ExpressionUsage")
      .value("OptionalOutputArgument", ExpressionUsage::OptionalOutputArgument)
      .value("OutputArgument", ExpressionUsage::OutputArgument)
      .value("ReturnValue", ExpressionUsage::ReturnValue);

  py::class_<OutputKey>(m, "OutputKey")
      .def(py::init<ExpressionUsage, std::string_view>(), py::arg("usage"), py::arg("name"));

  py::class_<ExpressionGroup>(m, "ExpressionGroup")
      .def(py::init<std::vector<Expr>, OutputKey>(), py::arg("expressions"), py::arg("output_key"))
      .def(py::init([](const MatrixExpr& m, OutputKey key) {
             return ExpressionGroup(m.to_vector(), key);
           }),
           py::arg("expressions"), py::arg("output_key"));

  py::enum_<StdMathFunction>(m, "StdMathFunction")
      .value("Cos", StdMathFunction::Cos)
      .value("Sin", StdMathFunction::Sin)
      .value("Tan", StdMathFunction::Tan)
      .value("ArcCos", StdMathFunction::ArcCos)
      .value("ArcSin", StdMathFunction::ArcSin)
      .value("ArcTan", StdMathFunction::ArcTan)
      .value("Log", StdMathFunction::Log)
      .value("Sqrt", StdMathFunction::Sqrt)
      .value("Abs", StdMathFunction::Abs)
      .value("Signum", StdMathFunction::Signum)
      .value("Arctan2", StdMathFunction::Arctan2)
      .value("Powi", StdMathFunction::Powi)
      .value("Powf", StdMathFunction::Powf)
      .def(
          "to_string",
          [](StdMathFunction name) { return string_from_standard_library_function(name); },
          py::doc("Convert to string."));

  py::enum_<NumericType>(m, "NumericType")
      .value("Bool", NumericType::Bool)
      .value("Integer", NumericType::Integer)
      .value("Real", NumericType::Real)
      .value("Complex", NumericType::Complex);

  py::enum_<RelationalOperation>(m, "RelationalOperation")
      .value("LessThan", RelationalOperation::LessThan)
      .value("LessThanOrEqual", RelationalOperation::LessThanOrEqual)
      .value("Equal", RelationalOperation::Equal);

  py::enum_<ast::ArgumentDirection>(m, "ArgumentDirection")
      .value("Input", ast::ArgumentDirection::Input)
      .value("Output", ast::ArgumentDirection::Output)
      .value("OptionalOutput", ast::ArgumentDirection::OptionalOutput);

  py::class_<ast::ScalarType>(m, "ScalarType")
      .def(py::init<NumericType>())
      .def_property_readonly("numeric_type", &ast::ScalarType::numeric_type)
      .def("__repr__", &format_ast<ast::ScalarType>);

  py::class_<ast::MatrixType>(m, "MatrixType")
      .def(py::init<index_t, index_t>(), py::arg("rows"), py::arg("cols"))
      .def_property_readonly("num_rows", &ast::MatrixType::rows)
      .def_property_readonly("num_cols", &ast::MatrixType::cols)
      .def("compute_indices", &ast::MatrixType::compute_indices)
      .def("__repr__", &format_ast<ast::MatrixType>);

  py::class_<ast::VariableRef>(m, "VariableRef")
      .def_property_readonly("name", [](const ast::VariableRef& v) { return v.name; })
      .def("__repr__", &format_ast<ast::VariableRef>);

  py::class_<ast::FunctionSignature>(m, "FunctionSignature")
      .def(py::init<std::string>(), py::arg("name"), py::doc("Construct with string name."))
      .def_property_readonly("name",
                             [](const ast::FunctionSignature& s) { return s.function_name; })
      .def("__repr__",
           [](const ast::FunctionSignature& s) {
             return fmt::format("FunctionSignature('{}', {} args)", s.function_name,
                                s.arguments.size());
           })
      .def(
          "add_argument",
          [](ast::FunctionSignature& self, const std::string_view name,
             const std::variant<std::monostate, ast::ScalarType, ast::MatrixType>& type,
             ast::ArgumentDirection direction) {
            return self.add_argument(name, type_from_default_constructible_variant(type),
                                     direction);
          },
          py::arg("name"), py::arg("type"), py::arg("direction"))
      .def(
          "set_return_type",
          [](ast::FunctionSignature& self,
             const std::variant<std::monostate, ast::ScalarType, ast::MatrixType>& type) {
            self.return_value = type_from_default_constructible_variant(type);
          },
          py::arg("type"))
      .def_property_readonly("return_type",
                             [](const ast::FunctionSignature& self) { return self.return_value; })
      .def_property_readonly("arguments",
                             [](const ast::FunctionSignature& self) { return self.arguments; });

  // Use std::shared_ptr to store argument, since this is what ast::FunctionSignature uses.
  // If we don't do this, we might free something incorrectly when accessing arguments.
  py::class_<ast::Argument, std::shared_ptr<ast::Argument>>(m, "Argument")
      .def_property_readonly("name", &ast::Argument::name)
      .def_property_readonly("type", &ast::Argument::type)
      .def_property_readonly("is_optional", &ast::Argument::is_optional)
      .def("__repr__",
           [](const ast::Argument& self) { return fmt::format("Argument('{}')", self.name()); });

  py::class_<ast::Add>(m, "Add")
      .def_property_readonly("left", [](const ast::Add& x) { return *x.left; })
      .def_property_readonly("right", [](const ast::Add& x) { return *x.right; })
      .def("__repr__", &format_ast<ast::Add>);

  py::class_<ast::AssignTemporary>(m, "AssignTemporary")
      .def_property_readonly("left", [](const ast::AssignTemporary& x) { return x.left; })
      .def_property_readonly("right",
                             [](const ast::AssignTemporary& x) {
                               WF_ASSERT(x.right);
                               return *x.right;
                             })
      .def("__repr__", &format_ast<ast::AssignTemporary>);

  py::class_<ast::AssignOutputArgument>(m, "AssignOutputArgument")
      .def_property_readonly("argument",
                             [](const ast::AssignOutputArgument& x) {
                               WF_ASSERT(x.argument);
                               return *x.argument;
                             })
      .def_property_readonly("values", [](const ast::AssignOutputArgument& x) { return x.values; })
      .def("__repr__", &format_ast<ast::AssignOutputArgument>);

  py::class_<ast::Branch>(m, "Branch")
      .def_property_readonly("condition", [](const ast::Branch& c) { return *c.condition; })
      .def_property_readonly("if_branch", [](const ast::Branch& c) { return c.if_branch; })
      .def_property_readonly("else_branch", [](const ast::Branch& c) { return c.else_branch; })
      .def("__repr__", &format_ast<ast::Branch>);

  py::class_<ast::Call>(m, "Call")
      .def_property_readonly("function", [](const ast::Call& c) { return c.function; })
      .def_property_readonly("args", [](const ast::Call& c) { return c.args; })
      .def("__repr__", &format_ast<ast::Call>);

  py::class_<ast::Cast>(m, "Cast")
      .def_property_readonly("destination_type",
                             [](const ast::Cast& c) { return c.destination_type; })
      .def_property_readonly("arg",
                             [](const ast::Cast& c) {
                               WF_ASSERT(c.arg);
                               return *c.arg;
                             })
      .def("__repr__", &format_ast<ast::Cast>);

  py::class_<ast::Compare>(m, "Compare")
      .def_property_readonly("left",
                             [](const ast::Compare& c) {
                               WF_ASSERT(c.left);
                               return *c.left;
                             })
      .def_property_readonly("right",
                             [](const ast::Compare& c) {
                               WF_ASSERT(c.right);
                               return *c.right;
                             })
      .def_property_readonly("operation", [](const ast::Compare& c) { return c.operation; })
      .def("__repr__", &format_ast<ast::Compare>);

  py::class_<ast::ConstructReturnValue>(m, "ConstructReturnValue")
      .def_property_readonly("type", [](const ast::ConstructReturnValue& c) { return c.type; })
      .def_property_readonly("args", [](const ast::ConstructReturnValue& c) { return c.args; })
      .def("__repr__", &format_ast<ast::ConstructReturnValue>);

  py::class_<ast::Declaration>(m, "Declaration")
      .def_property_readonly("name", [](const ast::Declaration& d) { return d.name; })
      .def_property_readonly("type", [](const ast::Declaration& d) { return d.type; })
      .def_property_readonly("value",
                             [](const ast::Declaration& d) -> std::optional<ast::Variant> {
                               if (d.value) {
                                 return *d.value;
                               } else {
                                 return std::nullopt;
                               }
                             })
      .def("__repr__", &format_ast<ast::Declaration>);

  py::class_<ast::FloatConstant>(m, "FloatConstant")
      .def_property_readonly("value", [](const ast::FloatConstant& f) { return f.value; })
      .def("__repr__", &format_ast<ast::FloatConstant>);

  py::class_<ast::InputValue>(m, "InputValue")
      .def_property_readonly("argument", [](const ast::InputValue& v) { return v.argument; })
      .def_property_readonly("element", [](const ast::InputValue& v) { return v.element; })
      .def("__repr__", &format_ast<ast::InputValue>);

  py::class_<ast::IntegerConstant>(m, "IntegerConstant")
      .def_property_readonly("value", [](const ast::IntegerConstant& i) { return i.value; })
      .def("__repr__", &format_ast<ast::IntegerConstant>);

  py::class_<ast::Multiply>(m, "Multiply")
      .def_property_readonly("left",
                             [](const ast::Multiply& x) {
                               WF_ASSERT(x.left);
                               return *x.left;
                             })
      .def_property_readonly("right",
                             [](const ast::Multiply& x) {
                               WF_ASSERT(x.right);
                               return *x.right;
                             })
      .def("__repr__", &format_ast<ast::Multiply>);

  py::class_<ast::OptionalOutputBranch>(m, "OptionalOutputBranch")
      .def_property_readonly("argument",
                             [](const ast::OptionalOutputBranch& b) { return b.argument; })
      .def_property_readonly("statements",
                             [](const ast::OptionalOutputBranch& b) { return b.statements; })
      .def("__repr__", &format_ast<ast::OptionalOutputBranch>);

  m.def(
      "generate_cpp",
      [](const ast::FunctionSignature& signature, const std::vector<ast::Variant>& ast)
          -> std::string { return CppCodeGenerator{}.generate_code(signature, ast); },
      "signature"_a, "ast"_a,
      py::doc("Generate C++ code from the given function signature and expressions."));

  m.def(
      "generate_rust",
      [](const ast::FunctionSignature& signature, const std::vector<ast::Variant>& ast)
          -> std::string { return RustCodeGenerator{}.generate_code(signature, ast); },
      "signature"_a, "ast"_a,
      py::doc("Generate Rust code from the given function signature and expressions."));
}

}  // namespace math