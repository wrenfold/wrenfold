// Copyright 2023 Gareth Cross

#define PYBIND11_DETAILED_ERROR_MESSAGES

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "code_generation/ast.h"
#include "code_generation/expression_group.h"
#include "code_generation/ir_builder.h"
#include "expression.h"
#include "expressions/function_argument.h"
#include "matrix_expression.h"

namespace py = pybind11;
using namespace py::literals;

using namespace math;

PYBIND11_MODULE(pycodegen, m) {
  m.def(
      "create_function_argument",
      [](std::size_t index) { return FunctionArgument::Create(index, 0); }, py::arg("index"));
  m.def(
      "create_matrix_function_argument",
      [](std::size_t index, index_t rows, index_t cols) {
        std::vector<Expr> expressions{};
        expressions.reserve(static_cast<std::size_t>(rows * cols));
        for (std::size_t i = 0; i < rows * cols; ++i) {
          expressions.push_back(FunctionArgument::Create(index, i));
        }
        return MatrixExpr::Create(rows, cols, std::move(expressions));
      },
      py::arg("index"), py::arg("rows"), py::arg("cols"));

  m.def(
      "generate_func",
      [](const ast::FunctionSignature& signature, const std::vector<ExpressionGroup>& expressions) {
        FlatIr ir{expressions};
        ir.EliminateDuplicates();
        OutputIr output_ir{std::move(ir)};
        return ast::CreateAST(output_ir, signature).body;
      },
      py::arg("signature"), py::arg("expressions"),
      py::doc("Generate function AST from signature and output expressions."));

  py::enum_<ExpressionUsage>(m, "ExpressionUsage")
      .value("OptionalOutputArgument", ExpressionUsage::OptionalOutputArgument)
      .value("OutputArgument", ExpressionUsage::OutputArgument)
      .value("ReturnValue", ExpressionUsage::ReturnValue);

  py::class_<OutputKey>(m, "OutputKey")
      .def(py::init<ExpressionUsage, std::size_t>(), py::arg("usage"), py::arg("arg_position"));

  py::class_<ExpressionGroup>(m, "ExpressionGroup")
      .def(py::init<std::vector<Expr>, OutputKey>(), py::arg("expressions"), py::arg("output_key"));

  py::enum_<UnaryFunctionName>(m, "UnaryFunctionName")
      .value("Cos", UnaryFunctionName::Cos)
      .value("Sin", UnaryFunctionName::Sin)
      .value("Tan", UnaryFunctionName::Tan)
      .value("ArcCos", UnaryFunctionName::ArcCos)
      .value("ArcSin", UnaryFunctionName::ArcSin)
      .value("ArcTan", UnaryFunctionName::ArcTan)
      .value("Log", UnaryFunctionName::Log)
      .value("Sqrt", UnaryFunctionName::Sqrt)
      .def(
          "to_string", [](UnaryFunctionName name) { return ToString(name); },
          py::doc("Convert to string."));

  py::enum_<BinaryFunctionName>(m, "BinaryFunctionName")
      .value("Mod", BinaryFunctionName::Mod)
      .value("Pow", BinaryFunctionName::Pow);

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
      .def_property_readonly("numeric_type", &ast::ScalarType::GetNumericType);

  py::class_<ast::MatrixType>(m, "MatrixType")
      .def(py::init<index_t, index_t>(), py::arg("rows"), py::arg("cols"))
      .def_property_readonly("num_rows", &ast::MatrixType::NumRows)
      .def_property_readonly("num_cols", &ast::MatrixType::NumCols)
      .def("compute_indices", &ast::MatrixType::ComputeIndices);
  //      .def("__repr__", &ast::MatrixType::ToString);

  py::class_<ast::VariableRef>(m, "VariableRef")
      .def_property_readonly("name", [](const ast::VariableRef& v) { return v.name; });
  //      .def("__repr__", &ast::VariableRef::ToString);

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
            // workaround for `type` requiring a default constructor:
            ast::Type type_casted = std::visit(
                [](const auto& element) -> ast::Type {
                  using T = std::decay_t<decltype(element)>;
                  if constexpr (std::is_same_v<T, std::monostate>) {
                    throw TypeError("`type` cannot be None");
                  } else {
                    return element;
                  }
                },
                type);
            return self.AddArgument(name, std::move(type_casted), direction);
          },
          py::arg("name"), py::arg("type"), py::arg("direction"))
      .def(
          "add_return_value",
          [](ast::FunctionSignature& self,
             const std::variant<std::monostate, ast::ScalarType, ast::MatrixType>& type) {
            // workaround for `type` requiring a default constructor:
            ast::Type type_casted = std::visit(
                [](const auto& element) -> ast::Type {
                  using T = std::decay_t<decltype(element)>;
                  if constexpr (std::is_same_v<T, std::monostate>) {
                    throw TypeError("`type` cannot be None");
                  } else {
                    return element;
                  }
                },
                type);
            self.AddReturnValue(type_casted);
          },
          py::arg("type"))
      .def_property_readonly("arguments",
                             [](const ast::FunctionSignature& self) { return self.arguments; })
      .def_property_readonly("return_values",
                             [](const ast::FunctionSignature& self) { return self.return_values; });

  // Use std::shared_ptr to store argument, since this is what ast::FunctionSignature uses.
  // If we don't do this, we might free something incorrectly when accessing arguments.
  py::class_<ast::Argument, std::shared_ptr<ast::Argument>>(m, "Argument")
      .def_property_readonly("name", &ast::Argument::Name)
      .def_property_readonly("type", &ast::Argument::Type)
      .def_property_readonly("is_optional", &ast::Argument::IsOptional)
      .def("__repr__",
           [](const ast::Argument& self) { return fmt::format("Argument('{}')", self.Name()); });

  py::class_<ast::Add>(m, "Add")
      .def_property_readonly("left", [](const ast::Add& x) { return *x.left; })
      .def_property_readonly("right", [](const ast::Add& x) { return *x.right; });
  //      .def("__repr__", &ast::Add::ToString);

  py::class_<ast::AssignTemporary>(m, "AssignTemporary")
      .def_property_readonly("left", [](const ast::AssignTemporary& x) { return x.left; })
      .def_property_readonly("right", [](const ast::AssignTemporary& x) {
        ASSERT(x.right);
        return *x.right;
      });

  py::class_<ast::AssignOutputArgument>(m, "AssignOutputArgument")
      .def_property_readonly("argument",
                             [](const ast::AssignOutputArgument& x) {
                               ASSERT(x.argument);
                               return *x.argument;
                             })
      .def_property_readonly("values", [](const ast::AssignOutputArgument& x) { return x.values; });

  py::class_<ast::Branch>(m, "Branch")
      .def_property_readonly("condition", [](const ast::Branch& c) { return *c.condition; })
      .def_property_readonly("if_branch", [](const ast::Branch& c) { return c.if_branch; })
      .def_property_readonly("else_branch", [](const ast::Branch& c) { return c.else_branch; });
  //      .def("__repr__", &ast::Branch::ToString);

  py::class_<ast::Call>(m, "Call")
      .def_property_readonly("function", [](const ast::Call& c) { return c.function; })
      .def_property_readonly("args", [](const ast::Call& c) { return c.args; });
  //      .def("__repr__", &ast::Call::ToString);

  py::class_<ast::Cast>(m, "Cast")
      .def_property_readonly("destination_type",
                             [](const ast::Cast& c) { return c.destination_type; })
      .def_property_readonly("arg", [](const ast::Cast& c) {
        ASSERT(c.arg);
        return *c.arg;
      });
  //      .def("__repr__", &ast::Call::ToString);

  py::class_<ast::Compare>(m, "Compare")
      .def_property_readonly("left",
                             [](const ast::Compare& c) {
                               ASSERT(c.left);
                               return *c.left;
                             })
      .def_property_readonly("right",
                             [](const ast::Compare& c) {
                               ASSERT(c.right);
                               return *c.right;
                             })
      .def_property_readonly("operation", [](const ast::Compare& c) { return c.operation; });

  py::class_<ast::ConstructReturnValue>(m, "ConstructReturnValue")
      .def_property_readonly("position",
                             [](const ast::ConstructReturnValue& c) { return c.position; })
      .def_property_readonly("type", [](const ast::ConstructReturnValue& c) { return c.type; })
      .def_property_readonly("args", [](const ast::ConstructReturnValue& c) { return c.args; });

  py::class_<ast::Declaration>(m, "Declaration")
      .def_property_readonly("name", [](const ast::Declaration& d) { return d.name; })
      .def_property_readonly("type", [](const ast::Declaration& d) { return d.type; })
      .def_property_readonly("value", [](const ast::Declaration& d) -> std::optional<ast::Variant> {
        if (d.value) {
          return *d.value;
        } else {
          return std::nullopt;
        }
      });
  //      .def("__repr__", &ast::Declaration::ToString);

  py::class_<ast::FloatConstant>(m, "FloatConstant")
      .def_property_readonly("value", [](const ast::FloatConstant& f) { return f.value; });
  //      .def("__repr__", &ast::FloatConstant::ToString);

  py::class_<ast::InputValue>(m, "InputValue")
      .def_property_readonly("argument", [](const ast::InputValue& v) { return v.argument; })
      .def_property_readonly("element", [](const ast::InputValue& v) { return v.element; });
  //      .def("__repr__", &ast::InputValue::ToString);

  py::class_<ast::IntegerConstant>(m, "IntegerConstant")
      .def_property_readonly("value", [](const ast::IntegerConstant& i) { return i.value; });
  //      .def("__repr__", &ast::IntegerConstant::ToString);

  py::class_<ast::Multiply>(m, "Multiply")
      .def_property_readonly("left",
                             [](const ast::Multiply& x) {
                               ASSERT(x.left);
                               return *x.left;
                             })
      .def_property_readonly("right", [](const ast::Multiply& x) {
        ASSERT(x.right);
        return *x.right;
      });
  //      .def("__repr__", &ast::Multiply::ToString);

  py::class_<ast::OutputExists>(m, "OutputExists")
      .def_property_readonly("argument", [](const ast::OutputExists& b) { return b.argument; });
  //      .def("__repr__", &ast::OutputExists::ToString);

  py::class_<ast::ReturnValue>(m, "ReturnValue")
      .def_property_readonly("values", [](const ast::ReturnValue& v) { return v.values; });
  //      .def("__repr__", &ast::ReturnValue::ToString);

}  // PYBIND11_MODULE
