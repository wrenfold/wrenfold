// Copyright 2023 Gareth Cross

#define PYBIND11_DETAILED_ERROR_MESSAGES

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "ast.h"
#include "code_generation.h"
#include "expression.h"

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

  py::class_<ir::Value>(m, "IrValue").def("__repr__", [](const ir::Value& v) {
    return fmt::format("IrValue({})", v.Id());
  });

  // TODO: Not sure if this needs to be wrapped, but it might be convenient for now.
  py::class_<IrBuilder>(m, "IrBuilder")
      // Construct from a vector of expressions:
      .def(py::init<const std::vector<Expr>&>())
      .def("output_values", &IrBuilder::OutputValues, py::doc("Retrieve list of output values."))
      .def("format_ir", &IrBuilder::FormatIR,
           py::doc("Format the IR for a specific output value into a string."))
      .def("create_expression", &IrBuilder::CreateExpression,
           py::doc("Rebuild the expression tree for a given IR value."))
      .def("eliminate_duplicates", &IrBuilder::EliminateDuplicates,
           py::doc("Eliminate duplicate sub-expressions."))
      .def("create_ast", &IrBuilder::CreateAST);

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

  py::class_<ast::ScalarType>(m, "ScalarType")
      .def(py::init<>())
      .def("__repr__", &ast::ScalarType::ToString);

  py::class_<ast::MatrixType>(m, "MatrixType")
      .def(py::init<index_t, index_t>(), py::arg("rows"), py::arg("cols"))
      .def_property_readonly("num_rows", &ast::MatrixType::NumRows)
      .def_property_readonly("num_cols", &ast::MatrixType::NumCols)
      .def("compute_indices", &ast::MatrixType::ComputeIndices)
      .def("__repr__", &ast::MatrixType::ToString);

  py::class_<ast::VariableRef>(m, "VariableRef")
      .def_property_readonly("name", [](const ast::VariableRef& v) { return v.name; })
      .def("__repr__", &ast::VariableRef::ToString);

  py::class_<ast::FunctionSignature>(m, "FunctionSignature")
      .def(py::init<std::string>(), py::arg("name"), py::doc("Construct with string name."))
      .def_property_readonly("name",
                             [](const ast::FunctionSignature& s) { return s.function_name; })
      .def("__repr__",
           [](const ast::FunctionSignature& s) {
             return fmt::format("FunctionSignature('{}', {} args)", s.function_name,
                                s.input_args.size() + s.output_args.size());
           })
      .def(
          "add_input_arg",
          [](ast::FunctionSignature& self, const std::string_view name, ast::Type type) {
            self.AddInput(name, std::move(type), false);
          },
          py::arg("name"), py::arg("type"))
      .def(
          "add_output_arg",
          [](ast::FunctionSignature& self, const std::string_view name, ast::Type type,
             bool optional) { return self.AddOutput(name, std::move(type), optional); },
          py::arg("name"), py::arg("type"), py::arg("optional") = false)
      .def("add_return_value", &ast::FunctionSignature::AddReturnValue, py::arg("type"))
      .def_property_readonly("input_args",
                             [](const ast::FunctionSignature& self) { return self.input_args; })
      .def_property_readonly("output_args",
                             [](const ast::FunctionSignature& self) { return self.output_args; })
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
      .def_property_readonly("right", [](const ast::Add& x) { return *x.right; })
      .def("__repr__", &ast::Add::ToString);

  //  py::class_<ast::Assignment>(m, "Assignment")
  //      .def_property_readonly("left", [](const ast::Assignment& x) { return *x.left; })
  //      .def_property_readonly("right", [](const ast::Assignment& x) { return *x.right; })
  //      .def("__repr__", &ast::Assignment::ToString);

  py::class_<ast::Call>(m, "Call")
      .def_property_readonly("function", [](const ast::Call& c) { return c.function; })
      .def_property_readonly("args", [](const ast::Call& c) { return c.args; })
      .def("__repr__", &ast::Call::ToString);

  py::class_<ast::Conditional>(m, "Conditional")
      .def_property_readonly("condition", [](const ast::Conditional& c) { return *c.condition; })
      .def_property_readonly("if_branch", [](const ast::Conditional& c) { return c.if_branch; })
      .def_property_readonly("else_branch", [](const ast::Conditional& c) { return c.else_branch; })
      .def("__repr__", &ast::Conditional::ToString);

  py::class_<ast::ConstructMatrix>(m, "ConstructMatrix")
      .def_property_readonly("type", [](const ast::ConstructMatrix& c) { return c.type; })
      .def_property_readonly("args", [](const ast::ConstructMatrix& c) { return c.args; })
      .def("__repr__", &ast::ConstructMatrix::ToString);

  py::class_<ast::Declaration>(m, "Declaration")
      .def_property_readonly("name", [](const ast::Declaration& d) { return d.name; })
      .def_property_readonly("type", [](const ast::Declaration& d) { return d.type; })
      .def_property_readonly("value", [](const ast::Declaration& d) { return *d.value; })
      .def("__repr__", &ast::Declaration::ToString);

  py::class_<ast::FloatConstant>(m, "FloatConstant")
      .def_property_readonly("value", [](const ast::FloatConstant& f) { return f.value; })
      .def("__repr__", &ast::FloatConstant::ToString);

  py::class_<ast::InputValue>(m, "InputValue")
      .def_property_readonly("argument", [](const ast::InputValue& v) { return v.argument; })
      .def_property_readonly("element", [](const ast::InputValue& v) { return v.element; })
      .def("__repr__", &ast::InputValue::ToString);

  py::class_<ast::IntegerConstant>(m, "IntegerConstant")
      .def_property_readonly("value", [](const ast::IntegerConstant& i) { return i.value; })
      .def("__repr__", &ast::IntegerConstant::ToString);

  py::class_<ast::Multiply>(m, "Multiply")
      .def_property_readonly("left", [](const ast::Multiply& x) { return *x.left; })
      .def_property_readonly("right", [](const ast::Multiply& x) { return *x.right; })
      .def("__repr__", &ast::Multiply::ToString);

  py::class_<ast::OutputExists>(m, "OutputExists")
      .def_property_readonly("argument", [](const ast::OutputExists& b) { return b.argument; })
      .def("__repr__", &ast::OutputExists::ToString);

  py::class_<ast::ReturnValue>(m, "ReturnValue")
      .def_property_readonly("values", [](const ast::ReturnValue& v) { return v.values; })
      .def("__repr__", &ast::ReturnValue::ToString);

}  // PYBIND11_MODULE
